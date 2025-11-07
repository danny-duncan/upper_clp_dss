plot_train_test_ensemble_acc <- function(models, voting_method = "soft", train_df,test_df,
                                         target_col = "TOC_cat", units = "mg/L",title = "",
                                         class_levels = c("0-2", "2-4", "4-8", "8+")) {

  pred_col = paste0(target_col, "_guess")

  if(!models[[1]]$params$objective %in% c("multi:softmax", "multi:softprob")) {
    stop("Only multi-class classification objectives 'multi:softmax' and 'multi:softprob' are supported.")
  }else{

    num_class <- length(class_levels)

    # --- Collect predictions from each model ---
    pred_list <- map(models, ~{
      fold_model <- .x

      if (fold_model$params$objective == "multi:softprob") {
        # softprob → probabilities per class
        train_preds <- predict(fold_model, as.matrix(train_df[, fold_model$feature_names]))
        train_mat <- matrix(train_preds, ncol = num_class, byrow = TRUE)
        train_probs <- as_tibble(train_mat, .name_repair = ~paste0("class_", seq_len(num_class))) %>%
          mutate(pred_class = max.col(train_mat) - 1,
                 max_prob = do.call(pmax, as.data.frame(train_mat)))

        test_preds <- predict(fold_model, as.matrix(test_df[, fold_model$feature_names]))
        test_mat <- matrix(test_preds, ncol = num_class, byrow = TRUE)
        test_probs <- as_tibble(test_mat, .name_repair = ~paste0("class_", seq_len(num_class))) %>%
          mutate(pred_class = max.col(test_mat) - 1,
                 max_prob = do.call(pmax, as.data.frame(test_mat)))

      } else {
        # softmax → direct class labels (no probabilities returned)
        train_pred_classes <- predict(fold_model, as.matrix(train_df[, fold_model$feature_names]))
        test_pred_classes  <- predict(fold_model, as.matrix(test_df[, fold_model$feature_names]))

        # Convert to consistent tibble format (dummy probs = NA)
        train_probs <- tibble(pred_class = train_pred_classes, max_prob = NA_real_)
        test_probs  <- tibble(pred_class = test_pred_classes,  max_prob = NA_real_)
      }

      list(train = train_probs, test = test_probs)
    })

    # --- Combine model predictions depending on voting type ---
    combine_predictions <- function(df_list, voting, num_class) {
      if (voting == "soft" && "class_1" %in% names(df_list[[1]])) {
        # Average probabilities across models, then choose class with max mean prob
        prob_array <- array(
          unlist(map(df_list, ~select(.x, starts_with("class_")))),
          dim = c(nrow(df_list[[1]]), num_class, length(df_list))
        )
        mean_probs <- apply(prob_array, c(1, 2), mean)
        pred_class <- max.col(mean_probs) - 1
        tibble(
          pred_class = pred_class,
          max_prob = do.call(pmax, as.data.frame(mean_probs))
        )
      } else {
        # Hard voting — majority vote among predicted classes
        class_mat <- map(df_list, "pred_class") %>% bind_cols()
        pred_class <- apply(class_mat, 1, function(x) {
          as.integer(names(sort(table(x), decreasing = TRUE)[1]))
        })
        tibble(pred_class = pred_class, max_prob = NA_real_)
      }
    }

    # --- Combine across models ---
    train_comb <- combine_predictions(map(pred_list, "train"), voting_method, num_class)
    test_comb  <- combine_predictions(map(pred_list, "test"), voting_method, num_class)

    # --- Bind predictions back to data ---
    train_df <- train_df %>%
      mutate(!!pred_col := train_comb$pred_class,
             !!paste0(pred_col, "_probs") := train_comb$max_prob)

    test_df <- test_df %>%
      mutate(!!pred_col := test_comb$pred_class,
             !!paste0(pred_col, "_probs") := test_comb$max_prob)
  }

  train_set <- train_df %>%
    mutate(target_cat = factor(!!sym(target_col),
                               levels = seq_along(class_levels) - 1,
                               labels = class_levels),

           pred_cat = factor(!!sym(pred_col),
                             levels = seq_along(class_levels) - 1,
                             labels = class_levels)
    )

  test_set <- test_df %>%
    mutate( target_cat = factor(!!sym(target_col),
                                levels = seq_along(class_levels) - 1,
                                labels = class_levels),

            pred_cat = factor(!!sym(pred_col),
                              levels = seq_along(class_levels) - 1,
                              labels = class_levels)
    )

  conf_plot <- function(pred_df,true_col = "target_cat", pred_col = "pred_cat", title = "") {
    # Align factor levels
    df <- pred_df %>%
      mutate(
        target_cat = as.factor(target_cat),
        pred_cat := factor(pred_cat, levels = levels(target_cat))
      )
    group_lookup <- tibble(groups = levels(pred_df$target_cat))%>%
      rownames_to_column()%>%
      mutate(rowname = as.numeric(rowname))

    # Generate confusion matrix
    conf_table <- df %>%
      yardstick::conf_mat(truth = target_cat, estimate = pred_cat)

    performance <- summary(conf_table)
    accuracy <- performance %>%
      filter(.metric == "accuracy") %>%
      pull(.estimate)
    kappa <- performance %>%
      filter(.metric == "kap") %>%
      pull(.estimate)
    f_score <- performance %>%
      filter(.metric == "f_meas") %>%
      pull(.estimate)

    conf_df <- conf_table%>%
      tidy()%>%
      mutate(Pred = as.numeric(str_split(name, "_", simplify = TRUE)[,2]),
             True = as.numeric(str_split(name, "_", simplify = TRUE)[,3]))%>%
      mutate(value_perc = replace_na((value / sum(value)),replace = 0), .by = "True") %>%
      #add in the labels for plotting
      left_join(group_lookup%>%select(rowname, groups)%>%rename(True = rowname, True_lab = groups), by = "True")%>%
      left_join(group_lookup%>%select(rowname, groups)%>%rename(Pred = rowname, Pred_lab = groups), by = "Pred")
    #calculate the accuracy per class for annotation
    perc_corr <- conf_df %>%
      group_by(True_lab)%>%
      summarize(percent_corr = mean(value[Pred_lab==True_lab])/ sum(value))%>%
      mutate(percent_corr = round(replace_na(percent_corr*100, 0), 1))%>%
      ungroup()
    #Create plot
    ggplot(conf_df, aes(x = True_lab, y = Pred_lab, fill = value_perc)) +
      geom_tile(size = 0.2, color = grey(0.5)) +
      geom_tile(
        data = filter(conf_df, True_lab == Pred_lab),
        aes(x = Pred_lab, y = True_lab),
        size = 1, color = "black", fill = "transparent"
      ) +
      geom_text(aes(label = value), size = 4, colour = "black") +
      geom_text(
        data = perc_corr,
        inherit.aes = F,
        aes(x = True_lab, y = length(unique(conf_df$Pred_lab)) + .25,
            label = paste0("Acc: ", percent_corr, "%")),
        color = "red",
        size = 4,
        fontface = "bold"
      )+
      scale_fill_gradient(
        low = "white",
        high = "blue",
        labels = scales::percent,
        limits = c(0, 1),
        breaks = c(0, 0.5, 1)
      ) +
      labs(
        x = paste0("True Class (", units, ")"),
        y = paste0("Predicted Class (", units, ")"),
        fill = NULL,
        title = title,
        subtitle = paste0( "Accuracy ",
                           round(100 * accuracy, 1),
                           "%    F1: ",round(f_score, 2), "     Kappa: ",round(kappa, 2))
      ) +
      guides(size = "none") +
      theme_bw() +
      theme(
        #panel.border = element_blank(),
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text = element_text(color = "black", size = 11),
        axis.title = element_text(size = 12),
        axis.text.x.top = element_text(angle = 30, vjust = 0, hjust = 0),
        axis.ticks = element_blank()
      )


  }

  train_plot <- conf_plot(pred_df = train_set,
                          title = "Train Set Ensemble")
  test_plot <- conf_plot(pred_df = test_set,
                           title =  "Test Set Ensemble")

  return(list(t_plot = train_plot, test_plot  = test_plot) )
}
