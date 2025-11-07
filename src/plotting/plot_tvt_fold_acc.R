#' Plot Confusion Matrix with Accuracy and Cohen's Kappa for Train/Validation Sets
#'
#' This function generates confusion matrices for a specific fold of a model's
#' training and validation data, converting numeric class predictions to
#' categorical bins, and displays them as heatmaps. It also overlays per-class
#' accuracy above each column and includes overall accuracy and Cohen's weighted
#' kappa in the plot title.
#'
#' @param fold_model An XGBoost or compatible model object used for predictions.
#' @param fold_num Numeric. The fold number to extract for validation; the rest of the
#'   data is treated as the training set.
#' @param train_df A data frame or tibble containing training data and all associated features.
#' @param val_df A data frame or tibble containing validation data and all associated features.
#' @param target_col Character. Name of the target column in `train_val` containing
#'   numeric class labels (0, 1, 2, ...).
#' @param units Character. Optional units to display for the target (default: "mg/L").
#' @param class_levels Character vector specifying the categorical bin labels
#'   corresponding to numeric class values (default: c("0-2", "2-4", "4-8", "8+")).
#'
#' @return A combined ggplot object showing the train and validation confusion matrices
#'   as heatmaps, including per-class accuracy above each column and overall
#'   accuracy and Cohen's kappa in the titles.
#'
#' @details
#' - The function first separates the training and validation sets based on `fold_id`.
#' - Predictions are generated for both sets and converted from numeric class labels
#'   to factors using `class_levels`.
#' - A nested `conf_plot()` function computes the confusion matrix using `yardstick::conf_mat`,
#'   calculates overall accuracy and Cohen's kappa, and constructs a ggplot heatmap.
#' - The main function returns a combined plot of both training and validation set confusion matrices.
#'
#' @examples
#' # Example usage:
#' plot_tv_fold_acc(
#'   fold_model = best_models[[1]],
#'   fold_num = 1,
#'   train_val = train_val_df,
#'   target_col = "TOC_cat",
#'   class_levels = c("0-2", "2-4", "4-8", "8+")
#' )
plot_tvt_fold_acc <- function(fold_model, fold_num, train_df, val_df,test_df = NULL,
                             target_col = "TOC_cat", units = "mg/L",title = "",
                             class_levels = c("0-2", "2-4", "4-8", "8+")) {

  pred_col = paste0(target_col, "_guess")

  if(fold_model$params$objective %in% c("multi:softmax", "multi:softprob")) {

    if(fold_model$params$objective == "multi:softprob") {
      # For softprob, predictions are returned as probabilities for each class
      val_preds <- predict(fold_model, as.matrix(val_df[, fold_model$feature_names]))
      val_pred_classes <- max.col(matrix(val_preds, ncol = length(class_levels), byrow = TRUE)) - 1

      train_preds <- predict(fold_model, as.matrix(train_df[, fold_model$feature_names]))
      train_pred_classes <- max.col(matrix(train_preds, ncol = length(class_levels), byrow = TRUE)) - 1

      val_df[[pred_col]] <- val_pred_classes
      train_df[[pred_col]] <- train_pred_classes

      if(!is.null(test_df)){
        test_preds <- predict(fold_model, as.matrix(test_df[, fold_model$feature_names]))
        test_pred_classes <- max.col(matrix(test_preds, ncol = length(class_levels), byrow = TRUE)) - 1
        test_df[[pred_col]] <- test_pred_classes
      }

    }else{
      # For softmax, predictions are returned as class labels directly
      val_df[[pred_col]] <- predict(fold_model, as.matrix(val_df[, fold_model$feature_names]))
      train_df[[pred_col]] <- predict(fold_model, as.matrix(train_df[, fold_model$feature_names]))

      if(!is.null(test_df)){
        test_df[[pred_col]] <- predict(fold_model, as.matrix(test_df[, fold_model$feature_names]))
      }
    }

    train_set <- train_df %>%
      mutate(target_cat = factor(!!sym(target_col),
                                 levels = seq_along(class_levels) - 1,
                                 labels = class_levels),

             pred_cat = factor(!!sym(pred_col),
                               levels = seq_along(class_levels) - 1,
                               labels = class_levels)
      )

    val_set <- val_df %>%
      mutate( target_cat = factor(!!sym(target_col),
                                  levels = seq_along(class_levels) - 1,
                                  labels = class_levels),

              pred_cat = factor(!!sym(pred_col),
                                levels = seq_along(class_levels) - 1,
                                labels = class_levels)
      )
    # Add test set processing if provided
    if(!is.null(test_df)){
      test_set <- test_df %>%
        mutate( target_cat = factor(!!sym(target_col),
                                    levels = seq_along(class_levels) - 1,
                                    labels = class_levels),

                pred_cat = factor(!!sym(pred_col),
                                  levels = seq_along(class_levels) - 1,
                                  labels = class_levels)
        )
    }
  } else {
    stop("Model objective must be multi:softprob or multi:softmax for classification.")
  }


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
                          title = paste0(title, "Fold ", fold_num, " Train Set") )

  val_plot <- conf_plot(pred_df = val_set,
                        title = paste0(title, "Fold ", fold_num, " Val Set") )
  if(!is.null(test_df)){
    test_plot <- conf_plot(pred_df = test_set,
                           title = paste0(title, "Fold ", fold_num, " Test Set") )
    return(list(t_plot = train_plot, v_plot  = val_plot, test_plot = test_plot) )
  }
  return(list(t_plot = train_plot, v_plot  = val_plot) )
}
