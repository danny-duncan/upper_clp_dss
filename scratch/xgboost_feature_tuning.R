#' @title XGboost model training with site-stratified feature tuning
#'
#' @description
#' This script with take in training/validation data, hyper parameters, site info on train/val splits and column info and then train/hypertune XGBoost models using caret
#'
#' @param data Data frame containing the features and target variable. This data should be normalized/standardized already and should not contain any testing data
#'
#' @param weights Optional weighting scheme for observations.
#'   - `NULL`: all observations receive equal weight (default).
#'   - A numeric vector of length `nrow(data)`: each observation is assigned the
#'     corresponding weight, which will be subset by fold during training/validation.
#'   - A function that takes a data.frame and returns a numeric vector of weights.
#' @param feature_grid expanded grid of hyperparameters to tune over.
#'
#' @param target_col Character string indicating column of the target variable to be predicted.
#'
#' @param site_col Character string indicating column of the site ids to retrieve data for.
#'
#' @param units Character string indicating the units of the target parameter (for plotting purposes only).
#'
#' @param fold_ids dataframe containing the fold number and a list of the corresponding validation site ids
#'
#' @param plot_dir Character string indicating directory to save plots. If NULL, plots will not be saved.
#'
#' @return
#'
#' @examples
# here, we assume that val_set_1 (and other objects in the list) contain vectors of identifiers present in the `data` dataframe
#' folds <- data.frame(
#'   fold = 1:4,
#'   val_ids = list(val_set_1, val_set_2, val_set_3, val_set_4)
#'  )

#' model <- xgboost_site_stratified_tuning(
#'    data = train_val %>% select(TOC, id, any_of(features)),
#'    tune_grid = expand.grid(
#'      nrounds = 10000,
#'      max_depth = c(2, 3, 4),
#'      eta = c(0.005, 0.01, 0.1),
#'      gamma = c(0.6, 0.8),
#'      colsample_bytree = c(0.5, 0.8),
#'      min_child_weight = c(2,4, 6),
#'      subsample = c(0.5, 0.8)
#'    ),
#'  target_col = "TOC",
#'  site_col = "id",
#' fold_ids =  folds,
#' units = "mg/L"
#')

xgboost_feature_tuning <- function(data, target_col = "TOC", site_col = "id", weights = NULL,
                                   feature_grid = NULL, hyper_params, fold_ids, units = "mg/L",
                                   plot_dir = NULL) {

  # Create empty lists for folds
  fold_indices <- list()
  fold_indices_out <- list()

  # define indices for each fold based on the fold_ids input vectors
  for (i in 1:nrow(fold_ids)) {
    val_sites <- fold_ids$val_ids[[i]]

    # Indices for validation
    val_idx <- which(data[[site_col]] %in% val_sites)
    # Indices of training set
    train_idx <- setdiff(1:nrow(data), val_idx)
    #store indices in list
    fold_indices[[i]] <- train_idx
    fold_indices_out[[i]] <- val_idx
  }

  # Train model
  cat("Starting site-stratified hyperparameter tuning...\n")
  #create empty lists to store output
  fold_models <- list()
  fold_results <- list()
  best_params <- list()

  # Run through each train/val fold: determine best features and hypertune model parameters
  for (i in 1:length(fold_indices)) {
    cat(paste0("Tuning fold ", i, " of ", nrow(fold_ids), "...\n"))

    #setup performance dataframe
    perf <- data.frame()
    all_importances <- data.frame()

    # Split data into training and validation sets based on indices above
    train_data <- data[fold_indices[[i]], ]
    #train_data_lists[[i]] <- train_data
    val_data   <- data[fold_indices_out[[i]], ]
    #val_data_lists[[i]] <- val_data

    # set up weights (if none provided, default to 1)
    if (is.null(weights)) {
      w_train <- rep(1, nrow(train_data))
      w_val   <- rep(1, nrow(val_data))
    } else if (is.function(weights)) {
      #set custom weights function
      w_train <- weights(train_data)
      w_val   <- weights(val_data)
    } else if (length(weights) == nrow(data)) {
      #use external weighting function
      w_train <- weights[fold_indices[[i]]]
      w_val   <- weights[fold_indices_out[[i]]]
    } else {
      stop("`weights` must be NULL, a function(data) -> numeric, or a vector of length nrow(data)")
    }


    #run through all hyper parameters and save to fold_models with name foldi_gridj
    for (j in 1:nrow(feature_grid)) {

      #set instance of features to test
      features <- feature_grid$features[[j]]

      # set up data matrix for xgb
      dtrain <- xgb.DMatrix(
        data = as.matrix(train_data[, features]),
        label = train_data[[target_col]],
        weight = w_train # add weights
      )

      dval <- xgb.DMatrix(
        data = as.matrix(val_data[, features]),
        label = val_data[[target_col]],
        weight = w_val # add weights
      )

      # Set train vs val for early stopping
      watchlist <- list(train = dtrain, eval = dval)

      # define "naive" model to choose features
      params <- list(
        objective        = "reg:squarederror",
        eval_metric      = "rmse",
        eta              = hyper_params$eta[1],
        gamma            = hyper_params$gamma[1],
        alpha            = hyper_params$alpha[1],
        lambda           = hyper_params$lambda[1],
        max_depth        = hyper_params$max_depth[1],
        subsample        = hyper_params$subsample[1],
        colsample_bytree = hyper_params$colsample_bytree[1],
        min_child_weight  = hyper_params$min_child_weight[1]
      )

      #train model with hyper parameters
      model_ij <- xgb.train(
        params = params,
        data = dtrain,
        nrounds = hyper_params$nrounds[1],
        watchlist = watchlist,
        #change early stopping rounds based on eta (smaller eta, larger rounds)
        early_stopping_rounds = ifelse(hyper_params$eta[1] >= 0.1, 250,
                                       ifelse(hyper_params$eta[1] >= 0.01, 500,
                                              1000)),
        print_every_n = 1000,
        verbose = 0
      )

      pred_col <- paste0(target_col, "_guess")
      # Predictions on validation set
      val_data[[pred_col]] <- predict(
        model_ij, dval,
        iterationrange = c(1, model_ij$best_iteration)
      )
      rmse_val  <- rmse(val_data[[target_col]], val_data[[pred_col]])
      mae_val   <- mae( val_data[[target_col]], val_data[[pred_col]])
      bias_val  <- bias(val_data[[target_col]], val_data[[pred_col]])

      # Predictions on training set (to measure overfitting)
      train_data[[pred_col]] <- predict(
        model_ij, dtrain,
        iterationrange = c(1, model_ij$best_iteration)
      )
      rmse_train  <- rmse(train_data[[target_col]], train_data[[pred_col]])
      mae_train   <- mae( train_data[[target_col]], train_data[[pred_col]])
      bias_train  <- bias(train_data[[target_col]], train_data[[pred_col]])

      # store in a master tibble
      all_importances <- bind_rows(all_importances,
                                   xgb.importance(feature_names = features, model = model_ij) %>%
                                     mutate(fold = i, grid_id = j))
      # Playing with shapley values
      #       library(SHAPforxgboost)
      #       shap_values <- shap.values(xgb_model = model_ij, X_train = dtrain)
      #
      #       # shap_values$shap_score is a matrix of Shapley values
      #       head(shap_values$shap_score)
      #
      #       # Summary plot
      #       shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = as.matrix(train_data[, features]))
      #       shap.plot.summary(shap_long)

      perf <- rbind(perf, data.frame(
        fold       = i,
        grid_id    = j,
        best_iter  = model_ij$best_iteration,  # from early stopping
        rmse_val   = rmse_val,
        mae_val    = mae_val,
        bias_val   = bias_val,
        rmse_train = rmse_train,
        mae_train  = mae_train,
        bias_train = bias_train,
        diff       = abs(rmse_val - rmse_train)
      ))
      # Save model temporarily
      fold_models[[paste0("fold", i, "_grid", j)]] <- model_ij

      if(j/100 == round(j/100)){
        cat(paste0("  Completed ", j, " of ", nrow(feature_grid), " feature sets...\n"))
      }

    }

    gain_p <- ggplot(all_importances, aes(x = reorder(Feature, Gain, FUN = median), y = Gain)) +
      geom_violin(fill = "steelblue", alpha = 0.6) +
      stat_summary(fun = median, geom = "point", size = 2, color = "red") +
      coord_flip() +
      labs(
        x = "Feature",
        y = "Gain"
      ) +
      ROSS_theme

    freq_p <-ggplot(all_importances, aes(x = reorder(Feature, Gain, FUN = median), y = Frequency)) +
      geom_violin(fill = "steelblue", alpha = 0.6) +
      stat_summary(fun = median, geom = "point", size = 2, color = "red") +
      coord_flip() +
      labs(
        x = "",
        y = "Frequency"
      ) +
      ROSS_theme

    cover_p <- ggplot(all_importances, aes(x = reorder(Feature, Gain, FUN = median), y = Cover)) +
      geom_violin(fill = "steelblue", alpha = 0.6) +
      stat_summary(fun = median, geom = "point", size = 2, color = "red") +
      coord_flip() +
      labs(
        x = "Feature",
        y = "Cover"
      ) +
      ROSS_theme

    imp_plot <- ggarrange(gain_p, freq_p, cover_p, ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")

    plot(imp_plot)

    p <- ggplot(perf) +
      geom_histogram(aes(x = rmse_val, fill = "Val")) +
      geom_histogram(aes(x = rmse_train, fill = "Train")) +
      labs(title = paste("Fold", i, "Val & Train RMSE by Grid ID"),
           x = "RMSE", y = "density", fill = "Group") +
      ROSS_theme

    plot(p)


    # --- Pick top 10 by validation RMSE ---
    top10 <- perf[order(perf$rmse_val), ][1:10, ]

    # --- From those, choose 6 smallest train-val gap ---
    best_rows  <- top10[order(top10$diff), ][1:6,]

    #order from lowest to highest val RMSE
    best_rows <- best_rows[order(best_rows$rmse_val), ]

    #blank lists for plots
    eval_plots <- list()
    train_val_plots <- list()

    for (k in 1:nrow(best_rows)) {
      # find fold and grid id
      fold_id <- best_rows$fold[k]
      grid_id <- best_rows$grid_id[k]
      target_pred_col <- paste0(target_col, "_guess_", fold_id)
      model_key <- paste0("fold", fold_id, "_grid", grid_id)

      #Objects for learning rate plot
      eval_log <- fold_models[[model_key]][["evaluation_log"]]
      #get input features for the model
      features <- fold_models[[model_key]][["feature_names"]]
      # collapse features into a single string
      feature_text <- paste(
        names(features), "", unlist(features),
        collapse = "\n"
      )
      # Get best iteration and corresponding RMSE
      best_iter <- which.min(eval_log$eval_rmse)

      # Predictions on validation/training set
      #convert train_val sets to matrix for prediction
      val_matrix <- val_data[, features]%>%
        mutate(across(everything(), as.numeric)) %>%
        as.matrix()
      train_matrix <- train_data[, features]%>%
        mutate(across(everything(), as.numeric)) %>%
        as.matrix()
      #make predictions
      val_data[[target_pred_col]] <-  predict(fold_models[[model_key]], val_matrix, iterationrange = c(1, fold_models[[model_key]]$best_iteration))
      val_data$group <-  "Validation"
      train_data[[target_pred_col]] <-  predict(fold_models[[model_key]], train_matrix, iterationrange = c(1, fold_models[[model_key]]$best_iteration))
      train_data$group <-  "Train"

      # Calculate RMSE/MAE for annotation
      train_rmse <- rmse(train_data[[target_col]],train_data[[target_pred_col]]) %>% round(3)
      val_rmse <- rmse(val_data[[target_col]],val_data[[target_pred_col]]) %>% round(3)
      train_mae <- mae(train_data[[target_col]], train_data[[target_pred_col]]) %>% round(3)
      val_mae <- mae(val_data[[target_col]], val_data[[target_pred_col]]) %>% round(3)

      # Create evaluation plot (learning rate)
      eval_plot <- ggplot(eval_log, aes(x = iter)) +
        geom_line(aes(y = train_rmse, color = "Train RMSE")) +
        geom_line(aes(y = eval_rmse, color = "Eval RMSE")) +
        geom_vline(xintercept = best_iter, linetype = "dashed", color = "black") +
        labs(title = paste("Grid", grid_id, ":Best Iter:", best_iter, "Val RMSE:", round(val_rmse, 3)),
             x = "Iteration", y = "RMSE", color = "Metric")+
        annotate(
          "text",
          x = max(eval_log$iter) * .5,   # position on x-axis
          y = max(c(eval_log$train_rmse, eval_log$eval_rmse)) * 0.9,  # position on y-axis
          label = paste0("Features:\n", feature_text),
          hjust = 0,
          vjust = 1,
          size = 4,
          color = "black"
        )+ROSS_theme

      #give one plot the legend
      if(k == 5){
        eval_plots[[k]] <- eval_plot +
          theme(legend.position = "bottom",
                plot.title = element_text(hjust = 0.5, face = "bold"))
      }else{
        eval_plots[[k]] <- eval_plot +
          guides(color = "none")+
          theme(legend.position = "none",
                plot.title = element_text(hjust = 0.5, face = "bold"))
      }


      # Create performance plot on Train/val

      # Combine train and val sets for plotting
      plot_data <- bind_rows(train_data, val_data)
      # get plot bounds
      min_val <- min(plot_data[[target_col]], na.rm = TRUE)
      max_val <- max(plot_data[[target_col]], na.rm = TRUE)
      plot_range <- max_val - min_val
      box_width <- plot_range * 0.5
      box_height <- plot_range * 0.2
      box_xmin <- min_val + plot_range * 0.02
      box_xmax <- box_xmin + box_width
      box_ymax <- max_val - plot_range * 0.02
      box_ymin <- box_ymax - box_height
      text_x <- (box_xmin + box_xmax) / 2
      text_y1 <- box_ymax - box_height * 0.15
      text_y2 <- box_ymax - box_height * 0.5
      text_y3 <- box_ymax - box_height * 0.85

      train_val_plot <- ggplot(plot_data, aes(x = .data[[target_col]], y = .data[[target_pred_col]], color = group))+ #shape = collector)) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth = 1.2) +

        # Training points (all same shape)
        geom_point(
          data = filter(plot_data, group == "Train"),
          #shape = 16,  # solid circle
          size = 4,
          alpha = 0.6) +
        geom_point( # Testing points (shape by id)
          data = filter(plot_data, group == "Validation"),
          shape = 17,
          #aes(shape = id),
          size = 4,
          alpha = 0.9
        ) +
        annotate("rect", xmin = box_xmin, xmax = box_xmax, ymin = box_ymin, ymax = box_ymax,
                 fill = "white", color = "black", alpha = 1, linewidth = 0.5) +
        annotate("text", x = text_x, y = text_y1, label = paste0("Training samples: n = ", (nrow(plot_data)- nrow(val_data))),
                 color = "black", size = 3) +
        annotate("text", x = text_x, y = text_y2, label = paste0("Validation samples: n = ", nrow(val_data)),
                 color = "black", size = 3) +
        annotate("text", x = text_x, y = text_y3, label = paste0("Val RMSE: ", val_rmse,
                                                                 " ",units,", MAE:", val_mae, " ", units),
                 color = "black", fontface = 2, size = 3.5) +
        scale_color_manual(values = c("Train" = "#002EA3", "Validation" = "#FFCA3A")) +
        #scale_shape_manual(values = c("ROSS" = 15, "FC" = 17, "Virridy" = 19)) +
        labs(
          x = paste0("Measured ", target_col, " (", units, ")"),
          y = paste0("Predicted ", target_col, " (", units, ")"),
          color = "Model Group",
          #shape = "Site",
          title = paste0("Model Fold: ", fold_id, " Grid: ", grid_id)
        ) +
        theme_bw(base_size = 16) +
        xlim(min_val, max_val) +
        ylim(min_val, max_val)

      #give one plot the legend
      if(k == 5){
        train_val_plots[[k]] <- train_val_plot +
          theme(legend.position = "bottom",
                plot.title = element_text(hjust = 0.5, face = "bold"))
      }else{
        train_val_plots[[k]] <- train_val_plot+
          guides(color = "none")+
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "none"
            # legend.position = c(0.975, 0.05),
            # legend.justification = c("right", "bottom"),
            # legend.box.background = element_rect(color = "black", fill = "white"),
            # legend.margin = margin(6, 6, 6, 6)
          )
      }

    }

    #Create plot with top 6 eval plots
    eval_grid <- wrap_plots(eval_plots, nrow = 2, ncol = 3, common.legend = TRUE) +
      theme(legend.position = "bottom")

    print(eval_grid)

    #Create plot with top 6 perf plots
    train_val_grid <- wrap_plots(train_val_plots, nrow = 2, ncol = 3, common.legend = TRUE) +
      theme(legend.position = "bottom")

    print(train_val_grid)

    if(!is.null(plot_dir)){
      #Save performance plot
      ggsave(p, filename = file.path(plot_dir, paste0("fold", i, "_val_train_rmse_hist_", Sys.Date(), ".png")), width = 8, height = 6)
      #Save importance plot
      ggsave(imp_plot, filename = file.path(plot_dir, paste0("fold", i, "_feature_importance_", Sys.Date(), ".png")), width = 12, height = 8)
      #Save eval grid
      ggsave(eval_grid, filename = file.path(plot_dir, paste0("fold", i, "_top6_eval_plots_", Sys.Date(), ".png")), width = 16, height = 10)
      #Save train val grid
      ggsave(train_val_grid, filename = file.path(plot_dir, paste0("fold", i, "_top6_train_val_plots_", Sys.Date(), ".png")), width = 16, height = 10)
    }

    #Ask user for best grid
    best_choice <- as.integer(readline(prompt = "Enter the grid ID of the best model from the top 5 (or type 0 to select the one with smallest train-val gap): "))

    #Select user's choice or default to smallest train-val gap
    if (best_choice %in% best_rows$grid_id) {
      best_row <- best_rows[best_rows$grid_id == best_choice, ]
    } else {
      best_row <- best_rows[1, ]  # Default to smallest train-val gap
      cat("Invalid choice. Defaulting to model with smallest train-val gap.\n")
    }

    model_key <- paste0("fold", i, "_grid", best_row$grid_id)
    sel_params <- fold_models[[model_key]][["feature_names"]]
    best_params[[i]] <- sel_params


    # Save only the best model for this fold
    fold_results[[i]] <- list(
      model = fold_models[[paste0("fold", i, "_grid", best_row$grid_id)]],
      perf  = best_row
    )

    cat("Best params for fold", i, ":\n")
    print(best_params[[i]])
    cat("\n Model performance:\n")
    print(fold_results[[i]]$perf)
    #clean up meemory
    gc()
  }

  return(fold_results)
}



