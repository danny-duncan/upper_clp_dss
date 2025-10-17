#' Plot Ensemble Training vs Testing Performance
#'
#' @description
#' This function visualizes the performance of an XGBoost ensemble model across training
#' and testing datasets. It generates a measured vs. predicted plot using the mean predictions
#' across folds for both datasets, with error bars representing the prediction range
#' across folds in the testing set.
#'
#' @details
#' The function assumes that the model object (`fold_models`) is a list of XGBoost models
#' trained on different cross-validation folds. For each fold, predictions are generated
#' on both the training/validation data (`train_val_df`) and the holdout test data (`test_df`).
#'
#' The mean, minimum, and maximum predictions across folds are calculated to summarize
#' ensemble uncertainty. The plot displays:
#' - Blue points for training predictions.
#' - Pink triangles for testing predictions.
#' - Vertical error bars showing the prediction range across folds for testing data.
#' - A dashed 1:1 line for reference.
#' - Annotated performance metrics (RMSE, MAE, sample counts).
#'
#' @param train_val_df A data frame containing the training and validation samples used
#'   to fit the ensemble models. Must include columns for predictor variables and the
#'   response variable specified by `target_col`.
#' @param test_df A data frame containing the test samples for model evaluation.
#'   Must include the same predictor columns as in `train_val_df`.
#' @param fold_models A list of trained XGBoost models (one per fold), each containing
#'   `feature_names` and `best_iteration` elements.
#' @param target_col A string specifying the name of the response variable column
#'   (default = `"TOC"`).
#' @param units A string giving the units of the response variable to be displayed in axis labels
#'   (default = `"mg/L"`).
#' @param subtitle_arg  String for plot subtitle (default = `"CLP Samples Only"`).
#'
#' @return
#' A `ggplot2` object showing:
#' - Ensemble predictions (mean ± range) for the testing data.
#' - Mean ensemble predictions for the training/validation data.
#' - Annotated model performance metrics (RMSE, MAE).
#'
#' @examples
#' \dontrun{
#' p <- plot_tvt_ensemble_perf(
#'   train_val_df = train_val,
#'   test_df = test_data,
#'   fold_model = model_list,
#'   target_col = "TOC",
#'   units = "mg/L",
#'   subtitle_arg = "Cross-Validation Ensemble"
#' )
#' print(p)
#' }

plot_tvt_ensemble_perf <- function( train_val_df, test_df, fold_models, target_col = "TOC", units = "mg/L", subtitle_arg = "CLP Samples Only") {

    # Make predictions with every fold

    test_preds_all <- map_dfc(fold_models, function(m) {
      features <- m$feature_names
      predict(m, as.matrix(test_df[, features]), iteration_range = c(1, m$best_iteration))
    })
    colnames(test_preds_all) <- paste0("fold_", seq_along(fold_models))

    test_preds_all <- test_preds_all %>%
      mutate(id = test_df$id,
             observed = test_df[[target_col]])

    test_summary <- test_preds_all %>%
      pivot_longer(starts_with("fold_"), names_to = "fold", values_to = "pred") %>%
      group_by(id, observed) %>%
      summarise(
        pred_mean = mean(pred, na.rm = TRUE),
        pred_min  = min(pred, na.rm = TRUE),
        pred_max  = max(pred, na.rm = TRUE),
        .groups = "drop"
      )

    # --- Training predictions (mean across folds) ---
    train_preds_all <- map_dfc(fold_models, function(m) {
      predict(m, as.matrix(train_val_df[, features]), iteration_range = c(1, m$best_iteration))
    })
    colnames(train_preds_all) <- paste0("fold_", seq_along(fold_models))

    train_summary <- train_preds_all %>%
      mutate(id = train_val_df$id,
             sensor_datetime = train_val_df$sensor_datetime,
             observed = train_val_df[[target_col]],
             pred_mean = rowMeans(select(., starts_with("fold_")), na.rm = TRUE))

    # Calculate metrics
    train_rmse <- rmse(train_summary$observed, train_summary$pred_mean) %>% round(2)
    train_mae  <- mae(train_summary$observed, train_summary$pred_mean) %>% round(2)
    test_rmse  <- rmse(test_summary$observed, test_summary$pred_mean) %>% round(2)
    test_mae   <- mae(test_summary$observed, test_summary$pred_mean) %>% round(2)

    #set axis limits
    min_val <- min(c(test_summary$observed, train_summary$observed), na.rm = TRUE)
    max_val <- max(c(test_summary$observed, train_summary$observed), na.rm = TRUE)
    plot_range <- max_val - min_val

    # annotation box coords
    box_width <- plot_range * 0.4
    box_height <- plot_range * 0.2
    box_xmin <- min_val + plot_range * 0.02
    box_xmax <- box_xmin + box_width
    box_ymax <- max_val - plot_range * 0.02
    box_ymin <- box_ymax - box_height
    text_x <- (box_xmin + box_xmax) / 2
    text_y1 <- box_ymax - box_height * 0.15
    text_y2 <- box_ymax - box_height * 0.5
    text_y3 <- box_ymax - box_height * 0.85

    #plot
    p <- ggplot() +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1.5) +

      # Training points (blue circles)
      geom_point(
        data = train_summary,
        aes(x = observed, y = pred_mean,
            color = "Training", shape = "Training"),
        size = 4, alpha = 0.7
      ) +

      # Test points (pink triangles + errorbars)
      geom_errorbar(
        data = test_summary,
        aes(x = observed, ymin = pred_min, ymax = pred_max),
        width = 0.2, color = "#E70870", alpha = 0.6
      ) +
      geom_point(
        data = test_summary,
        aes(x = observed, y = pred_mean,
            color = "Testing", shape = "Testing"),
        size = 4, alpha = 0.9
      ) +
      # Annotation box
      annotate("rect", xmin = box_xmin, xmax = box_xmax, ymin = text_y3, ymax = box_ymax ,
               fill = "white", color = "black", alpha = 1, linewidth = 0.5) +
      annotate("text", x = text_x, y = text_y1,
               label = paste0("Training samples: n = ", nrow(train_summary),
                              "\nTesting samples: n = ", nrow(test_summary)),
               color = "black", size = 4, fontface = 2) +
      annotate("text", x = text_x, y = text_y2,
               label = paste0("Testing RMSE: ", test_rmse,
                              " mg/L\nTesting MAE: ", test_mae, " mg/L"),
               color = "black", fontface = 2, size = 4.5) +

      labs(
        x = paste0("Measured", target_col, " (", units, ")"),
        y = paste0("Predicted", target_col, " (", units, ")"),
        title = "Ensemble Predictions (Mean ± Range across folds)",
        subtitle = subtitle_arg,
        color = "Model Group", shape = "Model Group"
      ) +
      scale_color_manual(values = c("Training" = "#002EA3", "Testing" = "#E70870")) +
      scale_shape_manual(values = c("Training" = 16, "Testing" = 17)) + # 16=circle, 17=triangle
      xlim(min_val, max_val) +
      ylim(min_val, max_val) +
      theme_bw(base_size = 20) +
      ROSS_theme +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16, face = "bold")
      )

    return(p)
  }
