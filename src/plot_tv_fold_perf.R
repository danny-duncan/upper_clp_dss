#' Plot Model Performance for a Given Fold
#'
#' @description
#' This function creates diagnostic plots showing the training and validation
#' performance for a specific cross-validation fold. It compares observed versus
#' predicted values for both training and validation datasets, and return a
#' plot that help visualize model fit and performance.
#'
#' @param fold_set Integer or numeric.
#' The fold identifier to select which subset of the data to use for validation.
#' All other folds are used as the training set.
#'
#' @param train_val_df Data frame.
#' A combined dataset containing all folds, model features, target variable, and
#' a column named `fold_id` indicating which fold each observation belongs to.
#'
#' @param fold_model Trained model object.
#' An XGBoost or similar model trained on the current fold’s training data.
#' Must support `predict()` for generating predictions.
#'
#' @param target_col Character, default = `"TOC"`.
#' The name of the target (response) column in `train_val_df`.
#'
#' @param units Character, default = `"mg/L"`.
#' Units for the target variable, used for axis labeling in the plots.
#'
#' @details
#' The function splits the provided dataset into training and validation subsets
#' based on `fold_set`, generates model predictions for both subsets, and produces
#' scatter plots comparing observed vs. predicted values. It includes a
#' 1:1 reference line, and annotations of training and validation RMSE to evaluate
#' model performance.
#'
#' @return
#' A ggplot object showing observed vs predicted values for both training (blue) and validation (yellow) sets.
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' p <- plot_tv_fold_perf(
#'   fold_set = 1,
#'   train_val_df = training_data,
#'   fold_model = xgb_model_fold_1,
#'   target_col = "TOC",
#'   units = "mg/L"
#' )
#'
#' # Example for multiple folds
#' map(1:5, ~plot_tv_fold_perf(fold_set = .x,
#'                              train_val_df = training_data,
#'                              fold_model = xgb_mod_list[[.x]], # a list where each element is a model for that fold
#'                              target_col = "TOC", units = "mg/L")
#'                              )
#'
#' }
#'
plot_tv_fold_perf <- function(fold_set, train_val_df, fold_model, target_col = "TOC", units = "mg/L") {

  # Split data into training and validation sets based on fold_id
  val_data <- train_val_df %>% filter(fold_set == fold_id)
  train_data <- anti_join(train_val_df, val_data, by = "fold_id")
  # Create prediction column name
  target_pred_col <- paste0(target_col, "_guess")
  # Get model features
  features <- fold_model$feature_names

  #convert train_val sets to matrix for prediction
  val_matrix <- val_data[, features]%>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  train_matrix <- train_data[, features]%>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()

  #make predictions
  val_data[[target_pred_col]] <-  predict(fold_model, val_matrix, iterationrange = c(1, fold_model$best_iteration))
  val_data$group <-  "Validation"
  train_data[[target_pred_col]] <-  predict(fold_model, train_matrix, iterationrange = c(1, fold_model$best_iteration))
  train_data$group <-  "Train"

  # Calculate RMSE/MAE for annotation & round to 3 decimals
  train_rmse <- rmse(train_data[[target_col]],train_data[[target_pred_col]]) %>% round(3)
  val_rmse <- rmse(val_data[[target_col]],val_data[[target_pred_col]]) %>% round(3)
  train_mae <- mae(train_data[[target_col]], train_data[[target_pred_col]]) %>% round(3)
  val_mae <- mae(val_data[[target_col]], val_data[[target_pred_col]]) %>% round(3)

  # Create performance plot on Train/val
  # Combine train and val sets for plotting
  data <- bind_rows(train_data, val_data)
  # get plot bounds
  min_val <- min(data[[target_col]], na.rm = TRUE)
  max_val <- max(data[[target_col]], na.rm = TRUE)

  # Creating bounding for text box
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

  # Plot observed vs predicted for train and val
  train_val_plot <- ggplot(data, aes(x = .data[[target_col]], y = .data[[target_pred_col]], color = group))+
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1.2) +

    # Training points (all same shape)
    geom_point(
      data = filter(data, group == "Train"),
      #shape = 16,  # solid circle
      size = 4,
      alpha = 0.6) +
    geom_point( # Testing points (shape by id)
      data = filter(data, group == "Validation"),
      shape = 17, # solid triangle
      size = 4,
      alpha = 0.9
    ) +
    annotate("rect", xmin = box_xmin, xmax = box_xmax, ymin = box_ymin, ymax = box_ymax,
             fill = "white", color = "black", alpha = 1, linewidth = 0.5) +
    annotate("text", x = text_x, y = text_y1, label = paste0("Training samples: n = ", (nrow(data) - nrow(val_data))),
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
      title = paste0("Model Fold: ", fold_set)
    ) +
    theme_bw(base_size = 16) +
    xlim(min_val, max_val) +
    ylim(min_val, max_val) +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "bold"))

  train_val_plot

}
