#' Plot Training, Validation, and Testing Performance for a Single Fold
#'
#' @description
#' This function generates a measured vs. predicted plot for a single XGBoost fold,
#' comparing model performance across the **training**, **validation**, and **testing** datasets.
#' It visualizes model predictions against observed values with distinct shapes and colors for
#' each data group and includes annotated model performance metrics (RMSE and MAE).
#'
#' @details
#' The function assumes that `train_val_df` contains a `fold_id` column specifying
#' which samples were used for validation in each fold. The function separates the data into
#' training and validation subsets, makes predictions using the specified fold’s model,
#' and compares them with predictions on the test dataset.
#'
#' Predictions are plotted against observed values using:
#' - Blue circles for training samples
#' - Yellow squares for validation samples
#' - Pink triangles for testing samples
#'
#' A dashed 1:1 line indicates perfect model agreement, and an annotation box reports
#' the fold number and model performance metrics.
#'
#' @param train_val_df A data frame containing the combined training and validation data
#'   for all folds. Must include:
#'   - A column `fold_id` identifying validation samples for each fold.
#'   - Predictor columns used in model training.
#'   - The target variable column specified by `target_col`.
#'
#' @param test_df A data frame containing the independent test data with the same
#'   predictor and target columns as `train_val_df`.
#'
#' @param fold_set The numeric or character ID of the fold to evaluate (used to filter
#'   validation samples from `train_val_df`).
#'
#' @param fold_model A trained XGBoost model object corresponding to the current fold.
#'   Must include:
#'   - `feature_names`: names of predictors used in the model.
#'   - `best_iteration`: number of boosting rounds for prediction.
#'
#' @param target_col A string giving the name of the target variable column
#'   (default = `"TOC"`).
#'
#' @param units A string giving the measurement units for the target variable
#'   (default = `"mg/L"`).
#'
#' @param subtitle_arg A string providing the plot subtitle
#'   (default = `"CLP Samples Only"`).
#'
#' @return
#' A `ggplot2` object visualizing observed vs. predicted concentrations across training,
#' validation, and testing datasets, annotated with:
#' - Model fold number
#' - Validation RMSE/MAE
#' - Testing RMSE/MAE
#'
#' @examples
#' \dontrun{
#' p <- plot_train_val_test(
#'   train_val_df = train_val_data,
#'   test_df = test_data,
#'   fold_set = 1,
#'   fold_model = xgb_model_fold1,
#'   target_col = "TOC",
#'   units = "mg/L",
#'   subtitle_arg = "Fold 1 Results"
#' )
#' print(p)
#' }
#'

plot_tvt_fold_perf <- function( train_val_df, test_df, fold_set, fold_model, target_col = "TOC", units = "mg/L", subtitle_arg = "CLP Samples Only") {

  #Check that train val df has fold_id column
  if(!"fold_id" %in% colnames(train_val_df)){
    stop("train_val_df must contain a 'fold_id' column indicating fold assignments.")
  }
  #split data into training, validation
  val_set <- train_val_df %>% filter(fold_id == fold_set)
  train_set <- anti_join(train_val_df, val_set, by = "id")

  #use the fold model to get feature names
  features <- fold_model$feature_names
  # Create matrix for train/val/test datasets
  val_matrix <- val_set[, features]%>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  train_matrix <- train_set[, features]%>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  test_matrix <- testing[, features]%>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  # Define prediction column name
  pred_col <- glue("{target}_guess_fold{fold_set}")

  # make predictions
  val_set[[pred_col]] <-  predict(fold_model, val_matrix, iterationrange = c(1, fold_model$best_iteration))
  val_set$group <-  "Validation"

  train_set[[pred_col]] <-  predict(fold_model, train_matrix, iterationrange = c(1, fold_model$best_iteration))
  train_set$group <-  "Train"

  test_df[[pred_col]] <-  predict(fold_model, test_matrix, iterationrange = c(1, fold_model$best_iteration))
  test_df$group <-  "Test"


  # Calculate RMSE/MAE for annotation
  train_rmse <- rmse(train_set[[target_col]],train_set[[pred_col]]) %>% round(2)
  val_rmse <- rmse(val_set[[target_col]],val_set[[pred_col]]) %>% round(2)
  train_mae <- mae(train_set[[target_col]], train_set[[pred_col]]) %>% round(2)
  val_mae <- mae(val_set[[target_col]], val_set[[pred_col]]) %>% round(2)
  test_rmse <- rmse(test_df[[target_col]], test_df[[pred_col]]) %>% round(2)
  test_mae <- mae(test_df[[target_col]], test_df[[pred_col]]) %>% round(2)

  # Combine all sets for plotting
  data <- bind_rows(train_set, val_set, test_df)
  #set plotting ranges
  min_val <- min(data[[target_col]], na.rm = TRUE)
  max_val <- max(data[[target_col]], na.rm = TRUE)
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

  ggplot(data, aes(x = .data[[target_col]], y = .data[[pred_col]], color = group)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1.2) +

    # Training points (all same shape)
    geom_point(
      data = filter(data, group == "Train"),
      shape = 16,  # solid circle
      size = 4,
      alpha = 0.7) +
    geom_point(
      data = filter(data, group == "Validation"),
      shape = 15,  # solid square
      size = 4,
      alpha = 0.7) +
    geom_point( # Testing points (shape by id)
      data = filter(data, group == "Test"),
      shape = 17,
      #aes(shape = id),
      size = 4,
      alpha = 0.99
    ) +
    annotate("rect", xmin = box_xmin, xmax = box_xmax, ymin = box_ymin, ymax = box_ymax,
             fill = "white", color = "black", alpha = 1, linewidth = 0.5) +
    annotate("text", x = text_x, y = text_y1, label = paste0("Model Fold: ", fold_set),
             color = "black", size = 4.5, fontface = 2) +
    annotate("text", x = text_x, y = text_y2, label = paste0("Validation RMSE: ", val_rmse,
                                                             " mg/L, MAE:", val_mae, " mg/L"),
             color = "black", size = 3, fontface = 2) +
    annotate("text", x = text_x, y = text_y3, label = paste0("Testing RMSE: ", test_rmse,
                                                             " mg/L, MAE:", test_mae, " mg/L"),
             color = "black", fontface = 2, size = 3.5) +
    scale_color_manual(values = c("Train" = "#002EA3","Validation" = "#FFCA3A","Test" = "#E70870"),
                       breaks = c("Train", "Validation", "Test"))+
    #scale_shape_manual(values = c("ROSS" = 15, "FC" = 17, "Virridy" = 19)) +
    labs(
      x = paste0("Measured ",target_col," (", units, ")"),
      y = paste0("Predicted ",target_col," (", units, ")"),
      subtitle = subtitle_arg,
      color = "Model Group"
    ) +
    xlim(min_val, max_val) +
    ylim(min_val, max_val) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"))+
    ROSS_theme

}
