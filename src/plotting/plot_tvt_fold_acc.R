#' @title Plot Train, Validation, and Test Fold Accuracy for Multiclass Models
#'
#' @description
#' Generates confusion matrix plots and summary statistics (accuracy, F1 score, and Cohen's kappa)
#' for each dataset provided (training, validation, and test). Designed for evaluating
#' multi-class classification models such as XGBoost with objectives
#' `"multi:softprob"` or `"multi:softmax"`.
#'
#' This function applies model predictions, builds labeled confusion matrices,
#' and visualizes performance with per-class accuracy annotations.
#'
#' @param fold_model An XGBoost model object trained with either
#'   `"multi:softprob"` or `"multi:softmax"` objective.
#' @param fold_num Numeric or character identifier for the fold (used in plot titles).
#' @param train_df,val_df,test_df Optional data frames for training, validation,
#'   and test datasets. At least one must be provided. Each dataset must include
#'   the target column and all model feature columns.
#' @param target_col Character string specifying the target column name in the datasets.
#' @param units Character string for display units in plot axis labels.
#' @param title Character string used as a prefix in plot titles.
#' @param class_levels Character vector defining the factor levels (in order)
#'   for the target and predicted categories.
#'
#' @details
#' Internally, the function:
#' 1. Checks that at least one dataset (`train_df`, `val_df`, or `test_df`) is provided.
#' 2. Verifies that the model objective is appropriate for multi-class prediction.
#' 3. Uses `predict()` to generate class predictions and converts them into
#'    factor labels based on `class_levels`.
#' 4. Computes confusion matrices using `yardstick::conf_mat()` and summarizes
#'    accuracy, F1 score, and Cohen's kappa.
#' 5. Creates a ggplot-based confusion matrix heatmap with true vs. predicted labels,
#'    frequency shading, and per-class accuracy
#'
#' @return
#' A named list of ggplot objects:
#' \describe{
#'   \item{t_plot}{Training set confusion matrix plot (if `train_df` provided).}
#'   \item{v_plot}{Validation set confusion matrix plot (if `val_df` provided).}
#'   \item{test_plot}{Test set confusion matrix plot (if `test_df` provided).}
#' }
#' If multiple datasets are provided, all corresponding plots are returned.
#' If none are provided, the function stops.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' results <- plot_tvt_fold_acc(
#'   fold_model = xgb_model,
#'   fold_num = 3,
#'   train_df = train_data,
#'   val_df = val_data,
#'   target_col = "TOC_cat",
#'   units = "mg/L",
#'   title = "Fold Performance: ",
#'   class_levels = c("0-2", "2-4", "4-8", "8+")
#' )
#'
#' # Display validation plot
#' results$v_plot
#' }
#'
plot_tvt_fold_acc <- function(fold_model, fold_num,
                              train_df = NULL, val_df = NULL, test_df = NULL,
                              target_col = "TOC_cat", units = "mg/L", title = "",
                              class_levels = c("0-2", "2-4", "4-8", "8+")) {
  #See if there are any datasets
  input_datasets <- list(train_df = train_df, val_df = val_df, test_df = test_df)

  if(all(map_lgl(input_datasets, is.null))) {
    stop("At least one dataset (train_df, val_df, or test_df) must be provided.")
  }

  pred_col <- paste0(target_col, "_guess")

  #Check model is categorical
  if (!fold_model$params$objective %in% c("multi:softmax", "multi:softprob")) {
    stop("Model objective must be 'multi:softprob' or 'multi:softmax'.")
  }

  #predict classes and label
  predict_classes <- function(df) {
    if(is.null(df)){
      return(NULL)
    }

    preds <- predict(fold_model, as.matrix(df[, fold_model$feature_names]))

    if(fold_model$params$objective == "multi:softprob"){
      preds_vector <- max.col(matrix(preds, ncol = length(class_levels), byrow = TRUE)) - 1
    }else{
      preds_vector <- preds
    }
    df <- df %>%
      #if softprob, convert probs to class labels (max prob)
      mutate(!!pred_col := preds_vector)
    #convert to factors with levels
    df %>%
      mutate(
        target_cat = factor(.data[[target_col]],
                            levels = seq_along(class_levels) - 1,
                            labels = class_levels),
        pred_cat = factor(.data[[pred_col]],
                          levels = seq_along(class_levels) - 1,
                          labels = class_levels)
      )
  }

  #Predict for available datasets
  sets <- input_datasets %>%
    imap(~predict_classes(.x))

  #confusion matrix plot
  conf_plot <- function(pred_df, plot_title) {
    if (is.null(pred_df)) return(NULL)

    df <- pred_df %>%
      mutate(pred_cat = factor(pred_cat, levels = levels(target_cat)))
    #compute performance metrics
    conf_table <- conf_mat(df, truth = target_cat, estimate = pred_cat, dnn = c("predicted", "truth"))
    performance <- summary(conf_table)

    acc  <- performance %>% filter(.metric == "accuracy") %>% pull(.estimate)
    kap  <- performance %>% filter(.metric == "kap") %>% pull(.estimate)
    f1   <- performance %>% filter(.metric == "f_meas") %>% pull(.estimate)

    conf_df <- conf_table$table %>%
      as_tibble() %>%
      mutate(
        value_perc = replace_na(n / sum(n), 0),
        True_lab = factor(truth, levels = levels(df$target_cat)),
        Pred_lab = factor(predicted, levels = levels(df$pred_cat))
      )

    perc_corr <- conf_df %>%
      group_by(True_lab) %>%
      summarize(percent_corr = round(sum(n[True_lab == Pred_lab]) /
                                       sum(n) * 100, 1)) %>%
      ungroup()

    ggplot(conf_df, aes(x = True_lab, y = Pred_lab, fill = value_perc)) +
      geom_tile(color = grey(0.6), size = 0.2) +
      geom_tile(data = filter(conf_df, True_lab == Pred_lab),
                color = "black", fill = "transparent", size = 1) +
      geom_text(aes(label = n), size = 4) +
      geom_text(
        data = perc_corr,
        aes(x = True_lab, y = length(unique(conf_df$Pred_lab)) + 0.25,
            label = paste0("Acc: ", percent_corr, "%")),
        inherit.aes = FALSE,
        color = "red", size = 4, fontface = "bold"
      ) +
      scale_fill_gradient(low = "white", high = "blue",
                          labels = percent, limits = c(0, 1)) +
      labs(
        x = paste0("True Class (", units, ")"),
        y = paste0("Predicted Class (", units, ")"),
        title = plot_title,
        subtitle = paste0("Accuracy ", round(100 * acc, 1),
                          "%  F1: ", round(f1, 2),
                          "  Kappa: ", round(kap, 2)),
        fill = "Percent Correct"
      ) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text = element_text(size = 11, color = "black"),
        axis.title = element_text(size = 12),
        axis.ticks = element_blank()
      )
  }

  #Generate Plots
  results <- list(
    t_plot = if (!is.null(sets$train_df)) conf_plot(sets$train_df, paste0(title, "Fold ", fold_num, " Train Set")) else NULL,
    v_plot = if (!is.null(sets$val_df))   conf_plot(sets$val_df,   paste0(title, "Fold ", fold_num, " Val Set")) else NULL,
    test_plot = if (!is.null(sets$test_df)) conf_plot(sets$test_df, paste0(title, "Fold ", fold_num, " Test Set")) else NULL
  ) %>%
    compact()  # removes NULL entries

  return(results)
}
