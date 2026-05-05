#' @title XGboost model training with site-stratified hyperparameter tuning
#'
#' @description
#' This function trains and tunes XGBoost models across multiple folds.
#' It handles the splitting of data into training and validation sets based on
#' a provided fold identifier (`fold_id`), applies a custom weighting scheme (`weights`)if provided,
#' and evaluates a grid of hyperparameters (`tune_grid`) for each fold.
#' Performance is evaluated using root mean squared error (RMSE), mean absolute error (MAE),
#' and bias. The top 10 models based on validation RMSE are identified, and
#' from those, the 6 models with the smallest gap between training and validation RMSE
#' are selected as the best performing models to prevent overfitting.
#' The function returns performance distributions, learning rate evaluations, and
#' diagnostic plots for the best models.
#'
#' @param train_val_df Data frame containing the feature columns, target variable,
#'   and a `fold_id` column used for cross-validation splitting. The dataset
#'   should be pre-processed (e.g., standardized to a range of 0-1) and contain no testing data.
#' @param feature_cols A character vector of column names to be used as predictors.
#' @param target_col Character string indicating the column name of the target variable
#'   to be predicted. Default is `"TOC"`.
#' @param site_col Character string indicating the site identifier column. Default is `"id"`.
#' @param weights Optional weighting scheme for observations.
#'   - `NULL`: all observations receive equal weight (default).
#'   - A numeric vector of length `nrow(data)`: assigns a specific weight to each observation.
#'   - A function that takes a data.frame and returns a numeric vector of weights.
#' @param tune_grid A data frame containing combinations of hyperparameters to tune over.
#'   If `NULL`, a default grid is used.
#' @param units Character string indicating the units of the target parameter, used
#'   for plot labeling. Default is `"mg/L"`.
#'
#' @return A list of lists (one per fold) containing:
#' \itemize{
#'   \item \code{tv_plot}: A combined `ggplot` of the training vs. validation performance for the top 6 models.
#'   \item \code{eval_plot}: A combined `ggplot` showing the learning curves (eval log) for the top 6 models.
#'   \item \code{perf_dist}: A `ggplot` histogram showing the distribution of training and validation RMSE across all tested parameter grids.
#'   \item \code{grid_results}: A sub-list containing the model object, performance metrics, and hyperparameters for each of the top 6 models.
#' }
#'
#' @examples
#' \dontrun{
#' # Assuming `train_val` data frame has a `fold_id` column
#'
#' tune_grid <- expand.grid(
#'   nrounds = 10000,
#'   max_depth = c(2, 3, 4),
#'   eta = c(0.005, 0.01, 0.1),
#'   gamma = c(0.4, 0.6),
#'   colsample_bytree = c(0.5, 0.8),
#'   min_child_weight = c(2, 4, 6),
#'   subsample = c(0.5, 0.8),
#'   lambda = 1,
#'   alpha = 0
#' )
#'
#' results <- xgboost_hyperparameter_tuning(
#'   train_val_df = train_val,
#'   feature_cols = c("Temp", "Turbidity", "FDOM"),
#'   target_col = "TOC",
#'   site_col = "id",
#'   weights = NULL,
#'   tune_grid = tune_grid,
#'   units = "mg/L"
#' )
#' }

xgboost_hyperparameter_tuning <- function(train_val_df, feature_cols, target_col = "TOC", site_col = "id", weights = NULL,
                                           tune_grid = NULL, units = "mg/L") {
  source("src/plotting/plot_tv_fold_perf.R") #load plotting function

  # Create default grid if not provided
  if (is.null(tune_grid)) {
    tune_grid <- expand.grid(
      nrounds = 10000,
      max_depth = c(2, 3, 4),
      eta = c(0.005, 0.01, 0.1),
      gamma = c(0.4, 0.6),
      colsample_bytree = c(0.5, 0.8),
      min_child_weight = c(2, 4, 6),
      subsample = c(0.5, 0.8),
      lambda = c(1),
      alpha = c(0)
    )
  }

  fold_ids <- sort(unique(train_val_df$fold_id))
  #Set up the fold indices
  n_folds <- length(fold_ids)

  # Train model
  cat("Starting site-stratified hyperparameter tuning...\n")

  fold_models <- list()
  fold_results <- list()
  best_params <- list()
  #Testing to see if data is being stored in correctly between groups
  #train_data_lists <- list()
  #val_data_lists <- list()

# Run through each train/val fold and hypertune
  for (i in seq_along(fold_ids)) {
    cat(paste0("Tuning fold ", i, " of ", n_folds, "...\n"))

    #setup performance dataframe
    perf <- data.frame()

    # Split data into training and validation sets based on fold id

    train_data <- train_val_df%>%
      filter(fold_id != i | is.na(fold_id)) %>%
      select(-fold_id)
    #train_data_lists[[i]] <- train_data
    val_data   <- train_val_df %>%
      filter(fold_id == i & !is.na(fold_id)) %>%
      select(-fold_id)

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
      w_train <- weights[train_idx]
      w_val   <- weights[val_idx]
    } else {
      stop("`weights` must be NULL, a function(data) -> numeric, or a vector of length nrow(data)")
    }

    # set up data matrix for xgb
    dtrain <- xgb.DMatrix(
      data = as.matrix(train_data[, feature_cols]),
      label = train_data[[target_col]],
      weight = w_train # add weights
    )

    dval <- xgb.DMatrix(
      data = as.matrix(val_data[, feature_cols]),
      label = val_data[[target_col]],
      weight = w_val # add weights
    )

    # Set train vs val for early stopping
    watchlist <- list(train = dtrain, eval = dval)

    #run through all hyper parameters and save to fold_models with name foldi_gridj
    for (j in 1:nrow(tune_grid)) {

      #setup parameters for tune
      params <- list(
        objective        = "reg:squarederror",
        eval_metric      = "rmse",
        tree_method = "exact", #deterministic method for reproducibility
        eta              = tune_grid$eta[j],
        gamma            = tune_grid$gamma[j],
        alpha            = tune_grid$alpha[j],
        lambda           = tune_grid$lambda[j],
        max_depth        = tune_grid$max_depth[j],
        subsample        = tune_grid$subsample[j],
        colsample_bytree = tune_grid$colsample_bytree[j],
        min_child_weight  = tune_grid$min_child_weight[j],
        nthread = 1
      )
      #set seed for reproducibility
      set.seed(123)

      #train model with hyper parameters
      model_ij <- xgb.train(
        params = params,
        data = dtrain,
        nrounds = tune_grid$nrounds[j],
        evals = watchlist,
        #change early stopping rounds based on eta (smaller eta, larger rounds)
        early_stopping_rounds = ifelse(tune_grid$eta[j] >= 0.1, 250,
                                       ifelse(tune_grid$eta[j] >= 0.01, 500,
                                              1000)),
        print_every_n = 1000,
        verbose = 0
      )

      pred_col <- paste0(target_col, "_guess")

      best_iter <- xgb.attr(model_ij, "best_iteration") %>% as.numeric()

      # Predictions on validation set
      val_data[[pred_col]] <- predict(
        model_ij, dval,
        iterationrange = c(1, best_iter),
        validate_features = TRUE
      )
      rmse_val  <- rmse(val_data[[target_col]], val_data[[pred_col]])
      mae_val   <- mae( val_data[[target_col]], val_data[[pred_col]])
      bias_val  <- bias(val_data[[target_col]], val_data[[pred_col]])

      # Predictions on training set (to measure overfitting)
      train_data[[pred_col]] <- predict(
        model_ij, dtrain,
        iterationrange = c(1, best_iter),
        validate_features = TRUE
      )
      rmse_train  <- rmse(train_data[[target_col]], train_data[[pred_col]])
      mae_train   <- mae( train_data[[target_col]], train_data[[pred_col]])
      bias_train  <- bias(train_data[[target_col]], train_data[[pred_col]])

      perf <- rbind(perf, data.frame(
        fold       = i,
        grid_id    = j,
        best_iter  = best_iter,  # from early stopping
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

      if(j/50 == round(j/50)){
        cat(paste0("  Completed ", j, " of ", nrow(tune_grid), " hyperparameter combinations...\n"))
      }
    }

    perf_dist <- ggplot(perf) +
      geom_histogram(aes(x = rmse_val, fill = "Val")) +
      geom_histogram(aes(x = rmse_train, fill = "Train")) +
      labs(title = paste("Fold", i, "Val & Train RMSE by Grid ID"),
           x = "RMSE", y = "density", fill = "Group") +
      theme_minimal()

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
      eval_log <- attributes(fold_models[[model_key]])$evaluation_log
      params <- attributes(fold_models[[model_key]])$params
      #remove objective, eval metric, validate parameters, tree method, nthread from params
      params <- params[!names(params) %in% c("objective", "eval_metric", "validate_parameters", "tree_method", "nthread", "seed")]
      # collapse params into a single string
      param_text <- paste(
        names(params), "=", unlist(params),
        collapse = "\n"
      )
      # Get best iteration and corresponding RMSE
      best_iter <- xgb.attr(fold_models[[model_key]], "best_iteration") %>% as.numeric()

      # Predictions on validation/training set
      #convert train_val sets to matrix for prediction
      val_matrix <- val_data[, feature_cols]%>%
        mutate(across(everything(), as.numeric)) %>%
        as.matrix()
      train_matrix <- train_data[, feature_cols]%>%
        mutate(across(everything(), as.numeric)) %>%
        as.matrix()
      #make predictions
      val_data[[target_pred_col]] <-  predict(fold_models[[model_key]], val_matrix, iterationrange = c(1, best_iter), validate_features = T)
      val_data$group <-  "Validation"
      train_data[[target_pred_col]] <-  predict(fold_models[[model_key]], train_matrix, iterationrange = c(1, best_iter), validate_features = T)
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
          label = param_text,
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



      train_val_plot <- plot_tv_fold_perf(fold_set = fold_id, train_val_df = train_val_df, fold_model = fold_models[[model_key]],
                                          target_col = "TOC", units = units)+
        labs(title = paste("Fold", fold_id, "Grid", grid_id))

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

    #Create plot with top 6 perf plots
    train_val_grid <- wrap_plots(train_val_plots, nrow = 2, ncol = 3, common.legend = TRUE) +
      theme(legend.position = "bottom")

    # Initialize list for this fold
    fold_results[[i]] <- list()
    fold_results[[i]]$tv_plot <- train_val_grid
    fold_results[[i]]$eval_plot <- eval_grid
    fold_results[[i]]$perf_dist <- perf_dist


    # Loop through each candidate model (e.g., in best_rows)
    for (j in seq_len(nrow(best_rows))) {
      grid_id <- best_rows$grid_id[j]
      model_key <- paste0("fold", i, "_grid", grid_id)

      # Extract model and info
      model_obj <- fold_models[[model_key]]
      perf_row  <- best_rows[j, ]
      params <- attributes(fold_models[[model_key]])$params
      params <- params[!names(params) %in% c("objective", "eval_metric", "validate_parameters", "tree_method", "nthread", "seed")]%>%
        as_tibble()

      # Store everything in a sub-list for this grid
      fold_results[[i]][["grid_results"]][[paste0("grid_", grid_id)]] <- list(
        model       = model_obj,
        perf        = perf_row,
        params      = params
      )
    }
    #clean up memory
    gc()
  }

  return(fold_results)
}



