#' @title XGboost model training with site-stratified hyperparameter tuning
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
#' @param tune_grid expanded grid of hyperparameters to tune over. Please double check with `caret` and `xgboost` packages before inputting hyperparameters.
#' If NULL, a default grid will be used.
#'
#' @param target_col Character string indicating column of the target variable to be predicted.
#'
#' @param site_col Character string indicating column of the site ids to retrieve data for.
#'
#' @param units Character string indicating the units of the target parameter (for plotting purposes only).
#'
#' @param fold_ids dataframe containing the fold number and a list of the corresponding validation site ids
#'
#' @return A caret model object containing the trained XGBoost model for each fold and performance metrics (train/val RMSE, MAE, Bias).
#' Best performing models determined by taking the top 10 val RMSE scores and then selecting the one with the smallest train-val gap.
#'
#'
#' @examples
#' folds <- tibble(
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
#' units = "mg/L",
#' class_levels = c("0-2", "2-4", "4-8", "8+")
#')

xgboost_hyperparameter_tuning_cat <- function(data, target_col = "TOC_cat", site_col = "id", weights = NULL,
                                              tune_grid = NULL, units = "mg/L",class_levels = c("0-2", "2-4", "4-8", "8+")) {

  # Create default grid if not provided
  if (is.null(tune_grid)) {
    default_hyper_params <- list(
      objective        = "multi:softmax",
      eval_metric      = "mlogloss",
      nrounds = 10000,
      eta              = 0.05,
      gamma            = 0.6,
      alpha            = 0,
      lambda           = 1,
      max_depth        = 4,
      subsample        = 0.5,
      colsample_bytree = 0.5,
      min_child_weight  = 2,
      num_class        = length(unique(data[[target_col]]))
    )
  }

  #Set up the fold indices
  n_folds <- length(unique(data$fold_id%>%na.omit()))
  # Prepare features
  features <- setdiff(names(data), c(target_col, site_col, "fold_id"))

  # Train model
  cat("Starting site-stratified hyperparameter tuning...\n")

  fold_models <- list()
  fold_results <- list()
  best_params <- list()

  # Run through each train/val fold and hypertune
  for (i in seq(1,n_folds)) {
    cat(paste0("Tuning fold ", i, " of ", n_folds, "...\n"))

    #setup performance dataframe
    perf <- data.frame()

    # Split data into training and validation sets based on indices above

    train_data <- data%>%filter(fold_id != i)
    val_data   <- data%>%filter(fold_id == i)

    # set up weights (if none provided, default to 1)
    if (is.null(weights)) {
      w_train <- rep(1, nrow(train_data))
      w_val   <- rep(1, nrow(val_data))
    } else if (is.function(weights)) {
      #set custom weights function
      w_train <- weights(train_data)
      w_val   <- weights(val_data)
    } else {
      stop("`weights` must be NULL or a function(data) -> numeric")
    }

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

    #run through all hyper parameters and save to fold_models with name foldi_gridj
    for (j in 1:nrow(tune_grid)) {

      #setup parameters for tune
      params <- list(
        objective        = tune_grid$objective[j],
        eval_metric      = tune_grid$eval_metric[j],
        tree_method = "exact", #deterministic method for reproducibility
        eta              = tune_grid$eta[j],
        gamma            = tune_grid$gamma[j],
        alpha            = tune_grid$alpha[j],
        lambda           = tune_grid$lambda[j],
        max_depth        = tune_grid$max_depth[j],
        subsample        = tune_grid$subsample[j],
        colsample_bytree = tune_grid$colsample_bytree[j],
        min_child_weight  = tune_grid$min_child_weight[j],
        num_class        = length(unique(data[[target_col]]))
      )
      #set seed for reproducibility
      set.seed(123)

      #train model with hyper parameters
      model_ij <- xgb.train(
        params = params,
        data = dtrain,
        nrounds = tune_grid$nrounds[j],
        watchlist = watchlist,
        #change early stopping rounds based on eta (smaller eta, larger rounds)
        early_stopping_rounds = ifelse(tune_grid$eta[j] >= 0.1, 250,
                                       ifelse(tune_grid$eta[j] >= 0.01, 500,
                                              1000)),
        print_every_n = 1000,
        verbose = 0,
        nthread = 1
      )

      pred_col <- paste0(target_col, "_guess")

      # --- Validation Predictions ---
      pred_raw <- predict(model_ij, dval, iterationrange = c(1, model_ij$best_iteration))

      if (params$objective == "multi:softprob") {
        num_class <- params$num_class
        pred_mat <- matrix(pred_raw, nrow = nrow(val_data), ncol = num_class, byrow = TRUE)
        val_data[[pred_col]] <- max.col(pred_mat) - 1
        val_data[[paste0(pred_col, "_probs")]] <- apply(pred_mat, 1, max)
      } else {
        val_data[[pred_col]] <- pred_raw
      }

      # --- Training Predictions ---
      pred_raw <- predict(model_ij, dtrain, iterationrange = c(1, model_ij$best_iteration))

      if (params$objective == "multi:softprob") {
        num_class <- params$num_class
        pred_mat <- matrix(pred_raw, nrow = nrow(train_data), ncol = num_class, byrow = TRUE)
        train_data[[pred_col]] <- max.col(pred_mat) - 1
        train_data[[paste0(pred_col, "_probs")]] <- apply(pred_mat, 1, max)
      } else {
        train_data[[pred_col]] <- pred_raw
      }
      # --- Validation Metrics ---
      mlogloss_val <- model_ij$evaluation_log$eval_mlogloss[model_ij$best_iteration]
      # Convert to symbols for tidy evaluation
      true_sym <- sym(target_col)
      pred_sym <- sym(pred_col)
      # Use conf_mat and summary to get performance metrics
      conf_table <- val_data %>%
        mutate(
          !!true_sym := as.factor(!!true_sym),
          !!pred_sym := factor(!!pred_sym, levels = levels(!!true_sym))
        ) %>%
        yardstick::conf_mat(truth = !!true_sym, estimate = !!pred_sym)

      performance <- summary(conf_table)
      #extracting metrics
      kappa_val <- performance %>%
        filter(.metric == "kap") %>%
        pull(.estimate)
      f_score_val <- performance %>%
        filter(.metric == "f_meas") %>%
        pull(.estimate)
      acc_val <- performance %>%
        filter(.metric == "accuracy") %>%
        pull(.estimate)
      # --- Training Metrics ---
      mlogloss_train <- model_ij$evaluation_log$train_mlogloss[model_ij$best_iteration]
      # Convert to symbols for tidy evaluation
      true_sym <- sym(target_col)
      pred_sym <- sym(pred_col)
      # Use conf_mat and summary to get performance metrics
      conf_table <- train_data %>%
        mutate(
          !!true_sym := as.factor(!!true_sym),
          !!pred_sym := factor(!!pred_sym, levels = levels(!!true_sym))
        ) %>%
        yardstick::conf_mat(truth = !!true_sym, estimate = !!pred_sym)

      performance <- summary(conf_table)
      #extracting metrics
      kappa_train <- performance %>%
        filter(.metric == "kap") %>%
        pull(.estimate)
      f_score_train <- performance %>%
        filter(.metric == "f_meas") %>%
        pull(.estimate)
      acc_train <- performance %>%
        filter(.metric == "accuracy") %>%
        pull(.estimate)

      # --- Combine ---
      perf <- rbind(perf, data.frame(
        fold          = i,
        grid_id       = j,
        best_iter     = model_ij$best_iteration,
        mlogloss_val  = mlogloss_val,
        acc_val       = acc_val,
        kappa_val     = kappa_val,
        f_score_val  = f_score_val,
        mlogloss_train = mlogloss_train,
        acc_train     = acc_train,
        kappa_train   = kappa_train,
        f_score_train = f_score_train,
        diff_acc      = abs(acc_val - acc_train)
      ))
      # Store model
      fold_models[[paste0("fold", i, "_grid", j)]] <- model_ij

      if(j/50 == round(j/50)){
        cat(paste0("  Completed ", j, " of ", nrow(tune_grid), " hyperparameter combinations...\n"))
      }
    }

    perf_dist <- ggplot(perf) +
      geom_histogram(aes(x = mlogloss_val, fill = "Val")) +
      geom_histogram(aes(x = mlogloss_train, fill = "Train")) +
      labs(title = paste("Fold", i, "Val & Train mlogloss by Grid ID"),
           x = "mlogloss", y = "density", fill = "Group") +
      theme_minimal()

    # --- Pick top 10 by validation high to low kappa values ---
    top10 <- perf[order(perf$kappa_val,decreasing = T), ][1:10, ]

    # --- From those, choose 6 smallest train-val gap in accuracy ---
    best_rows  <- top10[order(top10$diff_acc), ][1:6,]

    #order from highest to lowest kappa
    best_rows <- best_rows[order(best_rows$kappa_val, decreasing = T), ]

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
      params <- fold_models[[model_key]][["params"]]
      #remove objective, eval metric, validate parameters, tree method, nthread from params
      params <- params[!names(params) %in% c("objective", "eval_metric", "validate_parameters", "tree_method", "nthread")]
      # collapse params into a single string
      param_text <- paste(
        names(params), "=", unlist(params),
        collapse = "\n"
      )
      # Get best iteration and corresponding mlogloss
      best_iter <- which.min(eval_log$eval_mlogloss)
      val_mlogloss <- eval_log$eval_mlogloss[best_iter]

      # Create evaluation plot (learning rate)
      eval_plot <- ggplot(eval_log, aes(x = iter)) +
        geom_line(aes(y = train_mlogloss, color = "Train mlogloss")) +
        geom_line(aes(y = eval_mlogloss, color = "Eval mlogloss")) +
        geom_vline(xintercept = best_iter, linetype = "dashed", color = "black") +
        labs(title = paste("Grid", grid_id, "  Best Iter:", best_iter, "  Val mlogloss:", round(val_mlogloss, 3)),
             x = "Iteration", y = "mlogloss", color = "Metric")+
        annotate(
          "text",
          x = max(eval_log$iter) * .5,   # position on x-axis
          y = max(c(eval_log$train_mlogloss, eval_log$eval_mlogloss)) * 0.9,  # position on y-axis
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
      # Create train vs val plot
      source("src/plotting/plot_tvt_fold_acc.R")
      train_val_plots[[k]]<- plot_tvt_fold_acc(train_df = train_data,
                                        val_df = val_data,
                                        fold_model = fold_models[[model_key]],
                                        fold_num = fold_id,
                                        target_col = target_col,
                                        title = paste0("Grid ", grid_id, " "),
                                        units = units, class_levels = class_levels)$v_plot

    }
    #Create plot with top 6 eval plots
    eval_grid <-   wrap_plots(eval_plots, nrow = 2, ncol = 3) +
      theme(legend.position = "bottom")

    #Create plot with top 6 perf plots
    train_val_grid <- wrap_plots(train_val_plots, nrow = 2, ncol = 3) +
                 plot_layout( guides = "collect") &
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
      params    <- model_obj[["params"]] %>%
        as_tibble() %>%
        select(-c(objective, eval_metric, validate_parameters))

      # Store everything in a sub-list for this grid
      fold_results[[i]][["grid_results"]][[paste0("grid_", grid_id)]] <- list(
        model       = model_obj,
        perf        = perf_row,
        params      = params
      )
    }
    #clean up meemory
    gc()
  }

  return(fold_results)
}



