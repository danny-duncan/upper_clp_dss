#' Sequentially Tune an XGBoost Model via Warm Start
#'
#' Updates an existing XGBoost model using new site data by performing a grid search
#' over L1 (alpha) and L2 (lambda) regularization parameters. It uses the original
#' model as a starting point to adapt to new data while keeping tree structure fixed.
#'
#' @param xgb_model An object of class \code{xgb.Booster}.
#' @param train_df Data frame containing the training samples for the new site. Must contain all features used in the original model and the target variable.
#' @param val_df Data frame containing the validation samples for the guardrail site. Must contain all features used in the original model and the target variable.
#' @param target_col Character string specifying the name of the target variable. Default is "TOC".
#'
#' @return An \code{xgb.Booster} object representing the best-tuned model based on
#' the smallest difference between training and validation RMSE.
#'
#' @examples
#'
#' #' # Assuming `base_model` is a pre-trained xgboost model and `train_data`,
#'  `val_data` are data frames with the appropriate features and target variable:
#'
#' tuned_model <- xgboost_sequential_tune(
#'  xgb_model = base_model,
#'  train_df = four_new_sites,
#'  val_df = two_new_sites,
#'  target_col = "TOC"
#'  )
#'
xgboost_sequential_tune <- function(xgb_model, train_df, val_df, target_col = "TOC"){
  #input checks
  if(!inherits(xgb_model, "xgb.Booster")){
    stop("xgb_model must be an xgboost model object of class 'xgb.Booster'.")
  }
  #Train and val df are data frames and not empty
  if(!is.data.frame(train_df) | !is.data.frame(val_df)){
    stop("train_df and val_df must be data frames or tibbles.")
  }
  if(nrow(train_df) == 0 | nrow(val_df) == 0){
    stop("train_df and val_df cannot be empty.")
  }
  #Target col exists in both data frames and is numeric
  if(!(target_col %in% colnames(train_df)) | !(target_col %in% colnames(val_df))){
    stop(glue("The target_col '{target_col}' is not in both train_df and val_df."))
  }

  if(!is.numeric(train_df[[target_col]]) | !is.numeric(val_df[[target_col]])){
    stop("The target_col must be numeric in both train_df and val_df.")
  }

  features <- xgb_model$feature_names
  #make sure the features is not null or length 0
  if(is.null(features) | length(features) == 0){
    stop("The xgb_model does not have feature names. Please ensure the model was saved with correct formatting")
  }
  #make sure we have all the features in our train/val sets
  if(!all(features %in% colnames(train_df))){
    missing_cols <- features[!features %in% colnames(train_df)]
    stop(glue("The following features are not in train_df: {paste(missing_cols, collapse = ', ')}"))
  }
  if(!all(features %in% colnames(val_df))){
    missing_cols <- features[!features %in% colnames(val_df)]
    stop(glue("The following features are not in train_df: {paste(missing_cols, collapse = ', ')}"))
  }


  # Convert to DMatrix using tidy selection
  dtrain <- xgb.DMatrix(
    data = as.matrix(train_df[, features]),
    label = train_df[[target_col]]
  )
  dval <- xgb.DMatrix(
    data = as.matrix(val_df[, features]),
    label = val_df[[target_col]]
  )
  watchlist <- list(train = dtrain, val = dval)

  models_tuned <- list()
  seq_perf <- data.frame()
  #Keep the params from the original model that affect tree structure fixed (max_depth, eta, gamma, colsample_bytree, min_child_weight, subsample)
  seq_params <- list(
    max_depth = xgb_model$params$max_depth,
    eta = xgb_model$params$eta,
    gamma = xgb_model$params$gamma,
    colsample_bytree = xgb_model$params$colsample_bytree,
    min_child_weight = xgb_model$params$min_child_weight,
    subsample = xgb_model$params$subsample
  )

  # Only vary REGULARIZATION parameters (lambda and alpha)
  # These can be changed because they don't affect tree structure
  warmstart_tune_grid <- expand.grid(
    lambda = c(0.5, 1, 2, 5), # L2 regularization
    alpha = c(0, 0.5, 1, 2)# L1 regularization
  )

  #run through all hyper parameters and save to fold_models with name foldi_gridj
  for (j in 1:nrow(warmstart_tune_grid)) {
    # Combine fixed structure with varying regularization
    params <- c(
      list(
        objective = "reg:squarederror",
        eval_metric = "rmse"
      ),
      seq_params,  # Fixed structural params
      list(
        alpha = warmstart_tune_grid$alpha[j],   # Varying
        lambda = warmstart_tune_grid$lambda[j]  # Varying
      )
    )

    model_ij <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = 10000,
      watchlist = watchlist,
      early_stopping_rounds = 2000,
      verbose = 0,
      nthread = 1,
      xgb_model = xgb_model  # Warm start from base model
    )

    models_tuned[[j]] <- model_ij

    val_rmse <- as.numeric(model_ij$best_score)
    train_rmse <- as.numeric(sub(".*train-rmse:([0-9.]+).*", "\\1", model_ij$best_msg))

    seq_perf <- rbind(seq_perf, data.frame(
      grid_id = j,
      lambda = warmstart_tune_grid$lambda[j],
      alpha = warmstart_tune_grid$alpha[j],
      best_iter = model_ij$best_iteration,
      rmse_val = val_rmse,
      rmse_train = train_rmse,
      diff = abs(val_rmse - train_rmse)
    ))

  }
  #choose the model with the smallest difference between train and val rmse as the best model, to avoid overfitting
  seq_perf <- seq_perf[order(seq_perf$diff), ]
  best_seq_model <- seq_perf$grid_id[1]
  seq_model <- models_tuned[[best_seq_model]]

  return(seq_model)
}
