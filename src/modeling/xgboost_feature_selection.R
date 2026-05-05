#' XGBoost Feature Selection with Cross-Validation and SHAP Values
#'
#' Trains XGBoost regression models across multiple folds to evaluate feature importance
#' and SHAP (SHapley Additive exPlanations) values. This function facilitates identifying
#' the most predictive features by aggregating performance and contribution metrics
#' across data splits.
#'
#' @param train_val_df A data frame or tibble containing predictor features,
#'   the target variable, and a `fold_id` column used for cross-validation.
#'   All rows must have a valid `fold_id`.
#' @param features_to_test A character vector of column names to be used as predictors.
#' @param target_col A string specifying the name of the response (target) variable.
#' @param default_hyper_params A list of hyperparameters to pass to \code{xgb.train}.
#'   If missing, the following defaults are used:
#'   \itemize{
#'     \item \code{objective = "reg:squarederror"}
#'     \item \code{eval_metric = "rmse"}
#'     \item \code{eta = 0.05}
#'     \item \code{nthread = 1}
#'   }
#' @param weight_fun A function that takes a data frame and returns a numeric vector
#'   of weights. If \code{NULL}, all observations are weighted equally (1).
#'
#' @return A named list containing:
#' \describe{
#'   \item{\code{model}}{A list of trained \code{xgb.Booster} objects, one per fold.}
#'   \item{\code{features}}{The unique vector of features used for training.}
#'   \item{\code{best_msg}}{A tibble summarizing best iterations and RMSE (train/val) for each fold.}
#'   \item{\code{model_importance}}{A tibble of XGBoost importance metrics (Gain, Cover, Frequency) for every fold.}
#'   \item{\code{shap_values}}{A tibble of mean absolute SHAP values for both training and validation sets per fold.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # 1. Define hyperparameters
#' params <- list(
#'   objective        = "reg:squarederror",
#'   eta              = 0.05,
#'   max_depth        = 4,
#'   subsample        = 0.5,
#'   colsample_bytree = 0.5
#' )
#'
#' # 2. Define a weighting function (optional)
#' # Emphasize high TOC (Total Organic Carbon) values
#' toc_weights <- function(df) {
#'   ifelse(df$TOC <= 4, 1, 1 + 3 * log1p(df$TOC - 4))
#' }
#'
#' # 3. Run feature selection
#' results <- xgboost_feature_selection(
#'   train_val_df = train_val,
#'   features_to_test = c("FDOMc", "Sensor_Turb", "Sensor_Cond", "Temp"),
#'   target_col = "TOC",
#'   default_hyper_params = params,
#'   weight_fun = toc_weights
#' )
#'
#' # 4. Inspect importance
#' print(results$model_importance)
#' }
xgboost_feature_selection <- function(train_val_df,
                                          features_to_test,
                                          target_col,
                                          default_hyper_params,
                                          weight_fun = NULL) {

  #Check inputs
  if(!is.data.frame(train_val_df)){
    stop("train_val_df must be a data frame or tibble.")
  }
  #make sure a user selected features are all in the dataframe
  if(!all(features_to_test %in% colnames(train_val_df))){
    missing_cols <- features_to_test[!features_to_test %in% colnames(train_val_df)]
    stop(glue("The following features are not in train_val_df: {paste(missing_cols, collapse = ', ')}"))
  }
  #make sure target_col is a string
  if(!is.character(target_col) | length(target_col) != 1){
    stop("target_col must be a single character string.")
  }
  #make sure target_col is in the dataframe
  if(!(target_col %in% colnames(train_val_df))){
    stop(glue("The target_col '{target_col}' is not in train_val_df."))
  }

  #Check that default_hyper_params is a list
  if(!is.list(default_hyper_params)){
    stop("default_hyper_params must be a list.")
  }
  #check that default_hyper_params has an objective, eval metric and eta
  #All other parameters have fine defaults so we can ignore them if a user does not input them
  required_params <- c("objective", "eval_metric", "eta", "nthread")

  if(!all(required_params %in% names(default_hyper_params))){
    missing_params <- required_params[!required_params %in% names(default_hyper_params)]
    message(glue("The following required hyperparameters are missing from default_hyper_params: {paste(missing_params, collapse = ', ')}"))
    message("Defaulting to: eta = 0.05, objective = 'reg:squarederror', eval_metric = 'rmse'")
    #adding in defaults as needed
    if("eta" %in% missing_params) {
      default_hyper_params$eta <- 0.05
    }
    if("objective" %in% missing_params) {
      default_hyper_params$objective <- "reg:squarederror"
    }
    if("eval_metric" %in% missing_params) {
      default_hyper_params$eval_metric <- "rmse"
    }
    if("nthread" %in% missing_params) {
      default_hyper_params$nthread <- 1
    }
  }

  #check if weight_fun is NULL and if so set to default function (everything weighted the same)
  if(is.null(weight_fun)){
    weight_fun <- function(df) {rep(1, nrow(df))}
  }
  #check to make sure weight_fun is a function
  if(!is.function(weight_fun)){
    stop("weight_fun must be a function that takes a data frame and returns a numeric vector of weights.")
  }


  # double check that there are no duplicates in features
  features_to_test <- unique(features_to_test)
  #get unique fold ids
  fold_ids <- unique(train_val_df$fold_id)

  if(any(is.na(fold_ids)) | length(fold_ids) == 0 | any(is.null(fold_ids))){
    stop("All rows in train_val_df must have a fold_id value and cannot be NULL or NA.")
  }

  #create blank dataframes to store importance/shap values
  importance_df <- tibble(Feature = features_to_test)
  shap_val_df <- tibble(Feature = features_to_test)
  best_msgs <- tibble()
  models <- list()
  fold_ids <- sort(unique(train_val_df$fold_id), decreasing = FALSE)


  for(i in fold_ids){
    #train val split
    train_data <- train_val_df %>%
      filter(fold_id != i) %>%
      select(any_of(c(features_to_test, target_col, "id", "sensor_datetime", "collector")))

    val_data <- suppressMessages(
      train_val_df %>%
        anti_join(train_data)
    )

    #weight data
    w_train <- weight_fun(train_data)
    w_val   <- weight_fun(val_data)

    #prep matrices
    dtrain <- xgb.DMatrix(
      data = as.matrix(train_data[, features_to_test]),
      label = train_data[[target_col]],
      weight = w_train
    )

    dval <- xgb.DMatrix(
      data = as.matrix(val_data[, features_to_test]),
      label = val_data[[target_col]],
      weight = w_val
    )

    watchlist <- list(train = dtrain, eval = dval)

    #Train model with defaults
    model <- xgb.train(
      params = default_hyper_params,
      data = dtrain,
      nrounds = 10000,
      evals = watchlist,
      early_stopping_rounds = 1000,
      print_every_n = 1000,
      verbose = 0
    )
    #Save model
    models[[i]] <- model
    #get eval log
    eval_log <- attributes(model)$evaluation_log
    #get best iteration
    best_iter <- as.numeric(xgb.attr(model, "best_iteration"))
  #Extract best message
    best_msg <- tibble(
      fold_id = i,
      best_iteration = best_iter,
      # We use best_iter + 1 because R is 1-indexed and XGBoost is 0-indexed
      train_rmse = eval_log$train_rmse[best_iter + 1],
      eval_rmse  = eval_log$eval_rmse[best_iter + 1]
    )

    #best message
    best_msgs <- bind_rows(best_msgs, best_msg)%>%
      distinct()

    #Feature importance
    importance_df <- importance_df%>%
      left_join(xgb.importance(feature_names = features_to_test, model = model)%>%
                  dplyr::rename_with( #fix column names
                    ~ glue::glue("{.x}_fold_{i}"),
                    .cols = c(Gain, Cover, Frequency)
                  ), by = "Feature")

    train_X <- as.matrix(train_data[, features_to_test])
    val_X <- as.matrix(val_data[, features_to_test])
    #In case we need to predict and output a plot of predicted vs actual for train and val
    # train_data$pred <- predict(model, newdata = train_X)
    # val_data$pred <- predict(model, newdata = val_X)
    #
    # max_val <- max(c(train_data[[target_col]], val_data[[target_col]]), na.rm = TRUE)
    # min_val <- min(c(train_data[[target_col]], val_data[[target_col]]), na.rm = TRUE)
    #
    # ggplot(train_data, aes(x = !!sym(target_col), y = pred )) +
    #   geom_point() +
    #   geom_point(data = val_data, aes(x = !!sym(target_col), y = pred), color = "blue") +
    #   geom_abline(slope = 1, intercept = 0, color = "red") +
    #   labs(title = glue("Fold {i} - Train: Predicted vs Actual"),
    #        x = paste0(target_col, " (Actual)"),
    #        y = paste0(target_col, " (Predicted)")) +
    #   ylim(min_val,max_val)+
    #   xlim(min_val,max_val)
    # SHAP (Training)
    shap_long_train <- shap.prep(xgb_model = model, X_train = train_X)
    shap_vals_train <- shap.values(xgb_model = model, X_train = train_X)$mean_shap_score %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Feature") %>%
      rename(train_mean_abs_shap = ".")

    # SHAP (Validation)
    shap_long_val <- shap.prep(xgb_model = model, X_train = val_X)
    shap_vals_val <- shap.values(xgb_model = model, X_train = val_X)$mean_shap_score %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Feature") %>%
      rename(val_mean_abs_shap = ".")

    shap_val <- shap_vals_val%>%
      left_join(shap_vals_train, by = "Feature")%>%
      dplyr::rename_with( #fix column names
        ~ glue::glue("{.x}_fold_{i}"),
        .cols = c(val_mean_abs_shap, train_mean_abs_shap)
      )
    shap_val_df <- shap_val_df%>%
      left_join(shap_val, by = c("Feature"))
  }

  #output
  list(
    model = models,
    features = features_to_test,
    best_msg = best_msgs,
    model_importance = importance_df,
    shap_values = shap_val_df
  )

}
