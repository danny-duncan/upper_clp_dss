#' XGBoost Feature Selection with Cross-Validation and SHAP Values for Regression Target
#'
#' Trains XGBoost models across multiple folds to evaluate feature importance
#' and SHAP values. This function helps identify the most predictive features
#' by aggregating performance and contribution metrics across data splits.
#'
#' @param train_val_df A data frame or tibble containing features, the target variable (`target_col`),
#'   and a `fold_id` column used for cross-validation splitting. All rows in `train_val_df` must have a `fold_id` value.
#' @param features_to_test A character vector of column names to be used as predictors.
#' @param target_col A string specifying the name of the response variable.
#' @param default_hyper_params A list of hyperparameters to pass to \code{xgb.train}.
#'   Must include the following: \code{objective}, \code{eval_metric}, \code{eta} for the model to run. Other parameters can be included but are not required.
#'   If these are not included, defaults will be set to: \code{objective = "reg:squarederror"}, \code{eval_metric = "rmse"} and  \code{eta = 0.05}.
#' @param weight_fun A function that takes a data frame and returns a numeric vector
#'   of weights. If \code{NULL}, all observations are weighted equally (1).
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{model}: A list of trained \code{xgb.Booster} objects (one per fold).
#'   \item \code{features}: The unique vector of features tested.
#'   \item \code{best_msg}: A tibble of performance metrics (rmse) and best iterations per fold.
#'   \item \code{model_importance}: A tibble of XGBoost importance metrics (Gain, Cover, Frequency) per fold.
#'   \item \code{shap_values}: A tibble of mean absolute SHAP values for train and validation sets per fold.
#' }
#'
#'
#' @examples
#' # Setting our default hyperparameters
#'
#'default_hyper_params <- list(
#'  objective        = "reg:squarederror",
#'  eval_metric      = "rmse",
#'  eta              = 0.05,
#'  gamma            = 0.6,
#'  alpha            = 0,
#'  lambda           = 1,
#'  max_depth        = 4,
#'  subsample        = 0.5,
#'  colsample_bytree = 0.5,
#'  min_child_weight  = 2
#')

#' # weighting function for TOC categories to emphasize high TOC values
#' Weighting function for TOC
#' toc_high_weights <-  function(df) {
#'  ifelse(df$TOC <= 4,1,                  # flat weight for low TOC
#'         1 + 3 *log1p(df$TOC - 4))  # grows slowly, no cap needed
#'}

#' #set aside sensor parameters that are generally useful
#'general_features <- c("FDOMc","Sensor_Turb","Sensor_Cond", "Chl_a", "Temp")
#'
#' #Running function on general features only
#'
#'general_feature_results <- xgboost_feature_selection(
#'  train_val_df = train_val,
#'  features_to_test = general_features,
#'  target_col = "TOC_cat",
#'  default_hyper_params = default_hyper_params,
#'  weight_fun = high_weight
#')

#'# Testing multiple feature sets to see if there is improvement

#' feature_results <- map(
#'  list(
#'    gen_feat = general_features,
#'    more_features = c(fewer_features, "sin_doy", "daily_canyon_mouth_flow")
#'    ),
#'   ~xgboost_feature_selection(
#'    train_val_df = train_val,
#'    features_to_test = .x,
#'    target_col = "TOC_cat",
#'    default_hyper_params = default_hyper_params
#'  )
#'
#' }
#'
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
  required_params <- c("objective", "eval_metric", "eta")

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

  for(i in unique(train_val$fold_id)){

    #train val split
    train_data <- train_val %>%
      filter(fold_id != i) %>%
      select(any_of(c(features_to_test, target, "id", "sensor_datetime", "collector")))

    val_data <- suppressMessages(
      train_val %>%
        anti_join(train_data)
    )
    if(is.null(weight_fun)){
      weight_fun <- function(df) {rep(1, nrow(df))}
    }
    #weight data
    w_train <- weight_fun(train_data)
    w_val   <- weight_fun(val_data)

    #prep matrices
    dtrain <- xgb.DMatrix(
      data = as.matrix(train_data[, features_to_test]),
      label = train_data[[target]],
      weight = w_train
    )

    dval <- xgb.DMatrix(
      data = as.matrix(val_data[, features_to_test]),
      label = val_data[[target]],
      weight = w_val
    )

    watchlist <- list(train = dtrain, eval = dval)

    #Train model with defaults
    model <- xgb.train(
      params = default_hyper_params,
      data = dtrain,
      nrounds = 10000,
      watchlist = watchlist,
      early_stopping_rounds = 1000,
      print_every_n = 1000,
      verbose = 0,
      nthread = 1
    )
    #Save model
    models[[i]] <- model
    #Save best msg
    best_msg <- tibble(
      fold_id = i,
      best_iteration = model$best_iteration,
      best_msg = model$best_msg
    ) %>%
      #clean up message
      mutate(
        train_rmse = str_extract(best_msg, "(?<=train-rmse:)\\d+\\.\\d+"),
        eval_rmse  = str_extract(best_msg, "(?<=eval-rmse:)\\d+\\.\\d+"),
        train_rmse = as.numeric(train_rmse),
        eval_rmse  = as.numeric(eval_rmse)
      )%>%
      select(-best_msg)
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

    # SHAP (Training)
    shap_long_train <- shap.prep(xgb_model = model, X_train = as.matrix(train_data[, features_to_test]))
    shap_vals_train <- shap.values(xgb_model = model, X_train = dtrain)$mean_shap_score %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Feature") %>%
      rename(train_mean_abs_shap = ".")

    # SHAP (Validation)
    shap_long_val <- shap.prep(xgb_model = model, X_train = as.matrix(val_data[, features_to_test]))
    shap_vals_val <- shap.values(xgb_model = model, X_train = dval)$mean_shap_score %>%
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
