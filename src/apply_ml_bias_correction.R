#' Apply Multiple Bias Correction Methods to an ML Model
#'
#' This function applies several bias correction techniques to predictions from a
#' trained machine learning model (e.g., XGBoost) and evaluates their performance
#' on both a validation and test dataset. It also fits a secondary ML model
#' (ML2-RES) to predict residuals from the first model and adds these corrections
#' back to the original predictions.
#'
#' @param train_val_df A data frame containing the combined training and validation data.
#'   Must include a `fold_id` column used to select the validation subset, and the
#'   response column specified by `target_col`.
#' @param testing_df A data frame containing the test dataset. Must include the
#'   same feature columns as the training data and the response variable (`target_col`).
#' @param fold_model A trained ML model object (typically an XGBoost model) that
#'   includes a `feature_names` element listing the predictor variables.
#' @param fold_num Integer specifying which fold to use as the validation subset
#'   (must match values in the `fold_id` column of `train_val_df`).
#' @param target_col Character string specifying the name of the target (response) variable.
#'   Defaults to `"TOC"`.
#'
#' @details
#' The function performs bias correction using multiple methods:
#'
#' \itemize{
#'   \item **Empirical Distribution Mapping (EDM):**
#'     Matches the empirical cumulative distribution function (ECDF) of predictions
#'     to that of observed values in the validation set.
#'   \item **Regression on Estimated (ROE):**
#'     Fits a simple linear regression between observed and predicted values
#'     (observed = m * predicted + b).
#'   \item **Linear Transfer Function (LTF):**
#'     Applies a linear scaling based on the covariance of predicted and EDM-corrected values.
#'   \item **Z-score Correction (ZZ):**
#'     Standardizes the prediction distribution to match the observed mean and variance.
#'   \item **ML2 Residual Correction (ML2-RES):**
#'     Trains a secondary XGBoost model to predict residuals
#'     (observed − predicted) from the original model using the same features
#'     plus the normalized prediction as an additional feature.
#' }
#'
#' The ML2-RES model uses default hyperparameters and is trained on the validation
#' data. Its predictions are then added to the original predictions for both the
#' validation and test datasets.
#'
#' The final output includes all correction methods applied to both the validation
#' and testing data, stacked into one data frame for comparison.
#'
#' @return
#' A combined data frame containing the validation and test subsets, with the following
#' columns added:
#' \itemize{
#'   \item `predicted` – raw model predictions
#'   \item `edm` – EDM bias-corrected predictions
#'   \item `roe` – ROE bias-corrected predictions
#'   \item `ltf` – LTF bias-corrected predictions
#'   \item `zz` – Z-score bias-corrected predictions
#'   \item `ml2_res_pred` – residual predictions from the ML2-RES model
#'   \item `ml2` – ML2-RES corrected predictions (`predicted + ml2_res_pred`)
#'   \item `residuals` – observed minus predicted values
#'   \item `group` – `"Validation"` or `"Test"`, indicating data subset
#' }
#'
#' @examples
#' corrected_df <- apply_ml_bias_correction(
#'   train_val_df = train_val_data,
#'   testing_df = test_data, # this could also be a separate unseen dataset (ie live sensor data)
#'   fold_model = xgb_model,
#'   fold_num = 1,
#'   target_col = "TOC"
#' )
#' #selecting ml2 corrected values for final output
#' output_df <- corrected_df %>%
#' filter(group == "Test") %>% #grab the new data
#'  select(id, fold_id, group, predicted , ml2) #grab original predictions and ml2 corrected predictions
#'
apply_ml_bias_correction <- function(train_val_df, testing_df, fold_model, fold_num, target_col = "TOC") {

  #Get features
  features <- fold_model$feature_names
  # compute min and max TOC across ALL input data for ML2-res scaling
  toc_min <- min(train_val_df[[target_col]], testing_df[[target_col]])
  toc_max <- max(train_val_df[[target_col]], testing_df[[target_col]])

  #Get validation data
  val_data <- train_val_df %>%
    filter(fold_id == fold_num)%>%
    mutate(
      #Make predictions on set
      predicted = predict(
        fold_model,
        newdata = as.matrix(select(., all_of(features)))),
      group = "Validation"
    )
  val_preds <- val_data$predicted
  val_obs <- val_data[[target_col]]

  # Function to perform Empirical Distribution Mapping (EDM) bias correction
  # For each guess in pred_vals: find its percentile in ref_pred_vals; then take that percentile quantile of ref_obs_vals
  #Def:
  #pred_vals: our model predictions (in this case, from validation or testing set)
  #ref_pred_vals: reference predictions (in this case, again from validation set)
  #ref_obs_vals: reference observations (in this case, observed TOC from validation set)
  ecdf_pred <- ecdf(val_preds) #calculate the empirical cumulative distribution function (ecdf) of the reference predictions

  val_corr <- val_data %>%
    #get the percentiles of our predictions based on the reference predictions, then convert to quantiles of observed values
    mutate(edm = quantile(val_obs, probs = ecdf_pred(predicted), type = 8))

  #ROE correction
  mroe = cov(val_preds, val_obs) / var(val_preds)
  broe = mean(val_obs) - mroe * mean(val_preds)

  val_corr <- val_corr%>%
    mutate(roe = predicted * mroe + broe)

  #LTF correction

  mltf = cov(val_preds, val_corr$edm) / var(val_preds)
  bltf = mean(val_obs) - mltf * mean(val_preds)

  val_corr <- val_corr%>%
    mutate(ltf = predicted * mltf + bltf)

  #Z-score correction
  mzz = sqrt(var(val_obs))/sqrt(var(val_preds))
  bzz = mean(val_obs) - mzz * mean(val_preds)

  val_corr <- val_corr%>%
    mutate(zz = predicted * mzz + bzz)

  #ml2-res correction
  X_res <- val_data %>%
    mutate(predicted = rescale(val_preds, to = c(0,1), from = c(toc_min,toc_max)))%>%
    select(all_of(features), predicted) %>%
    as.matrix()
  #residuals
  y_res <- val_obs - val_preds

  #Xgboost inputs/settings for ML2 Res model
  dres_train <- xgb.DMatrix(data = X_res, label = y_res)
  default_hyper_params <- list(
    objective        = "reg:squarederror",
    eval_metric      = "rmse",
    eta              = 0.1,
    gamma            = 0.6,
    alpha            = 0,
    lambda           = 1,
    max_depth        = 4,
    subsample        = 0.5,
    colsample_bytree = 0.5,
    min_child_weight  = 2
  )
  #train model on original features + predicted values  and try to predict residuals
  ml2_res_model <- xgb.train(default_hyper_params, data = dres_train, nrounds = 100,verbose = 0)
  #predict residuals on validation set
  val_corr <- val_corr%>%
    mutate(ml2_res_pred = predict(ml2_res_model,
                                  xgb.DMatrix(as.matrix(val_data %>%
                                                          mutate(predicted = rescale(val_preds, to = c(0,1), from = c(toc_min,toc_max)))%>%
                                                          select(all_of(features), predicted)))),
           #add in residual predictions to original predictions
           ml2 = predicted + ml2_res_pred,
           residuals = !!sym(target_col) - predicted
    )

  # Apply to testing set
  test_corr <- testing_df%>%
    select(id, all_of(features),!!sym(target_col))%>%
    mutate(
      fold_id = fold_num,
      #apply model to testing set
      predicted = predict(
        fold_model,
        newdata = as.matrix(select(., all_of(features)))
      ),
      #EDM
      edm = quantile(val_obs, probs = ecdf_pred(predicted), type = 8),
      #ROE
      roe = predicted * mroe + broe,
      #LTF
      ltf = predicted * mltf + bltf,
      #Z-score
      zz = predicted * mzz + bzz,
      #ML2-res
      ml2_res_pred = predict(
        ml2_res_model,
        xgb.DMatrix(as.matrix(
          select(., all_of(features)) %>% mutate(predicted = rescale(predicted, to = c(0,1), from = c(toc_min,toc_max))))
        )
      ),
      ml2 = predicted + ml2_res_pred,
      #calculate residuals
      residuals = !!sym(target_col) - predicted,
      group = "Test"
    )
  #return as joined dataset
  bind_rows(val_corr, test_corr)

}

