#' Calculate lagged values for time series data
#'
#' This function computes lagged values for a specified column in a data frame
#' containing time series data. It allows for grouping by site and parameter
#' columns, and creates a new column with the lagged values.
#'
#'
#' @param wide_df A dataframe in wide format with a column for each parameter,
#'  a column for site identifiers, and a column for datetime information.
#'  The datetime column must be of type `POSIXct` or numeric, or otherwise
#' @param site_col A string giving the name of the column containing site
#'   identifiers.
#' @param parameter_col A string specifying the parameter to apply lagging to. Default
#'  is "Temperature".
#' @param dt_col A string giving the name of the column containing datetime
#'   information. Must be a `POSIXct` or numeric type, or otherwise convertible
#'   to numeric.
#' @param lag_fun A string specifying the lagging function to use. Should be one of
#'  "mean", "median", "min", "max", "sd", or "var". Default is "median".
#' @param time_period A string specifying the time period for lagging. Should be one of
#'  "min", "hour", "day" & "week" with a number specifying the number that unit. Default is "12 hour".
#'
#' @param new_value_col A string giving the name of the new column to store the
#'   gap-filled values. Default is `"lagged_median"`.
#'
#' @return A data frame with the original columns plus an additional column
#'   containing the lagged parameter values based on the `lag_fun`, `time_period` and `parameter_col`
#'   specified. The new column is named according to the `new_value_col` parameter.
#' @details
#'
#' This function uses the `slide_period_dbl` function from the `slider` package to compute lagged values.
#' It groups the data by the specified site column and applies the lagging function over the defined time period (`time_period`).
#' The function supports various lagging functions including mean, median, min, max, standard deviation, and variance.
#' The function is designed to handle missing values appropriately based on the chosen lagging function but in all cases,
#' does not require a complete window to perform calculations
#'
#'
#'

calc_lagged_parameter <- function(wide_df, site_col = "site", parameter_col = "Temperature",
                                  dt_col = "DT_round", lag_fun = "median",
                                  time_period = "12 hour", new_value_col = "lagged_median") {

  # separate time period
  time_period_num <- as.numeric(str_split_i(time_period, " ", 1))
  time_period_str <- str_split_i(time_period, " ", 2)

  # parsing function
  fn <- switch(lag_fun,
               "var" = stats::var,
               "mean" = mean,
               "median" = median,
               "max" = max,
               "min" = min,
               "sd" = sd,
               stop("Unsupported lag_fun: ", lag_fun)
  )

  # safe wrapper
  fn_used <- if (lag_fun %in% c("mean", "median", "max", "min", "sum")) {
    function(x) {
      x <- x[!is.na(x)] # check if the window is all NAs (return NA rather than evaluating to Inf/NaN)
      if (length(x) == 0) NA_real_ else fn(x, na.rm = TRUE)
    }
  } else if (lag_fun %in% c("var")) {
    function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0) NA_real_ else fn(x, use = "na.or.complete")
    }
  } else {
    function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0) NA_real_ else fn(x)
    }
  }

  wide_df %>%
    arrange(!!sym(site_col), !!sym(dt_col)) %>%
    group_by(!!sym(site_col)) %>%
    mutate(
      !!sym(new_value_col) := slide_period_dbl(
        .x = !!sym(parameter_col),
        .i = !!sym(dt_col),
        .period = time_period_str,
        .f = fn_used,
        .before = time_period_num,
        .complete = FALSE
      )
    ) %>%
    ungroup()
}


