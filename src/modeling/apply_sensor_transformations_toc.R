#' Apply sensor-based transformations for TOC modeling
#'
#' This function takes a wide-format water quality dataset and applies a set of
#' transformations commonly used to model Total Organic Carbon (TOC).
#'
#' @param wide_df A data frame in wide format containing at least the following
#'   columns: datetime (see `dt_col`), `FDOM Fluorescence`, `Chl-a Fluorescence`,
#'   `Turbidity`, `Specific Conductivity`, and `Temperature`.
#'  @param dt_col A string specifying the name of the datetime column in `wide_df`.
#'
#'
#' @return A transformed tibble with:
#'   * Derived feature columns including FDOM ratios, products, logarithmic and squared terms,
#'     and seasonal sine of day-of-year (`sin_doy`).
#'   * Renamed sensor columns for clarity (`FDOM`, `Chl_a`, `Sensor_Turb`,
#'     `Sensor_Cond`, `Temp`).
#'   * Rows filtered to exclude missing values in key sensor variables.
#'
#' @details
#' - `Turbidity` values are constrained to the range [0.001, 1000] to remove
#'   physically unreasonable values.
#' - Feature engineering includes:
#'   - Ratios: FDOM/Temp, FDOM/Cond, FDOM/Turb, Turb/FDOM, Temp/FDOM
#'   - Log-transformation: log1p(FDOM)
#'   - Interactions: Temp×FDOM, Temp×Turb, FDOM×Cond, FDOM×Turb
#'   - Quadratic: FDOM²×Temp
#'   - Seasonal: sine of day-of-year
#'
#' @examples
#' transformed <- apply_sensor_transformations_toc(wide_df, dt_col = "DT_round")
#'
apply_sensor_transformations_toc <- function(wide_df, dt_col = "DT_round") {

  wide_df %>%
    mutate(
      doy = yday(!!sym(dt_col)),
      sin_doy = sin(2 * pi * doy / 365.25),
      Turbidity = if_else(Turbidity <= 0, 0.1, if_else(Turbidity >= 1000, 1000, Turbidity)), # constrain turbidity to reasonable range
      # FDOM_corr_25c features
      f_temp    = FDOM_corr_25c / Temperature,
      f_sc      = FDOM_corr_25c / `Specific Conductivity`,
      f_turb    = FDOM_corr_25c / Turbidity,
      turb_f    = Turbidity / FDOM_corr_25c,
      temp_f    = Temperature / FDOM_corr_25c,
      f_log     = log1p(FDOM_corr_25c),
      temp_x_f  = Temperature * FDOM_corr_25c,
      temp_x_turb = Temperature * Turbidity,
      f_x_sc    = FDOM_corr_25c * `Specific Conductivity`,
      f_x_turb  = FDOM_corr_25c * Turbidity,
      f2_temp   = (FDOM_corr_25c^2) * Temperature)%>%
    rename(
      FDOM = `FDOM Fluorescence`,
      FDOMc = FDOM_corr_25c,
      Chl_a = `Chl-a Fluorescence`,
      Sensor_Turb = Turbidity,
      Sensor_Cond = `Specific Conductivity`,
      Temp = Temperature
    )%>%
    filter( !is.na(FDOM) & !is.na(Chl_a) & !is.na(Sensor_Turb) & !is.na(Temp)& !is.na(Sensor_Cond))
}
