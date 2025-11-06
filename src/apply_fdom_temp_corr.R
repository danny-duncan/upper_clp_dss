#' @title Correct FDOM for Temperature Dependence
#'
#' @description
#' Function to correct raw FDOM signal for temperature dependence. This is done by standardizing the FDOM values to a specific temperature (e.g., 25°C).
#' Applying methods from the USGS (https://pubs.usgs.gov/tm/01/d11/tm1d11.pdf Appendix 1, page 37), Watras, 2011 (https://aslopubs.onlinelibrary.wiley.com/doi/epdf/10.4319/lom.2011.9.296) and
#' Downing, 2012 (https://aslopubs.onlinelibrary.wiley.com/doi/epdf/10.4319/lom.2012.10.767)
#' Correction formula: fDOMT = fDOMo/(1+ρ (Tm−Tr))
#' where:
#' fDOMT = temperature-corrected FDOM fluorescence
#' fDOMo = observed FDOM fluorescence
#' Tm = measured temperature (°C)
#' Tr = reference temperature (°C), typically 25°C
#' ρ = FDOM to temperature coefficient (per °C). Typically between 0.45% - 3% (average: ~ -1.5%) per °C, (ρ = −0.0045 to −0.03, average: -0.015)
#'
#' Corrections are not applied when data is missing or if the denominator is zero or NA.
#'
#' @param wide_df Input data containing water quality measurements. Must include columns for #' site, datetime, parameter, and value.
#' @param fdom_col The name of the column containing FDOM fluorescence values. Default is "FDOM".
#' @param temp_col The name of the column containing temperature values. Default is "Temperature".
#' @param Tr The reference temperature for correction. Default is 25 degrees Celsius.
#' @param rho The temperature coefficient for FDOM correction. Default is -0.015 per degree Celsius.
#'  This is an average value from literature (https://pubs.usgs.gov/tm/01/d11/tm1d11.pdf & https://aslopubs.onlinelibrary.wiley.com/doi/epdf/10.4319/lom.2012.10.767)
#'  The default value represents a 1.5% decrease in FDOM Fluorescence signal per degree C increase
#'  @param new_value_col The name of the new column to store temperature-corrected FDOM values. #' Default is "FDOM_corr_25C". #'
#'  @return #' A dataframe with the original data plus an additional column containing #' temperature-corrected FDOM values #'
#'  @examples
#'   \dontrun{
#'    # Correct FDOM by temperature
#'    result <- all_sensor_data_wide %>%
#'     apply_fdom_temp_corr(
#'      fdom_col = "FDOM Fluorescence",
#'      temp_col = "Temperature",
#'      new_value_col = "FDOM_corr_25C",
#'       Tr = 25, #' rho = -0.015
#'     )
#'   }
#'

apply_fdom_temp_corr <- function(wide_df,
                                 fdom_col = "FDOM Fluorescence",
                                 temp_col = "Temperature",
                                 fdom_corr_col = "FDOM_corr_25C",
                                 Tr = 25,
                                 rho = -0.015) {

  wide_df %>%
      dplyr::mutate(
        !!fdom_corr_col := dplyr::if_else(
          !is.na(.data[[fdom_col]]) & !is.na(.data[[temp_col]]),
          {
            denom <- 1 + rho * (.data[[temp_col]] - Tr)
            ifelse(!is.na(denom) & denom != 0, .data[[fdom_col]] / denom, NA_real_) # check if denom is real and isnt 0
          },
          NA_real_
        )
      ) %>%
      dplyr::ungroup()


}











