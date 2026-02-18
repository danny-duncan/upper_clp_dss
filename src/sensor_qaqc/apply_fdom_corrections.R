#' @title Correct FDOM for Temperature and Turbidity Dependence
#'
#' @description
#' This function performs a correction on raw FDOM signals to account for environmental
#' interference from temperature and turbidity, followed by a final calibration.
#' ** This turbidity correction methods were not be developed for In-Situ Inc sensors so proceed with caution **
#'
#' **Temperature Correction**
#' Standardizes FDOM to a reference temperature (typically 25°C) using methods from USGS,
#' Watras (2011), and Downing (2012).
#' Applying methods from the USGS (https://pubs.usgs.gov/tm/01/d11/tm1d11.pdf Appendix 1, page 37),
#'  Watras, 2011 (https://aslopubs.onlinelibrary.wiley.com/doi/epdf/10.4319/lom.2011.9.296) and
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
#' See also the `apply_fdom_temp_corr()` function for temperature correction only.
#'
#' **Turbidity Correction**
#' Adjusts the temperature-corrected signal for turbidity interference using an exponential
#' decay function (YSI methods outlined in this paper: https://agupubs.onlinelibrary.wiley.com/doi/10.1002/2017WR020678 ).
#' ** This method has not be developed for In-Situ Inc sensors so proceed with caution **
#' To avoid divide-by-zero errors, turbidity values of 0
#' are treated as 0.1.
#' Formula: $fDOM_{Turb} = fDOM_{Temp} / (2.7183^{(Turbidity * -0.006)})$
#'
#' * $fDOM_{Turb}$: Turbidity-corrected FDOM fluorescence.
#' * $fDOM_{Temp}$: Temperature-corrected FDOM fluorescence (from Stage 1).
#' * $Turbidity$: Measured Turbidity (NTU).
#'
#' **Final Calibration**
#' Applies a second-order polynomial adjustment to the corrected values.(YSI methods outlined in this paper: https://agupubs.onlinelibrary.wiley.com/doi/10.1002/2017WR020678 ).
#' ** This method has not be developed for In-Situ Inc sensors so proceed with caution **
#' Formula: $(0.0044 * fDOM_{corr}^2) + (0.7324 * fDOM_{corr})$
#'
#'
#'
#' @param wide_df Input dataframe containing water quality measurements.
#' @param fdom_col Name of raw FDOM column. Default "FDOM Fluorescence".
#' @param temp_col Name of Temperature column. Default "Temperature".
#' @param turb_col Name of Turbidity column. Default "Turbidity".
#' @param Tr Reference temperature (°C). Default is 25.
#' @param rho Temperature coefficient. Default is -0.015 (1.5% decrease per °C).
#' @param fdom_temp_col Name for intermediate temp-corrected column. Default "FDOM_corr_25C".
#' @param fdom_turb_col Name for intermediate turbidity-corrected column. Default "FDOM_turb_corr".
#' @param fdom_final_col Name for the final corrected FDOM column. Default "FDOMc".
#'
#' @return A dataframe with additional columns for intermediate and final corrected FDOM values.
#' @export
apply_fdom_corrections <- function(wide_df,
                                   fdom_col = "FDOM Fluorescence",
                                   temp_col = "Temperature",
                                   turb_col = "Turbidity",
                                   Tr = 25,
                                   rho = -0.015,
                                   fdom_temp_col = "FDOM_corr_25C",
                                   fdom_turb_col = "FDOM_turb_corr",
                                   fdom_final_col = "FDOMc") {

  wide_df %>%
    dplyr::mutate(
      # --- Temperature Correction ---
      !!fdom_temp_col := dplyr::if_else(
        !is.na(.data[[fdom_col]]) & !is.na(.data[[temp_col]]),
        {
          denom_temp <- 1 + rho * (.data[[temp_col]] - Tr)
          dplyr::if_else(!is.na(denom_temp) & denom_temp != 0,
                         .data[[fdom_col]] / denom_temp, NA_real_)
        },
        NA_real_
      ),

      # ---  Turbidity Correction ---
      !!fdom_turb_col := dplyr::if_else(
        !is.na(.data[[fdom_temp_col]]) & !is.na(.data[[turb_col]]),
        {
          # Floor turbidity at 0.1 to avoid divide-by-zero
          safe_turb <- dplyr::if_else(.data[[turb_col]] == 0, 0.1, .data[[turb_col]])
          correction_factor <- 2.7183^(safe_turb * 0.006)

          dplyr::if_else(!is.na(correction_factor) & correction_factor != 0,
                         .data[[fdom_temp_col]] * correction_factor, NA_real_)
        },
        NA_real_
      ),

      # --- Final Correction ---
      !!fdom_final_col := dplyr::if_else(
        !is.na(.data[[fdom_turb_col]]),
        {
          (0.0044 * .data[[fdom_turb_col]]^2) + (0.7324 * .data[[fdom_turb_col]])
        },
        NA_real_
      )
    ) %>%
    dplyr::ungroup()
}
