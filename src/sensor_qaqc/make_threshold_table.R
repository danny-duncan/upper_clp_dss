#' @title Generate Threshold Table for QAQC
#'
#' @description
#' A function designed to generate a threshold table for QAQC. This table
#' contains the thresholds for the mean, slope_behind, and standard deviation
#' of the mean for each site and season.
#'
#' @param df A data frame of cleaned, verified data
#'
#' @return A data frame with the thresholds for the mean, slope_behind, and
#' standard deviation of the mean for each site and season.
#'
#' @examples
#' make_threshold_table(df = all_data_flagged$`archery-Actual Conductivity`)

make_threshold_table <- function(df){

  slope_down <- df %>%
    # Get threshold for negative slope data
    dplyr::filter(slope_behind < 0) %>%
    dplyr::group_by(season) %>%
    dplyr::summarize(f_slope_behind_01 = quantile(slope_behind, 0.01, na.rm = TRUE))

  slope_up <- df %>%
    # Get threshold for positive slope data
    dplyr::filter(slope_behind > 0) %>%
    dplyr::group_by(season) %>%
    dplyr::summarize(f_slope_behind_99 = stats::quantile(slope_behind, 0.99, na.rm = TRUE))

  good_data_stats <- df %>%
    dplyr::group_by(season) %>%
    # join our slope data thresholds:
    dplyr::left_join(slope_up, by = "season") %>%
    dplyr::left_join(slope_down, by = "season") %>%
    # develop other thresholds across all data
    dplyr::mutate(f01 = stats::quantile(mean, 0.01, na.rm = TRUE),
                  f99 = stats::quantile(mean, 0.99, na.rm = TRUE)) %>%
    # THEN, GET STANDARD DEVIATION OF ONLYYYY VALUES WITHIN THE 1-99th PERCENTILE OF THAT GOOD DATA:
    dplyr::filter(mean > f01 & mean < f99) %>%
    # SD is the ONLY statistic that uses this winnowed-down data set in its development.
    # All else use the full, "good" data set.
    dplyr::summarize(site = paste0(unique(site)),
                     parameter = paste0(unique(parameter)),
                     t_mean01 = as.numeric(paste0(unique(f01))),
                     t_mean99 = as.numeric(paste0(unique(f99))),
                     t_slope_behind_01 = as.numeric(paste0(unique(f_slope_behind_01))),
                     t_slope_behind_99 = as.numeric(paste0(unique(f_slope_behind_99))),
                     # This stat is {{currently}} useless, but keeping for now:
                     t_sd_0199 = stats::sd(mean, na.rm = T))

  return(good_data_stats)

}
