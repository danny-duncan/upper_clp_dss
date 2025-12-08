#' Retrieves National Water Prediction Service (NWPS) Hydro-Ensemble Forecast
#' System (HEFS) Quantile Forecast Data.
#' https://api.water.noaa.gov/hefs/v1/docs/#/
#'
#' This function queries HEFS for quantile discharge forecasts for a specific
#' NWPS gage ID. It handles API communication, parses the complex JSON
#' structure, and flattens the quantile array into separate, named columns.
#'
#' @param ID A character string representing a single NWPS gauge ID (e.g., "COLOC070"),
#' In the return from get_hefs_gauges(), the NWPS gauge id id is `lid`.
#' @param date Date (in YYYY-MM-DD format) for which a quantile forecast is desired
#' Defaults to NULL, which will pull the latest forecast.
#'
#' @return A tibble containing the forecast date_time, max_value, and separate
#' columns for each quantile (e.g., Q0.05, Q0.50), or NULL on failure.
get_hefs_quantiles <- function(ID, date = NULL) {

  # --- 1. Input Validation ---
  if (is.null(ID) || !is.character(ID) || length(ID) != 1) {
    warning("Input 'ID' must be a single character string representing the gage ID.")
    return(NULL)
  }

  if (!is.null(date)) {
    request_suffix <- paste0("&forecast_datetime=", date, "T12:00:00Z")
  } else {
    request_suffix <- NULL
  }

  # Define the full request URL for quantile forecast data (QINE is instantaneous flow)
  full_request <- paste0("https://api.water.noaa.gov/hefs/v1/hydrograph-quantiles/?location_id=",
                         ID,
                         "&parameter_id=QINE",
                         request_suffix)

  message(sprintf("Requesting forecast data for ID %s", ID))

  # Perform the GET request
  forecast_raw <- httr::GET(url = full_request)

  # --- 2. API Request and Status Check ---

  # Extract content as text
  unpacked_data <- httr::content(forecast_raw, as = "text", encoding = "UTF-8")

  if (httr::status_code(forecast_raw) != 200) {
    warning(sprintf("API request failed for ID %s with message %s",
                    ID, str_match(unpacked_data, '"error":"([^"]+)"')[,2]))
    return(NULL)
  }

  # --- 3. JSON Parsing and Structure Check ---
  tryCatch({
    # Parse the text content into an R list/data frame
    forecast_data <- jsonlite::fromJSON(unpacked_data, simplifyVector = TRUE)
  }, error = function(e) {
    warning(sprintf("JSON Parsing Error for ID %s. Message: %s", ID, e$message))
    return(NULL)
  })

  # Check for the primary expected data element
  if (is.null(forecast_data) || !("value_set" %in% names(forecast_data))) {
    warning(sprintf("Forecast API response for ID %s is missing the expected 'value_set' element.", ID))
    return(NULL)
  }

  # --- 4. Data Transformation and Cleanup ---

  # Extract the quantile names from metadata (e.g., "0.05", "0.50") and convert to character
  quantiles <- forecast_data$metadata$exceedance_quantiles %>%
    as.character()

  forecast_date <- forecast_data$metadata$forecast_datetime %>%
    lubridate::ymd_hms()

  # Ensure we have quantile names before proceeding with unnesting/renaming
  if (is.null(quantiles) || length(quantiles) == 0) {
    warning(sprintf("Could not extract quantile names for ID %s", ID))
    return(NULL)
  }

  # Start processing the value set
  forecast_dat <- forecast_data$value_set %>%
    dplyr::as_tibble() %>%

    # Flatten the 'quantile_values' list column into separate columns.
    # New columns will be named 'quantile_values_1', 'quantile_values_2', etc. (based on names_sep = "_")
    tidyr::unnest_wider(
      col = quantile_values,
      names_sep = "_",
      names_repair = "unique"
    ) %>%

    # Rename the newly created quantile columns using the actual quantile values (e.g., Q0.05)
    dplyr::rename_with(
      # Apply the character vector of quantiles as new names, prefixing with "Q" for clarity
      .fn = ~ paste0("Q", quantiles),
      # Select the columns created by unnest_wider (e.g., quantile_values_1, ...)
      .cols = dplyr::starts_with("quantile_values_")
    ) %>%

    # Rename the datetime column
    dplyr::rename(date_time = valid_datetime) %>%

    # Convert 'valid_datetime' (ISO string) to POSIXct datetime object
    dplyr::mutate(date_time = lubridate::ymd_hms(date_time),
                  group = "Forecast",
                  forecast_date = forecast_date) %>%

    # Filter out rows where the maximum discharge value is <= 0 (often metadata or junk data)
    dplyr::filter(max_value > 0)

  message(sprintf("Successfully retrieved %s forecast records for ID %s",
                  nrow(forecast_dat), ID))

  if (lubridate::date(forecast_date) != lubridate::date(Sys.Date())) {
    message(sprintf("Note, the forecast retrieved is not current. The most recent forecast is dated %s",
                    lubridate::date(forecast_date)))
  }

  return(forecast_dat)

}
