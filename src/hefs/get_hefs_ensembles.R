#' Retrieves National Water Prediction Service (NWPS) Hydro-Ensemble Forecast
#' System (HEFS) Enseble Forecast Data.
#' https://api.water.noaa.gov/hefs/v1/docs/#/
#'
#' This function queries HEFS for ensemble discharge forecasts for a specific
#' NWPS gage ID. It handles API communication, parses the complex JSON
#' structure, and flattens the quantile array into separate, named columns.
#'
#' @param ID A character string representing a single NWPS gauge ID (e.g., "COLOC070"),
#' In the return from get_hefs_gauges(), the NWPS gauge id id is `lid`.
#' @param param A character string indicating the forecast type desired. While
#' other forecasts are available, this function is written to retrieve QINE only.
#' @param date Date (in YYYY-MM-DD format) for which a quantile forecast is desired
#' Defaults to NULL, which will pull the latest forecast.
#'
#' @return A tibble containing the forecast date_time, value, and and ensemble
#' member for each of the 30-member ensemble
#'
get_hefs_ensemble <- function(ID, date = NULL, param = "QINE") {


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
  full_request <- paste0("https://api.water.noaa.gov/hefs/v1/ensembles/?location_id=",
                         ID,
                         "&parameter_id=QINE",
                         request_suffix)
  message(sprintf("Requesting forecast data for ID %s", ID))

  # Perform the GET request
  forecast_raw <- httr::GET(url = full_request)

  # --- 2. API Request and Status Check ---
  if (httr::status_code(forecast_raw) != 200) {
    status <- httr::status_code(forecast_raw)
    warning(sprintf("API request failed for ID %s with HTTP Status Code: %s", ID, status))
    return(NULL)
  }

  # Extract content as text
  unpacked_data <- httr::content(forecast_raw, as = "text", encoding = "UTF-8")

  # check to make sure that there is content in the forecast
  if (nchar(unpacked_data) == 2) {
    warning(sprintf("Forecast is not available for the selected date at ID %s", ID))
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
  if (is.null(forecast_data) || !("events" %in% names(forecast_data[[1]]))) {
    warning(sprintf("Forecast API response for ID %s is missing the expected 'events' element.", ID))
    return(NULL)
  }

  # --- 4. Data Transformation and Cleanup ---

  forecast_date <- forecast_data[[1]]$forecast_datetime %>%
    first() %>%
    lubridate::ymd_hms()

  # Get the list of ensemblenames
  ens_names <- forecast_data[[1]]$ensemble_member_index

  # Start processing the ensemble members set
  forecast_dat <- map2(.x = forecast_data[[1]]$events,
                       .y = ens_names,
                       ~ dplyr::as_tibble(.x) %>%
                         mutate(ensemble_member = .y,
                                valid_datetime = ymd_hms(valid_datetime))) %>%
    bind_rows() %>%
    dplyr::mutate(group = "Forecast",
                  forecast_date = forecast_date) %>%
    # Filter out rows where the maximum discharge value is <= 0 (often metadata or junk data)
    dplyr::filter(value > 0) %>%
    rename(date_time = valid_datetime)

  message(sprintf("Successfully retrieved %s forecast records for ID %s.", nrow(forecast_dat), ID))

  return(forecast_dat)

}


