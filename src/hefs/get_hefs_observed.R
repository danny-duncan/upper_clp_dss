#' Retrieves National Water Prediction Service (NWPS) Hydro-Ensemble Forecast
#' System (HEFS) observed stage and flow data.
#' https://api.water.noaa.gov/nwps/v1/docs/#/Gauges/Gauges_GetStageFlow
#'
#' This function queries the observed stage and flow for a specific NWPS gage ID.
#' It handles HTTP status checks, JSON parsing, and standardizes the output:
#' - Renames columns for clarity (e.g., 'primary' to 'stage_ft').
#' - Converts the 'validTime' string to a POSIXct datetime object.
#' - Converts discharge from thousands of cubic feet per second (kcfs) to cfs.
#'
#' @param ID A character string representing a single NWPS gauge ID (e.g., "COLOC070"),
#' In the return from get_hefs_gauges(), the NWPS gauge id id is `lid`.
#' @return A tibble containing the date_time, stage_ft, and discharge_cfs for the
#' previous 30 days of recorded data, or NULL if the API request or parsing fails.
get_hefs_observed <- function (ID) {

  # --- 1. Input Validation ---
  if (is.null(ID) || !is.character(ID) || length(ID) != 1) {
    warning("Input 'ID' must be a single character string representing the gage ID.")
    return(NULL)
  }

  # Define the full request URL for observed data
  # this returns the previous 30 days of data
  full_request <- paste0("https://api.water.noaa.gov/nwps/v1/gauges/", ID, "/stageflow/observed")
  message(sprintf("Requesting observed data for ID %s", ID))

  # --- 2. API Request and Status Check ---
  # Perform the GET request
  observed_raw <- httr::GET(url = full_request)

  # Check the HTTP status code (crucial for troubleshooting 404/500 errors)
  if (httr::status_code(observed_raw) != 200) {
    status <- httr::status_code(observed_raw)
    warning(sprintf("API request failed for ID %s with HTTP Status Code: %s. Check the full_request URL and API documentation.", ID, status))
    return(NULL)
  }

  # Extract content as text
  unpacked_data <- httr::content(observed_raw, as = "text", encoding = "UTF-8")

  # --- 3. JSON Parsing and Structure Check ---
  tryCatch({
    # Parse the text content into an R list/data frame
    raw_data <- jsonlite::fromJSON(unpacked_data, simplifyVector = TRUE)
  }, error = function(e) {
    warning(sprintf("JSON Parsing Error for ID %s. Message: %s", ID, e$message))
    return(raw_data <- NULL)
  })

  # Check if the expected data element ('data') exists in the response
  if (is.null(raw_data) || !("data" %in% names(raw_data))) {
    warning(sprintf("API response for ID %s is missing the expected 'data' element.", ID))
    return(NULL)
  }

  # --- 4. Data Transformation and Cleanup ---
  observed_dat <- raw_data$data %>%
    dplyr::as_tibble() %>%
    # Rename columns for clarity and consistency
    dplyr::rename(
      stage_ft = primary,
      # The secondary column is discharge in kcfs (thousands of cubic feet per second).
      discharge_kcfs = secondary,
      date_time = validTime
    ) %>%

    # Convert 'validTime' (ISO string) to POSIXct datetime object and
    # convert discharge from kcfs to cfs.
    dplyr::mutate(
      date_time = lubridate::ymd_hms(date_time),
      discharge_cfs = discharge_kcfs * 1000, # Convert from kcfs to cfs
      group = "Observed"
    ) %>%

    # Remove rows with discharge < 0 - not physically possible - also typically
    # null vals applied
    dplyr::filter(discharge_cfs >= 0) %>%

    # Select final columns (and exclude the original kcfs column and metadata)
    dplyr::select(date_time, stage_ft, discharge_cfs, group)

  message(sprintf("Successfully retrieved %s observed records for ID %s", nrow(observed_dat), ID))
  return(observed_dat)
}
