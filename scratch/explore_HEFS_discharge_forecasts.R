#' Test out NOAA HEFS discharge forecast retrieval
#'
#' The HEFS is a system developed by the National Weather Service (NWS) to
#' provide ensemble streamflow predictions. HEFS has been implemented at NWS
#' River Forecast Centers (RFCs) for over 3,000 locations throughout the United
#' States. HEFS integrates meteorological forecasts with hydrologic modeling to
#' produce probabilistic streamflow forecasts and other hydrological variables.
#' The system assesses the potential impacts of high-flow events and assesses
#' flood risk. Most importantly, HEFS is a valuable tool for decision-makers
#' and emergency managers, providing them with the necessary information to
#' make informed decisions about water-related activities and potential flood
#' hazards.
#'
#'
#' Read more about the NWPS Hydrologic Ensemble Forecast Service (HEFS) and API
#' here: https://water.noaa.gov/about/api
#' and here: https://github.com/NOAA-OWP/data-service-notebooks/blob/master/HEFS/0_executive_overview.ipynb
#'
#' Short blurb about the HEFS API from that link:
#' The experimental NWPS HEFS Forecast API can be used to retrieve real-time
#' and historical HEFS forecast data for NWPS locations. The forecast data
#' includes complete ensembles and the set of ensemble time-step quantiles. The
#' HEFS is a service developed by the National Weather Service (NWS) to provide
#' ensemble streamflow predictions. HEFS has been implemented at NWS River
#' Forecast Centers (RFCs) for over 3,000 locations throughout the United
#' States. HEFS integrates meteorological forecasts with hydrologic modeling to
#' produce probabilistic streamflow forecasts and other hydrological variables.
#' The service assesses the potential impacts of high-flow events and assesses
#' flood risk. The service currently serves 10 days of data. Guidance from
#' individual RFCs may not be available at all times.


# Import libraries
library(httr)
library(jsonlite)
library(sf)
library(leafem)
library(mapview)
library(nhdplusTools)
library(tidyverse)

# Define functions to retrieve gauge stations, observed discharge, and
# forecast discharge:

#' Retrieves National Water Prediction Service (NWPS) Hydro-Ensemble Forecast
#' System (HEFS) gauges within a given Area of Interest (AOI).
#'
#' This function converts the bounding box of an input spatial feature to a
#' query string, makes an API call to a NOAA service, and attempts to return the
#' results as an sf object. Includes error checking for input object and HTTP
#' response status.
#'
#' @param aoi An object of class 'sf', 'sfg', or 'sfc' representing the area of interest.
#' @param base_url The base URL for the HEFS API endpoint. Defaults to a placeholder
#'                 URL; users should verify the exact endpoint based on API documentation.
#' @return An sf object containing the retrieved gages (points), or NULL if the
#'         input is invalid or the API request fails.
get_hefs_gauges <- function(aoi, base_url = "https://api.water.noaa.gov/nwps/v1/gauges") {

  # --- 1. Input Validation ---
  if (!inherits(aoi, "sf") && !inherits(aoi, "sfg") && !inherits(aoi, "sfc")) {
    warning("Input 'aoi' must be an sf object (sf, sfc, or sfg). Returning NULL.")
    return(NULL)
  }

  # --- 2. Coordinate System Standardization ---
  # Transform the AOI to the standard WGS 84 (EPSG 4326) required by api
  aoi <- sf::st_transform(aoi, crs = 4326)

  # --- 3. Bounding Box Extraction and Formatting ---
  # Extract the bounding box (bbox) from the transformed AOI.
  aoi_bbox <- sf::st_bbox(aoi)

  # Format the bounding box into the required URL query string.
  # This uses the same %.4f precision for standard API compliance.
  bbox_string <- sprintf(
    "?bbox.xmin=%.4f&bbox.ymin=%.4f&bbox.xmax=%.4f&bbox.ymax=%.4f",
    aoi_bbox["xmin"],
    aoi_bbox["ymin"],
    aoi_bbox["xmax"],
    aoi_bbox["ymax"]
  )

  # Construct the final API request URL, including the requested spatial
  # reference ID (SRID)
  full_request <- paste0(base_url, bbox_string, "&srid=EPSG_4326")
  message(sprintf("Requesting data from %s", full_request))

  # --- 4. API Request and Status Check ---
  # Perform the GET request
  raw_dat <- httr::GET(url = full_request)

  # Check the HTTP status code (crucial for troubleshooting 404/500 errors)
  if (httr::status_code(raw_dat) != 200) {
    status <- httr::status_code(raw_dat)
    warning(sprintf("API request failed with HTTP Status Code: %s. Check the full_request URL and API documentation.", status))
    return(NULL)
  }

  # Extract content as text
  unpacked_data <- httr::content(raw_dat, as = "text", encoding = "UTF-8")

  # --- 5. JSON Parsing and Structure Check ---
  gauge_data <- NULL
  tryCatch({
    # Parse the text content into an R list/data frame
    gauge_data <- jsonlite::fromJSON(unpacked_data, simplifyVector = TRUE)
  }, error = function(e) {
    warning(sprintf("JSON Parsing Error: Could not convert response to JSON. Message: %s", e$message))
    return(NULL)
  })

  # Check if the expected data element (gauges) exists in the response
  if (is.null(gauge_data) || !("gauges" %in% names(gauge_data))) {
    warning("API response structure is missing the expected 'gauges' element. Returning NULL.")
    return(NULL)
  }

  # --- 6. Convert to SF Object ---
  # Extract the gauges data, convert to a standard data frame (tibble),
  # and then to an sf object using the 'longitude' and 'latitude' columns.
  gauge_data <- gauge_data$gauges %>%
    dplyr::as_tibble() %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  # --- 7. Return Result ---
  message(sprintf("Successfully retrieved %s gauges.", nrow(gauge_data)))
  return(gauge_data)
}


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


#' Retrieves Colorado Basin River Forecast Center Hydro-Ensemble Forecast
#' System (HEFS) Enseble Forecast Data.
#' https://www.cbrfc.noaa.gov/lmap/lmap.php
#'
#' This function queries HEFS for ensemble discharge forecasts for a specific
#' NWPS gage ID. It handles API communication, parses the complex JSON
#' structure, and flattens the quantile array into separate, named columns.
#'
#' @param ID A character string representing a single NWPS gauge ID (e.g., "COLOC070"),
#' In the return from get_hefs_gauges(), the NWPS gauge id id is `lid`.
#' @param date Date (in YYYY-MM-DD format) for which a quantile forecast is desired
#' Defaults to NULL, which will pull the latest forecast.
#' @param nday Integer number of days to return forecast. Default is 5. Maximum
#' # days is 28 and minimum is 5.
#'
#' @return A tibble containing the forecast date_time, value, and and ensemble
#' member for each of the 30-member ensemble
#'
get_hefs_ensemble_cbrfc <- function(ID, date = NULL, nday = 5) {

  # --- 1. Input Validation ---
  if (is.null(ID) || !is.character(ID) || length(ID) != 1) {
    warning("Input 'ID' must be a single character string representing the gage ID.")
    return(NULL)
  }

  if (is.null(date)) {
    warning("No date input so using today's date.")
    date <- Sys.Date()
  }

  # make sure nday within reasonable range
  if (nday > 28) {
    nday <- 28
  } else if (nday < 5) {
    nday <- 5
  }

  # --- 2. Request json data from site using httr
  full_request <- paste0("https://www.cbrfc.noaa.gov/dbdata/station/ensgraph/ensgraph_data.py?id=",
                         ID,
                         "&ndays=",
                         nday,
                         "&linear_flow=1&fdate=",
                         date)

  message(sprintf("Requesting forecast data for ID %s", ID))


  #Perform the GET request
  forecast_raw <- httr::GET(url = full_request)


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
    forecast_full <- jsonlite::fromJSON(unpacked_data, simplifyVector = TRUE)
  }, error = function(e) {
    warning(sprintf("JSON Parsing Error for ID %s. Message: %s", ID, e$message))
    return(NULL)
  })

  # Check for the primary expected data element
  if (is.null(forecast_full) || !("traces" %in% names(forecast_full))) {
    warning(sprintf("Forecast API response for ID %s is missing the expected 'value_set' element.", ID))
    return(NULL)
  }


  # --- 4. Data Transformation and Cleanup ---
  # forecast_data$traces contains individual ensemble members
  forecast_data <- forecast_full$traces %>%
    map2(names(.), ~ .x %>%
           as_tibble() %>%
           # The columns in the new tibble are automatically named V1 and V2
           rename(date_time = V1) %>%
           rename(!!paste0("ens_",.y) := V2) %>%
           # 4. Only keep the two columns we care about
           #select(date_time, !!as.character(.y))) %>%
           select(date_time, starts_with("ens"))) %>%
    reduce(full_join, by = "date_time") %>%
    mutate(date_time = as.POSIXct(date_time / 1000, origin = "1970-01-01", tz = "UTC")) %>%
    rowwise() %>%
    # Calculate ensemble stats
    mutate(
      min_ens = min(c_across(starts_with("ens_")), na.rm = TRUE),
      q_25 = quantile(c_across(starts_with("ens_")), probs = 0.25, na.rm = TRUE) %>% as.numeric(),
      q_50 = quantile(c_across(starts_with("ens_")), probs = 0.5, na.rm = TRUE) %>% as.numeric(),
      q_75 = quantile(c_across(starts_with("ens_")), probs = 0.75, na.rm = TRUE) %>% as.numeric(),
      max_ens = max(c_across(starts_with("ens_")), na.rm = TRUE)
    ) %>%
    ungroup()

  # Ensure we have quantile names before proceeding with unnesting/renaming
  if (is.null(forecast_data) || length(forecast_data) == 0) {
    warning(sprintf("No ensemble data returned for: ", ID))
    return(NULL)
  }

  message(sprintf("Successfully retrieved %s forecast records for ID %s",
                  nrow(forecast_data), ID))

  return(forecast_data)

}


# Test workflow
huc8 <- get_huc(id="10190007", type = "huc08") # poudre

# Download and plot gauge sites in AOI
gauge_sites <- get_hefs_gauges(huc8)

# plot in space
a <- mapview(gauge_sites %>% select(lid, geometry)) +
  mapview(huc8,
          col.regions = "hotpink",
          alpha.regions = 0.2)

addStaticLabels(a,
                label = gauge_sites$lid,
                noHide = TRUE,
                direction = 'right',
                textOnly = TRUE,
                textsize = "13px",
                style = list("color" = "black", "font-weight" = "normal"))


# From that map, the FTDC2 is the gauge we care most about
ID <- "FTDC2"

# Pull in recent observed and current quantile forecast data
observed <- get_hefs_observed(ID)
forecast <- get_hefs_quantiles(ID)

current_forecast <- bind_rows(observed, forecast)

# Plot
ggplot(current_forecast) +
  geom_line(aes(x = date_time, y = discharge_cfs, color = group)) +
  geom_ribbon(aes(x = date_time, ymin = min_value, ymax = max_value, fill = group), alpha = 0.3) +
  geom_line(aes(x = date_time, y = Q0.5, color = group)) +
  theme_bw() +
  labs(y = "Discharge (cfs)", x = NULL) +
  scale_color_manual("", values = c("tomato","black")) +
  scale_fill_manual("", values = c("tomato","black")) +
  scale_x_datetime(minor_breaks = "1 day")

# Get hindcast data and compare with observed
hindcast_date = Sys.Date() - days(10)
hindcast_forecast <- get_hefs_ensemble(ID, date = hindcast_date) %>%
  summarize(median_value = median(value),
         max_value = max(value),
         min_value = min(value),
         .by = c(date_time, forecast_date, group))
ten_day_hindcast <- bind_rows(observed, hindcast_forecast) %>%
  filter(between(date_time, ymd(hindcast_date), ymd(hindcast_date) + days(10)))

ggplot(ten_day_hindcast) +
  geom_line(aes(x = date_time, y = discharge_cfs, color = group)) +
  geom_ribbon(aes(x = date_time, ymin = min_value, ymax = max_value, fill = group), alpha = 0.3) +
  geom_line(aes(x = date_time, y = median_value, color = group)) +
  theme_bw() +
  labs(y = "Discharge (cfs)", x = NULL) +
  scale_color_manual("", values = c("tomato","black")) +
  scale_fill_manual("", values = c("tomato","black")) +
  scale_x_datetime(minor_breaks = "1 day")



# Test workflow
huc8 <- get_huc(id="10190007", type = "huc08") # poudre

# Download and plot gauge sites in AOI
gauge_sites <- get_hefs_gauges(huc8)

# plot in space
a <- mapview(gauge_sites %>% select(lid, geometry)) +
  mapview(huc8,
          col.regions = "hotpink",
          alpha.regions = 0.2)

addStaticLabels(a,
                label = gauge_sites$lid,
                noHide = TRUE,
                direction = 'right',
                textOnly = TRUE,
                textsize = "13px",
                style = list("color" = "black", "font-weight" = "normal"))


# From that map, the FTDC2 is the gauge we care most about
ID <- "FTDC2"

# Pull in recent observed and current quantile forecast data
observed <- get_hefs_observed(ID)
forecast <- get_hefs_quantiles(ID)

current_forecast <- bind_rows(observed, forecast)

# Plot
ggplot(current_forecast) +
  geom_line(aes(x = date_time, y = discharge_cfs, color = group)) +
  geom_ribbon(aes(x = date_time, ymin = min_value, ymax = max_value, fill = group), alpha = 0.3) +
  geom_line(aes(x = date_time, y = Q0.5, color = group)) +
  theme_bw() +
  labs(y = "Discharge (cfs)", x = NULL) +
  scale_color_manual("", values = c("tomato","black")) +
  scale_fill_manual("", values = c("tomato","black")) +
  scale_x_datetime(minor_breaks = "1 day")

# Get hindcast data and compare with observed
hindcast_date = Sys.Date() - days(10)
hindcast_forecast <- get_hefs_ensemble(ID, date = hindcast_date) %>%
  summarize(median_value = median(value),
            max_value = max(value),
            min_value = min(value),
            .by = c(date_time, forecast_date, group))
ten_day_hindcast <- bind_rows(observed, hindcast_forecast) %>%
  filter(between(date_time, ymd(hindcast_date), ymd(hindcast_date) + days(5)))

ggplot(ten_day_hindcast) +
  geom_line(aes(x = date_time, y = discharge_cfs, color = group)) +
  geom_ribbon(aes(x = date_time, ymin = min_value, ymax = max_value, fill = group), alpha = 0.3) +
  geom_line(aes(x = date_time, y = median_value, color = group)) +
  theme_bw() +
  labs(y = "Discharge (cfs)", x = NULL) +
  scale_color_manual("", values = c("tomato","black")) +
  scale_fill_manual("", values = c("tomato","black")) +
  scale_x_datetime(minor_breaks = "1 day")



# Get CWCB hindcast for random date and "NULL" date

rand_date <- get_hefs_ensemble_cbrfc(ID = "FTDC2",
                                date = "2024-05-11")

null_date <- get_hefs_ensemble_cbrfc(ID = "FTDC2", # Pulls today forecast
                                     date = NULL)

ggplot(rand_date) +
  geom_ribbon(aes(x = date_time, ymin = min_ens, ymax = max_ens, fill = "Min-Max"), alpha = 0.4) +
  geom_ribbon(aes(x = date_time, ymin = q_25, ymax = q_75, fill = "Q25-Q75"), alpha = 0.4) +
  geom_line(aes(x = date_time, y = q_50, fill = "Q50", color = "Q50")) +
  theme_bw() +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual("ensemble", values = c("lightblue", "dodgerblue", "blue", "navy"), na.translate = FALSE) +
  scale_color_manual("", values = c("black"))
