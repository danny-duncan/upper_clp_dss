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
library(dplyr)
library(lubridate)
library(readr)
library(leafem)

# Define functions to retrieve gauge stations, observed discharge, and
# forecast discharge:

#' Retrieves NWPS Hydro-Ensemble Forecast System (HEFS) gages within a given
#' Area of Interest (AOI).
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
get_hefs_gages <- function(aoi, base_url = "https://api.water.noaa.gov/nwps/v1/gauges") {

  # --- 1. Input Validation ---
  if (!inherits(aoi, "sf") && !inherits(aoi, "sfg") && !inherits(aoi, "sfc")) {
    warning("Input 'aoi' must be an sf object (sf, sfc, or sfg). Returning NULL.")
    return(NULL)
  }

  # --- 2. Coordinate System Standardization ---
  # Transform the AOI to the standard WGS 84 (EPSG 4326) required by api
  aoi <- st_transform(aoi, crs = 4326)

  # --- 3. Bounding Box Extraction and Formatting ---
  # Extract the bounding box (bbox) from the transformed AOI.
  aoi_bbox <- st_bbox(aoi)

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
  message(paste("Requesting data from:", full_request))

  # --- 4. API Request and Status Check ---
  # Perform the GET request
  raw_dat <- httr::GET(url = full_request)

  # Check the HTTP status code (crucial for troubleshooting 404/500 errors)
  if (httr::status_code(raw_dat) != 200) {
    status <- httr::status_code(raw_dat)
    warning(paste0("API request failed with HTTP Status Code: ", status,
                   ". Check the full_request URL and API documentation."))
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
    warning(paste("JSON Parsing Error: Could not convert response to JSON. Message:", e$message))
    return(NULL)
  })

  # Check if the expected data element (gauges) exists in the response
  if (is.null(gauge_data) || !("gauges" %in% names(gauge_data))) {
    warning("API response structure is missing the expected 'gauges' element. Returning NULL.")
    return(NULL)
  }

  # --- 6. Convert to SF Object ---
  # Extract the gages data, convert to a standard data frame (tibble),
  # and then to an sf object using the 'longitude' and 'latitude' columns.
  gauge_data <- gauge_data$gauges %>%
    dplyr::as_tibble() %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  # --- 7. Return Result ---
  message(paste("Successfully retrieved", nrow(gauge_data), "gages."))
  return(gauge_data)
}





#' Retrieves NWPS Hydro-Ensemble Forecast System (HEFS) observed stage and flow data.
#' https://api.water.noaa.gov/nwps/v1/docs/#/Gauges/Gauges_GetStageFlow
#'
#' This function queries the observed stage and flow for a specific NWPS gage ID.
#' It handles HTTP status checks, JSON parsing, and standardizes the output:
#' - Renames columns for clarity (e.g., 'primary' to 'stage_ft').
#' - Converts the 'validTime' string to a POSIXct datetime object.
#' - Converts discharge from thousands of cubic feet per second (kcfs) to cfs.
#'
#' @param ID A character string representing a single NWPS gage ID (e.g., "COLOC070").
#' @return A tibble containing the date_time, stage_ft, and discharge_cfs,
#'         or NULL if the API request or parsing fails.
get_hefs_observed <- function (ID) {

  # --- 1. Input Validation ---
  if (is.null(ID) || !is.character(ID) || length(ID) != 1) {
    warning("Input 'ID' must be a single character string representing the gage ID.")
    return(NULL)
  }

  # Define the full request URL for observed data
  full_request <- paste0("https://api.water.noaa.gov/nwps/v1/gauges/", ID, "/stageflow/observed")
  message(paste("Requesting observed data for ID:", ID))

  # --- 2. API Request and Status Check ---
  # Perform the GET request
  observed_raw <- httr::GET(url = full_request)

  # Check the HTTP status code (crucial for troubleshooting 404/500 errors)
  if (httr::status_code(observed_raw) != 200) {
    status <- httr::status_code(observed_raw)
    warning(paste0("API request failed for ID ", ID, " with HTTP Status Code: ", status,
                   ". Check the full_request URL and API documentation."))
    return(NULL)
  }

  # Extract content as text
  unpacked_data <- httr::content(observed_raw, as = "text", encoding = "UTF-8")

  # --- 3. JSON Parsing and Structure Check ---
  raw_data <- NULL
  tryCatch({
    # Parse the text content into an R list/data frame
    raw_data <- jsonlite::fromJSON(unpacked_data, simplifyVector = TRUE)
  }, error = function(e) {
    warning(paste("JSON Parsing Error for ID ", ID, ". Message:", e$message))
    return(NULL)
  })

  # Check if the expected data element ('data') exists in the response
  if (is.null(raw_data) || !("data" %in% names(raw_data))) {
    warning(paste0("API response for ID ", ID, " is missing the expected 'data' element."))
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
    dplyr::select(date_time, stage_ft, discharge_cfs)

  message(paste("Successfully retrieved", nrow(observed_dat), "observed records for ID:", ID))
  return(observed_dat)
}






#' Retrieves NWPS HEFS Quantile Forecast Data.
#'https://api.water.noaa.gov/hefs/v1/docs/#/
#'
#' This function queries the Hydro-Ensemble Forecast System (HEFS) for quantile
#' discharge forecasts for a specific NWPS gage ID. It handles API communication,
#' parses the complex JSON structure, and flattens the quantile array into
#' separate, named columns.
#'
#' @param ID A character string representing a single NWPS gage ID (e.g., "COLOC070").
#' @return A tibble containing the forecast date_time, max_value, and separate
#'         columns for each quantile (e.g., Q0.05, Q0.50), or NULL on failure.
get_hefs_forecast <- function(ID, param = c("QINE")) {

  # --- 1. Input Validation ---
  if (is.null(ID) || !is.character(ID) || length(ID) != 1) {
    warning("Input 'ID' must be a single character string representing the gage ID.")
    return(NULL)
  }

  # Define the full request URL for quantile forecast data (QINE is instantaneous flow)
  full_request <- paste0("https://api.water.noaa.gov/hefs/v1/hydrograph-quantiles/?location_id=", ID, "&parameter_id=QINE")
  message(paste("Requesting forecast data for ID:", ID))

  # Perform the GET request
  forecast_raw <- httr::GET(url = full_request)

  # --- 2. API Request and Status Check ---
  if (httr::status_code(forecast_raw) != 200) {
    status <- httr::status_code(forecast_raw)
    warning(paste0("API request failed for ID ", ID, " with HTTP Status Code: ", status))
    return(NULL)
  }

  # Extract content as text
  unpacked_data <- httr::content(forecast_raw, as = "text", encoding = "UTF-8")

  # --- 3. JSON Parsing and Structure Check ---
  forecast_data <- NULL
  tryCatch({
    # Parse the text content into an R list/data frame
    forecast_data <- jsonlite::fromJSON(unpacked_data, simplifyVector = TRUE)
  }, error = function(e) {
    warning(paste("JSON Parsing Error for ID ", ID, ". Message:", e$message))
    return(NULL)
  })

  # Check for the primary expected data element
  if (is.null(forecast_data) || !("value_set" %in% names(forecast_data))) {
    warning(paste0("Forecast API response for ID ", ID, " is missing the expected 'value_set' element."))
    return(NULL)
  }

  # --- 4. Data Transformation and Cleanup ---

  # Extract the quantile names from metadata (e.g., "0.05", "0.50") and convert to character
  quantiles <- forecast_data$metadata$exceedance_quantiles %>%
    as.character()

  # Ensure we have quantile names before proceeding with unnesting/renaming
  if (is.null(quantiles) || length(quantiles) == 0) {
    warning(paste0("Could not extract quantile names for ID ", ID, ". Returning NULL."))
    return(NULL)
  }

  # Start processing the value set
  forecast_dat <- forecast_data$value_set %>%
    dplyr::as_tibble() %>%

    # Flatten the 'quantile_values' list column into separate columns.
    # New columns will be named 'quantile_values_1', 'quantile_values_2', etc. (based on names_sep = "_")
    unnest_wider(
      col = quantile_values,
      names_sep = "_",
      names_repair = "unique"
    ) %>%

    # Rename the newly created quantile columns using the actual quantile values (e.g., Q0.05)
    dplyr::rename_with(
      # Apply the character vector of quantiles as new names, prefixing with "Q" for clarity
      .fn = ~ paste0("Q", quantiles),
      # Select the columns created by unnest_wider (e.g., quantile_values_1, ...)
      .cols = starts_with("quantile_values_")
    ) %>%

    # Rename the datetime column
    dplyr::rename(date_time = valid_datetime) %>%

    # Convert 'valid_datetime' (ISO string) to POSIXct datetime object
    dplyr::mutate(date_time = lubridate::ymd_hms(date_time),
                  group = "Forecast") %>%

    # Filter out rows where the maximum discharge value is <= 0 (often metadata or junk data)
    dplyr::filter(max_value > 0)

  message(paste("Successfully retrieved", nrow(forecast_dat), "forecast records for ID:", ID))
  return(forecast_dat)
}




# Test workflow
sonde_locs <- read_csv("data/metadata/sonde_location_metadata.csv") %>%
  mutate(lat = str_split(lat_long, pattern = ",", simplify = TRUE)[,1] %>% as.numeric(),
         lng = str_split(lat_long, pattern = ",", simplify = TRUE)[,2] %>% as.numeric()) %>%
  st_as_sf(coords = c("lng","lat"), crs = 4326)

# Define search aoi based on bounding box of sonde locations
search_aoi <- sonde_locs %>%
  st_buffer(1000) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_transform(4326)


# Download and plot gauge sites in AOI
gauge_sites <- get_hefs_gages(search_aoi)
a <- mapview(gauge_sites,
             col.regions = "dodgerblue") +
  mapview(search_aoi %>% st_sf(name = "search_aoi",
                             geometry = .),
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

# Pull in recent observed and forecast data
observed <- get_hefs_observed(ID)
forecast <- get_hefs_forecast(ID)

pdat <- bind_rows(observed, forecast)

# Plot
ggplot(pdat) +
  geom_line(aes(x = date_time, y = discharge_cfs, color = group)) +
  geom_ribbon(aes(x = date_time, ymin = min_value, ymax = max_value, fill = group), alpha = 0.3) +
  geom_line(aes(x = date_time, y = Q0.5, color = group)) +
  theme_bw() +
  labs(y = "Discharge (cfs)") +
  scale_color_manual("", values = c("tomato","black")) +
  scale_fill_manual("", values = c("tomato","black"))

