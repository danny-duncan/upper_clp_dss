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
