#' @title Process and standardize ROSS water chemistry grab sample data
#'
#' @description
#' Processes pre-cleaned water chemistry data from the ROSSyndicate (ROSS) Cache la Poudre
#' watershed monitoring program. This function applies site name standardization based on
#' sensor deployment history, updates site codes to reflect naming convention changes,
#' and formats the data for integration with other datasets. This data extraction is
#' based on the harmonization of the outputs from `pull_ROSS_zenodo_data()` that were in
#' `02_sensor_grab_munge.Rmd` and `add_FC_TOC_baseline.Rmd`. Currently there is
#' no direct or indirect interaction with `pull_ROSS_zenodo_data()`, though there can
#' be in the future, if there is desire for it.
#'
#' The function performs several key data processing steps:
#' - Applies site code corrections based on sensor deployment periods
#' - Updates legacy site names to current naming conventions
#' - Standardizes datetime formats and creates character representations
#' - Selects and renames columns to match standardized data structure
#' - Preserves spatial metadata (coordinates, upstream distance) and campaign information
#'
#' **Site Name Updates:**
#' The function handles two types of site name changes:
#' 1. **Lower Poudre Virridy sensor colocation period corrections** (before 2024-11-30): Appends "_virridy" suffix
#'    to distinguish data collected during Virridy sensor deployments
#' 2. **Legacy name updates**: Maps old site names to current naming conventions
#'    (e.g., "lincoln" → "udall", "legacy" → "salyer")
#'
#' This standardization is required, but will reduce the generalizability of the
#' function.
#'
#' @param raw_ross_chem_data_path Character string specifying the file path to the
#'   pre-cleaned ROSS chemistry data file (typically CSV format). Default points to
#'   the most recent cleaned dataset in the standard project directory structure.
#' @param output_directory Character string specifying the directory where processed data
#'   should be saved when update_data is TRUE. Default uses here() to point to the
#'   modeling data directory.
#' @param update_data Logical indicating whether to save the processed data to a parquet
#'   file. When TRUE, creates a timestamped parquet file. Default is FALSE.
#'
#' @return A tibble containing standardized ROSS chemistry data.
#'
#' @examples
#' \dontrun{
#' # Process ROSS data without saving
#' ross_data <- generate_ross_grab_sample_data()
#'
#' # Process and save ROSS data
#' ross_data <- generate_ross_grab_sample_data(
#'   raw_ross_chem_data_path = "path/to/ross/cleaned_data.csv",
#'   output_directory = "path/to/output",
#'   update_data = TRUE
#' )
#'
#' # Filter for specific analysis periods
#' recent_data <- ross_data %>%
#'   filter(Date >= as.Date("2023-01-01"),
#'          !str_detect(site_code, "virridy"))
#' }
#'
#' @details
#' **Site Code Logic:**
#' - Samples collected before 2024-11-30 at archery, timberline, and prospect sites
#'   receive "_virridy" suffix to indicate Virridy sensor was colocated at this site and data should be compared to the virridy sonde
#' - Samples from these sites after 2024-11-30 use updated site names
#'   (timberline → riverbend, prospect → cottonwood)
#'
#' **Input Data Requirements:**
#' This function expects pre-cleaned ROSS data containing specific columns:
#' - `DT_mst_char`: Character datetime in format readable by ymd_hms()
#' - `site_code`: Original site codes for mapping
#' - `Site`: Full site names
#' - Various chemistry parameters (TOC, DOC, NO3, etc.)
#' - Spatial metadata (distance_upstream_km, Lat, Long)
#'
#' The function by default uses the data that has been saved most recently by
#' `pull_ROSS_zenodo_data()`.
#'
#' The function will fail if these expected columns are not present in the input data.
#'
#' **Data Source:**
#' Based on cleaning methods from `02_sensor_grab_munge.Rmd` and `add_FC_TOC_baseline.Rmd`.
#' Input data should be the most recent cleaned dataset from the ROSSyndicate Zenodo repository.
#'

generate_ross_grab_sample_data <- function(
    raw_ross_chem_data_path = here("data", "upper_clp_dss", "ross_clp_chem", "data", "cleaned", "CLP_chemistry_up_to_20250701.csv"),
    output_directory = here("data", "upper_clp_dss", "modeling"),
    update_data = FALSE
) {
  # Argument checks ----
  # Check that the raw data path is real
  if (!file.exists(raw_ross_chem_data_path)) {
    stop(raw_ross_chem_data_path, "(raw_ross_chem_data_path) does not exist.")
  }

  # Check that an output directory was supplied if update_data is TRUE
  if (is.null(output_directory) && update_data) {
    stop("output_directory must be provided when update_data is TRUE")
  }

  # Check that the output directory exists if it was supplied and update_data is TRUE,
  # If it does not exist, create it.
  if (!dir.exists(output_directory) && update_data) {
    dir.create(output_directory, recursive = TRUE)
    message("Created ross_output_directory: ", output_directory)
  }

  # Grab and clean the ROSS chem data ----
  ross_chem_data <- read_ext(raw_ross_chem_data_path) %>%
    mutate(
      collector = "ROSS",
      site_code = tolower(site_code),
      DT_mst = ymd_hms(DT_mst_char),
      DT_mst_char = as.character(DT_mst_char),
      # fixing site names based on sonde deployments with ROSS/Virridy sondes
      site_code = case_when(
        site_code == "archery" & DT_mst <= ymd("2024-11-30") ~ "archery_virridy",
        site_code == "timberline" & DT_mst <= ymd("2024-11-30") ~ "riverbend_virridy",
        site_code == "prospect" & DT_mst <= ymd("2024-11-30") ~ "cottonwood_virridy",
        # updating to new names
        site_code == "timberline" & DT_mst >= ymd("2024-11-30") ~ "riverbend",
        site_code == "prospect" & DT_mst >= ymd("2024-11-30") ~ "cottonwood",
        site_code == "lincoln" ~ "udall",
        site_code == "legacy" ~ "salyer",
        site_code == "boxelder" ~ "elc",
        site_code == "tamasag" ~ "bellvue",
        T ~ site_code
      )
    ) %>%
    select(
      # DT columns
      Date, DT_sample = DT_mst, DT_mst_char,
      # ID columns
      site_name = Site, site_code, collector,
      # TOC and Chemical Data Columns
      TOC, DOC, NO3, SC, Cl, TN, lab_turb = Turbidity, ChlA, NH4, PO4, TSS,
      # Spatial information columns
      distance_upstream_km,
      # Coordinate columns
      Lat, Long,
      # Other columns
      Campaign, location_type
    ) %>%
    distinct()

  # Update the saved data ----
  if (update_data) {
    date <- Sys.Date()
    file_name <- paste0("ross_chem_data_", date, ".parquet")
    output_path <- file.path(output_directory, file_name)
    write_parquet(ross_chem_data, output_path)
    message("Saved updated ROSS chemistry data to: ", output_path)
  }

  # Return the ROSS chem data ----
  return(ross_chem_data)
}
