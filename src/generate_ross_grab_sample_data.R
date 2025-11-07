#' @title Generate and standardize ROSS water chemistry grab sample data
#'
#' @description
#' Downloads (if necessary), processes, and standardizes pre-cleaned water chemistry data
#' from the ROSSyndicate (ROSS) Cache la Poudre watershed monitoring program hosted on Zenodo.
#'
#' This function:
#' - Connects to the latest release of the ROSS CLP dataset on Zenodo (DOI: [10.5281/zenodo.11100085](https://doi.org/10.5281/zenodo.11100085))
#' - Checks whether the most recent version is already downloaded locally
#' - Downloads and unzips new data releases if not present
#' - Reads, harmonizes, and standardizes the ROSS chemistry dataset for downstream analysis
#' - Optionally saves the processed data as a timestamped parquet file
#'
#' The function also applies consistent site naming conventions and harmonized datetime formats,
#' ensuring compatibility with modeling and visualization workflows.
#'
#' @section Site Name Standardization:
#' Site name adjustments are based on sensor deployment history and naming convention updates:
#'
#' 1. **Virridy colocation corrections (before 2024-11-30)**
#'    Appends the suffix `"_virridy"` to `archery`, `timberline`, and `prospect` sites to
#'    distinguish data collected during Virridy sensor deployments.
#'
#' 2. **Legacy site name updates (after 2024-11-30)**
#'    Updates older site codes to reflect current conventions:
#'    `lincoln → udall`, `legacy → salyer`, `boxelder → elc`, `tamasag → bellvue`,
#'    `timberline → riverbend`, and `prospect → cottonwood`.
#'
#' These adjustments ensure standardized site codes across datasets, though they reduce
#' generalizability for use with other naming schemes.
#'
#' @section Zenodo Version Handling:
#' - The function uses the Zenodo **concept DOI** (`10.5281/zenodo.11100085`) to automatically
#'   fetch metadata for the latest dataset version.
#' - If the current version is **not already downloaded** in `raw_ross_chem_data_directory`,
#'   it will be silently downloaded and unzipped.
#' - Data are stored in a versioned subfolder named after the Zenodo release version(ie `v2025.07.05`).
#' - Users are prompted to optionally update (`update_data = TRUE`) if new data are detected.
#'
#' @param raw_ross_chem_data_directory Character string specifying the local directory containing
#'   the pre-cleaned ROSS chemistry data or where new Zenodo releases should be downloaded.
#'   Defaults to `here("data", "raw", "chem", "ross_clp_chem")`.
#'
#' @param output_directory Character string specifying the output directory for saving processed
#'   parquet files if `update_data = TRUE`. Defaults to `here("data", "collated", "chem")`.
#'
#' @param update_data Logical; if `TRUE`, saves the standardized dataset as a timestamped parquet
#'   file in `output_directory`. Default is `FALSE`. If a user downloads a new version of the data,
#'   they will be prompted to set this to `TRUE` to save the updated data.
#'
#' @return A tibble containing standardized and harmonized ROSS chemistry data with consistent
#' site names, timestamps, and spatial attributes.
#'
#' @examples
#' \dontrun{
#' # Load and process ROSS data without saving
#' ross_data <- generate_ross_grab_sample_data()
#'
#' # Load and save processed data
#' ross_data <- generate_ross_grab_sample_data(
#'   raw_ross_chem_data_directory = "data/raw/chem/ross_clp_chem",
#'   output_directory = "data/collated/chem",
#'   update_data = TRUE
#' )
#'
#' # Example: Filter for 2023 samples excluding Virridy deployments
#' library(dplyr)
#' recent <- ross_data %>%
#'   filter(Date >= as.Date("2023-01-01"),
#'          !stringr::str_detect(site_code, "virridy"))
#' }
#'
#' @details
#' **Data Requirements:**
#' Input data must be a pre-cleaned ROSS chemistry dataset containing the following fields:
#' - `DT_mst_char`: Pos datetime (compatible with `lubridate::ymd_hms()`)
#' - `site_code`: Site identifier
#' - `Site`: Full site name
#' - Chemical parameters (e.g., `TOC`, `DOC`, `NO3`, `SC`, `TN`, `PO4`, etc.)
#' - Spatial metadata (`distance_upstream_km`, `Lat`, `Long`)
#' - Campaign metadata (`Campaign`, `location_type`)
#'
#' **Data Source:**
#' The function harmonizes downloading derived from `src/pull_ROSS_zenodo_data()` and the `zen4R` package,
#' and data prep methods from  `modeling/toc/01_raw_data_prep.Rmd`, and `modeling/toc/toc_forecast/add_FC_TOC_baseline.Rmd`.
#'
#' **Output:**
#' - Returns a tibble of harmonized chemistry data.
#' - Optionally writes a timestamped `.parquet` file  to the output directory if `update_data = TRUE`.
#'

generate_ross_grab_sample_data <- function(raw_ross_chem_data_directory = here("data", "raw","chem","ross_clp_chem"),
                                           output_directory = here("data", "collated", "chem"),
                                           update_data = FALSE) {
  #Check to make sure zen4R is installed
  if (!requireNamespace("zen4R", quietly = TRUE)) install.packages("zen4R")

  # Get read_ext function ----
  source("src/read_ext.R")

  # Argument checks ----
  # Check that the raw data path is real
  if (!dir.exists(raw_ross_chem_data_directory)) {
    stop(raw_ross_chem_data_path, " (raw_ross_chem_data_directory) does not exist.")
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

  ### Checking that we have downloaded the most recent version from Zenodo ###
  #fixed DOI for the ROSS CLP data
  DOI = "10.5281/zenodo.11100085"
  #connect to zenodo
  zenodo <- zen4R::ZenodoManager$new(url = "https://zenodo.org/api",sandbox = FALSE,logger = "INFO")
  #get info from concept DOI (this is the DOI for all the CLP versions and defaults to the latest)
  rec <- suppressMessages(zenodo$getRecordByConceptDOI(DOI))
  #get the version name
  version_name <- rec$metadata$version

  #if we don't have this version saved, download it to a folder with the version name in the raw_ross_chem_data_directory
  if(!dir.exists(here(raw_ross_chem_data_directory, version_name))){

    message(paste0("Downloading ROSS CLP version: ", version_name) )
    #Download if version is not in download directory
    suppressMessages(rec$downloadFiles(path = tempdir()))

    #unzip to temp dir and find the unzipped folder in temp_dir
    dwn_file <- list.files(tempdir(), pattern = ".zip", full.names = TRUE)
    unzip(zipfile = dwn_file, exdir = tempdir())
    unzip_file <- list.files(tempdir(), pattern = "rossyndicate", full.names = TRUE)
    #move unzip folder to raw_ross_chem_data_directory and rename to version number
    file.rename(unzip_file, file.path(raw_ross_chem_data_directory, version_name))
    message(paste0("ROSS CLP version: ", version_name, " downloaded to ", file.path(raw_ross_chem_data_directory, version_name)))

    #Prompt user to set update_data to TRUE to save updated data
    if(update_data == F){
      update <- readline(prompt = "Note: You have downloaded a new version of the ROSS CLP data.Update_data is currently F.\nPress Y to set update_data to T:  ")
      if(tolower(update) == "y"){
        update_data = T
      }
    }

  }

  ### Grabbing and cleaning the ROSS chem data ###

  #find the csv file in the cleaned directory of the most recent version
  raw_ross_chem_data_path <- list.files(path = here(raw_ross_chem_data_directory,
                                                    version_name,
                                                    "data",
                                                    "cleaned"),
                                        full.names = TRUE,
                                        pattern = ".csv")

  if(length(raw_ross_chem_data_path) == 0){
    stop("No cleaned data file found. Please check the directory: ",
         here(raw_ross_chem_data_directory, version_name,"data","cleaned"))
  }

  # Grab and clean the ROSS chem data (originally pulled from zenodo)
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
      # in-situ field measurements
      Field_DO_mgL, Field_Cond_uS_cm = Field_Cond_µS_cm, Field_Temp_C,
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

  return(ross_chem_data)

}
