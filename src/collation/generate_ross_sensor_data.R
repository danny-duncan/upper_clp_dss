#' @title Generate and standardize ROSS sonde sensor data
#'
#' @description
#' Processes, and standardizes water quality sensor data at varying stages of
#' manual/automated quality assurance and control to create a single harmonized dataset for all sensor data collected
#' from the ROSSyndicate (ROSS) Cache la Poudre watershed monitoring program.
#'
#' This function:
#' - Imports all datasets saved to the `raw_ross_sensor_data_directory`
#' - For each field season (2018-2025), the highest version of qaqc'ed data is selected
#' - Based on the year, applies necessary transformations to harmonize column names and formats
#' - Removes strictly erroneous data
#' - Joins all datasets into a single harmonized tibble
#'
#' The function also applies consistent site naming conventions and harmonized datetime formats,
#' ensuring compatibility with modeling and visualization workflows.
#'
#' @section Site Name Standardization:
#' Site name adjustments are based on sensor deployment history and naming convention updates:
#'
#' 1. **Virridy colocation (09/2023-11/2024)**
#'    Three sites (`archery`, `timberline`, and `prospect`) have the suffix `"_virridy"` to
#'    distinguish data collected on colocated sondes during Virridy sensor deployments.
#'
#' 2. **Legacy site name updates (after 2024-11-30)**
#'    Updates older site codes to reflect current conventions:
#'    `lincoln → udall`, `legacy → salyer`, `boxelder → elc`, `tamasag → bellvue`,
#'    `timberline → riverbend`, and `prospect → cottonwood`.
#'
#' These adjustments ensure standardized site codes across datasets, though they reduce
#' generalizability for use with other naming schemes.
#'
#' @section Manual Data Verification changes:
#' - In 2023, ROSS began a process of manual data verification for sonde data. Originally this system relied on a binary system (Pass vs Fail) and
#' the pass fail system was to confirm or deny the auto qaqc generated flags. Therefore if a sensor was autoflagged and the user passed it, the data is
#' invalid. In contrast, if a sensor was autoflagged and the user failed it, the data is valid. Lastly, if a sensor was not autoflagged and the
#' user passed it, the data is valid. This system was only used for 2023 data.
#' - In 2024, this process was updated to a more interactive season and included a triple system (Pass, Flagged, Omit). A pass status indicates that the
#' data is valid and can be used. A flagged status indicates that the data is suspect and should be used with caution. An omit status indicates that the
#' data should not be used in downstream analysis. This system will be the primary system moving forward and will be applied to the 2018-2022 and 2025 - onwards
#' data as well.
#'
#' - This function incorporates these by applying a slightly different filtering method for the 2023 dataset but otherwise should be generalized for the other years
#' as well.
#'
#' @param raw_ross_sensor_data_directory Character string specifying the local directory containing the sensor
#'  data organized by site-parameters by year.Defaults to `here("data", "raw", "sensor", "manual_data_verification")`.
#'
#' @param output_directory Character string specifying the output directory for saving processed
#'   parquet files if `update_data = TRUE`. Defaults to `here("data", "collated", "chem")`.
#'
#' @param update_data Logical; if `TRUE`, saves the standardized dataset as a timestamped parquet
#'   file in `output_directory`. Default is `FALSE`. As users verify more data or as new data is downloaded to the raw directory,
#'   the collated dataset should be updated.
#'
#' @return A tibble containing standardized and harmonized ROSS sensor data with consistent
#' site names, timestamps, flag columns.
#'
#' @examples
#'
#' @details
#'
#' **Data Source:**
#' The function harmonizes data prep methods from  `modeling/toc/01_raw_data_prep.Rmd`.
#'
#' **Output:**
#' - Returns a tibble of harmonized sensor data.
#' - Optionally writes a timestamped `.parquet` file  to output if `update_data = TRUE`.
#'

generate_ross_sensor_data <- function(raw_ross_sensor_data_directory = here("data", "raw","sensor","manual_data_verification"),
                                           output_directory = here("data", "collated", "sensor"),
                                           update_data = FALSE) {

  # Get read_ext function ----
  source("src/read_ext.R")
  # Argument checks ----
  # Check that the raw data path is real
  if (!dir.exists(raw_ross_chem_data_directory)) {
    stop(raw_ross_chem_data_path, "(raw_ross_chem_data_directory) does not exist.")
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

  # Update the saved data ----
  if (update_data) {
    date <- Sys.Date()
    file_name <- paste0("ross_sensor_data_", date, ".parquet")
    output_path <- file.path(output_directory, file_name)
    write_parquet(ross_chem_data, output_path)
    message("Saved updated ROSS chemistry data to: ", output_path)
  }

  return(ross_chem_data)
}
