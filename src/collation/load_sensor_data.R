#' @title Load or update combined sensor data from ROSS & FC sources
#'
#' @description
#' This function handles loading existing processed sensor data or updating data
#' from raw sources (ROSS and Fort Collins). This function serves as the
#' main entry point for accessing combined sensor datasets and provides options
#' for updating individual data sources or the combined dataset.
#'
#' The function operates in two primary modes:
#' 1. **Data loading mode** (`update_all_data & update_ross_data & update_fc_data = FALSE`, default): Reads the most
#'    recently created combined Parquet file from the `output_directory`.
#' 2. **Data updating mode**: (`update_all_data = TRUE` or `update_ross_data = TRUE` or `update_fc_data = TRUE`)
#'    Calls internal generator functions to process raw data from ROSS and Fort Collins sources,
#'    combines the results, and creates a new timestamped output file.
#'    -`update_ross_data = TRUE` Calls `generate_ross_sensor_data(update_data = TRUE)`.
#'    - `update_fc_data = TRUE` Calls `generate_fc_sensor_data(update_data = TRUE)`.
#'    - `update_all_data = TRUE` Calls both generator functions to update all data.
#'    - **Combination**: Harmonizes time zones and binds the two cleaned datasets.
#'    - **Output**: Saves the combined dataset as `compiled_all_sensor_data_YYYY-MM-DD.parquet`.
#'
#' @param input_all_sensor_data_path Character string specifying the full path to an existing
#'   combined sensor data file (e.g., `.parquet`). Used only when `update_data = FALSE`.
#'   Defaults to the most recent version of `compiled_all_sensor_data_...` found
#'   in the `output_directory`.
#' @param update_all_data Logical flag indicating whether to update and regenerate
#'   all sensor data by running the individual data pipelines. When `TRUE`,
#'   it ignores any pre-existing input file and rebuilds the combined data from scratch.
#'   Default is `FALSE`.
#' @param update_ross_data Logical flag indicating whether to update and regenerate
#'   all sensor data by rerunning the ROSS data pipeline but not FC. When `TRUE`,
#'   it ignores any pre-existing input file and rebuilds the combined data from scratch.
#'   Default is `FALSE`.
#' @param update_fc_data Logical flag indicating whether to update and regenerate
#'   all sensor data by rerunning the FC data pipeline but not ROSS. When `TRUE`,
#'   it ignores any pre-existing input file and rebuilds the combined data from scratch.
#'   Default is `FALSE`.
#' @param output_directory Character string specifying the directory where the final
#'   processed combined `.parquet` file should be saved/loaded from. The directory
#'   will be created if it does not exist and `update_data = TRUE`.
#'   (default: `here("data", "collated", "sensor")`).
#'
#' @return A tibble containing the combined, cleaned, and processed sensor data
#'   from both ROSS and Fort Collins sources.
#'  Columns:
#'  - `DT_round`: POSIXct timestamp in Mountain Standard Time (MST). Rounded to the nearest 15-minute interval
#'  - `site`: Character string indicating the data source site (e.g., "sfm", "salyer").
#'  - `parameter`: Character string indicating the measured parameter (e.g., "Specific Conductivity", "Temperature").
#'  - `mean`: Numeric value representing the mean of the measured parameter over the 15-minute interval.
#'  - `clean_flag`: Integer flag indicating data quality flag. Most are generated from `ross.wq.tools` auto QAQC process or
#'  may be generated from manual user verification in `rossyndicate/poudre_sonde_network` repository.
#'  - `mean_cleaned`: Numeric value representing the cleaned mean of the measured parameter.
#'  If data is not approved, this will be NA
#'  Generated from `src/sensor_qaqc/apply_cleaning_filters.R` or `src/collation/generate_ross_sensor_data.R`
#'  - `mean_filled`: Numeric value representing the filled mean of the measured parameter.
#'  Generated from `src/sensor_qaqc/apply_interpolation_missing_data.R`
#'  - `smoothed_mean`: Numeric value representing the smoothed mean of the measured parameter.
#'  Generated from `src/sensor_qaqc/apply_low_pass_binomial_filter.R`
#'  - `hourly_median`: Numeric value representing the hourly median of the measured parameter.
#'  Generated from `src/sensor_qaqc/apply_timestep_median.R`
#'  - `last_site_visit`: POSIXct timestamp (UTC) of the last site visit for maintenance or calibration.
#'    May be used a proxy measurement for biofouling
#'  - `status`: Character string indicating the original data source file or system.
#'
#'
#' @examples
#' \dontrun{
#' # Load existing combined data (default behavior)
#' all_sensor <- load_sensor_data()
#'
#' # Update combined data from the raw sources
#' all_sensor_updated <- load_sensor_data(update_data = TRUE)
#' }
#'
#' @details
#' **Dependencies and File Naming:**
#' - This function relies on `generate_ross_sensor_data()` and `generate_fc_sensor_data()`
#'   to handle the reading, cleaning, and flagging of raw data.
#' - The combined output file is timestamped with the current date:
#'   `compiled_all_sensor_data_YYYY-MM-DD.parquet`
#' - **Time Zone**: The final combined dataset uses **Mountain Standard Time (MST)** for all timestamps.
#'
#' **Data Source Requirements:**
#' The raw data structure requirements are managed internally by the
#' `generate_ross_sensor_data()` and `generate_fc_sensor_data()` functions.
#'
load_sensor_data <- function(
    input_all_sensor_data_path = list.files(here("data", "collated", "sensor"),
                                            full.names = T, pattern = "compiled_all_sensor_data") %>%
      utils::tail(1),
    update_all_data = FALSE,update_ross_data = FALSE, update_fc_data = FALSE,
    output_directory = here("data", "collated", "sensor")
 ){
  #---- Sourcing Functions ----
  # Get read_ext function
  source("src/collation/read_ext.R")
  # Get generator functions
  source("src/collation/generate_ross_sensor_data.R")
  source("src/collation/generate_fc_sensor_data.R")

  # Argument checks ----
  # Check that input file exists when not updating
  if (!update_all_data && !is.null(input_all_sensor_data_path) && !file.exists(input_all_sensor_data_path)) {
    stop("input_all_sensor_data_path does not exist:\n  ", input_all_sensor_data_path)
  }

  # Check update arguments are logical
  if (!is.logical(update_all_data)) {
    stop("update_all_data must be TRUE or FALSE, not: ", update_all_data)
  }
  if (!is.logical(update_ross_data)) {
    stop("update_ross_data must be TRUE or FALSE, not: ", update_ross_data)
  }
  if (!is.logical(update_fc_data)) {
    stop("update_fc_data must be TRUE or FALSE, not: ", update_fc_data)
  }

  # Check that directory arguments are not NULL when updates are requested
  if (update_all_data && is.null(output_directory)) {
    stop("output_directory cannot be NULL when update_all_data is TRUE")
  }

  # Create output directories if they don't exist (only when updating)
  if ((update_all_data | update_fc_data | update_ross_data) && !dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
    message("Created output_directory: ", output_directory)
  }

  if (update_ross_data) {
    message("Updating datasets...")
    message("Starting ROSS data update...")
    ross_data <- generate_ross_sensor_data(update_data = TRUE, output_directory = output_directory)
    # Just read in the FC data, no update
    fc_data <- generate_fc_sensor_data(update_data = FALSE, output_directory = output_directory)
    # ---- Combine Datasets ----
    all_data <- bind_rows(ross_data, fc_data) %>%
      distinct()
    #Save Combined Datasets
    write_parquet(all_data, file.path(output_directory, paste0("compiled_all_sensor_data_", Sys.Date(), ".parquet")))
    # Return the data
    return(all_data)
    }

  if (update_fc_data) {
    message("Updating datasets...")
    #just read in the ROSS data, no update
    ross_data <- generate_ross_sensor_data(update_data = FALSE, output_directory = output_directory)
    message("Starting Fort Collins data update...")
    fc_data <- generate_fc_sensor_data(update_data = TRUE, output_directory = output_directory)
    # ---- Combine Datasets ----
    all_data <- bind_rows(ross_data, fc_data) %>%
      distinct()
    #Save Combined Datasets
    write_parquet(all_data, file.path(output_directory, paste0("compiled_all_sensor_data_", Sys.Date(), ".parquet")))
    # Return the data
    return(all_data)
    }


  # Update collated datasets if update_all_data is TRUE
  if (update_all_data) {
    message("Updating datasets...")
    message("Starting ROSS data update...")
    ross_data <- generate_ross_sensor_data(update_data = TRUE, output_directory = output_directory)
    message("Starting Fort Collins data update...")
    fc_data <- generate_fc_sensor_data(update_data = TRUE, output_directory = output_directory)
    # ---- Combine Datasets ----
    all_data <- bind_rows(ross_data, fc_data) %>%
      distinct()
    #Save Combined Datasets
    write_parquet(all_data, file.path(output_directory, paste0("compiled_all_sensor_data_", Sys.Date(), ".parquet")))
    # Return the data
    return(all_data)
    }
  # If update all is F then just read in the data from the input path
  if (length(input_all_sensor_data_path) == 0 || is.na(input_all_sensor_data_path)) {
    stop("No input file provided and no default found. Please provide 'input_all_sensor_data_path' or set 'update_all_data = TRUE'.")
  }

  message("Loading existing data from: ", input_all_sensor_data_path)
  all_data <- read_ext(input_all_sensor_data_path)

  # Return the data
  return(all_data)
}
