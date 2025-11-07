#' @title Load or update combined water chemistry grab sample data from multiple sources
#'
#' @description
#' A flexible data management function that handles loading existing processed chemistry data
#' or updating data from raw sources (ROSS and Fort Collins). This function serves as the
#' main entry point for accessing combined water chemistry datasets and provides options
#' for updating individual data sources or the combined dataset.
#'
#' The function operates in two primary modes:
#' 1. **Data loading mode** (default): Reads existing processed data files
#' 2. **Data updating mode**: Processes raw data through individual generator functions
#'    and creates new timestamped output files
#'
#' **Update Logic:**
#' - When `update_data = TRUE`, raw data from both ROSS and Fort Collins sources
#'   are processed and combined into three files:
#'   - `ross_chem_data_YYYY-MM-DD.parquet`
#'   - `fc_chem_data_YYYY-MM-DD.parquet`
#'   - `all_chem_data_YYYY-MM-DD.parquet`
#' - When `update_data = FALSE` (default), the function loads the existing combined
#'   file specified by `input_all_chem_data_path`
#'
#' The function automatically combines ROSS and Fort Collins datasets using `full_join()`,
#' ensuring all records from both sources are preserved. Once this data is loaded,
#' simple `filter()` and `select()` calls should get the desired data. The purpose
#' of this function is to munge all of the available data into a format that is
#' easier to work with.
#'
#' @param input_all_chem_data_path Character string specifying the path to an existing
#'   combined chemistry data file (e.g., `.parquet`). Used when no update is requested.
#'   Defaults to the most recent version in the collated directory.
#' @param update_data Logical flag indicating whether to update and regenerate
#'   all chemistry data from raw sources (`ROSS` and `Fort Collins`). When TRUE,
#'   ignores any pre-existing input file and rebuilds data from scratch. Default is FALSE.
#' @param raw_ross_chem_data_path Character string specifying the path to the raw ROSS
#'   chemistry data directory or file. Required when `update_data = TRUE`.
#' @param raw_fc_chem_data_path Character string specifying the path to the raw Fort Collins
#'   chemistry data directory. Required when `update_data = TRUE`.
#' @param output_directory Character string specifying the directory where processed
#'   `.parquet` files should be saved. The directory will be created if it does not exist.
#'
#' @return A tibble containing combined water chemistry data from ROSS and Fort Collins
#'   sources. The structure includes columns from both datasets with the naming
#'   conventions this group has followed thus far. Column availability depends on
#'   the source (e.g., DOC only available from ROSS data, spatial coordinates vary by source).
#'
#' @examples
#' \dontrun{
#' # Load existing combined data (default behavior)
#' all_chem <- load_grab_sample_data()
#'
#' # Update data from raw sources
#' all_chem <- load_grab_sample_data(
#'   update_data = TRUE,
#'   raw_ross_chem_data_path = "path/to/ross/raw_data_dir",
#'   raw_fc_chem_data_path = "path/to/fc/raw_data_dir",
#'   output_all_directory = "path/to/output",
#'   output_ross_directory = "path/to/output",
#'   output_fc_directory = "path/to/output"
#' )
#'
#' # This is how I made the current all chem data file (`all_chem_data_2025-11-04.parquet`):
#' load_grab_sample_data(
#'   update_data = TRUE,
#'   raw_ross_chem_data_path = here("data", "upper_clp_dss", "ross_clp_chem"),
#'   raw_fc_chem_data_path = here("data", "upper_clp_dss", "fc_clp_chem")
#' )
#'
#' @details
#' **File Output Behavior:**
#' All output files are timestamped with the current date (YYYY-MM-DD format):
#' - ROSS data: `ross_chem_data_YYYY-MM-DD.parquet`
#' - FC data: `fc_chem_data_YYYY-MM-DD.parquet`
#' - Combined data: `all_chem_data_YYYY-MM-DD.parquet`
#'
#' **Data Source Requirements:**
#' - When updating data, corresponding raw data paths (`raw_ross_chem_data_path`,`raw_fc_chem_data_path`) must be provided and exist
#' - When not updating, `input_all_chem_data_path` must be provided and exist
#' - Output directories are created automatically if they don't exist
#'
#' **Error Handling:**
#' The function validates all path arguments and update logic combinations, providing
#' error messages for common configuration mistakes.
#'
#' **Data Processing Note:**
#' This function is primarily a data management wrapper. Actual data processing and
#' harmonization is handled by `generate_ross_grab_sample_data()` and
#' `generate_fc_grab_sample_data()`.
#'

load_grab_sample_data <- function(
    input_all_chem_data_path = list.files(here("data", "collated","chem"),
                                          full.names = T, pattern = "all_chem_data")%>%
      utils::tail(1),
    update_data = FALSE,
    raw_ross_chem_data_path = here("data", "raw", "chem", "ross_clp_chem"),
    raw_fc_chem_data_path = here("data", "raw", "chem", "fc_clp_chem"),
    output_directory = here("data", "collated", "chem")
){

  # Get read_ext function ----
  source("src/read_ext.R")
  # Get generator functions ----
  source("src/generate_ross_grab_sample_data.R")
  source("src/generate_fc_grab_sample_data.R")

  # Argument checks ----

  # Check input paths (only if not NULL)
  if (!is.null(input_all_chem_data_path) && !file.exists(input_all_chem_data_path)) {
    stop("input_all_chem_data_path does not exist:\n  ", input_all_chem_data_path)
  }

  # Check raw path arguments (file or directory must exist)
  if (!is.null(raw_ross_chem_data_path) && !file.exists(raw_ross_chem_data_path)) {
    stop("raw_ross_chem_data_path does not exist:\n  ", raw_ross_chem_data_path)
  }

  if (!is.null(raw_fc_chem_data_path) && !file.exists(raw_fc_chem_data_path)) {
    stop("raw_fc_chem_data_path does not exist:\n  ", raw_fc_chem_data_path)
  }

  # Check update arguments are logical
  if (!is.logical(update_data)) {
    stop("update_data must be TRUE or FALSE, not: ", update_data)
  }

  # Check that directory arguments are not NULL when updates are requested
  if (update_data && is.null(output_directory)) {
    stop("output_all_directory cannot be NULL when update_data is TRUE")
  }

  # Create output directories if they don't exist (only when updating)
  if (update_data && !dir.exists(output_directory)) {
    dir.create(output_all_directory, recursive = TRUE)
    message("Created output_all_directory: ", output_all_directory)
  }

  # Update collated datasets if update_data is TRUE
  if (update_data) {
    # ROSS samples
    ross_data <- generate_ross_grab_sample_data(
      raw_ross_chem_data_directory = raw_ross_chem_data_path,
      output_directory = output_directory,
      update_data = T)
    #FC samples
    fc_data <- generate_fc_grab_sample_data(
      raw_fc_chem_data_path = raw_fc_chem_data_path,
      output_directory = output_directory,
      update_data = T)
    #join the two datasets
    all_data <- full_join(ross_data, fc_data) %>%
      distinct()

    # Save the updated data to the output directory with timestamped filename
    date <- Sys.Date()
    file_name <- paste0("all_chem_data_", date, ".parquet")
    output_path <- file.path(output_directory, file_name)
    write_parquet(all_data, output_path)
    message("Saved updated chemistry data to: ", output_path)

    # Return the data
    return(all_data)

  } else {

    # If update all is F then just read in the data from the input path
    all_data <- read_ext(input_all_chem_data_path)

    # Return the data
    return(all_data)

  }

}


