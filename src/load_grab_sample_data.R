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
#' - `update_all_data = TRUE`: Processes both ROSS and FC data, creates 3 files (ross, fc, combined)
#' - `update_ross_data = TRUE`: Updates only ROSS data, creates 2 files (ross, combined)
#' - `update_fc_data = TRUE`: Updates only FC data, creates 2 files (fc, combined)
#' - `update_*_data = FALSE` (default): Simply loads existing combined data file
#'
#' The function automatically combines ROSS and Fort Collins datasets using `full_join()`,
#' ensuring all records from both sources are preserved. Once this data is loaded,
#' simple `filter()` and `select()` calls should get the desired data. The purpose
#' of this function is to munge all of the available data into a format that is
#' easier to work with.
#'
#' @param input_all_chem_data_path Character string specifying path to existing combined
#'   chemistry data file. Used when no updates are requested. Default points to most
#'   recent combined dataset.
#' @param input_ross_chem_data_path Character string specifying path to existing processed
#'   ROSS data file. Used when ROSS data is not being updated but FC data is.
#' @param input_fc_chem_data_path Character string specifying path to existing processed
#'   FC data file. Used when FC data is not being updated but ROSS data is.
#' @param update_all_data Logical indicating whether to update both ROSS and FC data
#'   from raw sources. When TRUE, ignores individual update flags. Default is FALSE.
#' @param update_ross_data Logical indicating whether to update ROSS data from raw source.
#'   Default is FALSE.
#' @param update_fc_data Logical indicating whether to update FC data from raw source.
#'   Default is FALSE.
#' @param raw_ross_chem_data_path Character string specifying path to raw ROSS chemistry
#'   data file. Required when `update_ross_data` or `update_all_data` is TRUE.
#' @param raw_fc_chem_data_path Character string specifying path to raw FC chemistry
#'   data directory. Required when `update_fc_data` or `update_all_data` is TRUE.
#' @param output_all_directory Character string specifying directory for combined data output.
#'   Required when any update operation is performed.
#' @param output_ross_directory Character string specifying directory for ROSS data output.
#'   Required when `update_ross_data` or `update_all_data` is TRUE.
#' @param output_fc_directory Character string specifying directory for FC data output.
#'   Required when `update_fc_data` or `update_all_data` is TRUE.
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
#' # Update all data from raw sources
#' all_chem <- load_grab_sample_data(
#'   update_all_data = TRUE,
#'   raw_ross_chem_data_path = "path/to/ross/raw_data.csv",
#'   raw_fc_chem_data_path = "path/to/fc/raw_data_dir",
#'   output_all_directory = "path/to/output",
#'   output_ross_directory = "path/to/output",
#'   output_fc_directory = "path/to/output"
#' )
#'
#' # Update only ROSS data
#' all_chem <- load_grab_sample_data(
#'   input_fc_chem_data_path = "path/to/existing/fc_data.parquet",
#'   update_ross_data = TRUE,
#'   raw_ross_chem_data_path = "path/to/new/ross_data.csv",
#'   output_all_directory = "path/to/output",
#'   output_ross_directory = "path/to/output"
#' )
#' }
#'
#' # This is how I made the current all chem data file (`all_chem_data_2025-10-15.parquet`):
#' load_grab_sample_data(
#'   input_all_chem_data_path = NULL,
#'   input_ross_chem_data_path = here("data", "upper_clp_dss", "modeling", "ross_chem_data_2025-10-15.parquet"),
#'   input_fc_chem_data_path = here("data", "upper_clp_dss", "modeling", "fc_chem_data_2025-10-15.parquet"),
#'   update_all_data = TRUE,
#'   raw_ross_chem_data_path = here("data", "upper_clp_dss", "ross_clp_chem", "data", "cleaned", "CLP_chemistry_up_to_20250701.csv"),
#'   raw_fc_chem_data_path = here("data", "upper_clp_dss", "fc_clp_chem"),
#'   output_all_directory = here("data", "upper_clp_dss", "modeling"),
#'   output_ross_directory = here("data", "upper_clp_dss", "modeling"),
#'   output_fc_directory = here("data", "upper_clp_dss", "modeling")
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
#' - When updating data, corresponding raw data paths must be provided and exist
#' - When not updating, corresponding input data paths must be provided and exist
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
    input_all_chem_data_path = here("data", "upper_clp_dss", "modeling", "all_chem_data_2025-10-15.parquet"),
    input_ross_chem_data_path = NULL,
    input_fc_chem_data_path = NULL,
    update_all_data = FALSE,
    update_ross_data = FALSE,
    update_fc_data = FALSE,
    raw_ross_chem_data_path = NULL,
    raw_fc_chem_data_path = NULL,
    output_all_directory = NULL,
    output_ross_directory = NULL,
    output_fc_directory = NULL
){
  # Argument checks ----
  # Check input paths (only if not NULL)
  if (!is.null(input_all_chem_data_path) && !file.exists(input_all_chem_data_path)) {
    stop("input_all_chem_data_path does not exist:\n  ", input_all_chem_data_path)
  }

  if (!is.null(input_ross_chem_data_path) && !file.exists(input_ross_chem_data_path)) {
    stop("input_ross_chem_data_path does not exist:\n  ", input_ross_chem_data_path)
  }

  if (!is.null(input_fc_chem_data_path) && !file.exists(input_fc_chem_data_path)) {
    stop("input_fc_chem_data_path does not exist:\n  ", input_fc_chem_data_path)
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

  # Check raw path arguments (file or directory must exist)
  if (!is.null(raw_ross_chem_data_path) && !file.exists(raw_ross_chem_data_path)) {
    stop("raw_ross_chem_data_path does not exist:\n  ", raw_ross_chem_data_path)
  }

  if (!is.null(raw_fc_chem_data_path) && !file.exists(raw_fc_chem_data_path)) {
    stop("raw_fc_chem_data_path does not exist:\n  ", raw_fc_chem_data_path)
  }

  # Check that directory arguments are not NULL when updates are requested
  if (update_all_data && is.null(output_all_directory)) {
    stop("output_all_directory cannot be NULL when update_all_data is TRUE")
  }

  if (update_ross_data && is.null(output_ross_directory)) {
    stop("output_ross_directory cannot be NULL when update_ross_data is TRUE")
  }

  if (update_fc_data && is.null(output_fc_directory)) {
    stop("output_fc_directory cannot be NULL when update_fc_data is TRUE")
  }

  # Create output directories if they don't exist (only when updating)
  if (update_all_data && !dir.exists(output_all_directory)) {
    dir.create(output_all_directory, recursive = TRUE)
    message("Created output_all_directory: ", output_all_directory)
  }

  if (update_ross_data && !dir.exists(output_ross_directory)) {
    dir.create(output_ross_directory, recursive = TRUE)
    message("Created output_ross_directory: ", output_ross_directory)
  }

  if (update_fc_data && !dir.exists(output_fc_directory)) {
    dir.create(output_fc_directory, recursive = TRUE)
    message("Created output_fc_directory: ", output_fc_directory)
  }

  # Load in the data ----
  if (any(update_all_data, update_ross_data, update_fc_data)) {

    if (update_ross_data || update_all_data) {
      ross_data <- generate_ross_grab_sample_data(
        raw_ross_chem_data_path = raw_ross_chem_data_path,
        output_directory = output_ross_directory,
        update_data = update_ross_data
      )
    } else {
      # Must have input path if not updating
      if (is.null(input_ross_chem_data_path)) {
        stop("input_ross_chem_data_path must be provided when update_ross_data is FALSE")
      }
      ross_data <- read_ext(input_ross_chem_data_path)
    }

    if (update_fc_data || update_all_data) {
      fc_data <- generate_fc_grab_sample_data(
        raw_fc_chem_data_path = raw_fc_chem_data_path,
        output_directory = output_fc_directory,
        update_data = update_fc_data
      )
    } else {
      # Must have input path if not updating
      if (is.null(input_fc_chem_data_path)) {
        stop("input_fc_chem_data_path must be provided when update_fc_data is FALSE")
      }
      fc_data <- read_ext(input_fc_chem_data_path)
    }

    all_data <- full_join(ross_data, fc_data) %>%
      distinct()

    # Save the updated data
    date <- Sys.Date()
    file_name <- paste0("all_chem_data_", date, ".parquet")
    output_path <- file.path(output_all_directory, file_name)
    write_parquet(all_data, output_path)
    message("Saved updated chemistry data to: ", output_path)

    # Return the data
    return(all_data)
  }

  # Just read in the data
  all_data <- read_ext(input_all_chem_data_path)

  # Return the data
  return(all_data)

}


