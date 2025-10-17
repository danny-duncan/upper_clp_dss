#' @title Process and harmonize Fort Collins water chemistry grab sample data
#'
#' @description
#' Reads, processes, and harmonizes water chemistry data and field notes from the City of
#' Fort Collins Upper Cache la Poudre watershed monitoring program. This function handles
#' multiple file formats and structures, standardizes column names and units, and combines
#' chemistry data with corresponding field sampling metadata. This data extraction is
#' based on the harmonization of the FC data that were in
#' `02_sensor_grab_munge.Rmd` and `add_FC_TOC_baseline.Rmd`.
#'
#' The function performs several key data processing steps:
#' - Automatically detects and separates chemistry data files from field note files based on filename patterns
#' - Harmonizes chemistry data by mapping various column name conventions to standardized names
#' - Converts units where necessary (TDS to specific conductivity, NO3_N to NO3)
#' - Extracts site codes from site names using pattern matching
#' - Combines chemistry data with field sampling times and metadata
#' - Applies data quality flags (e.g., "ND" for non-detect TOC values)
#'
#' This harmonization approach consolidates data from multiple years and file formats into
#' a consistent structure that facilitates filtering, analysis, and modeling workflows.
#' The function is designed to work with the data that we have available to us as of
#' 2025-10-15, an changes to the file naming conventions or the structure of the files
#' themselves could break this function.
#'
#' @param raw_fc_chem_data_path Character string specifying the directory path containing
#'   Fort Collins chemistry data files. Default uses here() to point to the standard
#'   project data directory structure.
#' @param output_directory Character string specifying the directory where processed data
#'   should be saved when update_data is TRUE. Default uses here() to point to the
#'   modeling data directory.
#' @param update_data Logical indicating whether to save the processed data to a parquet
#'   file. When TRUE, creates a timestamped parquet file. Default is FALSE.
#'
#' @return A tibble containing harmonized Fort Collins chemistry data.
#'
#' @examples
#' \dontrun{
#' # Process FC data without saving
#' fc_data <- generate_fc_grab_sample_data()
#'
#' # Process and save FC data
#' fc_data <- generate_fc_grab_sample_data(
#'   raw_fc_chem_data_path = "path/to/fc/data",
#'   output_directory = "path/to/output",
#'   update_data = TRUE
#' )
#'
#' # Filter processed data for specific analyses
#' toc_data <- fc_data %>%
#'   filter(!is.na(TOC), site_code %in% c("chd", "pbd", "sfm"))
#' }
#'
#' @details
#' **File Processing Logic:**
#' - Files containing "field" (case-insensitive) are treated as field note files
#' - All other files are treated as chemistry data files
#' - Both file types are read using read_ext() which handles multiple formats
#'
#' **Unit Conversions:**
#' - NO3_N values are converted to NO3 by dividing by 0.2259
#' - TDS values are converted to specific conductivity by dividing by 0.65
#'
#' **Site Code Extraction:**
#' - If site name contains a hyphen, extracts text before the first hyphen
#' - Otherwise, takes the first 3 characters of the site name
#' - All site codes are converted to lowercase
#'
#' **Field Data Requirements:**
#' The function expects field note files to contain columns named:
#' - "Date (MM/DD/YYYY)" for sample dates
#' - "Time (HH:MM:SS)" for sample times
#' - "Site Name" for site identifiers
#'
#' The function will fail if these exact column names are not present in field files.

generate_fc_grab_sample_data <- function(
    raw_fc_chem_data_path = here("data", "upper_clp_dss", "fc_clp_chem"),
    output_directory = here("data", "upper_clp_dss", "modeling"),
    update_data = FALSE
) {
  # Argument checks ----
  # Check that the raw data path is real
  if (!dir.exists(raw_fc_chem_data_path)) {
    stop(raw_fc_chem_data_path, "(raw_fc_chem_data_path) does not exist.")
  }

  # Check that an output directory was supplied if update_data is TRUE
  if (is.null(output_directory) && update_data) {
    stop("output_directory must be provided when update_data is TRUE")
  }

  # Check that the output directory exists if it was supplied and update_data is TRUE,
  # If it does not exist, create it.
  if (!dir.exists(output_directory) && update_data) {
    dir.create(output_directory, recursive = TRUE)
    message("Created fc_output_directory: ", output_directory)
  }

  # Read in the file names from the selected directory ----
  file_paths <- list.files(raw_fc_chem_data_path, full.names = TRUE)

  # Determine the file types (data or field notes) and separate the lists based on that ----
  chem_data_file_paths <- file_paths[!grepl("field", file_paths, ignore.case = TRUE)]
  field_note_file_paths <- file_paths[grepl("field", file_paths, ignore.case = TRUE)]

  # Read in the raw data for each file type ----
  chem_data_raw <- map(chem_data_file_paths, function(f) {read_ext(f)})
  field_data_raw <- map(field_note_file_paths, function(f) {read_ext(f)})

  # Harmonize the raw data for each file type ----

  # Harmonize the chem data.
  chem_data <- map(
    chem_data_raw,
    function(df) {
      df %>%
        select(
          # DT columns
          Date = any_of(c("Date", "Sampled Date")),
          # ID columns
          site_name = any_of(c("LongDesc", "Location Name")),
          # TOC and Chemical Data Columns
          TOC = any_of(c("TOC", "Corrected Result")),
          NO3 = any_of(c("NO3_N")),
          SC = any_of(c("TDS")),
          Cl = any_of(c("Cl")),
          lab_turb = any_of(c("Turbidity")),
          TN = any_of(c("TN_calc"))
        ) %>%
        # Add chemical columns for those that may not be found across all the data
        add_column_if_not_exists("TOC") %>%
        add_column_if_not_exists("NO3") %>%
        add_column_if_not_exists("SC") %>%
        add_column_if_not_exists("Cl") %>%
        add_column_if_not_exists("lab_turb") %>%
        add_column_if_not_exists("TN") %>%
        mutate(
          Date = parse_date_time(Date, orders = c("ymd", "mdy")),
          site_code = if_else(
            str_detect(site_name, "-"),
            tolower(str_extract(site_name, "^[^-]+")),
            tolower(str_sub(site_name, 1, 3))
          ),
          TOC = as.numeric(TOC),
          NO3 = (as.numeric(NO3) / 0.2259),
          SC = (as.numeric(SC) / 0.65), # converting TDS to SC
          Cl = as.numeric(Cl),
          lab_turb = as.numeric(lab_turb),
          TN = as.numeric(TN),
          collector = "FC",
          flag = NA
        ) %>%
        add_flag(is.na(TOC), "ND") %>%
        # set final order of the columns
        select(
          # DT columns
          Date,
          # ID columns
          site_name, site_code, collector,
          # TOC and Chemical Data Columns
          TOC, NO3, SC, Cl, TN, lab_turb,
          # Flag column
          flag
        )
    }
  ) %>%
    reduce(full_join)

  # Harmonize the field data ----
  field_data <- map(
    field_data_raw,
    function(df) {
      # This harmonization is based on `02_sensor_grab_munge.Rmd`
      df %>%
        mutate(
          Date = as.Date(`Date (MM/DD/YYYY)`),
          # Combine date and time into one datetime column from MT to MST for consistency
          # Note: This function assumes this structure in the column names. If this changes, this will break.
          DT_mst = with_tz(ymd_hms(paste(
            format(`Date (MM/DD/YYYY)`, "%Y-%m-%d"),
            format(`Time (HH:MM:SS)`, "%H:%M:%S")
          ), tz = "America/Denver"), tzone = "MST"),
          # Remove numbers from site name
          site_code = tolower(gsub("\\d+", "", `Site Name`)),
          DT_mst_char = as.character(DT_mst)
        )%>%
        dplyr::select(
          # DT columns
          Date, DT_sample = DT_mst, DT_mst_char,
          # ID columns
          site_code
        )
    }
  ) %>%
    reduce(full_join)

  # Join the chem data with the field notes ----
  fc_chem_data <- full_join(chem_data, field_data) %>%
    distinct()

  # Update the saved data ----
  if (update_data) {
    date <- Sys.Date()
    file_name <- paste0("fc_chem_data_", date, ".parquet")
    output_path <- file.path(output_directory, file_name)
    write_parquet(fc_chem_data, output_path)
    message("Saved updated FC chemistry data to: ", output_path)
  }

  # Return the combined FC chem data and FC field notes ----
  return(fc_chem_data)
}
