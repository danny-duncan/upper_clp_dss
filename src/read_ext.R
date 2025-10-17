#' @title Read data files with automatic format detection
#'
#' @description
#' A convenience function that automatically detects file format based on file extension
#' and reads the data using the appropriate R package. This function eliminates the need
#' to remember which specific read function to use for different file formats and provides
#' a consistent interface for reading common data file types.
#'
#' The function supports three file formats:
#' - CSV files: Uses `readr::read_csv()` with `show_col_types = FALSE` to suppress column type messages
#' - Excel files (.xlsx): Uses `readxl::read_xlsx()`
#' - Parquet files: Uses `arrow::read_parquet()` with `as_data_frame = TRUE` to ensure tibble output
#'
#' All additional arguments are passed through to the underlying read function, allowing
#' for customization of reading behavior (e.g., specifying sheet names for Excel files,
#' column types for CSV files, etc.).
#'
#' @param f Character string specifying the file path with extension. The file extension
#'   is used to determine the appropriate read function.
#' @param ... Additional arguments passed to the underlying read function
#'   (`readr::read_csv()`, `readxl::read_xlsx()`, or `arrow::read_parquet()`).
#'
#' @return A tibble or data frame containing the data from the specified file.
#'
#' @examples
#' \dontrun{
#' # Read a CSV file
#' data_csv <- read_ext("data/my_data.csv")
#'
#' # Read an Excel file with additional arguments
#' data_xlsx <- read_ext("data/my_data.xlsx", sheet = "Sheet2")
#'
#' # Read a Parquet file
#' data_parquet <- read_ext("data/my_data.parquet")
#'
#' # Pass additional arguments to underlying functions
#' data_csv_custom <- read_ext("data/my_data.csv",
#'                            col_types = cols(.default = "c"),
#'                            skip = 1)
#' }
#'
#' @details
#' The function will throw an error if:
#' - The file extension is not one of: csv, xlsx, parquet
#' - The underlying read function encounters an error (e.g., file not found, corrupted file)
#'

read_ext <- function(f, ...) {
  # Determine extension
  file_extension <- tools::file_ext(f)

  if (file_extension == "csv") {
    data <- readr::read_csv(f, show_col_types = FALSE, ...)
  }

  if (file_extension == "xlsx") {
    data <- readxl::read_xlsx(f, ...)
  }

  if (file_extension == "parquet") {
    data <- arrow::read_parquet(f, as_data_frame = TRUE, ...)
  }

  if (!file_extension %in% c("csv", "xlsx", "parquet")) {
    stop("This function only recognizes `.csv`, `.xlsx`, and `.parquet` files.")
  }

  return(data)
}
