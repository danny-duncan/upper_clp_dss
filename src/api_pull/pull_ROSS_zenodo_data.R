#' @title Download water chemistry data from ROSS Zenodo publication
#'
#' @description
#' Downloads ROSSyndicate water quality data from the ROSS Zenodo publication and grabs relevant datasets for downstream use (most_recent_chem, most_recent_meta, chem_units).
#' Check the Zenodo publication for the most recent version of the data (https://zenodo.org/records/15883685).
#'
#' @param data_version Character string for the version of the ROSS Zenodo publication to download. See Zenodo for most recent version (https://zenodo.org/records/15883685)
#' Note: This will have the format "vYYYY.MM.DD", where YYYY is the year, MM is the month, and DD is the max date of the dataset
#'
#' @param DOI Character string for the Zenodo DOI of the ROSS Zenodo publication containing the water quality data. See Zenodo for most recent version (https://zenodo.org/records/15883685)
#'
#' @param save_folder_dir Character string for the folder path where the downloaded data will be saved.
#' If this folder does not exist, it will be created and new data will be downloaded
#'
#'
#' @return A list of dataframes, each containing the water quality data or metadata from the ROSS Zenodo publication
#' - `most_recent_chem`: The chemistry dataset from the cleaned directory in the Zenodo publication. See README file in publication (https://zenodo.org/records/15883685) for details
#' - `most_recent_meta`: The metadata dataset from the metadata directory in the Zenodo publication. See README file in publication (https://zenodo.org/records/15883685) for details
#' - `chem_units`: Units for chemistry dataset from the metadata directory in the Zenodo publication. See README file in publication (https://zenodo.org/records/15883685) for details
#'
#' @examples
#'
#' #Call the function for the data version "v2025.07.01" and the Zenodo DOI "15883685", saving the data to the folder "data/upper_clp_dss/ross_clp_chem"
#' data <- pull_ROSS_zenodo_data(data_version = "v2025.07.01", DOI = "15883685", save_folder_dir = "data/upper_clp_dss/ross_clp_chem")
#'
#'# Grab the water chemistry dataset
#' water_chem <- data[["most_recent_chem"]]

pull_ROSS_zenodo_data <- function(data_version = "v2025.07.01",
                                  DOI = "15883685",
                                  save_folder_dir = "data/upper_clp_dss/ross_clp_chem") {


  data_folder <- list.files(path = here::here(save_folder_dir), full.names = T)

  # Check if a folder with the specified name exists in the "data" folder
  if (is_empty(data_folder)) {
    # Download the entire folder from Zenodo DOI as a zipped folder in your working directory
    dwnload_file <- paste0("https://zenodo.org/records/", DOI, "/files/rossyndicate/CLP_waterchem_data-", data_version, ".zip?download=1")
    download.file(url = dwnload_file, destfile = paste0(here::here(save_folder_dir), '.zip'))
    # Unzip this file
    unzip(paste0(here::here(save_folder_dir), '.zip'), exdir = here::here(save_folder_dir), overwrite = T)
    # Grab the name of the download file with rossyndicate in the name
    unzipped_folder <- list.files(path = here::here(save_folder_dir), full.names = T) %>%
      keep(~ grepl("rossyndicate", .))
    # Get all files/folders inside the rossyndicate folder
    folder_contents <- list.files(unzipped_folder, full.names = T)
    # Move each item to the save_folder_dir
    file.rename(folder_contents, file.path(here::here(save_folder_dir), basename(folder_contents)))
    # Remove the rossyndicate folder
    unlink(unzipped_folder, recursive = T)

    # Grab the most recent cleaned and collated chemistry dataset
    # For the current release that file is `CPF_reservoir_chemistry_up_to_071023.csv`
    cleaned_chem_file <- list.files(path = paste0(data_folder,"/data/cleaned/"), full.names = TRUE, pattern = ".rds")
    site_meta_file <- list.files(path = paste0(data_folder, "/data/metadata/"), full.names = TRUE, pattern = "location_metadata")
    units_meta_file <- list.files(path = paste0( data_folder, "/data/metadata/"), full.names = TRUE, pattern = "units")

    most_recent_chem <- read_rds(cleaned_chem_file)
    most_recent_meta <- read_csv_arrow(site_meta_file)
    chem_units <- readxl::read_xlsx(units_meta_file)
    rm(cleaned_chem_file, site_meta_file, data_folder,dwnload_file)
  } else {
    #Only grab previously downloaded file
    cleaned_chem_file <- list.files(path = paste0(here::here(save_folder_dir, "/data/cleaned/")), full.names = TRUE, pattern = ".rds")
    site_meta_file <- list.files(path = paste0(here::here(save_folder_dir, "/data/metadata/")), full.names = TRUE, pattern = "location_metadata")
    units_meta_file <- list.files(path = paste0(here::here(save_folder_dir, "/data/metadata/")), full.names = TRUE, pattern = "units")

    most_recent_chem <- readr::read_rds(cleaned_chem_file)%>%
      mutate(flow_gauge_id = ifelse(flow_gauge_source == "USGS", paste0("0", flow_gauge_id), flow_gauge_id))
    most_recent_meta <- read_csv_arrow(site_meta_file)
    chem_units <- readxl::read_xlsx(units_meta_file)



    rm(cleaned_chem_file, site_meta_file, data_folder, units_meta_file )

  }

  #check to see if version date is the same as the max date in the dataset
  max_date <- max(most_recent_chem$Date, na.rm = T)
  version_max_date <- as.POSIXct(data_version, format = "v%Y.%m.%d")
  if( max_date != version_max_date) {
    stop("The most recent data in the chemistry file is older than the specified data version.\nPlease check the data version or remove a old version.")
  }else{
    cat("Data Version: ",data_version, "\nCheck Zenodo to be sure this is the most up to date version")
  }

  return(list(
    most_recent_chem = most_recent_chem,
    most_recent_meta = most_recent_meta,
    chem_units = chem_units))
}





