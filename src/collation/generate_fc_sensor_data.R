#' Generate Fort Collins (FC) CLP Sensor Data
#'
#' This function reads, cleans, and compiles Fort Collins sonde data (historical and current)
#' from specified raw data sources (Contrail API and local log files). It implements the
#'  Quality Assurance/Quality Control (QA/QC) pipeline from the `ross.wq.tools`
#'  package, including: loading field notes, merging data, generating and applying
#' seasonal and specification thresholds, and applying single-sensor,
#' intra-sensor (inter-parameter), and network-level checks. Finally, it performs
#' data cleaning and smoothing from functions in `src/sensor_qaqc/` and generates a final dataset.
#' The processed dataset can be saved as a Parquet file for future use. If the user sets update_data to T,
#' the function will run the full pipeline from raw data to final output. If set to F, it will load the most recent compiled dataset.
#'
#'
#' The pipeline steps include:
#' \itemize{
#'   \item Importing and formatting **Fort Collins log files** (2024+ for complete sensor suite).
#'   \item Loading **historical Contrail API data** (2016+).
#'   \item Importing and cleaning **field notes** and **sensor malfunction records**.
#'   \item Consolidating datasets, summarizing to 15-minute intervals, and adding summary statistics.
#'   \item Applying **Single-Sensor Flags** (field visits, NA/missing, DO noise, drift, spec limits, seasonal thresholds).
#'   \item Applying **Intra-Sensor Flags** (freezing, burial, unsubmerged, resolving flag overlaps).
#'   \item Applying **Network-Level Flags** (cross-site comparison, isolated anomalies).
#'   \item Performing final **cleaning and smoothing** (remove malfunction data, apply cleaning filters, interpolation, binomial low-pass filter, hourly median).
#' }
#'
#' This function is sourced by `load_sensor_data()` to pull in all ROSS and Fort Collins CLP sensor data.
#'
#' @param data_dir Character. Path to the root directory containing
#'   raw FC sensor data, including subfolders like `historical_contrail_data` and
#'   `raw_log_data` (default: `here("data", "raw", "sensor", "fc_sensors")`).
#' @param output_directory Character. Path to the output directory for saving
#'   processed data if `update_data = TRUE`. The file is saved as a Parquet file
#'   (default: `here("data", "collated", "sensor")`).
#' @param field_notes_file Character (optional). Path to the CSV file containing
#'   Fort Collins field notes (default: `here("data", "raw", "field_notes", "fc_field_notes.csv")`).
#' @param seasonal_thresholds_file Character (optional). Path to the CSV file
#'   containing seasonal QA/QC thresholds (default:
#'   `here("data","derived","auto_qaqc_files","thresholds","fc_seasonal_thresholds_2025_sjs.csv")`).
#' @param contrail_creds_file Character (optional). Path to the YAML file
#'   containing Contrail API login credentials (username, password, login_url).
#'   Required if `update_data = TRUE` (default: `here("creds", "contrail_creds.yml")`).
#' @param mwater_creds_file Character (optional). Path to the YAML file
#'   containing mWater API credentials for pulling sensor malfunction notes.
#'   Required if `update_data = TRUE` (default: `here("creds", "mWaterCreds.yml")`).
#' @param site_order_file Character (optional). Path to the YAML file
#'   containing the FC site order information, used for network-level checks
#'   (default: `here("data","derived","auto_qaqc_files", "site_order", "fc_site_order.yml")`).
#' @param update_data Logical. If `TRUE`, the pipeline runs from raw data to
#'   the final flagged and smoothed dataset, which is then saved in the
#'   `output_directory`. If `FALSE`, the function loads the most recent
#'   pre-existing compiled Parquet file from the `output_directory` (default: `FALSE`).
#'
#' @return
#' A cleaned, compiled, and smoothed tibble of FC sensor data containing:
#' \itemize{
#'   \item `DT_round`: POSIXct timestamp (MST, rounded to 15-min interval).
#'   \item `site`: Site identifier (e.g., "pbr_fc", "pman_fc").
#'   \item `parameter`: Measured parameter name.
#'   \item `mean`: The raw measured value (15-minute mean).
#'   \item `clean_flag`: The final QA/QC flag, consolidating all checks.
#'   \item `mean_cleaned`: Value after removing flag-identified anomalies.
#'   \item `mean_filled`: Value after interpolation of short data gaps.
#'   \item `mean_smoothed`: Value after applying binomial low-pass filter.
#'   \item `hourly_median`: Final value aggregated to the hourly median.
#'   \item `last_site_visit`: Timestamp of the last site visit.
#'   \item `status`: Denotes data status (e.g., "auto_flagged").
#' }
#'
#' @examples
#' \dontrun{
#' # Generate and save FC data (requires API credentials and raw files)
#' fc_data_new <- generate_fc_sensor_data(update_data = TRUE)
#'
#' # Load the most recently compiled data
#' fc_data_old <- generate_fc_sensor_data(update_data = FALSE)
#' }
#'
#'

generate_fc_sensor_data <- function(data_dir = here("data","raw", "sensor", "fc_sensors"),
                                    output_directory = here("data", "collated", "sensor"),
                                    field_notes_file = here("data", "raw", "field_notes", "fc_field_notes.csv"),
                                    seasonal_thresholds_file = here("data","derived","auto_qaqc_files","thresholds","fc_seasonal_thresholds_2025_sjs.csv"),
                                    contrail_creds_file = here("creds", "contrail_creds.yml"),
                                    mwater_creds_file = here("creds", "mWaterCreds.yml"),
                                    site_order_file = here("data","derived","auto_qaqc_files", "site_order", "fc_site_order.yml"),
                                    update_data = FALSE) {
  #load custom functions needed
  source("src/setup_libraries.R")
  source("src/api_pull/pull_contrail_api.R")
  source("src/collation/read_ext.R")
  #functions for cleaning
  source("src/sensor_qaqc/apply_cleaning_filters.R")
  source("src/sensor_qaqc/apply_interpolation_missing_data.R")
  source("src/sensor_qaqc/apply_low_pass_binomial_filter.R")
  source("src/sensor_qaqc/apply_timestep_median.R")
  # Check that data dir and output dir exist (if needed)
  if(!dir.exists(data_dir)){
    stop(paste0("Data directory does not exist: ", data_dir))
  }

  if(!dir.exists(output_directory) && update_data){
    dir.create(output_directory, recursive = TRUE)
  }

  # Read in all raw data and pass through flagging pipeline if update_data is TRUE
  if(update_data){
    #check all the file paths
    if(!dir.exists(data_dir)){
      stop(paste0("Data directory does not exist: ", data_dir))
    }
    if(!file.exists(field_notes_file)){
      stop(paste0("Field notes file does not exist: ", field_notes_file))
    }
    if(!file.exists(contrail_creds_file)){
      stop(paste0("Contrail credentials file does not exist: ", contrail_creds_file))
    }
    if(!file.exists(mwater_creds_file)){
      stop(paste0("mWater credentials file does not exist: ", mwater_creds_file))
    }


    # API pull from Contrail
    # Pulling data from 2016-2025 from the Manners Indian Meadows site for FC sonde data. Indian Meadows was not deployed until 2020, so data before that will be empty.
    # Manner's did not have as many parameters until 2020 (originally just SC, Temperature and Turbidity)

    creds <- read_yaml(contrail_creds_file)
    username <- creds$username
    password <- creds$password
    url <- creds$login_url

    #Set years to pull
    max_year = year(Sys.Date())
    years <- seq(2016, max_year, by = 1)
    #Setup pull for each year
    all_contrail_data <- map(years, function(year){
      start_dt = ymd_hms(paste0(year, "-01-01 00:00:00"), tz = "MST")
      end_dt = ymd_hms(paste0(year, "-12-31 23:59:59"), tz = "MST")

      #check to see if file exists for year
      file_path <- here(data_dir,"historical_contrail_data", paste0("contrail_manners_indianmeadows_", year, ".parquet"))
      #TODO: if we are going to do this frequently, we may need to add in a check for current year's data
      if(!file.exists(file_path)){
        message(paste0("Pulling Contrail data for year: ", year))
        #do an API call for that year (get all sonde data)
        data <- pull_contrail_api(start_DT = start_dt,
                                  end_DT = end_dt, username = username,
                                  password = password,login_url = url)

        parsed_data <- data%>%
          keep(~nrow(.) > 0)

        if(length(parsed_data) > 0){
          combined_data <- bind_rows(parsed_data)
          #Save to a parquet file for each year if there is data available
          write_parquet(combined_data, here(data_dir,"historical_contrail_data", paste0("contrail_manners_indianmeadows_", year, ".parquet")))
        } else {
          combined_data <- data.frame()
        }
        return(combined_data)
      } else {
        return(read_parquet(file_path))
      }
    })%>%
      bind_rows()%>%
      filter(!parameter %in% c("Depth", "Flow"))

    # Log Data
    # There are some artifacts that may be from the telemetry system/ contrail issues so we will want to also pull in the data from the logs provided by Fort Collins.

    ## Load in 2024 algae logs & 2025 data
    ## 2024 Algae logs
    #These data were provided by Diana Schmidt at FC and contain data from the Total Algae Sensor at the Manners and Indian Meadows sondes.
    # Originally the algae sensors were not uploading to Contrail so these logs were provided to fill in that gap.
    ## 2025 log data
    #These data were provided by Diana Schmidt & Nate Sievers at FC and contain the entire log data from the sondes at the Manners and Indian Meadows stations in 2025

    all_log_data <- list.files(path = here(data_dir,"raw_log_data"),
                               full.names = TRUE)%>%
      #function to parse log files
      map(., function(file){
        #read in file
        raw <- read_ext(file)%>%
          clean_names()%>%
          mutate(date = mdy(date_mm_dd_yyyy),
                 DT = ymd_hms(paste(date, time_hh_mm_ss), tz = "America/Denver"), #parse DT
                 DT =  with_tz(DT, tz = "UTC"), # create date and convert to UTC
                 DT_round = with_tz(round_date(DT, "15 min"), tz = "UTC"), # round to 15 min
                 DT_join = as.character(DT_round), # create character version
                 site = ifelse(grepl("IndianMeadows", file), "pbr_fc", "pman_fc"))   #get site name based on file

        clean <- raw %>%
          rename(pH = any_of("p_h")) %>%
          select( DT, DT_round, DT_join, site,
                  `TAL PC Fluorescence` = grep("tal_pc_rfu", colnames(raw), value = TRUE), #These values change sometimes so setting to more open search
                  `Chl-a Fluorescence` = grep("chlorophyll_rfu", colnames(raw), value = TRUE), #These values change sometimes so setting to more open search
                  `Chl-a ugl` = grep("chlorophyll_ug_l", colnames(raw), value = TRUE),
                  `Specific Conductivity` = grep("sp_cond", colnames(raw), value = TRUE), #These values change sometimes so setting to more open search
                  DO = grep("odo_mg_l", colnames(raw), value = TRUE),
                  Temperature = grep("temp", colnames(raw), value = TRUE), #These values change sometimes so setting to more open search
                  any_of("pH"), #pH column name is consistent
                  Turbidity = grep("turbidity", colnames(raw), value = TRUE), #These values change sometimes so setting to more open search
                  Depth = grep("depth", colnames(raw), value = TRUE))%>%
          # #pivot longer to have one row per parameter
          pivot_longer(cols = -c(DT_round, site,DT, DT_join),
                       names_to = "parameter",
                       values_to = "value")
        return(clean)

      })%>%
      rbindlist()%>%
      # remove duplicate data uploaded
      distinct()%>%
      #depth is not accurate on these sondes so removing now
      filter(!parameter %in% c("Depth", "Flow"))%>%
      mutate(units = case_when(parameter == "Temperature" ~ "C",
                               parameter == "DO" ~ "mg/L",
                               parameter == "pH" ~ "pH",
                               parameter == "Specific Conductivity" ~ "uS/cm",
                               parameter == "Turbidity" ~ "NTU",
                               parameter == "Chl-a Fluorescence" ~ "RFU",
                               parameter == "TAL PC Fluorescence" ~ "RFU"))%>%
      #reorder cols
      select(site, DT, DT_round, DT_join, parameter, value, units)%>%
      filter(!is.na(value)&!is.na(DT_round)&!is.na(parameter)&!is.na(site)) #remove any rows with missing critical data


    # Join Contrail and Log Data

    #Preferentially join the log data where available (2024-2025) and use Contrail data for historical data (2016-2023).
    #When there is overlap, use the log data over Contrail. There are some instances where the sonde log could not be downloaded
    #but Contrail still received data so we want to keep that data.
    #Find instances where Contrail has data that log does not
    missing_in_log <- all_contrail_data %>%
      anti_join(all_log_data, by = c("DT_round", "site", "parameter"))
    # Combine both datasets
    all_fc_raw <- bind_rows(all_log_data,missing_in_log) %>%
      select(site, DT, DT_round, DT_join, parameter, value, units)%>%
      #prep for ross_wq_tools functions
      split(f = list(.$site, .$parameter), sep = "-") %>%
      keep(~nrow(.) > 0)

    #Read in recent field notes
    fc_field_notes <- read_ext(field_notes_file)%>%
      #Fix start DT and end DTs
      mutate(start_DT = mdy_hm(start_dt, tz = "America/Denver"),
             end_DT = if_else(is.na(end_dt), start_DT + hours(1), # if no end date, assume 1 hour site visit
                              mdy_hm(end_dt, tz = "America/Denver")),
             DT_round = with_tz(round_date(start_DT, "15 min"), tz = "UTC"), # round and convert to 15min
             DT_join = as.character(DT_round), #create character version
             date = as_date(DT_round),
             field_season = year(DT_round), #get year
             last_site_visit = DT_round, # get last site visit
             date = as.character(date), # get date
             sonde_employed = case_when(  #binary for sonde deployment (NA & 0 deployed, 1 == non employed moving forward)
               is.na(sensor_change)  ~ NA,
               sensor_change == "Swapped" ~ NA,
               sensor_change == "Removed" ~ 1,
               sensor_change == "Deployed" ~ 0),
             site  = ifelse(grepl("indian meadows", site), "pbr_fc", "pman_fc"))%>%
      # Sort by timestamp (most recent first)
      arrange(desc(DT_round))%>%
      #reorder columns to match typical mWater notes
      select( site,crew, DT_round, sonde_employed,cals_performed,sonde_moved,sensor_malfunction, sensor_change,
              DT_join, start_DT, end_DT, date, visit_comments = notes, field_season, last_site_visit)%>%
      #notes come in as MT, converting to UTC
      mutate(start_DT = with_tz(start_DT, tzone = "UTC"),
             end_DT = with_tz(end_DT, tzone = "UTC"))

    # Tidy all the raw files
    tidy_fc_data <- all_fc_raw %>%
      map(~tidy_api_data(api_data = .)) %>%  # the summarize interval default is 15 minutes
      keep(~!is.null(.))

    combined_data <- tidy_fc_data %>%
      map(~add_field_notes(df = ., notes = fc_field_notes)) #Add field notes to each dataset)
    # # Add summary statistics
    summarized_data <- combined_data %>%
      map(~generate_summary_statistics(.))
    # Load sensor specification thresholds
    sensor_thresholds_file <- here("data","derived","auto_qaqc_files","thresholds","sensor_real_thresholds.yml")
    # read threshold data
    sensor_thresholds <- read_yaml(sensor_thresholds_file)
    #load all thresholds and bind with FC thresholds
    season_thresholds <- read_csv(seasonal_thresholds_file, show_col_types = FALSE)

    # Pulling in the data from mWater (where we record our field notes)
    mWater_creds <- read_yaml(mwater_creds_file)
    mWater_data <- load_mWater(creds = mWater_creds)
    # Grab sensor malfunction notes from mWater (We don't have records of malfunctions from FC so we will use ours as a placeholder so flagging works as expected)
    sensor_malfunction_notes <- grab_mWater_malfunction_notes(mWater_api_data = mWater_data)%>%
      #notes come in as MST, converting to UTC
      mutate(start_DT = with_tz(start_DT, tzone = "UTC"),
             end_DT = with_tz(end_DT, tzone = "UTC"))

    # process data in chunks for memory efficiency
    summarized_data_chunks <- split(1:length(summarized_data),
                                    ceiling(seq_along(1:length(summarized_data))/4))

    single_sensor_flags <- list()

    for (chunk_idx in seq_along(summarized_data_chunks)) {
      message("\n=== Processing chunk ", chunk_idx, " of ", length(summarized_data_chunks), " ===")

      indices <- summarized_data_chunks[[chunk_idx]]
      chunk_data <- summarized_data[indices]

      # apply single-parameter flags
      chunk_results <- chunk_data %>%
        future_map(
          function(data) {
            flagged_data <- data %>%
              data.table(.) %>%
              # flag field visits
              add_field_flag(df = .) %>%
              # flag missing/NA values
              add_na_flag(df = .) %>%
              # flag dissolved oxygen noise patterns
              find_do_noise(df = .) %>%
              # # flag repeating/stuck values
              # add_repeat_flag(df = .) %>%
              # # flag depth shifts (sonde movement)
              # add_depth_shift_flag(df = ., level_shift_table = all_field_notes, post2024 = TRUE) %>%
              # flag sensor drift (FDOM, Chl-a, Turbidity)
              add_drift_flag(df = .)

            # apply sensor specification flags if thresholds exist
            if (unique(data$parameter) %in% names(sensor_thresholds)) {
              flagged_data <- flagged_data %>%
                data.table(.) %>%
                add_spec_flag(df = ., spec_table = sensor_thresholds)
            }

            # apply seasonal threshold flags if available
            if (unique(data$parameter) %in% unique(season_thresholds$parameter)) {
              flagged_data <- flagged_data %>%
                data.table(.) %>%
                add_seasonal_flag(df = ., threshold_table = season_thresholds)
            }

            return(flagged_data)
          },
          .progress = TRUE
        )

      single_sensor_flags <- c(single_sensor_flags, chunk_results)

      if (chunk_idx < length(summarized_data_chunks)) {
        gc()  # garbage collection between chunks
        Sys.sleep(0.1)
      }
    }

    # combine single-parameter flags by site
    intrasensor_flags <- single_sensor_flags %>%
      rbindlist(fill = TRUE) %>%
      split(by = "site")

    # process inter-parameter flags in chunks
    intrasensor_data_chunks <- split(1:length(intrasensor_flags),
                                     ceiling(seq_along(1:length(intrasensor_flags))/1))

    intrasensor_flags_list <- list()
    for (chunk_idx in seq_along(intrasensor_data_chunks)) {
      message("\n=== Processing chunk ", chunk_idx, " of ", length(intrasensor_data_chunks), " ===")

      indices <- intrasensor_data_chunks[[chunk_idx]]
      chunk_data <- intrasensor_flags[indices]

      chunk_results <- chunk_data %>%
        map(
          function(data) {
            flagged_data <- data %>%
              data.table() %>%
              # flag when water temperature below freezing
              add_frozen_flag(.) %>%
              # check for overlapping flags and resolve
              intersensor_check(.) %>%
              # flag potential sensor burial
              add_burial_flag(.) %>%
              # flag when sonde is above water surface
              add_unsubmerged_flag(.)

            return(flagged_data)
          }
        ) %>%
        rbindlist(fill = TRUE) %>%
        mutate(flag = ifelse(flag == "", NA, flag)) %>%
        split(f = list(.$site, .$parameter), sep = "-") %>%
        discard(~ nrow(.) == 0)%>%
        # # add known sensor malfunction periods
        map(~add_malfunction_flag(df = ., malfunction_records = sensor_malfunction_notes))

      intrasensor_flags_list <- c(intrasensor_flags_list, chunk_results)

      if (chunk_idx < length(intrasensor_data_chunks)) {
        gc()
        Sys.sleep(0.1)
      }
    }

    #Load in site_order yaml for FC sondes
    site_order <- load_site_order(site_order_file)

    # apply network-level quality control
    network_flags <- intrasensor_flags_list %>%
      # network check compares patterns across sites
      map(~network_check(df = ., intrasensor_flags_arg = intrasensor_flags_list, site_order_arg = site_order)) %>%
      rbindlist(fill = TRUE) %>%
      # clean up flag column formatting
      tidy_flag_column() %>%
      split(f = list(.$site, .$parameter), sep = "-") %>%
      # add suspect data flags for isolated anomalies
      map(~add_suspect_flag(.)) %>%
      rbindlist(fill = TRUE)

    message("Applying data cleaning")
    # final data cleaning and preparation
    v_final_flags <- network_flags %>%
      # Remove isolated suspect flags (single point anomalies)
      mutate(auto_flag = ifelse(
        is.na(auto_flag), NA,
        ifelse(auto_flag == "suspect data" &
                 is.na(lag(auto_flag, 1)) &
                 is.na(lead(auto_flag, 1)), NA, auto_flag)
      )) %>%
      # select final columns
      select(c("DT_round", "DT_join", "site", "parameter", "mean", "units",
                      "n_obs", "spread", "auto_flag", "mal_flag", "sonde_moved",
                      "sonde_employed", "season", "last_site_visit")) %>%
      # clean up empty flags
      mutate(auto_flag = ifelse(is.na(auto_flag), NA,
                                       ifelse(auto_flag == "", NA, auto_flag))) %>%
      # split back into site-parameter combinations
      split(f = list(.$site, .$parameter), sep = "-") %>%
      keep(~nrow(.) > 0)

    #Apply our smoothing and cleaning filters to the flagged data
    filtered_smoothed_dataset <- v_final_flags%>%
      map(~.x %>%
            #filter(DT_round >= "2025-04-10" & DT_round <= "2025-06-01")%>%
            filter( !is.na(site), !is.na(parameter), !is.na(DT_round)) %>%
            #converting to MST for later use
            mutate(
              DT_round = with_tz(DT_round, tz = "MST"), #convert to MST for standardization
              DT_join = as.character(DT_round),
              last_site_visit = with_tz(last_site_visit, "MST")
            )%>%
            select(DT_round, DT_join, site, parameter, mean , auto_flag, mal_flag,  last_site_visit) %>%
            #these are to reduce overflagging of specific sites/parameters known to have issues
            apply_cleaning_filters(df = ., new_value_col = "mean_cleaned") %>%
            #method = linear bc spline had some weird interactions on instances with negative values or where data had large swings and near 0
            apply_interpolation_missing_data(df = ., value_col = "mean_cleaned", new_value_col = "mean_filled", max_gap = 4, method = "linear")%>%
            apply_low_pass_binomial_filter(df = ., value_col = "mean_filled", new_value_col = "mean_smoothed")%>%
            apply_timestep_median(df = ., timestep  = "1 hour", value_col = "mean_smoothed", new_value_col = "hourly_median")%>%
            mutate(status = "auto_flagged")
      )
    ## Save Collated file to one large dataset
    complete_dataset <- filtered_smoothed_dataset %>%
      keep(~!all(is.na(.x)))%>% #remove years without data
      bind_rows()%>%
      select(DT_round, site, parameter, mean, clean_flag, mean_cleaned, mean_filled, mean_smoothed, DT_group, hourly_median, last_site_visit, status)

    # Create single large dataset and save this data for later use
    write_parquet(complete_dataset, here(output_directory, paste0("compiled_fc_sensor_data_", Sys.Date(), ".parquet")))
    return(complete_dataset)

  }

  # Attempt to load pre-existing compiled dataset
  compiled_files <- list.files(path = output_directory,
                               pattern = "compiled_fc_sensor_data_.*\\.parquet$",
                               full.names = TRUE)

  if(length(compiled_files) == 0){
    stop("No compiled FC sensor data file found in output_directory: ", output_directory)
  }
  # Load the most recent compiled dataset
  latest_file <- compiled_files%>%tail(1)

  complete_dataset <- read_parquet(latest_file)

  return(complete_dataset)
}




