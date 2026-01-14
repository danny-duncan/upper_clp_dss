#' Create Grab-Sensor Matchup Dataset
#' @title Match Lab Samples to Sensor Data using Non-Equi Joins (Forward/Backward Pass)
#'
#' @description
#' Creates a matchup dataset by linking individual water chemistry (grab) samples
#' to the closest available sensor reading within a specified time window.
#'
#' The function prioritizes a **forward-looking** match (sensor time >= lab time)
#' within the \code{acceptable_time_window_hours}. If no forward match is found,
#' it then attempts a **backward-looking** match (sensor time <= lab time).
#' Samples that fail both matching steps are retained with \code{NA} for sensor columns.
#'
#' @param water_chem_df A data frame containing water chemistry lab samples.
#' @param sensor_df A data frame containing sensor measurements.
#' @param features A character vector of sensor parameter columns to include in the matchup.
#'   These features are used to check for data availability (\code{NA} values).
#'   Default includes five common water quality parameters.
#' @param acceptable_time_window_hours A numeric value specifying the maximum
#'   acceptable time difference (in hours) between the lab sample and sensor reading
#'   for both forward and backward passes. Default is 3.
#' @param dt_sensor_col The name of the datetime column in `sensor_df`. Must be POSIXct.
#'   Default is "DT_round".
#' @param dt_chem_col The name of the datetime column in `water_chem_df` (lab sample time).
#'   Must be POSIXct and share the same timezone as \code{dt_sensor_col}. Default is "DT\_sample".
#' @param site_col The name of the site identifier column, which must be present in both
#'   \code{water_chem_df} and \code{sensor\_df}. Default is "site".
#'
#' @return A data frame where every row from the original \code{water_chem_df} is present,
#'    with the columns from the closest matched sensor reading added. Unmatched
#'   samples will have \code{NA} values for sensor-derived columns and the sensor datetime column.
#'   The output includes:
#'   \itemize{
#'   \item \code{site_col}: Site identifier. Default column name is "site".
#'   \item \code{sensor_datetime}: Datetime of the matched sensor reading (or \code{NA} if no match).
#'   \item \code{sample_datetime}: Datetime of the lab sample.
#'   \item Sensor feature columns specified in \code{features}.
#'   \item All original columns from \code{water_chem_df}.
#'
#' @details
#' The function will take in a water chemistry data frame and a sensor data frame, and output a matchup dataset based on the provided time window.
#' It performs input validation, checking for the existence of required columns, correct data types (POSIXct for `dt_sensor_col` and `dt_chem_col`), and timezone consistency.
#'
#' Sensor data availability (\code{data_avail}) is determined by checking if all
#' specified \code{features} are non-\code{NA} for a given timestamp. If a \code{data_avail}
#' column already exists in \code{sensor_df}, it is used directly; otherwise, it is
#' calculated internally.
#'
#' @examples
#' library(tidyverse)
#'
#' # Create sample data
#' source("src/collation/load_grab_sample_data.R")
#' water_chem_data <- load_grab_sample_data() %>%
#'  filter(site %in% c("pbd", "sfm") & !is.na(TOC))%>%
#'  mutate(DT_sample = force_tz(DT_sample, tz = "MST"))%>%
#'  select(DT_sample, site, TOC, NO3)
#'
#' # Create sensor dataset
#' source("src/collation/load_sensor_data.R")
#' sensor_data <- load_sensor_data() %>%
#' filter(site %in% c("pbd", "sfm"))%>%
#' pivot_wider(id_cols = c(site, DT_round),
#'             names_from = parameter, values_from = mean_smoothed)
#' # Run the matchup function
#' matchup_data <- create_grab_sensor_matchup(
#'   water_chem_df = water_chem_data,
#'   sensor_df = sensor_data,
#'   features = c("Turbidity", "Chl-a Fluorescence", "FDOM Fluorescence"),
#'   acceptable_time_window_hours = 3,
#'   dt_sensor_col = "DT_round",
#'   dt_chem_col = "DT_sample",
#'   site_col = "site"
#' )
#' head(matchup_data)
#'

create_grab_sensor_matchup <- function(water_chem_df,
                                       sensor_df,
                                       features = c("Chl-a Fluorescence", "FDOM Fluorescence", "Specific Conductivity", "Temperature", "Turbidity"),
                                       acceptable_time_window_hours = 3,
                                       dt_sensor_col = "DT_round",
                                       dt_chem_col = "DT_sample",
                                       site_col = "site") {

  #---- Input Validation ----

  # Check that none of the inputs are NULL or NA
  if (is.null(water_chem_df) || is.null(sensor_df) || is.null(features)|| is.null(acceptable_time_window_hours)|| is.null(dt_sensor_col)|| is.null(dt_chem_col)|| is.null(site_col)) {
    stop("Input data frames cannot be NULL.")
  }
  # Check that acceptable_time_window_hours is not NA and is numeric and is positive
  if (!is.na(acceptable_time_window_hours) & !is.numeric(acceptable_time_window_hours) & acceptable_time_window_hours <= 0) {
    stop("acceptable_time_window_hours must be a positive numeric value")
  }

  # Check that required columns exist in water_chem_df
  if(!all( c(site_col, dt_chem_col) %in% names(water_chem_df))) {
    stop("water_chem_df is missing required columns: ",
         paste(setdiff(c(site_col, dt_chem_col), colnames(water_chem_df)), collapse = ", "))
  }
  # Check that required columns exist in water_chem_df
  if(!all( c(site_col, dt_sensor_col) %in% names(sensor_df))) {
    stop("sensor_df is missing required columns: ",
         paste(setdiff(c(site_col, dt_sensor_col), colnames(sensor_df)), collapse = ", "))
  }
  #Check that dt_sensor_col and dt_chem_col are POSIXct and use have the same TZ
  is_dt_chem_posixct <- inherits(water_chem_df[[dt_chem_col]], "POSIXct")
  is_dt_sensor_posixct <- inherits(sensor_df[[dt_sensor_col]], "POSIXct")
  if( !is_dt_chem_posixct & !is_dt_sensor_posixct){
    stop(paste0("Both '", dt_chem_col, "' and '", dt_sensor_col, "' columns must be of POSIXct type"))
  }
  if(!is_dt_sensor_posixct){
    stop(paste0("sensor_df '", dt_sensor_col, "' column must be of POSIXct type"))
  }
  if(!is_dt_chem_posixct){
    stop(paste0("water_chem_df '", dt_chem_col, "' column must be of POSIXct type"))
  }
  #check that they have the same TZ
  if(is_dt_chem_posixct & is_dt_sensor_posixct){
    tz_chem <- tz(water_chem_df[[dt_chem_col]])
    tz_sensor <- tz(sensor_df[[dt_sensor_col]])
    if(tz_chem != tz_sensor){
      stop(paste0("Datetime columns '", dt_chem_col, "'(dt_chem_col) and '", dt_sensor_col, "'(dt_sensor_col) must have the same timezone.\nCurrently they are '", tz_chem, "' and '", tz_sensor, "' respectively.\nPlease correct one to match the other for accurate matches. "))
    }
  }

  #Checking if features are available in sensor_df
  if (!all(features %in% names(sensor_df))) {
    stop("sensor_df is missing required feature columns: ",
         paste(setdiff(features, names(sensor_df)), collapse = ", "))
  }

  #---- Pre Processing ----
  #check to see if data_avail column exists in sensor_df
  #if so, we will filter to true values only and do our column transformations
  if("data_avail" %in% names(sensor_df)){
    warning("'data_avail' column already exists in sensor_df and function will default to it's use")
    sensor_df_filtered <- sensor_df %>%
      select(site = !!sym(site_col), sensor_datetime = !!sym(dt_sensor_col), any_of(features)) %>%
      filter(data_avail == TRUE)# Only join to rows with available data

  }else{
    # Create the data_avail column in sensor_df (if it doesn't exist)
    sensor_df_filtered <- sensor_df %>%
      #column selection and renaming
      select(site = !!sym(site_col), sensor_datetime = !!sym(dt_sensor_col), any_of(features)) %>%
      rowwise() %>%
      mutate(
        data_avail = all(!is.na(c_across(all_of(features))))
      ) %>%
      ungroup() %>%
      filter(data_avail == TRUE) # Only join to rows with available data
  }

  # Cleaning up water_chem_df: select and rename columns
  water_chem_df <- water_chem_df %>%
    select(site = !!sym(site_col), sample_datetime = !!sym(dt_chem_col), everything())%>%
    #add in a "sample_id" to uniquely identify each row
    mutate(sample_id = row_number())


  # ---- Joining Datasets ----

  # ---- Forward Joins ----
  # Since we typically sample at the same time as sensor cleanings, we want to start with joining
  # sensor readings that are at or after the lab sample time (so the sensor is cleanest/most accurate)
  # We have a few conditions to satisfy:
  # 1. Site must match
  # 2. Sensor time must be >= Lab sample time
  # 3. Time difference must be within the acceptable window (e.g., 3 hours)
  # 4. Sensor data must be available (handled in preprocessing step)

  #This will get us all the data that matches the conditions above
  forward_matchup_all <- water_chem_df %>%
    left_join(
      sensor_df_filtered,
      # Define the join conditions using join_by()
      by = join_by(site == site, # Site match
                   sample_datetime <= sensor_datetime) # Sensor time is >= Lab time
    ) %>%
    # Filter the results by the time window (less than acceptable_time_window_hours)
    mutate(
      #Calculate time difference in hours
      time_diff_hours = as.numeric(difftime(sensor_datetime, sample_datetime, units = "hours"))
    )%>%
    filter( time_diff_hours <= acceptable_time_window_hours)

  # Since the join_by returns ALL matches, we need to pick the closest one.
  forward_matches <- forward_matchup_all  %>%
    # Find the row with the minimum time difference (the closest one)
    slice_min(n = 1, order_by = time_diff_hours, with_ties = FALSE, by = sample_id)    # Group by the unique identifier for the lab sample row

  # Now we will see which did not get joined (ie one of the conditions not met)
  unmatched_samples <- anti_join(
    water_chem_df,
    forward_matches,
    by = "sample_id"
  )

  #---- Backward Joins ----
  # For any lab samples that did not get a forward match, we will now try to
  # find a sensor reading that is BEFORE the lab sample time.
  #This may occur when a sensor was removed from deployment or if a sensor was improperly
  # cleaned/calibrated/etc right before a lab sample was taken.

  # The conditions are similar to above, but reversed:
  # 1. Site must match
  # 2. Sensor time must be <= Lab sample time
  # 3. Time difference must be within the acceptable window (e.g., 3 hours)
  # 4. Sensor data must be available (handled in preprocessing step)

  backward_matchup_all <- unmatched_samples %>%
    left_join(
      sensor_df_filtered,
      by = join_by(
        site == site, # Site match
        sample_datetime >= sensor_datetime) # Sensor time is BEFORE Lab time
    ) %>%
    # Calculate time difference and filter by the acceptable window
    mutate(
      # The time difference is positive: Lab time - Sensor time
      time_diff_hours = as.numeric(difftime(sample_datetime, sensor_datetime, units = "hours"))
    ) %>%
    filter(
      time_diff_hours <= acceptable_time_window_hours
    )

  # Select the Single Closest Backward Match
  backward_matches <- backward_matchup_all %>%
    # Find the row with the minimum time difference (closest *before* the sample)
    slice_min(n = 1, order_by = time_diff_hours, with_ties = FALSE, by = sample_id)     # Group by the unique lab sample ID

  # ---- Combine Results and Final Cleanup ----

  # Combine forward matches and backward matches
  all_matches <- bind_rows(forward_matches, backward_matches)
  # Find samples that did not get any match (neither forward nor backward)
  non_joined_samples <- anti_join( water_chem_df, all_matches, by = "sample_id" )

  # Bind in non joined samples with NA for sensor data
  # We want to have a record of which/how many samples did not get matched
  final_results <- all_matches%>%
    bind_rows(non_joined_samples) %>%
    # Select columns to keep, rename ID column
    select(
      !!sym(site_col) := site, # rename to user defined site_col (default = "site")
      sensor_datetime,
      sample_datetime,
      any_of(features),
      everything(), #this will get anything from water_chem_df
      -data_avail, -time_diff_hours, -sample_id #remove temporary columns
    )

  return(final_results)
}
