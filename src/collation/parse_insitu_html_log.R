#' @title Parse In-Situ log HTML File
#' @description Parses an Insitu HTML file to extract sensor data.
#' @param html_file_path Path to the HTML file to be parsed. Must be In Situ Log file created by VuSitu. File must be in the format: site_startdate_enddate_type.html
#' type can be either vulink or troll, start and end dates must be in the format YYYYMMDD and site is lower case.
#' @return A data frame containing the parsed sensor data.
#'
#' #' @examples
#' #Reading in one file
#'  pbr_data <- parse_insitu_html("data/sensor_data/2025/pbr_20250613_20250701_vulink.html")
#'
#' #Reading in many files
#'  all_files <- list.files("data/sensor_data/2025", pattern = "*.html", full.names = TRUE)
#'
#' all_data <- map(all_files, parse_insitu_html)%>%
#'  bind_rows()
parse_insitu_html_log <- function(html_file_path) {

  #check if troll in file name
  if(grepl("troll", html_file_path, ignore.case = TRUE)){
    troll_log = T
  }else{
    troll_log = F
  }
  #first element after last / and before _ is site name
  site_name <- gsub(".*\\/|_.*", "", html_file_path)%>%tolower()

  # Read the HTML file

  html_content <- read_html(html_file_path)

  # Find the data table
  data_table <- html_content %>%
    html_node("table#isi-report")

  # Extract header row
  headers <- data_table %>%
    html_nodes("tr.dataHeader td") %>%
    html_text() %>%
    trimws()

  if(troll_log == T){
    #remove the first three headers
    headers_clean <- tibble(col_name = headers,
    #find units from header name, first ()
    units = gsub(".*?\\((.*?)\\).*", "\\1", col_name))
  } else {
  #find the header with Vulink in it
  vulink_sn <- headers[grepl("Vulink", headers, ignore.case = TRUE)]%>%
    #remove the vulink part
    gsub("VuLink Cellular ", "", .)

  # troll_sn<- headers[grepl("troll", headers, ignore.case = TRUE)]%>%
  #   #remove everything after third space
  #   gsub(".*?\\s+\\S+\\s+\\S+\\s+", "", .)

  headers_clean <- tibble(col_name = headers[4:length(headers)])%>%
    mutate(col_name = case_when(str_detect(col_name, vulink_sn) ~ gsub(vulink_sn, "Vulink", col_name),
                                #str_detect(col_name, troll_sn) ~ gsub(troll_sn, "AT", col_name),
                                TRUE ~ col_name
                                ),
           #find units from header name, first ()
           units = gsub(".*?\\((.*?)\\).*", "\\1", col_name))
  }

  # Extract data rows
  data_rows <- data_table %>%
    html_nodes("tr.data")

  # Process each data row
  data_list <- list()
  for(i in seq_along(data_rows)) {
    row_data <- data_rows[[i]] %>%
      html_nodes("td") %>%
      html_text() %>%
      trimws()

    data_list[[i]] <- row_data
  }

  # Convert to data frame
  if(length(data_list) > 0) {
    df <- do.call(rbind, data_list) %>%
      as.data.frame()

    colnames(df) <- headers_clean$col_name

    param_oi_list <- paste(c("Temperature",
                        "Turbidity",
                        "Specific Conductivity",
                        "DO",
                        "pH",
                        "Depth",
                        "ORP",
                        "Chl-a Fluorescence",
                        "FDOM Fluorescence" ), collapse = "|")

    df_parsed <- df %>%
      mutate(site = site_name)%>%
      pivot_longer(cols = -c(site, `Date Time`),
                   names_to = "parameter",
                   values_to = "value")%>%
      left_join(headers_clean, by = c("parameter" = "col_name"))%>%
      dplyr::filter(!str_detect( parameter, "Vulink"),
             !str_detect( parameter, "pH mV"),
             !str_detect( parameter, "Saturation"))%>%
      #simplify parameter names
      mutate(parameter = case_when(
        str_detect(parameter, "Temperature") ~ "Temperature",
        str_detect(parameter, "Turbidity") ~ "Turbidity",
        str_detect(parameter, "Specific Conductivity") ~ "Specific Conductivity",
        str_detect(parameter, "RDO Concentration") ~ "DO",
        str_detect(parameter, "pH") ~ "pH",
        str_detect(parameter, "Depth") ~ "Depth",
        str_detect(parameter, "ORP") ~ "ORP",
        str_detect(parameter, "Chlorophyll-a") ~ "Chl-a Fluorescence",
        str_detect(parameter, "FDOM") ~ "FDOM Fluorescence",
        TRUE ~ parameter),
      #convert depth to M
      value = case_when(
        parameter == "Depth" ~ as.numeric(value) * 0.3048, # Convert feet to meters
        (parameter == "ORP" & units == "mV") ~ as.numeric(value) / 1000, # Convert mV to V
        TRUE ~ as.numeric(value)
      ),
      #change name to Depth (m)
      parameter = ifelse(parameter == "Depth (ft)", "Depth", parameter),
      #update units to match HV
      units = case_when(
        parameter == "Depth" ~ "m",
        parameter == "ORP" ~ "V",
        TRUE ~ units
      ),
      timestamp = ymd_hms(`Date Time`, tz = "America/Denver") %>% with_tz("UTC"))%>%
      filter(str_detect(parameter, param_oi_list))%>%
      mutate(#name = basename(html_file_path),
             #id = html_file_path,
             DT = as.POSIXct(timestamp, tz = "UTC"),
             DT_round = floor_date(DT, unit = "15 minutes"),
             DT_join = as.character(DT_round))%>%
      dplyr::select(site, timestamp, parameter, value, units, DT, DT_round, DT_join)


    return(df_parsed)
  } else {
    stop("No data rows found in HTML file")
  }
}

