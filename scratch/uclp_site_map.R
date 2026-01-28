source("src/collation/read_ext.R")
library(tidyverse)
library(sf)
library(tmap)
loc <- read_ext("data/raw/chem/ross_clp_chem/v2025.11.14/data/metadata//location_metadata.csv" )%>%
  mutate(site_code = tolower(site_code))%>%
  select(site_code, Lat, Long, Site_Name)%>%
  filter(site_code %in% c("chd", "pfal", "pbr", "sfm", "pman", "pbd"))%>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)%>%
  mutate(`Data Status` = if_else(site_code %in% c("pbd", "sfm", "chd", "pfal"), "Livesteaming",
                        if_else(site_code%in% c("pbr", "pman"), "Only FC Livestreaming", "No Livestreaming")),
         Site_Name = if_else(site_code == "cbri", "Joe Wright Outflow", Site_Name))


# Calculate a buffered bounding box (in meters)
loc_bbox_buf <- loc %>%
  st_transform(32613) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_buffer(dist = 6500) %>%
  st_transform(4326)

# Plot
tmap_mode("plot")

tm<-  tm_shape(loc_bbox_buf) +
  tm_basemap("CartoDB.Positron") +
  tm_shape(loc) +
  tm_symbols(
           fill = "Data Status",
           fill.scale = tm_scale_categorical(
             values = c("Livesteaming" = "#1E4D2B",
                        "Only FC Livestreaming" = "#256BF5",
                        "No Livestreaming" = "#d9d9d9")),
          shape = "Data Status",
shape.scale = tm_scale_categorical(
  values = c("Livesteaming" = 21,
             "Only FC Livestreaming" = 22,
             "No Livestreaming" = 1) ),
shape.legend = tm_legend_hide()) +
  tm_text("Site_Name", size = 0.7, ymod = 1.2) +
  tm_layout(title = "UCLP WQ Monitoring Sites",
            title.position = c("center", "top"),
            legend.position = c("right", "bottom"),
            legend.title.size = 1,
            legend.text.size = 0.8)



tmap_save(
  tm,
  filename = here("docs", "uclp_dss","figs","2026_ross_sites_map.png"),
  width = 8,height = 6, dpi = 300
)
