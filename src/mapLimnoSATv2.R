# mapLimnoSATv2 - Devin Hunt - April 4, 2023
# This is a cleaned up script of mapLimnoSAT, it is important to preserve experimental methods of v1

# Objectives of project
## Functions:
  # Assign fui value
  # Merge and get names from US_hydro dataset
  # Choosing and displaying a HUC-zone of points in fui color
  # Choose from a few major lakes (with names)
  # Apply timeseries to all data, slider and play timeseries
  # Clicking any lake point generates some plots (color distribution, etc...) Simon Topp

## Timeseries of summer month averages (june, july, aug)
  # View timeseries of month over all present years 1991:jan, 1992:


  # View timeseries of months over years  1991:jan, 1991:feb


  # Subsetting to Colorado so I can get this done, scaling up if feasible

# Load Packages ----

# Data
library(stringr)
library(tidyverse)
library(feather)
library(lubridate)

# Spatial
library(sf)
library(terra)
library(tigris)

# Visualization
library(leaflet)
library(leafpop)
library(leafgl)
library(mapview) # Utilize leafgl platform

# mapviewOptions(platform = "leafgl") # Severe performance throttling on all functions
sf_use_s2(FALSE)

# Cut to Colorado Polygon
inState <- function(sf_object, state = "MI"){
  
  # Read in state boundary from tigris
  state_boundary <- tigris::states() %>% 
    filter(STUSPS == state) %>% 
    select(STUSPS) %>% 
    st_transform(., 4326)
  
  # Correct crs
  sf_84 <- st_set_crs(sf_object, st_crs(state_boundary))
  
  out_state <- st_filter(sf_84, state_boundary)
  
  return(out_state)
}

# Assign color, input must have dWL column present, converts to Forel-Ule index
dWL_to_fui <- function(input_df) {
  
  # Conversion method used from Simon Topp. Some expressions added for min / max (<475,>583)
  input_df$fui_value <- case_when(
    input_df$dWL <= 470 ~ 0,
    input_df$dWL <= 475 & input_df$dWL > 470 ~ 1,
    input_df$dWL <= 480 ~ 2,
    input_df$dWL <= 485 ~ 3,
    input_df$dWL <= 489 ~ 4,
    input_df$dWL <= 495 ~ 5,
    input_df$dWL <= 509 ~ 6,
    input_df$dWL <= 530 ~ 7,
    input_df$dWL <= 549 ~ 8,
    input_df$dWL <= 559 ~ 9,
    input_df$dWL <= 564 ~ 10,
    input_df$dWL <= 567 ~ 11,
    input_df$dWL <= 568 ~ 12,
    input_df$dWL <= 569 ~ 13,
    input_df$dWL <= 570 ~ 14,
    input_df$dWL <= 571 ~ 15,
    input_df$dWL <= 573 ~ 16,
    input_df$dWL <= 575 ~ 17,
    input_df$dWL <= 577 ~ 18,
    input_df$dWL <= 579 ~ 19,
    input_df$dWL <= 581 ~ 20,
    input_df$dWL <= 583 ~ 21,
    input_df$dWL > 583 ~ 22,
    TRUE ~ 23)
  
  return(input_df)
}

# Wavelength Data ----

# LimnoSAT raw data
limno_wave <- read_feather("in/srCorrected_Hydrolakes.feather")

# Filter for Hylaks with > 30 observations
# limno_wave_obs <- filter(limno_wave, )

# Summarize data by month and year for each Hylak_id
limno_wave_month <- limno_wave %>% 
  group_by(Hylak_id, month(date), year) %>% 
  reframe(Hylak_id,
            dWL = round(mean(dWL, na.rm = TRUE), 0))

limno_month_d <- distinct(limno_wave_month)

limno_month_summer <- limno_month_d %>% 
  filter(`month(date)` %in% c(6:8)) 

limno_month_sum <- limno_month_summer %>% 
  group_by(Hylak_id, year) %>% 
  reframe(dWL = round(mean(dWL, na.rm = TRUE), 0))

limno_sum_fui <- dWL_to_fui(limno_month_sum)


## EXPORT FUI VALUES ----
write_feather(limno_sum_fui, "in/limno_summer_fui.feather")

# Clean up environment
rm("limno_month_d", "limno_wave", "limno_wave_month", "limno_month_summer")
gc()

# LimnoSAT shape points ----
limno_points <- st_read("in/HydroLakes_DP.shp")

# Subset limnosat for deepest points
dp_shp <- limno_points %>% filter(type == 'dp')

# Cut to state of choice
limno_point_state <- inState(dp_shp, "CO")

# Merge points with wavelength and fui
# limno_point_color <- sp::merge(limno_point_state, limno_sum_fui, by.x="Hylak_id", by.y="Hylak_id")

# Write spatial point and color observations
st_write(limno_point_state, "in/limno_point_state.shp", append = FALSE)
# sfarrow::st_write_feather(limno_point_color, "in/limno_point_color_co.feather")

# limno_point_color <- sfarrow::st_read_feather("in/limno_point_color_co.feather")
# Clean up environment
rm("limno_points", "limno_month_fui")
gc()

# US Polygons ----

us_water_poly <- st_read("in/USA_Detailed_Water_Bodies.shp")

us_poly_large <- filter(us_water_poly,
                           SQMI >= 10)


poly_state <- inState(us_poly_large, "MI")


poly_state_geom <- us_poly_large %>% 
  filter(OBJECTID %in% poly_state$OBJECTID)

# Simplify for analysis
limno_poly_simple <- st_cast(limno_poly_state, to = "POLYGON", do_split = FALSE)

# Spatial join Hylak_id to us_water_poly > 10 sq_mi
limno_poly_state <- st_join(x = poly_state_geom, y = limno_point_state,
                      join = st_is_within_distance,
                      dist = 50,
                      left = TRUE)
  filter(!is.na(Hylak_id)) %>% 
  select(!c(FTYPE, FCODE, FCODE_DESC, type, distance)) %>% 
  distinct(OBJECTID, .keep_all = TRUE)

# Convert to simple polygons


st_write(limno_poly_simple, "in/limno_poly_simple_state.shp", append = FALSE)

# Clean up environment
rm("us_poly_large", "us_water_poly")
gc()

# Spatial feather write
# sfarrow::st_write_feather(limno_poly_color, "in/limno_poly_color.feather")

st_write(limno_poly_color, "in/limno_poly_color.shp")

# Clear environment
rm(list = ls())
gc()

# ---- MAPPING ----

## Read in data ----

# limno_poly_simple <- st_read("in/limno_poly_simplified.shp")
# limno_poly_sub <- arrange(limno_poly_simple, desc(SQMI)) %>% 
#   head(n = 150)

limno_point_state <- st_read("in/limno_point_state.shp")

limno_sum_fui <- read_feather("in/limno_summer_fui.feather")

limno_sum_fui <- limno_sum_fui %>% 
  distinct(Hylak_id, .keep_all = TRUE)

# Merge points with wavelength and fui
limno_point_color <- sp::merge(limno_point_state, limno_sum_fui, by.x="Hylak_id", by.y="Hylak_id")

# Merge to get fui observations over time ~400k obs.
# limno_poly_color <- sp::merge(limno_poly_sub, limno_sum_fui, by.x="Hylak_id", by.y="Hylak_id")

# Forel-Ule index palette 22 = high, 23 = low, 24 = NA
fui_colors <- c(
  "#1b4796", "#2158bc", "#316dc5", "#327cbb", "#4b80a0", "#568f96", "#6d9298", "#698c86", 
  "#759e72", "#7ba654", "#7dae38", "#94b660", "#94b660", "#a5bc76", "#aab86d", 
  "#adb55f", "#a8a965", "#ae9f5c", "#b3a053", "#af8a44", "#a46905", "#9f4d04",
  "#9f3304")

fui_palette <- colorFactor(palette = fui_colors, levels = c(1:23))

# limno_point_color$fui_value <- as.factor(limno_point_color$fui_value)

# limno_poly_color$fui_value <- as.factor(limno_poly_color$fui_value)

# mapview(limno_poly_color, zcol = "fui_value",
#         layer.name = "Polygon_fui",
#         col.regions = fui_palette(limno_poly_color$fui_value))

# Our basemap (US points)

## Test state points
points_map = mapview(limno_point_color,
        zcol = "fui_value",
        layer.name = "Average Summer Fui",
        col.regions = fui_palette(c(0:23)),
        stroke = FALSE)
points_map

  # To add labels to points
# leafem::addStaticLabels(map = m1, label = limno_point_color$Hylak_id)




