# Setup ----
# Data
library(tidyverse)
library(feather)
# Spatial
library(sf)
library(tigris)
# Visualization
library(leaflet)
library(mapview)

# Functions ----
source("cropState.R")
source("assignFui.R")
source("generateMap.R")

# Datasets ----
limno_points <- st_read("in/HydroLakes_DP.shp")
limno_dp = filter(limno_points, type == "dp") %>% 
  select(Hylak_id) %>% 
  distinct()

limnoWave = read_feather("in/limno_summer_fui.feather")

# Choose state
limno_point_state <- cropState(limno_dp, stateCode = "CO", saveFile = FALSE,
                          fileDir = "data/")

# Assign color
limno_state_color <- assignFui(limno_point_state, limnoWave, saveFile = FALSE,
                               fileDir = "data/")


# Map Points
mapviewOptions(basemaps = c("CartoDB.Positron", "Esri.WorldImagery"))

new_map <- generateMap(limno_state_color, years = 2013)

new_map


inputSF_filter <- limno_state_color %>% 
  filter(year == 2019)


fui_colors <- c(
  "#1b4796", "#2158bc", "#316dc5", "#327cbb", "#4b80a0", "#568f96", "#6d9298", "#698c86", 
  "#759e72", "#7ba654", "#7dae38", "#94b660", "#94b660", "#a5bc76", "#aab86d", 
  "#adb55f", "#a8a965", "#ae9f5c", "#b3a053", "#af8a44", "#a46905", "#9f4d04",
  "#9f3304", "#CCCCCC")

fui_palette <- colorFactor(palette = fui_colors, levels = c(1:24))

inputSF_filter$fui_value <- as.factor(inputSF_filter$fui_value)

map = mapview(inputSF_filter,
              zcol = "fui_value",
              layer.name = "Fui",
              col.regions = fui_palette(c(0:23)),
              stroke = FALSE)
map


# Example timeseries plot of Horsetooth
inputSF_tooth <- limno_state_color %>% 
  filter(Hylak_id == 112043)

min_fui <- min(inputSF_tooth$fui_value, na.rm = TRUE)
max_fui <- max(inputSF_tooth$fui_value, na.rm = TRUE)

ggplot(inputSF_tooth, aes(x = year, y = fui_value, fill = fui_value)) + 
  geom_col() + 
  scale_fill_gradientn(colours = fui_colors[min_fui:max_fui]) +
  labs(title = "Horsetooth Forel-Ule Index",
       x = "Year",
       y = "Fui_value",
       fill = "Value")
