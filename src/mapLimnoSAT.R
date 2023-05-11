# Get functional limnosat data
# Devin Hunt, March 7, 2023

library(stringr)
library(tidyverse)
library(feather)
library(sfarrow)

library(sf)
library(USAboundaries)

library(mapview) # FlatFGB (geobuff)
library(leaflet)
library(leafpop)
library(leafgl)
library(leaflet.extras2)

## The grand solution!!
mapviewOptions(platform = "leafgl", fgb = NULL)


# Simon's graph tutorial for showing change in color over time
# download.file("https://raw.githubusercontent.com/SimonTopp/Walkthroughs/main/LimnoSat_US_Tutorial.html", "tutorial.html")

## Workflow
# Read data
# --Data lacking spatial.
# --Grab points of the deepest of all limnosat lakes
# ---Bind w/ limnosat corrected

# -Follow Simon's guide to get hex fui from wavelength


limno_wave <- read_feather("in/srCorrected_Hydrolakes.feather")
limno_points <- st_read("in/HydroLakes_DP.shp")

us_water_poly <- st_read("in/USA_Detailed_Water_Bodies.shp")

# mapview(us_water_poly_sub)

us_water_poly_sub <- us_water_poly %>% 
  filter(grepl("Laurel River", NAME))

dp_shp <- limno_points %>% filter(type == 'dp')

sf_use_s2(FALSE)

limno_centroid <- limno_points %>% 
  filter(type == "centroid")


# All Matching polygons
limno_poly <- st_join(x = us_water_poly, y = limno_centroid,
                      join = st_intersects,
                      left = TRUE)


# limno_poly_subset <- limno_poly %>% 
#   filter(!is.na(Hylak_id),
#          SQMI >= 100)
# 
# 
# laurel_poly <- limno_poly %>% 
#   filter(grepl("Laurel River", NAME))


## Check function
# laurel_wave <- color_df %>% 
#   filter(Hylak_id == "9292")

laurel_wave <- limno_wave %>% 
  filter(Hylak_id == "9292")

laurel_color <- dWL_to_hex(laurel_wave)


ggplot(laurel_color, aes(x = dWL, y = nrow(laurel_color), fill = hex_color)) + 
  geom_col() +
  scale_fill_gradient(laurel_color$hex_color) +
  labs(x = 'Wavelength (nm)', title = 'Overall Color Distribution') +
  theme_bw() +
  theme(legend.position = 'none')


dWL_to_hex <- function(input_df) {
  
  # Conversion method used from Simon Topp. Some expressions added for min / max (<475,>583)
  input_df$fui_value <- case_when(
    input_df$dWL <= 470 ~ 23,
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
    TRUE ~ 24)
  
  fui_colors <- c(
    "#2158bc", "#316dc5", "#327cbb", "#4b80a0", "#568f96", "#6d9298", "#698c86", 
    "#759e72", "#7ba654", "#7dae38", "#94b660", "#94b660", "#a5bc76", "#aab86d", 
    "#adb55f", "#a8a965", "#ae9f5c", "#b3a053", "#af8a44", "#a46905", "#9f4d04",
    "#9f3304", "#1b4796", "#CCCCCC")
  
  
  # input_df <- input_df %>% 
  #   mutate(rgb = rgb2hex(hex_color, alpha = FALSE))
  
  return(input_df)
}

limno_wave_unique <- limno_wave %>% 
  distinct(Hylak_id, .keep_all = TRUE)

color_df <- dWL_to_hex(limno_wave_unique)

fui_palette <- c(
  "#2158bc", "#316dc5", "#327cbb", "#4b80a0", "#568f96", "#6d9298", "#698c86", 
  "#759e72", "#7ba654", "#7dae38", "#94b660", "#94b660", "#a5bc76", "#aab86d", 
  "#adb55f", "#a8a965", "#ae9f5c", "#b3a053", "#af8a44", "#a46905", "#9f4d04",
  "#9f3304", "#1b4796", "#CCCCCC"
  )

fui_mapview <- colorFactor(palette = fui_palette, levels = 1:24,
                           na.color = "#CCCCCC")

mapview(laurel_poly,
        col.regions = fui_mapview(laurel_poly$fui_value))

limno_poly_color <- sp::merge(dp_shp, color_df,
                              by.x="Hylak_id", by.y="Hylak_id")

## Test sfarrow feather function
sfarrow::st_write_feather(limno_poly_color, "in/limno_fui_final.feather")


# ---- Work From Here ----
limno_poly_color <- sfarrow::st_read_feather("in/limno_fui_final.feather")

limno_unique <- limno_poly_color %>% 
  distinct(Hylak_id, .keep_all = TRUE)

remove(limno_poly_color)

lakes <- mapview(limno_poly_color, zcol = "fui_value",
        col.regions = fui_mapview(limno_poly_color$fui_value),
        stroke = FALSE,
        weight = 0)

lakes

addPlayback(lakes, data = limno_poly_color)

st_write_feather()




