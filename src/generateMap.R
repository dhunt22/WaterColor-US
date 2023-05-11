generateMap <- function(inputSF, years = 2000){
  
  inputSF_filter <- inputSF %>% 
    filter(year == years)
  
  
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
  
  return(map)

}
