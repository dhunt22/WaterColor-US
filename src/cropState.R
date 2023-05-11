# April 27, 2023 | Devin Hunt

#' @param inputSF sf object to be clipped to state boundary
#' @param stateCode A two-character state for tigris input, ex. "CO"
#' @param saveFile save the sf dataframe? TRUE or FALSE
#' @param fileDir directory for file to be saved within project ex. "data/out"
#' @output sf dataframe of limnosat points within the state boundary

cropState <- function(inputSF, stateCode = "CO", saveFile = TRUE,
                           fileDir = "data/"){
  
  # Read in state boundary from tigris
  state_boundary <- tigris::states() %>% 
    filter(STUSPS == stateCode) %>% 
    select(STUSPS) %>% 
    st_transform(., 4326)
  
  # Correct crs
  sf_84 <- st_set_crs(inputSF, st_crs(state_boundary))
  
  out_state <- st_filter(sf_84, state_boundary)
  
  if (saveFile){
    outDir = paste0(fileDir,"limno_point_state")
    st_write(out_state, outDir, append = FALSE)
  }
  
  return(out_state)
}