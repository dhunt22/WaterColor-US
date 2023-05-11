# April 27, 2023 | Devin Hunt

#' @param inputSF sf object to be assigned color
#' @param limnoWave dataset of fui values and Hylak_id
#' @param saveFile save the sf dataframe? TRUE or FALSE
#' @param fileDir directory for file to be saved within project ex. "data/out"
#' @output sf dataframe of limnosat objects with color assigned

assignFui <- function(inputSF, limnoWave, saveFile = TRUE,
                      fileDir = "data/"){
  
  inputDWL <- sp::merge(inputSF, limnoWave, by.x="Hylak_id", by.y="Hylak_id")
  
  # Conversion method used from Simon Topp. Some expressions added for min / max (<475,>583)
  inputDWL$fui_value <- case_when(
    inputDWL$dWL <= 470 ~ 0,
    inputDWL$dWL <= 475 & inputDWL$dWL > 470 ~ 1,
    inputDWL$dWL <= 480 ~ 2,
    inputDWL$dWL <= 485 ~ 3,
    inputDWL$dWL <= 489 ~ 4,
    inputDWL$dWL <= 495 ~ 5,
    inputDWL$dWL <= 509 ~ 6,
    inputDWL$dWL <= 530 ~ 7,
    inputDWL$dWL <= 549 ~ 8,
    inputDWL$dWL <= 559 ~ 9,
    inputDWL$dWL <= 564 ~ 10,
    inputDWL$dWL <= 567 ~ 11,
    inputDWL$dWL <= 568 ~ 12,
    inputDWL$dWL <= 569 ~ 13,
    inputDWL$dWL <= 570 ~ 14,
    inputDWL$dWL <= 571 ~ 15,
    inputDWL$dWL <= 573 ~ 16,
    inputDWL$dWL <= 575 ~ 17,
    inputDWL$dWL <= 577 ~ 18,
    inputDWL$dWL <= 579 ~ 19,
    inputDWL$dWL <= 581 ~ 20,
    inputDWL$dWL <= 583 ~ 21,
    inputDWL$dWL > 583 ~ 22,
    TRUE ~ 23)
  
  return(inputDWL)
  
}
