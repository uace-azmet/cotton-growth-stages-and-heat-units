#' `fxnFigureSubtitle.R` - Build subtitle for figure based on user input
#' 
#' @param plantingDate - Planting date of period of interest
#' @param endDate - End date of period of interest
#' @return `figureSubtitle` - Subtitle for figure based on selected dates


fxnFigureSubtitle <- function(plantingDate, endDate) {
  figureSubtitle <- 
    htmltools::p(
      htmltools::HTML(
        paste(
          #"Estimates based on the accumulation of heat units (86/55 °F) since ", 
          #gsub(" 0", " ", format(plantingDate, "%B %d, %Y")), 
          #"and through", 
          #gsub(" 0", " ", format(endDate, "%B %d, %Y")), 
          "Values are accumulations of daily heat units from ", 
          gsub(" 0", " ", format(plantingDate, "%B %d, %Y")), 
          "through", 
          gsub(" 0", " ", format(endDate, "%B %d, %Y")), 
          sep = " "
        )
      ), 
      
      class = "figure-subtitle"
    )
  
  return(figureSubtitle)
}
