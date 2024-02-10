#' `fxnFigureSubtitle.R` - Build subtitle for figure based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @return `figureSubtitle` - Subtitle for figure based on selected AZMet station


fxnFigureSubtitle <- function(azmetStation) {
  figureSubtitle <- 
    htmltools::p(
      htmltools::HTML(
        paste(
          "AZMet", azmetStation, "Station", 
          sep = " "
        ),
        #paste(
        #  "Values above bars are accumulations of daily heat units from ", 
        #  gsub(" 0", " ", format(startDate, "%B %d, %Y")), 
        #  "through", 
        #  gsub(" 0", " ", format(endDate, "%B %d, %Y")), 
        #  sep = " "
        #)
      ), 
      
      class = "figure-subtitle"
    )
  
  return(figureSubtitle)
}
