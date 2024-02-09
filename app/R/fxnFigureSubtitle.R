#' `fxnFigureSubtitle.R` - Build subtitle for figure based on user input
#' 
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `figureSubtitle` - Subtitle for figure based on selected dates


fxnFigureSubtitle <- function(startDate, endDate) {
  figureSubtitle <- 
    htmltools::p(
      htmltools::HTML(
        paste(
          "Values are accumulations of daily heat units from ", 
          gsub(" 0", " ", format(startDate, "%B %d, %Y")), 
          "through", 
          gsub(" 0", " ", format(endDate, "%B %d, %Y")), 
          sep = " "
        )
      ), 
      
      class = "figure-subtitle"
    )
  
  return(figureSubtitle)
}
