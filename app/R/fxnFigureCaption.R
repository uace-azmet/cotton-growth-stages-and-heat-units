#' `fxnfigureCaption.R` - Build caption for figure based on user input
#' 
#' @param azmetStation AZMet station selection by user
#' @param inData - data table of seasonal heat accumulation values by year
#' @param startDate - Planting date of period of interest
#' @param endDate - End date of period of interest
#' @return `figureCaption` Caption for figure based on selected station


fxnFigureCaption <- function(azmetStation, inData, startDate, endDate) {
  
  # Build caption text
  figureCaption <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "Heat accumulation at the AZMet ", azmetStation, " station indicates ",
          
          "of ", doyValue, " °F (black point) on ", gsub(" 0", " ", format(as.Date(max(inData$datetime)), "%B %d, %Y")), " at the AZMet ", azmetStation, " station indicates ", doyLevel, ". The maximum, average, and minimum of estimated canopy temperatures on ", format(as.Date(max(inData$datetime)), "%B"), " ", format(as.Date(max(inData$datetime)), "%d"), " (vertical dotted line) are ", doyMaxValue, ", ", doyAverageValue, ", and ", doyMinValue, " °F, respectively, based on data from ", doyMinYear, " through ", doyMaxYear, " (black and gray points)."
        ),
      ),
      
      class = "figure-caption"
    )
  
  return(figureCaption)
}
