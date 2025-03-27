#' `fxn_figureHelpText.R` - Build help text for figure
#' 
#' @return `slsGraphHelpText` - Help text for figure


fxn_figureHelpText <- function() {
  figureHelpText <- 
    htmltools::p(
      htmltools::HTML(
        "Hover over bars for values of cumulative heat units. Select from the icons to the right of the graph for additional functionality."
      ), 
      
      class = "figure-help-text"
    )
  
  return(figureHelpText)
}