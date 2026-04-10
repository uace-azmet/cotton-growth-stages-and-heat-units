#' `fxn_barChartTitle.R` - Build title for bar chart
#' 
#' @param azmetStation - AZMet station selection by user
#' @return `barChartTitle` - Title for bar chart based on selected station


fxn_barChartTitle <- function(azmetStation) {
 
   barChartTitle <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          bsicons::bs_icon("graph-up", class = "bolder-icon"), 
          htmltools::HTML("&nbsp;&nbsp;"),
          toupper(
            htmltools::HTML(
              paste0(
                "<strong>Cumulative Heat Units at the AZMet ", azmetStation, " Station</strong>"
              )
            )
          ),
          htmltools::HTML("&nbsp;"),
          bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "Hover over bars for values of total heat units. Click or tap on the 'Expand' button to the lower right of the chart to increase the viewing area.",
            id = "infoBarChartTitle",
            placement = "right"
          )
        )
      ),
      
      class = "bar-chart-title"
    )
  
  return(barChartTitle)
}
