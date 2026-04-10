#' `fxn_barChartCaption.R` - Build caption for bar chart based on user input
#' 
#' @param azmetStation AZMet station selection by user
#' @param inData - Data table [[2]] from `fxn_totalHeatUnits.R`
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `barChartCaption` Caption for bar chart based on selected station


fxn_barChartCaption <- function(azmetStation, inData, startDate, endDate) {
  
  azmetStationStartDate <- 
    dplyr::filter(azmetStationMetadata, meta_station_name == azmetStation) %>% 
    dplyr::pull(start_date)
  
  if (nrow(inData) == 1) {
    standardText <- 
      paste0(
        "Heat unit accumulation (black bar in graph) is based on the sum of daily totals during the period of interest and as estimated by the single sine curve method with upper and lower temperature thresholds of 86 and 55 °F, respectively. Data for the ", azmetStation, " station in the new AZMet database currently go back to ", gsub(" 0", " ", format(azmetStationStartDate, "%B %d, %Y")), "."
      )
  } else {
    standardText <- 
      paste0(
        "Heat unit accumulation for the current year (black bar in graph) is based on the sum of daily totals during the period of interest and as estimated by the single sine curve method with upper and lower temperature thresholds of 86 and 55 °F, respectively. Totals for past years (gray bars in graph) are based on the same start and end month and day, but during those respective years. Data for the ", azmetStation, " station in the new AZMet database currently go back to ", gsub(" 0", " ", format(azmetStationStartDate, "%B %d, %Y")), "."
      )
  }
  
  # Account for multi-month absence of YUG data in 2021
  nonOperational <- 0
  
  if (azmetStation == "Yuma N.Gila") {
    while (startDate >= azmetStationStartDate) {
      userDateRange <- lubridate::interval(start = startDate, end = endDate)
      
      if (lubridate::int_overlaps(int1 = yugNodataInterval, int2 = userDateRange) == TRUE) {
        nonOperational <- 1
      }
      
      startDate <- min(seq(startDate, length = 2, by = "-1 year"))
      endDate <- min(seq(endDate, length = 2, by = "-1 year"))
    }
  }
  
  # Generate figure footer based on presence/absence of non-operational dates
  if (azmetStation == "Yuma N.Gila" & nonOperational == 1) {
    barChartCaption <- 
      htmltools::p(
        htmltools::HTML(
          paste(
            standardText,
            "However, we do not show total evapotranspiration for the year with a month-day period that overlaps the period from June 16, 2021 through October 21, 2021, when the ", azmetStation, " station was not in operation.",
            sep = " "
          )
        ),
        
        class = "bar-chart-caption"
      )
  } else {
    barChartCaption <- 
      htmltools::p(
        htmltools::HTML(
          paste(standardText, sep = " ")
        ), 
        class = "bar-chart-caption"
      )
  }
  
  return(barChartCaption)
}
