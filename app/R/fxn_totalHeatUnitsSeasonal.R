#' `fxn_totalHeatUnitsSeasonal` calculates seasonal heat unit accumulation for an individual year
#' 
#' @param azmetStation - AZMet station selection by user
#' @param inData - Derived data table of daily values from `fxn_totalHeatUnits.R`
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param userDateRange - Date interval based on `startDate` and `endDate`
#' @return `totalHeatUnitsSeasonal` - Data table with total heat units for a single season of an individual year


fxn_totalHeatUnitsSeasonal <- function(azmetStation, inData, startDate, endDate, userDateRange) {
  
  totalHeatUnitsSeasonal <- inData %>%
    dplyr::group_by(meta_station_name) %>%
    dplyr::summarize(total_heat_units_seasonal = sum(heat_units_55F, na.rm = TRUE)) %>%
    dplyr::mutate(
      total_heat_units_seasonal_label = 
        format(round(total_heat_units_seasonal, digits = 1), nsmall = 1)
    ) %>% 
    dplyr::mutate(end_date_year = lubridate::year(endDate)) %>%
    dplyr::mutate(
      date_year_label = 
        dplyr::if_else(
          condition = lubridate::year(startDate) == lubridate::year(endDate),
          true = as.character(lubridate::year(startDate)),
          false = paste(lubridate::year(startDate), lubridate::year(endDate), sep = "-")
        )
    )
  
  if (azmetStation == "Yuma N.Gila" & lubridate::int_overlaps(int1 = yugNodataInterval, int2 = userDateRange) == TRUE) {
    totalHeatUnitsSeasonal$total_heat_units_seasonal <- NA_real_
    totalHeatUnitsSeasonal$total_heat_units_seasonal_label <- "NA"
  }
  
  return(totalHeatUnitsSeasonal)
}
