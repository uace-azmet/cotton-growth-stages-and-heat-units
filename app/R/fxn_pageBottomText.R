#' `fxn_pageBottomText.R` - Build supporting text for page
#' 
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `pageBottomText` - Supporting text for page


fxn_pageBottomText <- function(azmetStation, startDate, endDate) {
  
  
  # Define inputs -----
  
  apiURL <- 
    a(
      "api.azmet.arizona.edu", 
      href="https://api.azmet.arizona.edu/v1/observations/daily", # Daily data
      target="_blank"
    )
  
  azmetrURL <- 
    a(
      "azmetr", 
      href="https://uace-azmet.github.io/azmetr/",
      target="_blank"
    )
  
  bulletinURL <- 
    a(
      "AZ1602 'Heat Units'",
      href="https://extension.arizona.edu/sites/extension.arizona.edu/files/pubs/az1602.pdf",
      target="_blank"
    )
  
  heatUnitsWebpageURL <- 
    a(
      "heat units",
      href="https://azmet.arizona.edu/application-areas/heat-units",
      target="_blank"
    )
  
  todayDate <- gsub(" 0", " ", format(lubridate::today(), "%B %d, %Y"))
  
  todayYear <- lubridate::year(lubridate::today())
  
  webpageAZMet <- 
    a(
      "AZMet website", 
      href="https://azmet.arizona.edu/", 
      target="_blank"
    )
  
  webpageCode <- 
    a(
      "GitHub page", 
      href="https://github.com/uace-azmet/cotton-growth-stages-and-heat-units", 
      target="_blank"
    )
  
  webpageDataVariables <- 
    a(
      "data variables", 
      href="https://azmet.arizona.edu/about/data-variables", 
      target="_blank"
    )
  
  webpageNetworkMap <-
    a(
      "station locations", 
      href="https://azmet.arizona.edu/about/network-map", 
      target="_blank"
    )
  
  webpageStationMetadata <- 
    a(
      "station metadata", 
      href="https://azmet.arizona.edu/about/station-metadata", 
      target="_blank"
    )
  
  
  # Build text -----
  
  pageBottomText <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "Tables of daily totals of heat units for individual stations and the current calendar year are available from the AZMet webpage with additional ", heatUnitsWebpageURL, " resources. More information on single sine curve method, as well as the relationship between heat units and cotton growth stages, is in Extension bulletin ", bulletinURL, ".",  
          br(), br(), 
          "Daily AZMet data are from ", apiURL, " and accessed using the ", azmetrURL, " R package. Values from recent dates may be based on provisional data. More information about ", webpageDataVariables, ", ", webpageNetworkMap, ", and ", webpageStationMetadata, " is available on the ", webpageAZMet, ". Users of AZMet data and related information assume all risks of its use.",
          br(), br(),
          "To cite the above AZMet data, please use: 'Arizona Meteorological Network (", todayYear, ") Arizona Meteorological Network (AZMet) Data. https://azmet.arizona.edu. Accessed ", todayDate, "', along with 'Arizona Meteorological Network (", todayYear, ") Cotton Growth Stages and Heat Units. https://viz.datascience.arizona.edu/azmet/cotton-growth-stages-and-heat-units. Accessed ", todayDate, "'.",
          br(), br(),
          "For information on how this webpage is put together, please visit the ", webpageCode, " for this tool."
        )
      ),
      
      class = "page-bottom-text"
    )
  
  return(pageBottomText)
}
