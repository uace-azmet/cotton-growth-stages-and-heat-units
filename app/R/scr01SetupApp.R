# Load auxiliary files
stationNames <- vroom::vroom(
  file = "aux-files/azmet-stations-api-db.csv", 
  delim = ",", 
  col_names = TRUE, 
  show_col_types = FALSE
)

# Set auxiliary variables
if (Sys.Date() < as.Date(paste0(lubridate::year(Sys.Date()), "-02-02"))) {
  initialPlantingDate <- as.Date(paste0((lubridate::year(Sys.Date()) - 1), "-02-01"))
} else {
  initialPlantingDate <- as.Date(paste0(lubridate::year(Sys.Date()), "-02-01"))
}

if (Sys.Date() < as.Date(paste0(lubridate::year(Sys.Date()), "-02-02"))) {
  initialEndDate <- as.Date(paste0((lubridate::year(Sys.Date()) - 1), "-11-15"))
} else {
  initialEndDate <- (Sys.Date() - 1)
}
