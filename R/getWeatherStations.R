#' @title getWeatherStations
#' @description Downloads the information about the weather stations of DWD
#' @details This function downloads information about the weather stations
#' of Deutscher Wetterdienst and prints the information onto the screen.
#' @aliases getweatherstations
#' @aliases getWeatherstations
#' @aliases getweatherStations
#' @author Kai Budde
#' @export getWeatherStations

# Created:     06/17/2019
# Last edited: 06/21/2019

getWeatherStations <- function(){

  old.options <- getOption("stringsAsFactors")
  options(stringsAsFactors = FALSE)

  # Definitions
  website.stations <- paste("ftp://ftp-cdc.dwd.de/pub/CDC/",
                            "observations_germany/climate/daily/",
                            "kl/historical/",
                            "KL_Tageswerte_Beschreibung_Stationen.txt",
                            sep="")

  filename.stations <- "DWDstations.txt"
  column.widths <- c(5, 9, 9, 15, 12, 10, 41, 97)

  # Download file
  download.file(url = website.stations, destfile = filename.stations, method = "auto", quiet = TRUE, mode = "w",
                cacheOK = TRUE)

  # Create Data Frame with information
  df <- read.fwf(file = filename.stations, widths = column.widths, skip = 2, fileEncoding = "latin1")

  header <- readLines(con = file(filename.stations), n = 1)
  header <- strsplit(header, split = " ")


  # Remove downloaded file
  invisible(file.remove(filename.stations))

  # Reset options
  options(old.options)
}
