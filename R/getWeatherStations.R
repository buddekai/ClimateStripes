#' @title getWeatherStations
#' @description Downloads the information about the weather stations of DWD
#' @details This function downloads information about the weather stations
#' of Deutscher Wetterdienst and prints the information onto the screen.
#' @aliases getweatherstations
#' @aliases getWeatherstations
#' @aliases getweatherStations
#' @author Kai Budde
#' @export getWeatherStations

# Created:     06/21/2019
# Last edited: 06/23/2019

getWeatherStations <- function(){

  old.options <- getOption("stringsAsFactors")
  options(stringsAsFactors = FALSE)

  # Definitions
  #website.stations <- paste("ftp://ftp-cdc.dwd.de/pub/CDC/",
  #                          "observations_germany/climate/daily/",
  #                          "kl/historical/",
  #                          "KL_Tageswerte_Beschreibung_Stationen.txt",
  #                          sep="")
  website.stations <- paste("ftp://ftp-cdc.dwd.de/climate_environment/",
                            "CDC/observations_germany/climate/daily/kl/",
                            "historical/",
                            "KL_Tageswerte_Beschreibung_Stationen.txt",
                            sep="")

  filename.stations <- "DWDstations.txt"
  column.widths <- c(5, 9, 9, 15, 12, 10, 41, 97)

  # Download file
  download.file(url = website.stations, destfile = filename.stations,
                method = "auto", quiet = TRUE, mode = "w",
                cacheOK = TRUE)

  # Create Data Frame with information
  df.weather.stations <- read.fwf(
    file = filename.stations,
    widths = column.widths, skip = 2, fileEncoding = "latin1")

  # Get header and insert as column names
  header <- readLines(con = file(filename.stations), n = 1)
  header <- unlist(strsplit(header, split = " "))
  names(df.weather.stations) <- header

  # Delete empty spaces at the beginning/end of the field
  df.weather.stations[[7]] <- gsub("^ +", "", df.weather.stations[[7]])
  df.weather.stations[[7]] <- gsub(" +$", "", df.weather.stations[[7]])

  df.weather.stations[[8]] <- gsub("^ +", "", df.weather.stations[[8]])
  df.weather.stations[[8]] <- gsub(" +$", "", df.weather.stations[[8]])

  # Remove downloaded file
  invisible(file.remove(filename.stations))

  # Reset options
  options(stringsAsFactors = old.options)

  return(df.weather.stations)
}
