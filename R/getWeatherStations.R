#' @title getWeatherStations
#' @description Downloads information about all weather stations of DWD
#' @details This function downloads information about all weather stations
#' of Deutscher Wetterdienst (DWD) and saves the information in a data frame
#' @aliases getweatherstations
#' @aliases getWeatherstations
#' @aliases getweatherStations
#' @author Kai Budde
#' @export getWeatherStations
#' @import rjson
#' @import utils

# Created:     2019/06/21
# Last edited: 2022/01/01

getWeatherStations <- function(){

  old.options <- getOption("stringsAsFactors")
  options(stringsAsFactors = FALSE)

  # Definitions ############################################################
  # Current url with climate data is written in "./inst/links.json"

  json.file <- system.file("links.json", package = "climateStripes")
  json.file <- fromJSON(file = json.file)

  website.stations <- json.file$DWDstations

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
