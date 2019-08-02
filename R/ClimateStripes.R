#' @title climateStripes
#' @description Create a plot with annual deviation from mean temperature
#' @details This main function of the package creates a barplot where every
#' colored bar represents the deviation of the annual mean temperature from
#' a 30-years mean. Dark red colors stand for higher mean temperatures and
#' dark blue colors for lower mean temperatures.
#' @aliases warmingstripes
#' @aliases warmingStripes
#' @author Kai Budde
#' @export WarmingStripes
#' @import ggplot2
#' @import dplyr
#' @import lubridate
#' @import RColorBrewer
#' @import utils
#' @import RCurl
#' @param city.name A character
#' @param weather.station.id A character
#' @param startyear.mean A number
#' @param endyear.mean A number
#' @param data A character (can be temperatur, precipitation, sunlight,
#' windspeed)
#' @param style A character

# Created:     06/17/2019
# Last edited: 07/20/2019

ClimateStripes <- function(city.name = NULL,
                           weather.station.id = NULL,
                           startyear.mean = 1961,
                           endyear.mean = 1990,
                           data = "temperature",
                           style = "continuous") {

  # Set warnings off
  oldw <- getOption("warn")
  options(warn = -1)

  website.data <- paste("ftp://ftp-cdc.dwd.de/climate_environment/CDC/",
                        "observations_germany/climate/daily/",
                        "kl/historical/", sep="")

  filename.data <- "data.zip"

  # Get the correct station data ###########################################
  df.weather.stations <- getWeatherStations()

  if(!is.null(weather.station.id)){
    # Check whether ID is correct
    if(!(weather.station.id %in% df.weather.stations$Stations_id)){
      print("Please provide a valid weather station ID.")
      return(0)
    }
  }else if(!is.null(city.name)){
    line.of.station <- grep(city.name, df.weather.stations$Stationsname, ignore.case = TRUE)
    if(length(line.of.station) == 0){
      print("Station name not found")
      return(0)
    }

    weather.station.id <- df.weather.stations$Stations_id[line.of.station]

  }else{

    print("Please provide a city or weather station ID.")
    return(0)
  }

  # Download the data files ################################################

  # Paste the correct name of the file
  # 5 digit number of station
  weather.station.id <- sprintf("%05d", weather.station.id)

  filenames <-  getURL(url = website.data, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  filenames <- strsplit(filenames, "\r\n")
  filenames <-  unlist(filenames)

  filename.website.data <- filenames[
    grepl(pattern = paste("KL_", weather.station.id, sep=""),
          x = filenames)]

  filename.website.data <- paste(website.data, filename.website.data,
                                 sep="")
  # Download zip-container

  download.file(url = filename.website.data, destfile = filename.data,
                method = "auto", quiet = TRUE, mode = "wb",
                cacheOK = TRUE)

  # Open zip-Container and save data in data.frame
  unzip(zipfile = filename.data, exdir = "./unzippedData")

  filename.data <- grep("produkt", list.files("./unzippedData/"), value = TRUE)
  filename.data <- paste("./unzippedData/", filename.data, sep="")

  df.data <- read.csv2(file = filename.data, sep = ";",
                       fileEncoding = "latin1")

  # Work with data supposed to be shown ####################################

  # Yearly mean temperature ###
  df <- select(df.data, STATIONS_ID, MESS_DATUM, TMK)
  plot <- meanTemperaturePlot(df, startyear.mean, endyear.mean, style)

  # Turn warnings on
  options(warn = oldw)

  }