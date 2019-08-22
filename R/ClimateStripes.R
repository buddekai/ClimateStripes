#' @title climateStripes
#' @description Create a plot with annual deviation from mean temperature
#' @details This main function of the package creates a barplot where every
#' colored bar represents the deviation of the annual mean temperature from
#' a 30-years mean. Dark red colors stand for higher mean temperatures and
#' dark blue colors for lower mean temperatures.
#' @aliases warmingstripes
#' @aliases warmingStripes
#' @author Kai Budde
#' @export ClimateStripes
#' @import RCurl
#' @param city.name A character
#' @param weather.station.id A character
#' @param startyear.mean A number
#' @param endyear.mean A number
#' @param plot.what A character (can be warmingstripes, precipitation,
#' sunlight, windspeed)
#' @param style A character (could be for
#' plot.what == temperature: continuous or ...)

# Created:     06/17/2019
# Last edited: 08/22/2019

ClimateStripes <- function(city.name = NULL,
                           weather.station.id = NULL,
                           startyear.mean = 1961,
                           endyear.mean = 1990,
                           plot.what = "all",
                           style = "continuous") {

  # Set warnings off
  oldw <- getOption("warn")
  options(warn = -1)

  old.options <- getOption("stringsAsFactors")
  options(stringsAsFactors = FALSE)

  #climate.data <- paste("ftp://ftp-cdc.dwd.de/climate_environment/CDC/",
  #                      "observations_germany/climate/daily/",
  #                      "kl/historical/", sep="")

  json.file <- "./inst/links.json"
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
    line.of.station <- grep(city.name, df.weather.stations$Stationsname,
                            ignore.case = TRUE)
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

  # Get the link to the climate data

  json.file <- fromJSON(file = json.file)
  climate.data <- json.file$DWDdata

  # Get list with all zip-containers on the website and finde the one needed
  # for the specific station ID
  filenames <- getURL(url = climate.data, ftp.use.epsv = FALSE,
                      dirlistonly = TRUE)
  filenames <- strsplit(filenames, "\r\n")
  filenames <- unlist(filenames)

  filename.climate.data <- filenames[
    grepl(pattern = paste("KL_", weather.station.id, sep=""),
          x = filenames)]

  filename.climate.data <- paste(climate.data, filename.climate.data,
                                 sep="")
  # Download zip-container
  download.file(url = filename.climate.data, destfile = filename.data,
                method = "auto", quiet = TRUE, mode = "wb",
                cacheOK = TRUE)

  # Open zip-Container and save data in data.frame
  unzip(zipfile = filename.data, exdir = "./unzippedData")

  # Remove zip file
  invisible(file.remove(filename.data))

  # Read in unzipped data
  filename.data <- grep("produkt", list.files("./unzippedData/"),
                        value = TRUE)
  filename.data <- paste("./unzippedData/", filename.data, sep="")

  df.data <- read.csv2(file = filename.data, sep = ";",
                       fileEncoding = "latin1")

  # Remove unzipped files
  invisible(unlink(x = "unzippedData", recursive = TRUE))

  # Work with data supposed to be shown ####################################

  plot.what <- tolower(plot.what)

  # Yearly mean temperature ###
  if( plot.what == "all" | plot.what == "warmingstripes"){
    plot <- plotWarmingStripes(df.data, startyear.mean, endyear.mean, style)
  }else{
    print(paste("Nothing plotted.Please enter a suitable value for ",
                "the parameter plot.what.", sep = ""))
  }

  # Reset options ##########################################################
  # Turn warnings on
  options(warn = oldw)
  options(stringsAsFactors = old.options)

}
