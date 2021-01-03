#' @title climateStripes
#' @description Creates a climate plots that show annual deviation from a mean value
#' @details This main function of the package fetches the data and calls the
#' required plotting functions.
#' @aliases ClimateStripes
#' @aliases climatestripes
#' @author Kai Budde
#' @export climateStripes
#' @import RCurl
#' @param city.name A character
#' @param weather.station.id A character
#' @param startyear.mean A number
#' @param endyear.mean A number
#' @param plot.what A character (can be warmingstripes, precipitation,
#' sunlight, windspeed)
#' @param style A character (could be for
#' plot.what == temperature: continuous or ...)

# Created:     2019/06/17
# Last edited: 2020/01/03

climateStripes <- function(city.name = NULL,
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

  json.file <- system.file("links.json", package = "climateStripes")
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
      print("Station name not found. Check one of the displayed.")
      View(df.weather.stations)
      return(0)
    }

    weather.station.id <- df.weather.stations$Stations_id[line.of.station]
    station.name <- df.weather.stations$Stationsname[line.of.station]

  }else{

    print("Please provide a city or weather station ID.")
    return(0)
  }

  # Download the data files and merge data tables ##########################

  # Paste the correct name of the file
  # 5 digit number of station
  weather.station.id <- sprintf("%05d", weather.station.id)

  # Get the link to the climate data
  json.file <- fromJSON(file = json.file)
  link.to.historical.climate.data <- json.file$DWDdata
  link.to.recent.climate.data <- json.file$DWDdata_recent


  # Historical Data
  df.historical.data <- getWeatherData(
    link.to.data = link.to.historical.climate.data,
    weather.station.id = weather.station.id,
    filename.data = filename.data)

  # Recent Data
  df.recent.data <- getWeatherData(
    link.to.data = link.to.recent.climate.data,
    weather.station.id = weather.station.id,
    filename.data = filename.data)

  # Add Recent Data to Historical Data (only if colnames are the same)
  if(all.equal(colnames(df.historical.data), colnames(df.recent.data)) ==
     TRUE){
    df.data <- rbind(df.historical.data,
                     df.recent.data[
                       !(df.recent.data$MESS_DATUM %in% df.historical.data$MESS_DATUM),])

  }else{
    print("Not all column names of historical and recent data are equal.")
  }


  # Plot data supposed to be shown #########################################

  plot.what <- tolower(plot.what)
  nothingplotted <- TRUE

  # Yearly mean temperature ###
  if( plot.what == "all" | plot.what == "warmingstripes"){
    plot <- plotWarmingStripes(df.data, startyear.mean, endyear.mean,
                               style, station.name)
    nothingplotted <- FALSE
  }

  # Yearly precipitation ###
  if(plot.what == "all" | plot.what == "precipitationstripes"){
    plot <- plotPrecipitationStripes(df.data, startyear.mean, endyear.mean,
                                     style, station.name)
    nothingplotted <- FALSE
    # Nothing to plot ###
  }

  if(nothingplotted){
    print(paste("Nothing plotted.Please enter a suitable value for ",
                "the parameter plot.what.", sep = ""))
    }

  if(!nothingplotted){
    print(paste("Plot saved in ", getwd(), ".", sep=""))
  }

  # Reset options ##########################################################
  # Turn warnings on
  options(warn = oldw)
  options(stringsAsFactors = old.options)

}
