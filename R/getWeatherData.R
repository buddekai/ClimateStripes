#' @title getWeatherData
#' @description Downloads weather data of specific DWD waeather station
#' @details This function downloads all weather data of a specific weather
#' station from the servers of the Deutscher Wetterdienst (DWD)
#' @aliases getweatherdata
#' @aliases getWeatherdata
#' @aliases getweatherData
#' @author Kai Budde
#' @export getWeatherData
#' @param link.to.data A character (ftp link to weather data)
#' @param weather.station.id A character (5 digit station number)
#' @param filename.data A character (file name that contains the data)

# Created:     2021/01/03
# Last edited: 2021/01/03

getWeatherData <- function(link.to.data = NULL,
                           weather.station.id = NULL,
                           filename.data = NULL){

  # Check for NULLs in parameter values
  if(is.null(link.to.data)){
    paste("Please provide a link to DWD data.")
    return(0)
  }
  if(is.null(weather.station.id)){
    paste("Please provide a stations ID.")
    return(0)
  }
  if(is.null(filename.data)){
    paste("Please provide a file name of the zip container.")
    return(0)
  }

  # Get list with all zip-containers on the website and find the one needed
  # for the specific station ID
  filenames <- RCurl::getURL(url = link.to.data, ftp.use.epsv = FALSE,
                      dirlistonly = TRUE)
  filenames <- gsub(pattern = "\r", replacement = "", filenames)
  filenames <- strsplit(filenames, "\n")
  filenames <- unlist(filenames)

  filename.climate.data <- filenames[
    grepl(pattern = paste("KL_", weather.station.id, sep=""),
          x = filenames)]

  filename.climate.data <- paste(link.to.data, filename.climate.data,
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


  return(df.data)
}
