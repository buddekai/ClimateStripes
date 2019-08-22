#' @title plotWarmingStripes
#' @description Plot the mean temperature data
#' @details This function takes a data frame with daily temperature data for
#' given station, calculates annual mean and plots it.
#' @aliases plotwarmingstripes
#' @aliases plotWarmingstripes
#' @aliases plotwarmingStripes
#' @author Kai Budde
#' @export plotWarmingStripes
#' @import lubridate
#' @import dplyr
#' @import ggplot2
#' @import RColorBrewer
#' @param df.temp A data frame
#' @param staryear.mean A number
#' @param endyear.mean A number
#' @param style A character

# Created:     07/20/2019
# Last edited: 08/22/2019

plotWarmingStripes <- function(df.temp, startyear.mean, endyear.mean,
                               style = "continuous"){

  # Calculate annual temperature
  df.temp$TMK <- as.numeric(df.temp$TMK)
  df.temp$date <- as.character(df.temp$MESS_DATUM)
  df.temp$date <-  as.Date(df.temp$date, "%Y%m%d")
  df.temp$year <-  year(df.temp$date)
  df.temp.annual <- group_by(df.temp, year) %>% summarise(mean = mean(TMK))

  # Delete current year
  current.year <- year(Sys.time())
  df.temp.annual <- df.temp.annual[!(df.temp.annual$year == current.year),]

  # Calculate annual mean from start year (should be 1961) until
  # end year (should be 1990)
  mean.from.start.to.endyear <- mean(df.temp.annual$mean[df.temp.annual$year>= startyear.mean & df.temp.annual$year<= endyear.mean])

  df.temp.annual$deviations <- df.temp.annual$mean - mean.from.start.to.endyear

  # Year with highest negative and positive deviation from mean
  year.highest.negative.deviation <- df.temp.annual$year[
    which(df.temp.annual$deviations == min(df.temp.annual$deviations))]
  year.highest.positive.deviation <- df.temp.annual$year[
    which(df.temp.annual$deviations == max(df.temp.annual$deviations))]


  print(paste("Temperature mean from ",
              startyear.mean,
              " until ",
              endyear.mean,
              ": ",
              round(mean.from.start.to.endyear, digits = 1),
              "°C.",
              sep=""))
  print(paste("Year with highest negative deviation from mean: ",
              year.highest.negative.deviation,
              " with a mean temperature of ",
              round(df.temp.annual$mean[
                which(df.temp.annual$deviations == min(df.temp.annual$deviations))],
                digits = 1),
              "°C.",
              sep=""))
  print(paste("Year with highest positive deviation from mean: ",
              year.highest.positive.deviation,
              " with a mean temperature of ",
              round(df.temp.annual$mean[
                which(df.temp.annual$deviations == max(df.temp.annual$deviations))],
                digits = 1),
              "°C.",
              sep=""))

  # Save year as date
  df.temp.annual$year <- paste(df.temp.annual$year, "-01-01", sep="")
  df.temp.annual$year <- as.Date(df.temp.annual$year)

  # Deviations as categories
  df.temp.annual$deviationscat <- 1
  deviation.range <- max(df.temp.annual$deviations)-min(df.temp.annual$deviations)
  deviation.range <- 2*max(abs(min(df.temp.annual$deviations)), abs(max(df.temp.annual$deviations)))

  temp.lables <- replicate(11, NaN)
  for(i in 1:11){
    for(j in 1:length(df.temp.annual$year)){
      if(df.temp.annual$deviations[j] >= (-0.5*deviation.range + (i-1) * deviation.range/11)){
        df.temp.annual$deviationscat[j] <- i
      }
    }
    temp.lables[i] <- paste(
      round(-0.5*deviation.range + (i-1) * deviation.range/11, digits = 1),
      " .. ",
      round(-0.5*deviation.range + (i) * deviation.range/11, digits = 1),
      sep="")
  }
  df.temp.annual$deviationscat <- as.factor(df.temp.annual$deviationscat)


  # Creating Color Strips
  # theme_strip <- theme_minimal() +
  #   theme(axis.text.y = element_blank(),
  #         axis.line.y = element_blank(),
  #         axis.title = element_blank(),
  #         panel.grid.major=element_blank(),
  #         legend.title = element_blank(),
  #         axis.text.x=element_text(vjust=3),
  #         panel.grid.minor=element_blank(),
  #         plot.title=element_text(size=14,face="bold")
  #   )


  col_strip <- brewer.pal(11,"RdBu")

  if(style == "discrete"){
    plot.warmingStripes <-
      ggplot(df.temp.annual, aes(x=year, y=1, fill=deviationscat)) +
      geom_tile() +
      scale_x_date(date_breaks = "1 year",
                   date_labels = "%Y",
                   expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      scale_fill_manual(values = rev(col_strip)[sort(as.integer(levels(unique(df.temp.annual$deviationscat))))], name = "Abweichung\nvon der\nDurchschnitts-\ntemperatur\nin °C",
                        labels = temp.lables[sort(as.integer(levels(unique(df.temp.annual$deviationscat))))]) +
      # scale_fill_gradientn(colors = rev(col_strip)) +
      #guides(fill=guide_colorbar(barwidth = 1, title = "Abweichung\nvom Mittelwert\nin K"))+
      labs(title=paste("Abweichung von der Durchschnittstemperatur (",startyear.mean,"-",endyear.mean,") in Rostock-Warnemünde", sep=""),
           caption="Quelle: Deutscher Wetterdienst und Scientists For Future Rostock")+
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.line.y = element_blank(),
            axis.title = element_blank(),
            panel.grid.major=element_blank(),
            axis.text.x=element_text(vjust=0.5),
            panel.grid.minor=element_blank(),
            plot.title=element_text(size=14,face="bold")
      ) +
      theme(axis.text.x = element_text(angle = 90))
  }

  if(style == "continuous")
  {
    plot.warmingStripes <-
      ggplot(df.temp.annual, aes(x=year, y=1, fill=deviations)) +
      geom_tile() +
      scale_x_date(date_breaks = "1 year",
                   date_labels = "%Y",
                   expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      scale_fill_gradientn(colors = rev(col_strip), limits = c(-deviation.range/2,deviation.range/2)) +
      guides(fill=guide_colorbar(barwidth = 1, title = "Abweichung\nvon der\nDurchschnitts-\ntemperatur\nin °C")) +
      labs(title=paste("Abweichung von der Durchschnittstemperatur (",startyear.mean,"-",endyear.mean,") in Rostock-Warnemünde", sep=""),
           caption="Quelle: Deutscher Wetterdienst und Scientists For Future Rostock") +
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.line.y = element_blank(),
            axis.title = element_blank(),
            panel.grid.major=element_blank(),
            axis.text.x=element_text(vjust=0.5),
            panel.grid.minor=element_blank(),
            plot.title=element_text(size=14,face="bold")
      ) +
      theme(axis.text.x = element_text(angle = 90))
  }

  ggsave(filename = "WarmingStripes.pdf", width = 297, height = 210, units = "mm")

  return(plot.warmingStripes)

}

