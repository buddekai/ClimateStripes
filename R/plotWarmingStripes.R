#' @title plotWarmingStripes
#' @description Plot the deviation of mean temperature per year
#' @details This function takes a data frame with daily temperature data for
#' given station, calculates annual mean and the deviation from a
#' reference period and plots the deviations a color coded bars.
#' @aliases plotwarmingstripes
#' @aliases plotWarmingstripes
#' @aliases plotwarmingStripes
#' @author Kai Budde
#' @export plotWarmingStripes
#' @import lubridate
#' @import dplyr
#' @import ggplot2
#' @import RColorBrewer
#' @param df A data frame
#' @param startyear.mean A number
#' @param endyear.mean A number
#' @param style A character
#' @param station.name a character

# Created:     2019/07/20
# Last edited: 2020/04/24

plotWarmingStripes <- function(df, startyear.mean, endyear.mean,
                               style = "continuous",
                               station.name = NULL){

  # Remove missing values
  df$TMK <- as.numeric(df$TMK)
  df <- df[df$TMK != -999,]

  # Calculate annual temperature
  df$date <- as.character(df$MESS_DATUM)
  df$date <-  as.Date(df$date, "%Y%m%d")
  df$year <-  year(df$date)
  df.annual <- group_by(df, year) %>% summarise(mean = mean(TMK))

  # Delete current year
  current.year <- year(Sys.time())
  df.annual <- df.annual[!(df.annual$year == current.year),]

  # Calculate annual mean from start year (should be 1961) until
  # end year (should be 1990)
  mean.from.start.to.endyear <- mean(
    df.annual$mean[
      df.annual$year>= startyear.mean &
        df.annual$year<= endyear.mean])

  df.annual$deviations <-
    df.annual$mean - mean.from.start.to.endyear

  # Year with highest negative and positive deviation from mean
  year.highest.negative.deviation <- df.annual$year[
    which(df.annual$deviations == min(df.annual$deviations))]
  year.highest.positive.deviation <- df.annual$year[
    which(df.annual$deviations == max(df.annual$deviations))]


  print(paste("Temperature mean from ",
              startyear.mean,
              " until ",
              endyear.mean,
              ": ",
              round(mean.from.start.to.endyear, digits = 1),
              "\u00B0C.",
              sep=""))
  print(paste("Year with highest negative deviation from mean: ",
              year.highest.negative.deviation,
              " with a mean temperature of ",
              round(df.annual$mean[
                which(df.annual$deviations ==
                        min(df.annual$deviations))],
                digits = 1),
              "\u00B0C.",
              sep=""))
  print(paste("Year with highest positive deviation from mean: ",
              year.highest.positive.deviation,
              " with a mean temperature of ",
              round(df.annual$mean[
                which(df.annual$deviations ==
                        max(df.annual$deviations))],
                digits = 1),
              "\u00B0C.",
              sep=""))

  # Save year as date
  df.annual$year <- paste(df.annual$year, "-01-01", sep="")
  df.annual$year <- as.Date(df.annual$year)

  # Deviations as categories
  df.annual$deviationscat <- 1
  deviation.range <- max(df.annual$deviations) -
    min(df.annual$deviations)
  deviation.range <- 2*max(abs(min(df.annual$deviations)),
                           abs(max(df.annual$deviations)))

  plot.lables <- replicate(11, NaN)
  for(i in 1:11){
    for(j in 1:length(df.annual$year)){
      if(df.annual$deviations[j] >= (-0.5*deviation.range +
                                          (i-1) * deviation.range/11)){
        df.annual$deviationscat[j] <- i
      }
    }
    plot.lables[i] <- paste(
      round(-0.5*deviation.range + (i-1) * deviation.range/11, digits = 1),
      " .. ",
      round(-0.5*deviation.range + (i) * deviation.range/11, digits = 1),
      sep="")
  }
  df.annual$deviationscat <- as.factor(df.annual$deviationscat)

  col_strip <- brewer.pal(11,"RdBu")

  # Plotting ###############################################################

  # Creating stripes plot
  if(style == "discrete"){
    plot.warmingStripes <-
      ggplot(df.annual, aes(x=year, y=1, fill=deviationscat)) +
      geom_raster() +
      scale_x_date(date_breaks = "1 year",
                   date_labels = "%Y",
                   expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      scale_fill_manual(
        values = rev(col_strip)[
          sort(as.integer(levels(unique(df.annual$deviationscat))))],
        name = paste("Abweichung\nvon der\nDurchschnitts-\n",
                     "temperatur\nin \u00B0C", sep=""),
        labels = plot.lables[
          sort(as.integer(levels(unique(df.annual$deviationscat))))],
        guide = guide_legend(reverse=TRUE)) +
      labs(title=paste("Abweichung von der Durchschnittstemperatur (",
                       startyear.mean, "-", endyear.mean, ") in ",
                       station.name, sep=""),
           caption=paste("Quelle: Deutscher Wetterdienst und Scientists ",
                         "For Future Rostock", sep="")) +
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
      ggplot(df.annual, aes(x=year, y=1, fill=deviations)) +
      geom_raster() +
      scale_x_date(date_breaks = "1 year",
                   date_labels = "%Y",
                   expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      scale_fill_gradientn(
        colors = rev(col_strip),
        limits = c(-deviation.range/2,deviation.range/2)) +
      guides(
        fill=guide_colorbar(
          barwidth = 1.5,
          title = paste("Abweichung\nvon der\nDurchschnitts-\n",
                        "temperatur\nin \u00B0C", sep=""))) +
      labs(title=paste("Abweichung von der Durchschnittstemperatur (",
                       startyear.mean, "-", endyear.mean, ") in ",
                       station.name, sep=""),
           caption=paste("Quelle: Deutscher Wetterdienst und Scientists ",
                         "For Future Rostock", sep="")) +
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

  ggsave(filename = "WarmingStripes.pdf", width = 297,
         height = 210, units = "mm")
  ggsave(filename = "WarmingStripes.png", width = 297,
         height = 210, units = "mm")

  # Plot connected points
  plot.warmingPoints <-
    ggplot(df.annual, aes(x=year, y=mean)) +
    geom_point() +
    geom_line() +
    ylab("Mittlere Jahrestemperatur in \u00B0C") +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y",
                 expand=c(0.01, 0)) +
    geom_hline(yintercept=mean.from.start.to.endyear, color = "red") +
    labs(title=paste("Jahresdurchschnittstemperatur in ",
                     station.name,
                     " (Rote Linie: Durchschnitt der Jahre ",
                     startyear.mean, "-", endyear.mean, ")",
                     sep=""),
         caption=paste("Quelle: Deutscher Wetterdienst und Scientists ",
                       "For Future Rostock", sep="")) +
    theme_bw() +
    theme(axis.text.x = element_text(vjust=0.5, angle = 90),
          plot.title = element_text(size=14,face="bold"),
          panel.grid.minor=element_blank(),
          axis.title.x = element_blank())

  ggsave(filename = "WarmingPoints.pdf", width = 297,
         height = 210, units = "mm")
  ggsave(filename = "WarmingPoints.png", width = 297,
         height = 210, units = "mm")

  return(plot.warmingStripes)

}

