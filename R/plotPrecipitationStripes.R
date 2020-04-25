#' @title plotPrecipitationStripes
#' @description Plot the deviation of cumulative precipitation of a year
#' @details This function takes a data frame with daily precipitation (in mm)
#' for a given station, calculates annual sum and the deviation from a
#' reference period and plots the deviations as color coded bars.
#' @aliases plotprecipitationstripes
#' @aliases plotprecipitationStripes
#' @aliases plotPrecipitationstripes
#' @aliases plotPrecipitationStripes
#' @author Kai Budde
#' @export plotPrecipitationStripes
#' @import lubridate
#' @import dplyr
#' @import ggplot2
#' @import RColorBrewer
#' @param df A data frame
#' @param startyear.mean A number
#' @param endyear.mean A number
#' @param style A character
#' @param station.name a character

# Created:     2020/04/24
# Last edited: 2020/04/25

plotPrecipitationStripes <- function(df, startyear.mean, endyear.mean,
                              style = "continuous",
                              station.name = NULL){

  # Remove missing values
  df$RSK <- as.numeric(df$RSK)
  df <- df[df$RSK != -999,]

  # Calculate cumulative precipitation per year
  df$date <- as.character(df$MESS_DATUM)
  df$date <-  as.Date(df$date, "%Y%m%d")
  df$year <-  year(df$date)
  df.annual <- group_by(df, year) %>% summarise(sum = sum(RSK))

  # Delete current year
  current.year <- year(Sys.time())
  df.annual <- df.annual[!(df.annual$year == current.year),]

  # Calculate average annual precipitation from start year (should be 1961)
  # until end year (should be 1990)
  mean.from.start.to.endyear <- mean(
    df.annual$sum[
      df.annual$year>= startyear.mean &
        df.annual$year<= endyear.mean])

  df.annual$deviations <-
    df.annual$sum - mean.from.start.to.endyear

  # Year with highest negative and positive deviation from mean
  year.highest.negative.deviation <- df.annual$year[
    which(df.annual$deviations == min(df.annual$deviations))]
  year.highest.positive.deviation <- df.annual$year[
    which(df.annual$deviations == max(df.annual$deviations))]


  print(paste("Average yearly precipitation from ",
              startyear.mean,
              " until ",
              endyear.mean,
              ": ",
              round(mean.from.start.to.endyear, digits = 1),
              " mm.",
              sep=""))
  print(paste("Year with highest negative deviation from average ",
              "precipitation (driest year): ",
              year.highest.negative.deviation,
              " with a total precipitation of ",
              round(df.annual$sum[
                which(df.annual$deviations ==
                        min(df.annual$deviations))],
                digits = 1),
              " mm.",
              sep=""))
  print(paste("Year with highest positive deviation from average ",
              "precipitation (wettest year): ",
              year.highest.positive.deviation,
              " with a total precipitation of ",
              round(df.annual$sum[
                which(df.annual$deviations ==
                        max(df.annual$deviations))],
                digits = 1),
              " mm.",
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

  col_strip <- brewer.pal(11, "BrBG")

  # Plotting ###############################################################
  # Creating stripes plot
  if(style == "discrete"){
    plot.precipitationStripes <-
      ggplot(df.annual, aes(x=year, y=1, fill=deviationscat)) +
      geom_raster() +
      scale_x_date(date_breaks = "1 year",
                   date_labels = "%Y",
                   expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      scale_fill_manual(
        values = col_strip[
          sort(as.integer(levels(unique(df.annual$deviationscat))))],
        name = paste("Abweichung\nvon der\nDurchschnitts-\n",
                     "gesamtjahres-\nniederschlags-\nmenge in mm", sep=""),
        labels = plot.lables[
          sort(as.integer(levels(unique(df.annual$deviationscat))))],
        guide = guide_legend(reverse=TRUE)) +
      labs(title=paste("Abweichung von der Durchschnittsgesamtjahresniederschlagsmenge (",
                       startyear.mean, "-", endyear.mean, ") in ",
                       station.name, sep=""),
           caption=paste("Quelle: Deutscher Wetterdienst und Scientists ",
                         "For Future Rostock", sep="")) +
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.line.y = element_blank(),
            axis.title = element_blank(),
            panel.grid.major=element_blank(),
            axis.text.x=element_text(vjust=0.5, angle = 90),
            panel.grid.minor=element_blank(),
            plot.title=element_text(size=14,face="bold")
      )
  }

  if(style == "continuous")
  {
    plot.precipitationStripes <-
      ggplot(df.annual, aes(x=year, y=1, fill=deviations)) +
      geom_raster() +
      scale_x_date(date_breaks = "1 year",
                   date_labels = "%Y",
                   expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      scale_fill_gradientn(
        colors = col_strip,
        limits = c(-deviation.range/2,deviation.range/2)) +
      guides(
        fill=guide_colorbar(
          barwidth = 1.5,
          title = paste("Abweichung\nvon der\nDurchschnitts-\n",
                        "gesamtjahres-\nniederschlags-\nmenge in mm", sep=""))) +
      labs(title=paste("Abweichung von der Durchschnittsgesamtjahresniederschlagsmenge (",
                       startyear.mean, "-", endyear.mean, ") in ",
                       station.name, sep=""),
           caption=paste("Quelle: Deutscher Wetterdienst und Scientists ",
                         "For Future Rostock", sep="")) +
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.line.y = element_blank(),
            axis.title = element_blank(),
            panel.grid.major=element_blank(),
            axis.text.x=element_text(vjust=0.5, angle = 90),
            panel.grid.minor=element_blank(),
            plot.title=element_text(size=14,face="bold")
      )
  }

  ggsave(filename = "PrecipitationStripes.pdf", width = 297,
         height = 210, units = "mm")
  ggsave(filename = "PrecipitationStripes.png", width = 297,
         height = 210, units = "mm")

  # Plot connected points
  plot.precipitationPoints <-
    ggplot(df.annual, aes(x=year, y=sum)) +
    geom_point() +
    geom_line() +
    ylab("Niederschlag in mm/Jahr") +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y",
                 expand=c(0.01, 0)) +
    geom_hline(yintercept=mean.from.start.to.endyear, color = "red") +
    labs(title=paste("Jahresgesamtniederschlag in ",
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

  ggsave(filename = "PrecipitationPoints.pdf", width = 297,
         height = 210, units = "mm")
  ggsave(filename = "PrecipitationPoints.png", width = 297,
         height = 210, units = "mm")

  return(plot.precipitationStripes)

}
