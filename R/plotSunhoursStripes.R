#' @title plotSunhoursStripes
#' @description Plot the deviation of the mean daily sunshine duration
#' @details This function takes a data frame with daily sunshine duration
#' data for a given station, calculates annual mean and the deviation from a
#' reference period and plots the deviations a color coded bars.
#' @aliases plotsunhoursstripes
#' @aliases plotSunhoursstripes
#' @aliases plotsunhoursgStripes
#' @author Kai Budde
#' @export plotSunhoursStripes
#' @import lubridate
#' @import dplyr
#' @import ggplot2
#' @import RColorBrewer
#' @param df A data frame
#' @param startyear.mean A number
#' @param endyear.mean A number
#' @param style A character
#' @param station.name a character

# Created:     2021/01/03
# Last edited: 2021/01/03

plotSunhoursStripes <- function(df, startyear.mean, endyear.mean,
                               style = "continuous",
                               station.name = NULL){

  # Remove missing values
  df$SDK <- as.numeric(df$SDK)
  df <- df[df$SDK != -999,]


  # Calculate annual mean daily sunshine duration
  df$date <- as.character(df$MESS_DATUM)
  df$date <- as.Date(df$date, "%Y%m%d")
  df$year <- year(df$date)
  df.annual <- group_by(df, year) %>% summarise(mean = mean(SDK))

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


  print(paste("Mean daily sunshine duration from ",
              startyear.mean,
              " until ",
              endyear.mean,
              ": ",
              round(mean.from.start.to.endyear, digits = 1),
              "h.",
              sep=""))
  print(paste("Year with highest negative deviation from mean: ",
              year.highest.negative.deviation,
              " with a mean daily sunshine duration of ",
              round(df.annual$mean[
                which(df.annual$deviations ==
                        min(df.annual$deviations))],
                digits = 1),
              "h.",
              sep=""))
  print(paste("Year with highest positive deviation from mean: ",
              year.highest.positive.deviation,
              " with a mean daily sunshine duration of ",
              round(df.annual$mean[
                which(df.annual$deviations ==
                        max(df.annual$deviations))],
                digits = 1),
              "h.",
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

  col_strip <- brewer.pal(11,"RdYlBu")

  # Plotting ###############################################################

  # Creating stripes plot
  if(style == "discrete"){
    plot.sunhoursStripes <-
      ggplot(df.annual, aes(x=year, y=1, fill=deviationscat)) +
      geom_raster() +
      scale_x_date(date_breaks = "1 year",
                   date_labels = "%Y",
                   expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      scale_fill_manual(
        values = rev(col_strip)[
          sort(as.integer(levels(unique(df.annual$deviationscat))))],
        name = paste("Abweichung\nvon der\ntäglichen\n",
                     "Sonnenschein-\ndauer in h", sep=""),
        labels = plot.lables[
          sort(as.integer(levels(unique(df.annual$deviationscat))))],
        guide = guide_legend(reverse=TRUE)) +
      labs(title=paste("Abweichung von der mittleren täglichen Sonnenscheindauer (",
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
    plot.sunhoursStripes <-
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
          title = paste("Abweichung\nvon der\ntäglichen\n",
                        "Sonnenschein-\ndauer in h", sep=""))) +
      labs(title=paste("Abweichung von der mittleren täglichen Sonnenscheindauer (",
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

  ggsave(filename = "SunshineStripes.pdf", width = 297,
         height = 210, units = "mm")
  ggsave(filename = "SunshineStripes.png", width = 297,
         height = 210, units = "mm")

  # Plot connected points
  plot.sunshinePoints <-
    ggplot(df.annual, aes(x=year, y=mean)) +
    geom_point() +
    geom_line() +
    ylab("Mittlere Sonnenscheindauer pro Tag in h") +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y",
                 expand=c(0.01, 0)) +
    geom_hline(yintercept=mean.from.start.to.endyear, color = "red") +
    labs(title=paste("Mittlere Sonnenscheindauer pro Tag in ",
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

  ggsave(filename = "SunshinePoints.pdf", width = 297,
         height = 210, units = "mm")
  ggsave(filename = "SunshinePoints.png", width = 297,
         height = 210, units = "mm")

  return(plot.sunhoursStripes)

}
