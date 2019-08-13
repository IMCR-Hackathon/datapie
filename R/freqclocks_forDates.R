#' Create frequency of data plots for POSIXct-type data.
#' 
#' This function creates ggplot clocks of the frequency of data occurring at different time frequencies, if Date type data.
#' 
#' @usage
#'   freqclocks_forDates(df, var, varname)
#'
#' @param entity_df (data.frame) A data frame containing data. In this package context, this is most often the "data" child element within a metajam list output for a data entity.
#' @param varname (character) Name for a Date-type column within the given data frame.
#' 
#' @return
#'   (ggplot clocks) Of the frequency of data occurring at different time frequencies, if Date type data.

freqclocks_forDates <- function(entity_df, varname) {
  
  # data munging
  na.df <- as.data.frame(!is.na(entity_df))
  na.df[, varname] <- entity_df[, varname]
  long.df <-
    tidyr::gather(na.df, key = column.name, value = count, -varname)
  
  # plot how much data there is over the time span of the dataset
  freq.timeline <-
    ggplot(long.df[long.df$count %in% TRUE, ], aes(x = long.df[long.df$count %in% TRUE, 1], group = column.name)) +
    theme_bw() +
    geom_freqpoly(aes(colour = column.name), bins = 100) +
    ylab("Data Coverage") + xlab("Time")  +
    guides(colour = guide_legend("Column Names")) +
    #theme(legend.position = "top") +
    scale_x_date(labels = date_format("%b-%Y"))
  
  # plot how much data occurs in each month of the year
  month.clock <-
    ggplot(long.df[long.df$count %in% TRUE, ], aes(x = months(long.df[long.df$count %in% TRUE, 1], T), group = column.name)) +
    theme_bw() +
    geom_histogram(stat = "count", aes(fill = column.name)) +
    coord_polar() + xlim(month.abb) +
    ylab("Data Coverage") + xlab("Month of Year") +
    guides(fill = guide_legend("Column Names")) +
    theme(legend.position = "none")
  
  # create plot grid layout
  
  return(list(timeline = freq.timeline, clock = month.clock))
}