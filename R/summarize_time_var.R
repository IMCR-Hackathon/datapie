#' summarize_time_var
#' 
#' This function writes a summary table of time (POSIX, Date) objects
#' 
#' @usage
#'   summarize_time_var(var)
#'
#' @param entity_df (data.frame) A data frame containing data. In this package context, this is most often the "data" child element within a metajam list output for a data entity.
#' @param varname (character) Name for a POSIXct-type or Date-type column within the given data frame.
#'
#' @return (data.frame) A summary table of time (POSIX, Date) objects.
#'

summarize_time_var <- function(entity_df, varname) {
  
  # get data from one column
  var <- entity_df[[varname]]
  
  var_type <- class(var)[1]
  num_times <- length(levels(as.factor(var)))
  num_valid <- sum(!is.na(var))
  num_missing <- sum(is.na(var))
  first_time <- min(var, na.rm = T)
  last_time <- max(var, na.rm = T)
  
  # determine the most common lag between time steps
  mode_time_step <-
    names(which.max(table(diff(
      unique(var[order(var)]), lag = 1
    ))))
  
  #med_time_step <- median(diff(unique(var[order(var)]), lag = 1), na.rm = T)
  
  # assemble summary data frame
  df <-
    data.frame(var_type,
               first_time,
               last_time,
               mode_time_step,
               num_times,
               num_valid,
               num_missing)
  return(df)
}