#' 
#' Generate variable-level plots.
#' 
#' @param entity_df (data.frame) A data.frame containing entity-level data. The "data" child element of an entity-level list object in metajam output format.
#' @param varname (character) Name of variable of interest.
#' @param space_cols (character vector) Output from \code{space_detective}. Where data has explicit spatial columns, a two-element character vector indicating column names containing longitudes and latitudes. Otherwise, spatial plots will not be generated.
#' 
#' @return A list object containing variable name and relevant plots.
#' @import lubridate
#' @import cowplot
#' @import scales
#' @import ggplot2
#' 



static_report_variable <- function(entity_df, varname, space_cols = space_cols) {
  
  # get data from one column
  var <- entity_df[[varname]]
  
  # set ggplot theme
  theme_set(theme_bw(base_size = 8) + theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    plot.background = element_blank(),
    panel.border = element_blank()
  ))
  
  # lifted somewhat wholesale from John's Rmd report.
  if (is.numeric(var)) {
    
    x <- plot_numeric_var(entity_df, varname)
    plots <- list(x = x)
    
  } else if (is.factor(var) | is.character(var)) {
    
    x <- plot_cat_var(entity_df, varname)
    plots <- list(x = x)
    
  } else if (is.Date(var)) {
    
    x <- freqclocks_forDates(entity_df, varname)
    plots <- list(x = x)
    
  } else if (is.POSIXct(var)) {
    
    x <- freqclocks_forPOSIX(entity_df, varname)
    plots <- list(x = x)
    
  } else {
    plots <- NULL
  }
  
  # # check if any NAs
  # if (anyNA(var)) {
  #   missing <- plot_missing_values(var)
  # } else missing <- NULL

  # make spatial heatmaps
  space <- space_plot(space_cols = space_cols, entity_df = entity_df, varname = varname)
  
  # construct list output
  var_output <- list(
    var_name = varname,
    plots = plots,
    # missing = missing,
    space = space
    )
  
  return(var_output)
}