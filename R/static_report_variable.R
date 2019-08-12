#' 
#' Generate variable-level plots.
#' 
#' @param entity_df (data.frame) A data.frame containing entity-level data. The "data" child element of an entity-level list object in metajam output format.
#' @param varname (character) Name of variable of interest.
#' @param space_cols (character vector) Output from \code{space_detective}. Where data has explicit spatial columns, a two-element character vector indicating column names containing longitudes and latitudes. Otherwise, spatial plots will not be generated.
#' 
#' @return A list object containing variable name and relevant plots.
#' 
#' @export



static_report_variable <- function(entity_df, varname, space_cols = space_cols) {
  
  # get data from one column
  var <- entity_df[[varname]]
  
  # set ggplot theme
  theme_set(theme_bw(base_size = 7) + theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    plot.background = element_blank(),
    panel.border = element_blank()
  ))
  
  # lifted somewhat wholesale from John's Rmd report
  if (is.numeric(var)) {
    
    xsummary <- make_numeric_variable_summary_df(var)
    x <- make_numeric_histogram(entity_df, var, varname)
    plots <- list(xsummary = xsummary, x = x)
    
  } else if (is.factor(var) | is.character(var)) {
    
    xsummary <- make_cat_variable_summary_df(var)
    x <- make_categorical_histogram(entity_df, var, varname)
    plots <- list(xsummary = xsummary, x = x)
    
  } else if (is.Date(var)) {
    
    xsummary <- summarize_time_var(var)
    x <- freqclocks_forDates(entity_df, varname)
    plots <- list(x = x)
    
  } else if (is.POSIXct(var)) {
    
    xsummary <- summarize_time_var(var)
    x <- freqclocks_forPOSIX(entity_df, varname)
    plots <- list(x = x)
    
  } else {
    plots <- NULL
  }
  
  try(missing <- if (sum(plots$xsummary$num_missing) > 0) {
    make_missing_plot(var)
  })

  # make spatial heatmaps
  space <- space_plot(space_cols = space_cols, df = entity_df, var = varname)
  
  # construct list output
  var_output <- list(
    var_name = varname,
    plots = plots,
    missing = if (exists("missing"))
      missing
    else NULL,
    space = if (exists("space"))
      space
    else NULL
    )
  
  return(var_output)
}