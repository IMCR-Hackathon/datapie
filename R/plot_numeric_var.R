#' plot_numeric_var
#' 
#' Creates a histogram with density plot for numeric vectors
#' 
#' @usage
#'   plot_numeric_var(df, var, varname)
#'
#' @param entity_df (data.frame) A data.frame containing entity-level data. The "data" child element of an entity-level list object in metajam output format.
#' @param varname (character) Name of variable of interest.
#' 
#' @import ggplot2
#' 
#' @return
#'   A ggplot2 object containing the histogram and density plot for that column

plot_numeric_var <- function(entity_df, varname) {
  x <- ggplot(entity_df, aes(x = entity_df[[varname]])) + xlab(varname) +
    geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
    geom_density(alpha = .2, fill = "#FF6666") + 
    theme(aspect.ratio = 1/3)
  return(x)
}