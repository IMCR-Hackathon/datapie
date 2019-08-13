#' make_numeric_histogram
#' 
#' Creates a histogram with density plot for numeric vectors
#' 
#' @usage
#'   make_numeric_histogram(df, var, varname)
#'
#' @param entity_df (data.frame) A data.frame containing entity-level data. The "data" child element of an entity-level list object in metajam output format.
#' @param varname (character) Name of variable of interest.
#' 
#' @return
#'   A ggplot2 object containing the histogram and density plot for that column

make_numeric_histogram <- function(entity_df, varname) {
  x <- ggplot(entity_df, aes(x = entity_df[[varname]])) + xlab(varname) +
    geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
    geom_density(alpha = .2, fill = "#FF6666") + 
    theme(aspect.ratio = 1/3)
  return(x)
}