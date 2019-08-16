#' plot_cat_var
#' 
#' Creates a histogram of the frequency with which different codes occur for categorical variables. 
#' 
#' @usage
#'   plot_cat_var(df, var, varname)
#'
#' @param entity_df (data.frame) A data.frame containing entity-level data. The "data" child element of an entity-level list object in metajam output format.
#' @param varname (character) Name of variable of interest.
#'
#' @return
#'   A ggplot2 object containing the histogram for that column

plot_cat_var <- function(entity_df, varname) {
  x <- ggplot(entity_df, aes(x = entity_df[[varname]])) + 
    xlab(varname) +
    geom_bar(colour = "black", fill = "#FF6666", alpha = 0.2, width = 0.8) + 
    theme(aspect.ratio = 1 / 3)
  return(x)
}