#' Summarize categorical variables.
#'
#' Create a summary table with variable-wise statistics for categorical variables.
#'
#' @usage
#'   summarize_cat_var(entity_df)
#'
#' @param entity_df (data.frame) A data.frame containing entity-level data. The "data" child element of an entity-level list object in metajam output format.
#'
#' @return (data.frame) A table with variable-wise statistics for categorical variables.
#'
#' @import magrittr
#' 

summarize_cat_var <- function(entity_df)  {
  cat_var <- entity_df %>%
    dplyr::select_if( ~ is.character(.)) %>%
    tidyr::gather(key = "column_name", value = "level")
  
  overall <- cat_var %>%
    dplyr::group_by(column_name) %>%
    dplyr::summarize(
      n_categories = length(unique(level)),
    )
  
   levels <- cat_var %>%
    dplyr::group_by(column_name, level) %>%
    dplyr::tally() %>%
    dplyr::mutate(NAs = sum(is.na(level)))
  
  # a "col_palette" of 1 is a color-blind friendly palette
  inspectdf_plot <- inspectdf::inspect_cat(entity_df) %>% inspectdf::show_plot(col_palette = 1, high_cardinality = 2)
  
  return(list(overall, levels, inspectdf_plot))
}