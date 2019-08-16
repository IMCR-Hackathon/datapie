#' Summarize levels of categorical variables.
#'
#' Create a summary table with level-wise statistics for categorical variables.
#'
#' @usage
#'   summarize_cat_levels(entity_df)
#'
#' @param entity_df (data.frame) A data.frame containing entity-level data. The "data" child element of an entity-level list object in metajam output format.
#'
#' @return (data.frame) A table with level-wise statistics for categorical variables.
#'
#' @import magrittr

summarize_cat_levels <- function(entity_df)  {
  SummaryTable_Categorical_Individual <- entity_df %>%
    dplyr::select_if( ~ is.character(.) & length(unique(.)) < 20) %>%
    tidyr::gather(key = "column_name", value = 'level') %>%
    dplyr::group_by(column_name, level) %>%
    dplyr::tally() %>%
    dplyr::mutate(NAs = sum(is.na(level)))
  return(SummaryTable_Categorical_Individual)
}
