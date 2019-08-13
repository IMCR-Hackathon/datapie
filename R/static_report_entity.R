#'
#' @title Generate entity-level report.
#' 
#' @description 
#' 
#' @param entity_df (data.frame) (data.frame) A data.frame containing entity-level data. The "data" child element of an entity-level list object in metajam output format.
#'
#' @return (list) A list containing entity-level report items.
#'

static_report_entity <- function(entity_df) {
  list(numvars = summarize_numeric_var(entity_df),
       catvars = tryCatch({summarize_cat_var(entity_df)}, error = function(cond){return(NULL)}),
       levels = tryCatch({summarize_cat_levels(entity_df)}, error = function(cond){return(NULL)})
  )
}