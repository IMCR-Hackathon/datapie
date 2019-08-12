#' Summarize numerical variables.
#'
#' Create a summary table with variable-wise statistics for numerical variables.
#'
#' @usage
#'   summarize_numeric_var(entity_df)
#'
#' @param entity_df (data.frame) A data.frame containing entity-level data. The "data" child element of an entity-level list object in metajam output format.
#'
#' @return (data.frame) A table with variable-wise statistics for numerical variables.
#'
#' @import magrittr

summarize_numeric_var<-function(entity_df)  {
  SummaryTable_Numeric <- entity_df %>% 
    dplyr::select_if(is.numeric) %>%
    tidyr::gather(key="column_name", value='value') %>%
    dplyr::group_by(column_name) %>%
    dplyr::summarize(min=min(value, na.rm=TRUE),
              quantile_25 = quantile(value, 0.25, na.rm=TRUE),
              quantile_50 = quantile(value, 0.50, na.rm=TRUE),
              quantile_75 = quantile(value, 0.75, na.rm=TRUE),
              max = max(value, na.rm=TRUE), 
              mean = mean(value, na.rm=TRUE),
              Unique_Values = length(unique(value)),
              Total_Values = sum(is.finite(value)),
              NAs= sum(is.na(value))
    )
  
  return(SummaryTable_Numeric)
}