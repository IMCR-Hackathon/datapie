#'
#' Summarize all variables in a data.frame
#' 
#' @param entity_list
#' 
#'
#'

summarize_all_var <- function(entity_list) {
  entity_df <- entity_list[["data"]]
  if ("attribute_metadata" %in% names(entity_list)){
    
    indices <- match_names_2(entity_list)
    defs <- entity_list[["attribute_metadata"]][indices, "attributeDefinition"]
    
  summ <- data.frame(var = colnames(entity_df),
                     if(!is.null(defs)) defs = defs else defs = NULL,
                     class = sapply(sapply(entity_df, class), paste, collapse = ", "),
                     val = sapply(entity_df, function(x) sum(!is.na(x))),
                     na = sapply(entity_df, function(x) sum(is.na(x))))
  } else {
    summ <- data.frame(var = colnames(entity_df),
                       class = sapply(sapply(entity_df, class), paste, collapse = ", "),
                       val = sapply(entity_df, function(x) sum(!is.na(x))),
                       na = sapply(entity_df, function(x) sum(is.na(x))))
  }
  return(summ)
}
