#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'

summarize_all_var <- function(entity_df) {
  summ <- data.frame(var = colnames(entity_df),
                     class = sapply(sapply(entity_df, class), paste, collapse = ", "),
                     val = sapply(entity_df, function(x) sum(!is.na(x))),
                     na = sapply(entity_df, function(x) sum(is.na(x))))
  return(summ)
}
