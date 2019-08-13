#' make_cat_variable_summary_df
#' 
#' Creates a dataframe that contains summary information (type, number of levels, num_valid, num_missing)
#' about categorical variables. 
#' 
#' @usage
#'   make_cat_variable_summary_df(var)
#'
#' @param var
#'   The categorical vector to be summarized
#'
#' @return
#'   A dataframe containing summary information. 

make_cat_variable_summary_df<-function(var){
  var_type<-class(var)
  num_levels<-length(levels(as.factor(var)))
  num_valid<-sum(!is.na(var))
  num_missing<-sum(is.na(var))
  df<-data.frame(var_type,num_levels,num_valid,num_missing)
  return(df)
}