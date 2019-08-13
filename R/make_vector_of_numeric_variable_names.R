#' make_vector_of_numeric_variable_names
#' 
#' Describe what this function does
#' 
#' @usage
#'   make_vector_of_numeric_variable_names(df)
#'
#' @param df
#'   Define this parameter.
#'
#' @return
#'   What does this function return?

make_vector_of_numeric_variable_names<-function(df){
  numeric_var_name_vector<-c("")
  attach(df,warn.conflicts = F)
  for(varname in names(df)) {
    if(is.numeric(get(varname))){
      numeric_var_name_vector<-append(numeric_var_name_vector, varname, after = length(numeric_var_name_vector))
    }
  }
  detach(df)
  numeric_var_name_vector<-tail(numeric_var_name_vector,length(numeric_var_name_vector)-1)
  return(numeric_var_name_vector)
}