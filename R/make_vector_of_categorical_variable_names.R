#' make_vector_of_categorical_variable_names
#' 
#' Describe what this function does
#' 
#' @usage
#'   make_vector_of_categorical_variable_names(df)
#'
#' @param df
#'   Define this parameter.
#'
#' @return
#'   What does this function return?

make_vector_of_categorical_variable_names<-function(df){
  cat_var_name_vector<-c("")
  attach(df,warn.conflicts = F)
  for(varname in names(df)) {
    var<-get(varname)
    if(is.factor(var) | is.character(var)){
      cat_var_name_vector<-append(cat_var_name_vector, varname, after = length(cat_var_name_vector))
    }
  }
  detach(df)
  cat_var_name_vector<-tail(cat_var_name_vector,length(cat_var_name_vector)-1)
  return(cat_var_name_vector)
}