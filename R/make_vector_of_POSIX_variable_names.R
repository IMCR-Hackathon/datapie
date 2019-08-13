#' make_vector_of_POSIX_variable_names
#' 
#' This function makes a list of POSIX class columns in a data.frame
#' 
#' @usage
#'   make_vector_of_POSIX_variable_names(df)
#'
#' @param df
#'   Define this parameter.
#'
#' @return
#'   (list) A list of POSIX class columns in a data.frame.

make_vector_of_POSIX_variable_names<-function(df){
  POSIX_var_name_vector<-c("")
  attach(df,warn.conflicts = F)
  for(varname in names(df)) {
    var<-get(varname)
    if(is.POSIXt(var)){
      POSIX_var_name_vector<-append(POSIX_var_name_vector, varname, after = length(POSIX_var_name_vector))
    }
  }
  detach(df)
  POSIX_var_name_vector<-tail(POSIX_var_name_vector,length(POSIX_var_name_vector)-1)
  return(POSIX_var_name_vector)
}