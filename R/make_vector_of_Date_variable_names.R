#' make_vector_of_Date_variable_names
#' 
#' This function makes a list of POSIX class columns in a data.frame.
#' 
#' @usage
#'   make_vector_of_Date_variable_names(df)
#'
#' @param df
#'   Define this parameter.
#'
#' @return
#'   (list) A list of POSIX class columns in a data.frame.

make_vector_of_Date_variable_names<-function(df){
  Date_var_name_vector<-c("")
  attach(df,warn.conflicts = F)
  for(varname in names(df)) {
    var<-get(varname)
    if(is.Date(var)){
      Date_var_name_vector<-append(Date_var_name_vector, varname, after = length(Date_var_name_vector))
    }
  }
  detach(df)
  Date_var_name_vector<-tail(Date_var_name_vector,length(Date_var_name_vector)-1)
  return(Date_var_name_vector)
}