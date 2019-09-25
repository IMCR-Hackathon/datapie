#'
#' the space detective
#' 
#' Wrapper for \code{\link{classify_xy}}. Runs \code{\link{classify_xy}}, counts number of hits in each dimension, then decides what to depending on those two numbers. 
#'
#' @param entity_list (list) Metajam output for a single data entity
#' @param report (logical) Whether the function is used in the report-making environment. Defaults to FALSE.
#' 
#' @return A character vector whose two elements are named "x_col" and "y_col", indicating the columns (as appears in data) containing spatial information in the x and y dimensions respectively, or an message if not detected or if we aren't equipped to deal with the number of hits we got. If report == T, will also output messages.
#'
#' @export

space_detective <- function(entity_list, report = F) {
  
  # call to classify_xy
  x_detect <- classify_xy(entity_list, "x")
  y_detect <- classify_xy(entity_list, "y")
  
  # count hits, assuming all logical. TODO: what to do with possible hits
  
  x_hits <- sum(as.logical(unlist(x_detect)), na.rm = T)
  y_hits <-
    sum(as.logical(unlist(y_detect)), na.rm = T)
  
  # no hits at all
  
  if (x_hits == 0 & y_hits == 0) {
    
    
      msg <- "Space detective was not able to detect columns containing spatial information. Spatial plots will not be generated."
    
    if (!report) message(msg) else return(c(msg = msg))
  }
  
  # equal hits in both dimensions
  
  if (x_hits == y_hits) {
    
    # single pair
    
    if (x_hits == 1 & y_hits == 1) {
      
      # get target column names
      # x_col <- entity_list[["attribute_metadata"]][which(as.logical(x_detect)), "attributeName"]
      # y_col <- entity_list[["attribute_metadata"]][which(as.logical(y_detect)), "attributeName"]
      
      x_col <- colnames(entity_list[["data"]])[which(as.logical(x_detect))]
      y_col <- colnames(entity_list[["data"]])[which(as.logical(y_detect))]
      
      msg <- paste("Space detective found a single pair of columns containing spatial information. \n Latitude column: ", y_col, "\n Longitude column: ", x_col, "\n")
      
      if (!report) message(msg)
      
      cols <- c(x_col, y_col)
      names(cols) <- c("x_col", "y_col")
      if (report) return(list(msg = msg, cols = cols)) else return(cols)
    } else {
      
      # equal hits and larger than 1
      
      msg <- "Space detective found equal and larger than 1 numbers of x and y columns. What to do with this information pending. Meanwhile, spatial plots will not be generated."
      
      if (!report) message(msg) else return(c(msg = msg))
      
      }
  } else {
    
    # unequal number of hits
    
    msg <- "Space detective found unequal numbers of x and y columns. What to do with this information pending. Meanwhile, spatial plots will not be generated."
    if (!report) message(msg) else return(c(msg = msg))
    
    }
}