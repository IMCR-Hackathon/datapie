#'
#' Detect spatial information in data columns
#'
#' @param list (list) A metajam list output for a single data entity.
#' @param dim (character) "x" or "y", dimension to look for.
#'
#' @return A logical vector classifying variables as TRUE, containing spatial information, or FALSE, not containing spatial information of the dimension specifed. Variables are in order as appear in data.
#'

classify_xy <- function(list, dim) {
  
  # set up conditions depending on whether we are looking for lat or lon
  
  if (dim == "x") {
    name_matches <-
      paste(c("(lon){1,2}", "(lng){1,2}", "(^x){1,2}", "(x$){1,2}"),
            collapse = "|")
    def_matches <-
      paste(c("longitude", "coord", "wgs", "nad83", "utm"), collapse = "|")
  } else if (dim == "y") {
    name_matches <-
      paste(c("(lat){1,2}", "(^y){1,2}", "(y$){1,2}"), collapse = "|")
    def_matches <-
      paste(c("latitude", "coord", "wgs", "nad83", "utm"), collapse = "|")
  } else {
    stop("Please specify either x or y as criteria to classify columns.")
  }
  
  # use metadata if it exists
  
  if ("attribute_metadata" %in% names(list)) {
    unit_matches <- paste(c("deg", "min", "sec"), collapse = "|")
    scale_matches <-
      paste(c("int", "ord", "rat"), collapse = "|")
    
    use_metadata <- function(attr_row) {
      # defining condition layers
      name_cond <-
        grepl(name_matches, c(attr_row[["attributeName"]]), ignore.case = T)
      def_cond <-
        grepl(def_matches, c(attr_row[["attributeDefinition"]]), ignore.case = T)
      scale_cond <-
        grepl(scale_matches,
              c(attr_row[["measurementScale"]]), ignore.case = T)
      unit_cond <- if ("unit" %in% names(attr_row)) {
        grepl(unit_matches, c(attr_row[["unit"]]), ignore.case = T)
      } else FALSE
      # ---
      # nested conditions
      
      if (scale_cond) {
        if (name_cond & def_cond) {
          if (unit_cond) {
            return(T)
          } else
            return("possible")
        } else
          return(F)
      } else
        return(F)
    }
    xy_or_not <-
      apply(list[["attribute_metadata"]], 1, use_metadata)
    
    # match names and reorder
    indices <- match_names_2(list)
    
    xy_or_not <- xy_or_not[indices]
    
  } else {
    
    # only use data if metadata is not available
    
    data_only <- function (column) {
      name_cond <-
        grepl(name_matches, colnames(column), ignore.case = T)
      type_cond <- is.numeric(column)
      range_cond_x <- min(column) >= -180 & max(column) <= 180
      range_cond_y <- min(column) >= -90 & max(column) <= 90
      
      if (name_cond & type_cond) {
        if (dim == "x") {
          if (range_cond_x) {
            return(T)
          }
        } else if (dim == "y") {
          if (range_cond_y) {
            return(T)
          }
        }
      } else {
        return(F)
      }
    }
    xy_or_not <- apply(list[["data"]], 2, data_only)
  }
  return(xy_or_not)
}
