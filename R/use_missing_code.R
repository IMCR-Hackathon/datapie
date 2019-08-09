#'
#' Substitudes missing codes with NAs.
#' This function performs column-wise substitution of NAs for missing values codes, if specified in attribute metadata.
#' 
#' @usage 
#'   use_missing_code(
#'     entity_list
#'     )
#'     
#' @param entity_list (list) A list object containing information on a single data entity in metajam output format.
#'
#' @return (list) If attribute metadata is present and missing codes are specified: a list object but with NAs substituted for missing codes. Otherwise, a list object identical to input.
#'
#' @export

use_missing_code <- function(entity_list) {
  
  # ---
  # check for attribute metadata and missingValueCode field
  if ("attribute_metadata" %in% names(entity_list) &
      "missingValueCode" %in% names(entity_list[["attribute_metadata"]])) {
    
    # ---
    # subset into metadata and data
    attrs <- entity_list[["attribute_metadata"]]
    dat <- entity_list[["data"]]
    
    # ---
    # use match_names
    indices <- match_names(entity_list)
    
    # ---
    # column-wise loop
    for (i in 1:length(colnames(dat))) {
      
      # temporarily excluding dateTime variables: method not applicable to POSIXct variables
      if (attrs[indices[i], "measurementScale"] != "dateTime") {
        
        # get missingValueCode
        code <- attrs[indices[i], "missingValueCode"]

        # substitute where data matches code with NAs
        is.na(dat[[i]]) <-
          as.character(dat[[i]]) == as.character(code)

        # insert data back in
        entity_list[["data"]] <- dat
      }
    }
  } else message("attribute_metadata not a list item and/or missingValueCode not a column in attribute metadata. Double check list input to match with metajam list output structure.")
  

  # ---
  # return the same list but with NAs substituted for missing codes, or as-is if not successful
  return(entity_list)

}

# ----------------------------------------------------------------------------------------------------------------

#' Match data and metadata attribute names.
#' This function matches indices of column names in the data and attributeName's specified in metadata.
#' 
#' @usage 
#'   match_names(
#'     entity_list
#'     )
#' 
#' @param entity_list (list) A list object containing information on a single data entity in metajam output format.
#'
#' @return (vector) A numeric vector with row indices in attribute metadata to match column indices in data.


match_names <- function(entity_list) {

  # ---
  # get params
  cols <- colnames(entity_list[["data"]])
  cols_attr <- entity_list[["attribute_metadata"]][["attributeName"]]
  indices <- seq(1:length(cols))
  
  # ---
  # get indices of mismatched names or order
  mismatches <- which(!(cols == cols_attr))

  # ---
  # proceed only if there are mismatches
  if (length(mismatches) != 0) {
    
  # ---  
  # initialize 
    
  x <- c()
  for (i in 1:length(mismatches)) {
    
    # ---
    # fuzzy matching for matching indices, only where there wasn't an exact match earlier
    attr_index <- agrep(cols[mismatches][i], cols_attr[mismatches])
    
    # sub in matched index only if there's exactly one match
    if (length(attr_index) == 1) {
    x <- c(x, attr_index)
    } else {
      # otherwise, put in the exact same index we're looping through. FIXME: do something more intelligent here
      x <- c(x, mismatches[i])
    }
  }
  # insert matched indices back in
  indices[mismatches] <- indices[mismatches[x]]
  }

  # ---
  # return correct vector of indices
  return(indices)
  
}
