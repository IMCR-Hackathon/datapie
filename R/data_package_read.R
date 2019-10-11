#' Read a data package into R
#' 
#' This function reads a data package downloaded with the function
#' \code{data_package_read()} to the R environment as a list object.
#' 
#' @usage
#'   data_package_read(
#'     data.pkg.path = NULL
#'   )
#' 
#' @param data.pkg.path
#'   (character) Directory where a data package stored. Default (NULL) is
#'   the temporary data package directory defined by
#'   \code{paste0(tempdir(), 'data_package')}.
#'
#' @return
#'   (list) List of tibbles containing data and metadata. Each list object is
#'   named after the data object file name.
#'   
#' @details
#'   This function is a wrapper to \code{read_d1_files()} function of the 
#'   \href{https://github.com/NCEAS/metajam}{metajam} package but outputs an
#'   object with a slightly different format.
#'   
#' @export
#'
#' @examples
#' \dontrun{
#' # Read data package from temporary directory -------------------------------
#' 
#' # Download data package to temporary directory
#' data_package_download(data.pkg.doi = 'doi:10.18739/A2DP3X')
#' 
#' # Read data package into R
#' pkg <- data_package_read()
#' 
#' # View data package contents
#' View(pkg)
#' 
#' # Clean up
#' data_package_remove()
#' 
#' # Read data package from local directory -----------------------------------
#' 
#' # Download data package to local directory
#' data_package_download(
#'   data.pkg.doi = 'doi:10.18739/A2DP3X',
#'   download.dir = '/Desktop/data_packages'
#' )
#' 
#' # Read data package into R
#' pkg <- data_package_read(
#'   data.pkg.path = '/Desktop/data_packages/AlSR200915'
#' )
#' 
#' # View data package contents
#' View(pkg)
#' 
#' # Clean up
#' data_package_remove('/Desktop/data_packages/AlSR200915')
#' 
#' }
#' 
#' @importFrom data.table fread
#' @importFrom readxl read_excel

data_package_read <- function(data.pkg.path = NULL){
  
  # Check arguments and parameterize ------------------------------------------
  
  # Use temporary directory if data.pkg.path = NULL
  
  if (is.null(data.pkg.path)){
    data.pkg.path <- paste0(tempdir(), '/data_package')
  }
  
  # Error if data.pkg.path doesn't exist
  
  if (!dir.exists(data.pkg.path)){
    stop("Data package directory doesn't exist.")
  }
  
  # Read data package ---------------------------------------------------------

  message(paste0('Reading data package from "', data.pkg.path, '"'))
  
  # Get paths of each data object (a package may contain more than one data 
  # object and associated metadata stored within an associated directory).
  
  pkg_dir_full_paths <- list.dirs(data.pkg.path, recursive = FALSE)
  
  # parse file extensions to get read function to use
  read_fnc <- guess_read_fnc(pkg_dir_full_paths)
  
  # remove objects where data format is not supported
  will_read <- !grepl("not supported", read_fnc)
  message(paste(names(read_fnc[!will_read]), read_fnc[!will_read], sep = " is ", collapse = "\n"))
  pkg_dir_full_paths <- pkg_dir_full_paths[will_read]
  read_fnc <- read_fnc[will_read]

    # Read objects
  output <- lapply(
    seq_along(pkg_dir_full_paths),
    function(i) {
      tryCatch(metajam::read_d1_files(pkg_dir_full_paths[i], fnc = read_fnc[i]),
             error = function(e) {
               attr(e, "problems") <- e
               return(e)
               })
    }
  )
  
  # since data.table::fread reads in blank cells in character columns as ""
  # replace these with NAs
  
  for (i in seq_along(output)) {
    if (read_fnc[i] == "data.table::fread") {
     output[[i]][["data"]][output[[i]][["data"]] == ""] <- NA
    }
  }
  
  # Use object names for the output list
  
  names(output) <- paste0(
    stringr::str_replace(
      stringr::str_remove(
        pkg_dir_full_paths,
        '^[:graph:]*/'
      ),
      '__',
      '.'
    )
  )
  # Remove unreadable objects (e.g. xlsx)

  fnames_out <- rep(NA_character_, length(output))
  for (i in seq_along(output)){
    if (!is.null(attr(output[[i]]$data, which = 'problems')) &&
        (ncol(output[[i]]$data) == 1)){
      fnames_out[i] <- names(output)[i]
      output[[i]] <- NULL
    }
  }

  # Use missing codes if available; substitute with NAs

  output <- lapply(output, use_missing_code)
  
  # Return --------------------------------------------------------------------
  
  if (any(!is.na(fnames_out))){
    fnames_out <- fnames_out[!is.na(fnames_out)]
    message(
      paste0(
        'Sorry, parsing errors were encountered with "',
        fnames_out,
        '".',
        collapse = '<br>'
      )
    )
  }
  
  message('Done.')
  output
  
}

# ---
#' Determine which read function to use on data
#' @param pkg_dir_name (character) Character vector of full paths to directories containing data and metadata 
#' 
#' @return (character) Named character vector of recommended read functions. Indices correspond to directory order as supplied. Names are the directory names. 
#' @importFrom readxl excel_sheets

guess_read_fnc <- function(pkg_dir_full_paths) {
  file_ext <- sub(".*__", "", pkg_dir_full_paths)
  pkg_dir <- sub(".*/", "", pkg_dir_full_paths)
  read_fnc <- c()
  for (i in 1:length(pkg_dir_full_paths)) {
    if (grepl("csv|tsv|txt|tab|xls", file_ext[i])) {
      if (grepl("xls", file_ext[i])) {
        actual_excel <-
          list.files(pkg_dir_full_paths[i], full.names = T)[sapply(list.files(pkg_dir_full_paths[i]), function(x)
            grepl("\\.xls", x))]
        no_sheets <- length(readxl::excel_sheets(actual_excel))
        if (no_sheets == 1)
          read_fnc[i] <- "readxl::read_excel"
        else
          read_fnc[i] <- "not supported: Excel file has more than one sheet"
      } else
        read_fnc[i] <- "data.table::fread"
    } else
      read_fnc[i] <- "not supported: data not in delimited/Excel format"
  }
  names(read_fnc) <- pkg_dir
  return(read_fnc)
}

