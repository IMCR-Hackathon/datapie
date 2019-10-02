#' Download a data package from the DataONE network
#' 
#' This function downloads a data package available in the 
#' \href{https://search.dataone.org/data}{DataONE} network.
#' 
#' @usage
#'   data_package_download(
#'     data.pkg.doi,
#'     download.dir = NULL
#'   )
#' 
#' @param data.pkg.doi
#'   (character) DOI of a data package available in the DataONE network
#'   (e.g. "doi:10.18739/A2DP3X").
#' @param download.dir
#'   (character) Directory to which the data package will be downloaded. 
#'   Default is the temporary directory specified by 
#'   \code{paste0(tempdir(), '/data_package')}.
#'
#' @return
#'   (data package) Data package directory with sub-directories for each data
#'   object, each of which contains tabular data and metadata files as well as
#'   an .xml version of the metadata.
#'   
#' @details
#'   This function is a wrapper to \code{download_d1_data_pkg()} of the 
#'   \href{https://github.com/NCEAS/metajam}{metajam} package but outputs
#'   data object directories within a parent directory named after the data 
#'   package.
#'   
#' @export
#'
#' @examples
#' \dontrun{
#' # Download data package to temporary directory -----------------------------
#' 
#' # Make function call
#' data_package_download('doi:10.18739/A2DP3X')
#' 
#' # View directory contents
#' dir(paste0(tempdir(), '/data_package'))
#' 
#' # Clean up
#' data_package_remove()
#' 
#' # Download data package to local directory -----------------------------
#' 
#' # Make function call
#' data_package_download(
#'   data.pkg.doi = 'doi:10.18739/A2DP3X',
#'   download.dir = '/Desktop/data_packages'
#' )
#' 
#' # View directory contents
#' dir('/Desktop/data_packages/AlSR200915')
#' 
#' # Clean up
#' data_package_remove('/Desktop/data_packages/AlSR200915')
#' 
#' }
#' 

data_package_download <- function(data.pkg.doi, download.dir = NULL){
  
  # Create directory for data package -----------------------------------------
  
  # Create temporary directory if download.dir = NULL
  
  if (is.null(download.dir)){
    download.dir <- paste0(tempdir(), '/data_package')
    dir.create(download.dir)
  }
  
  # Error if download.dir doesn't exist
  
  if (!dir.exists(download.dir)){
    stop('Download directory does not exist.')
  }
  
  # Download data package -----------------------------------------------------
  
  message(paste0('Downloading data package to "', download.dir, '"'))

  # Download data package to download.dir
  
  pkg_dir_name <- try(
    unlist(
      metajam::download_d1_data_pkg(
        meta_obj = data.pkg.doi,
        path = download.dir
      )
    )
  )
  
  if (class(pkg_dir_name) == "try-error") {
    message(
      paste0("Sorry, datapie can't handle ", data.pkg.doi, " at this time.")
    )
    message("Done.")
    stop()
  }
  
  # Create parent directory for data packages downloaded to a user specified
  # directory. Otherwise the parent directory is the temporary directory 
  # specified by paste0(tempdir(), '/data_package').
  
  if (download.dir != paste0(tempdir(), '/data_package')){
    
    # Create parent directory
    
    parent_dir <- paste0(
      download.dir,
      '/',
      abbreviate(
        stringr::str_remove_all(
          stringr::str_remove(
            stringr::str_remove(
              pkg_dir_name[1],
              '__[:graph:]*'
            ),
            '([:graph:]*/)*'
          ),
          '_'
        ),
        10
      )
    )
    
    dir.create(parent_dir)
    
    # Move sub-directories to parent directory
    
    file.copy(
      from = pkg_dir_name,
      to = parent_dir,
      recursive = TRUE
    )
    
    unlink(pkg_dir_name, recursive = TRUE)
    
  }
  
}
