% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_package_download.R
\name{data_package_download}
\alias{data_package_download}
\title{Download a data package from the DataONE network}
\usage{
data_package_download(
    data.pkg.doi,
    download.dir = NULL
  )
}
\arguments{
\item{data.pkg.doi}{(character) DOI of a data package available in the DataONE network
(e.g. "doi:10.18739/A2DP3X").}

\item{download.dir}{(character) Directory to which the data package will be downloaded. 
Default is the temporary directory specified by 
\code{paste0(tempdir(), '/data_package')}.}
}
\value{
(data package) Data package directory with sub-directories for each data
  object, each of which contains tabular data and metadata files as well as
  an .xml version of the metadata.
}
\description{
This function downloads a data package available in the 
\href{https://search.dataone.org/data}{DataONE} network.
}
\details{
This function is a wrapper to \code{download_d1_data_pkg()} of the 
  \href{https://github.com/NCEAS/metajam}{metajam} package but outputs
  data object directories within a parent directory named after the data 
  package.
}
\examples{
\dontrun{
# Download data package to temporary directory -----------------------------

# Make function call
data_package_download('doi:10.18739/A2DP3X')

# View directory contents
dir(paste0(tempdir(), '/data_package'))

# Clean up
data_package_remove()

# Download data package to local directory -----------------------------

# Make function call
data_package_download(
  data.pkg.doi = 'doi:10.18739/A2DP3X',
  download.dir = '/Desktop/data_packages'
)

# View directory contents
dir('/Desktop/data_packages/AlSR200915')

# Clean up
data_package_remove('/Desktop/data_packages/AlSR200915')

}

}
