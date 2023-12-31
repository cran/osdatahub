% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/downloads_api.R
\name{list_os_datapackages}
\alias{list_os_datapackages}
\title{Retrieve information on premium OS data packages}
\usage{
list_os_datapackages(product_id, version_id, key = get_os_key(), ...)
}
\arguments{
\item{product_id}{(numeric or character) Retrieve information on a specific
data product. Optional.}

\item{version_id}{(numeric or character) Retrieve information on a specific
version of a data product. Optional and only available when
\code{product_id} has been specified.}

\item{key}{(character) OS API key. Default action is to search for an
environment variable using \code{get_os_key()}.}

\item{...}{Additional paramters. Not currently used.}
}
\value{
A \code{data.frame} or a \code{package_list}, which extends a
\code{data.frame}, containing the information on downloadable files from
the Downloads API.
}
\description{
Query the osdatahub Downloads API to gather information on available
downloads for a specific OS premium data package based on given filters.
}
\details{
The OS Downloads API assists with the discovery and download of OS
OpenData and OS premium data packages. This function is used for initial
listing and discovery of premium products. Use the product and version IDs
from this list to filter further or to initiate a download.

Before downloading a data package, it must be ordered online. See:
\url{https://osdatahub.os.uk/downloads/packages}.

When a \code{product_id} is not specified then all available data packages
are listed. The \code{version_id} filter can be used to find the specific
download, but this filter is only valid when a specific product has been
specified first.

For more information on the Downloads API, see
\url{https://osdatahub.os.uk/docs/downloads/technicalSpecification}.
}
\examples{
\dontrun{
# Retrieve a data.frame listing all OS Data Packages available.
# An API key is required and the packages must be ordered online first.
dp <- list_os_datapackages()

# Retrieve a specific data package.
# Note: 'product_id' will vary.
dp <- list_os_datapackages(product_id = 1234)
}

}
\seealso{
\code{\link[=download_os_datapackages]{download_os_datapackages()}}
}
