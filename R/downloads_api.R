
#' Retrieve information on OS OpenData Downloads
#'
#' Query the osdatahub Downloads API to gather information on available data
#' collections. An API key is not required to list OS OpenData.
#' @param product_id (character) Retrieve information on a specific data
#'   product. Optional.
#' @param file_name (character) Filter downloads to only include those with this
#'   file name. Optional.
#' @param file_format (character) Filter downloads to only include those with
#'   this format. Optional.
#' @param file_subformat (character) Filter downloads to only include those with
#'   this subformat. Optional.
#' @param area (character) Filter downloads for only this area. Use 'GB' for all
#'   Great Britain. Optional.
#' @param ... Additional paramters. Not currently used.
#'
#' @details The OS Downloads API assists with the discovery and download of OS
#'   OpenData and OS premium data packages. This function is used for initial
#'   listing and discovery of open data products. Use the product ID from this
#'   list to filter further or to initiate a download.
#'
#'   When a \code{product_id} is not specified then all available open data is
#'   listed. Additional filters (i.e. \code{file_name}, \code{file_format},
#'   \code{file_subformat}, \code{area}) can be used to find the specific
#'   download, but these filters are only valid when a specific product has been
#'   specified first.
#'
#'   The optional \code{area} filter is based on two-letter British National
#'   Grid tiles. Use 'GB' for all of Great Britain. Valid values area: GB, HP,
#'   HT, HU, HW, HX, HY, HZ, NA, NB, NC, ND, NF, NG, NH, NJ, NK, NL, NM, NN, NO,
#'   NR, NS, NT, NU, NW, NX, NY, NZ, OV, SD, SE, TA, SH, SJ, SK, TF, TG, SM, SN,
#'   SO, SP, TL, TM, SR, SS, ST, SU, TQ, TR, SV, SW, SX, SY, SZ, TV.
#'
#'   For more information on the Downloads API, see
#'   \url{https://osdatahub.os.uk/docs/downloads/technicalSpecification}.
#'
#' @returns A \code{data.frame} or a \code{product_list}, which extends a
#'   \code{data.frame}, containing the information on downloadable files from
#'   the Downloads API.
#'
#' @seealso [download_os_opendata()]
#'
#' @examples
#' \donttest{
#' # Retrieve a data.frame listing all OS OpenData products.
#' opendata <- list_os_opendata()
#' opendata[, c("name", "url")]
#' }
#'
#' @export
list_os_opendata <- function(product_id,
                             file_name,
                             file_format,
                             file_subformat,
                             area,
                             ...){

  # Set user-agent.
  ua <- httr::user_agent('osdatahub-r')

  # determine return type
  pl <- FALSE

  # Set up query parameters
  params <- list()

  # Check inputs.
  if(!missing(area)){
    if(!is.character(area) | !area %in% AREACODES$area){
      stop('`area` Argument must be a valid character value.',
           call. = FALSE)
    }
    params[['area']] <- area
  }

  if(!missing(file_name)){ params[['fileName']] <- file_name }
  if(!missing(file_format)){ params[['format']] <- file_format }
  if(!missing(file_subformat)){ params[['subformat']] <- file_subformat }

  # Build URL.
  DWNLD_ENDPOINT <- ENDPOINTS[ENDPOINTS$api == 'downloads', 'url']

  if(!missing(product_id)){
    if(length(product_id) > 1L){
      stop('Please provide only one product ID.', call. = )
    }

    url <- paste0(DWNLD_ENDPOINT, '/products/', product_id, '/downloads')
    pl <- TRUE  # returning a product list
  } else{
    url <- paste0(DWNLD_ENDPOINT, '/products')
  }

  # Update with parameters.
  url <- httr::modify_url(url,
                          query = params)

  # Query (key not needed for opendata).
  resp <- httr::GET(url,
                    ua)

  rc <- httr::status_code(resp)

  if (rc != 200) {
    stop(sprintf("OS Downloads API failed [%s] \nResponse: %s (%s)",
                 rc,
                 RESPONSECODES[RESPONSECODES$code == rc, 'description'],
                 RESPONSECODES[RESPONSECODES$code == rc, 'explanation']),
         call. = FALSE)
  }

  if(httr::http_type(resp) != 'application/json'){
    stop('API did not return json type.', call. = FALSE)
  }

  # Process response.
  parsed <- suppressMessages(jsonlite::fromJSON(httr::content(resp, as="text"),
                                                flatten = TRUE))

  # Update class to extend data.frame
  if(pl){
    class(parsed) <- c('product_list', class(parsed))
  }

  return(parsed)
}


#' Download OS OpenData Products
#'
#' Main function for downloading OS open data product files to your local
#' machine.
#' @param product A \code{product_list} object retrieved and filtered using
#'   \code{list_os_opendata}. Alternatively, a data product name as a character
#'   string.
#' @param file_name (character) Filter downloads to only include those with this
#'   file name. Optional.
#' @param file_format (character) Filter downloads to only include those with
#'   this format. Optional.
#' @param file_subformat (character) Filter downloads to only include those with
#'   this subformat. Optional and only used when \code{product} is a character
#'   string.
#' @param area (character) Filter downloads for only this area. Use 'GB' for all
#'   Great Britain. Optional.
#' @param output_dir Path to the directory where the downloaded files will be
#'   saved.
#' @param overwrite Boolean. Should existing files be overwritten? Default is
#'   \code{FALSE}.
#' @param ... Additional parameters. Not currently used.
#'
#' @details The OS Downloads API assists with the discovery and download of OS
#'   OpenData and OS Premium data packages. This function is used as the main
#'   step to download open data products to your local machine. It is designed
#'   to work best after \code{list_os_opendata} is first used to search and
#'   filter for the specific download product. The \code{product_list} returned
#'   by the listing step can be used as the input value to download the desired
#'   files. Alternatively, it is possible to supply a product name and filtering
#'   options based on file formats and areas.
#'
#'   The optional \code{area} filter is based on two-letter British National
#'   Grid tiles. Use 'GB' for all of Great Britain. Valid values area: GB, HP,
#'   HT, HU, HW, HX, HY, HZ, NA, NB, NC, ND, NF, NG, NH, NJ, NK, NL, NM, NN, NO,
#'   NR, NS, NT, NU, NW, NX, NY, NZ, OV, SD, SE, TA, SH, SJ, SK, TF, TG, SM, SN,
#'   SO, SP, TL, TM, SR, SS, ST, SU, TQ, TR, SV, SW, SX, SY, SZ, TV.
#'
#'   For more information on the Downloads API, see
#'   \url{https://osdatahub.os.uk/docs/downloads/technicalSpecification}.
#'
#' @returns Silently returns the directory where the downloaded files are
#'   stored.
#'
#' @seealso [list_os_opendata()]
#'
#' @examples
#' \donttest{
#' # Search and filter available open products.
#' prod_list <- list_os_opendata('OpenGreenSpace',
#'                               file_format = 'GeoPackage',
#'                               area = 'GB')
#'
#' # Use the product list to initiate a download.
#' download_os_opendata(prod_list, output_dir = tempdir())
#'
#' # Combine search and download.
#' # Be sure to know the products to avoid downloading more data than desired.
#' download_os_opendata(product = 'OpenGreenSpace',
#'                      file_format = 'GeoPackage',
#'                      area = 'GB',
#'                      output_dir = tempdir())
#' }
#'
#' @export
download_os_opendata <- function(product,
                                 ...){

  UseMethod('download_os_opendata', product)
}


#' @rdname download_os_opendata
#' @export
download_os_opendata.product_list <- function(product,
                                              file_name,
                                              file_format,
                                              area,
                                              output_dir,
                                              overwrite = FALSE,
                                              ...){

  # User has provided a 'product_list' object from `list_os_opendata`.

  # Apply optional filters to the product list.
  if(!missing(file_name)){
    product <- product[product$fileName %in% file_name, ]
  }

  if(missing(output_dir)){
    stop('Please provide a path to an output directory.', call. = FALSE)
  }

  if(!missing(file_format)){
    product <- product[product$format %in% file_format, ]
  }

  if(!missing(area)){
    if(!is.character(area) | !area %in% AREACODES$area){
      stop('`area` Argument must be a valid character value.',
           call. = FALSE)
    }
    product <- product[product$area %in% area, ]
  }

  # Check output location.
  if(!dir.exists(output_dir)){
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }

  for(i in 1:nrow(product)){
    file_name <- product[i, 'fileName']
    output_path <- file.path(output_dir, file_name)

    url <- product[i, 'url']

    tryCatch({
      if(file.exists(output_path) & !overwrite){
        message(paste0(file_name, ' already exists. Set `overwrite` to TRUE.'))
        next

      } else{
        val <- utils::download.file(url = url,
                                    destfile = output_path,
                                    mode = 'wb',
                                    quiet = FALSE,
                                    method = 'auto')

        # Check output
        if(val != 0){
          stop('Error downloading file.', call. = FALSE)
        }

        filematch <- product[i, 'md5'] == tools::md5sum(output_path)
        if(!filematch){
          stop('MD5 Checksums do not match.', call. = FALSE)
        }
      }

    },
    warning = function(w) print(w),
    error = function(e) print(paste('Download error:', e)))

  }

  invisible(output_dir)
}


#' @rdname download_os_opendata
#' @export
download_os_opendata.character <- function(product,
                                           file_name,
                                           file_format,
                                           file_subformat,
                                           area,
                                           output_dir,
                                           overwrite = FALSE,
                                           ...){

  # User has provided the name of a product to query and then download.
  pl <- list_os_opendata(product,
                         file_name = file_name,
                         file_format = file_format,
                         file_subformat = file_subformat,
                         area = area,
                         ...)

  # Initiate the download using the product list.
  dwnld <- download_os_opendata(pl,
                                output_dir = output_dir,
                                overwrite = overwrite,
                                ...)

  invisible(dwnld)
}


#' Retrieve information on premium OS data packages
#'
#' Query the osdatahub Downloads API to gather information on available
#' downloads for a specific OS premium data package based on given filters.
#' @param product_id (numeric or character) Retrieve information on a specific
#'   data product. Optional.
#' @param version_id (numeric or character) Retrieve information on a specific
#'   version of a data product. Optional and only available when
#'   \code{product_id} has been specified.
#' @param key (character) OS API key. Default action is to search for an
#'   environment variable using \code{get_os_key()}.
#' @param ... Additional paramters. Not currently used.
#'
#' @details The OS Downloads API assists with the discovery and download of OS
#'   OpenData and OS premium data packages. This function is used for initial
#'   listing and discovery of premium products. Use the product and version IDs
#'   from this list to filter further or to initiate a download.
#'
#'   Before downloading a data package, it must be ordered online. See:
#'   \url{https://osdatahub.os.uk/downloads/packages}.
#'
#'   When a \code{product_id} is not specified then all available data packages
#'   are listed. The \code{version_id} filter can be used to find the specific
#'   download, but this filter is only valid when a specific product has been
#'   specified first.
#'
#'   For more information on the Downloads API, see
#'   \url{https://osdatahub.os.uk/docs/downloads/technicalSpecification}.
#'
#' @returns A \code{data.frame} or a \code{package_list}, which extends a
#'   \code{data.frame}, containing the information on downloadable files from
#'   the Downloads API.
#'
#' @seealso [download_os_datapackages()]
#'
#' @examples
#' \dontrun{
#' # Retrieve a data.frame listing all OS Data Packages available.
#' # An API key is required and the packages must be ordered online first.
#' dp <- list_os_datapackages()
#'
#' # Retrieve a specific data package.
#' # Note: 'product_id' will vary.
#' dp <- list_os_datapackages(product_id = 1234)
#' }
#'
#' @export
list_os_datapackages <- function(product_id,
                                 version_id,
                                 key = get_os_key(),
                                 ...){

  # Set user-agent.
  ua <- httr::user_agent('osdatahub-r')

  # determine return type
  pl <- FALSE

  # Build URL.
  DWNLD_ENDPOINT <- ENDPOINTS[ENDPOINTS$api == 'downloads', 'url']

  if(!missing(product_id)){
    if(length(product_id) > 1L){
      stop('Please provide only one product ID to filter.', call. = FALSE)
    }

    url <- paste0(DWNLD_ENDPOINT, '/dataPackages/', product_id)

    if(!missing(version_id)){
      if(length(version_id) > 1L){
        stop('Please provide only one version ID to filter.', call. = FALSE)
      }

      url <- paste0(url, '/versions/', version_id)
      pl <- TRUE  # returning a data package list
    }

  } else{
    url <- paste0(DWNLD_ENDPOINT, '/dataPackages')
  }

  # Query (key not needed for opendata).
  resp <- httr::GET(url,
                    ua,
                    httr::add_headers(key = key,
                                      Accept = 'application/json'))

  rc <- httr::status_code(resp)

  if (rc != 200) {
    stop(sprintf("OS Downloads API failed [%s] \nResponse: %s (%s)",
                 rc,
                 RESPONSECODES[RESPONSECODES$code == rc, 'description'],
                 RESPONSECODES[RESPONSECODES$code == rc, 'explanation']),
         call. = FALSE)
  }

  if(httr::http_type(resp) != 'application/json'){
    stop('API did not return json type.', call. = FALSE)
  }

  # Process response.
  parsed <- suppressMessages(jsonlite::fromJSON(httr::content(resp, as="text"),
                                                flatten = TRUE))
  parsed <- data.frame(parsed)

  # Check for presence of data packages.
  if(nrow(parsed) == 0L){
    stop('No premium data packages available.
         Make sure that you first ordered a data package at:
         https://osdatahub.os.uk/downloads/premium', call. = FALSE)
  }

  # Update class to extend data.frame
  if(pl){
    class(parsed) <- c('package_list', class(parsed))
  }

  return(parsed)
}


#' Download OS premium data packages
#'
#' Main function for downloading OS data packages to your local machine.
#' @param product A \code{product_list} object retrieved and filtered using
#'   \code{list_os_datapackages}. Alternatively, a \code{data.frame} object or
#'   integer or character string of a product ID that can be filtered further.
#' @param version (numeric or character) Retrieve information on a specific
#'   version(s) of a data product. Required when \code{product} is a
#'   \code{data.frame}.
#' @param file_name (character) Filter downloads to only include those with this
#'   file name. Optional.
#' @param output_dir Path to the directory where the downloaded files will be
#'   saved.
#' @param overwrite Boolean. Should existing files be overwritten? Default is
#'   \code{FALSE}.
#' @param key (character) OS API key. Default action is to search for an
#'   environment variable using \code{get_os_key()}.
#' @param ... Additional parameters. Not currently used.
#'
#' @details The OS Downloads API assists with the discovery and download of OS
#'   OpenData and OS premium data packages. This function is used as the main
#'   step to download data packages to your local machine. It is designed to
#'   work best after \code{list_os_datapackages} is first used to search and
#'   filter for the specific download product. The \code{package_list} returned
#'   by the listing step can be used as the input value to download the desired
#'   files. Alternatively, it is possible to supply a product and version IDs
#'   directly when they are already known.
#'
#'   Before downloading a data package, it must be ordered online. See:
#'   \url{https://osdatahub.os.uk/downloads/packages}.
#'
#'   For more information on the Downloads API, see
#'   \url{https://osdatahub.os.uk/docs/downloads/technicalSpecification}.
#'
#' @returns Silently returns the directory where the downloaded files are
#'   stored.
#'
#' @seealso list_os_datapackages
#'
#' @examples
#' \dontrun{
#' # Search and filter available open products.
#' pkg_list <- list_os_datapackages()
#'
#' # Use the package list to initiate a download.
#' # Note: 'version' will vary.
#' download_os_datapackages(pkg_list, version = 123, output_dir = tempdir())
#' }
#'
#' @export
download_os_datapackages <- function(product,
                                     ...){

  UseMethod('download_os_datapackages', product)
}


#' @name download_os_datapackages
#' @export
download_os_datapackages.package_list <- function(product,
                                                  file_name,
                                                  output_dir,
                                                  overwrite = FALSE,
                                                  key = get_os_key(),
                                                  ...){
  # User has provided a 'package' object from `list_os_datapackages`.

  # Apply optional filters to the product list.
  if(!missing(file_name)){
    product <- product[product$downloads.fileName %in% file_name, ]
  }

  if(missing(output_dir)){
    stop('Please provide a path to an output directory.', call. = FALSE)
  }

  # Check output location.
  if(!dir.exists(output_dir)){
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }

  for(i in 1:nrow(product)){
    file_name <- product[i, 'downloads.fileName']
    output_path <- file.path(output_dir, file_name)

    url <- product[i, 'downloads.url']
    url <- paste0(url, '&key=', key)

    tryCatch({
      if(file.exists(output_path) & !overwrite){
        message(paste0(file_name, ' already exists. Set `overwrite` to TRUE.'))
        next

      } else{
        val <- utils::download.file(url = url,
                                    destfile = output_path,
                                    method = 'auto',
                                    quiet = FALSE,
                                    mode = 'wb',
                                    headers = c('key' = key)
                                   )

        # Check output
        if(val != 0){
          stop('Error downloading file.', call. = FALSE)
        }

        filematch <- product[i, 'downloads.md5'] == tools::md5sum(output_path)
        if(!filematch){
          stop('MD5 Checksums do not match.', call. = FALSE)
        }
      }

    },
    warning = function(w) print(w),
    error = function(e) print(paste('Download error:', e)))

  }

  invisible(output_dir)
}


#' @name download_os_datapackages
#' @export
download_os_datapackages.data.frame <- function(product,
                                                version,
                                                file_name,
                                                output_dir,
                                                overwrite = FALSE,
                                                key = get_os_key(),
                                                ...){
  if(missing(version)){
    if(nrow(product) > 1L){
      stop('Please provide one version number to download.', call. = FALSE)
    } else{
      product$version <- product[1, 'versions.id']
    }
  }

  # Generate a package list of desired products.
  pl <- list_os_datapackages(product_id = product[1, 'id'],
                             version_id = product[1, 'version'],
                             key = key)

  # Initiate the download of the package list.
  dwnld <- download_os_datapackages(pl,
                                    file_name = file_name,
                                    output_dir = output_dir,
                                    overwrite = overwrite,
                                    key = key,
                                    ...)
  invisible(dwnld)
}


#' @name download_os_datapackages
#' @export
download_os_datapackages.numeric <- function(product,
                                             version,
                                             file_name,
                                             output_dir,
                                             overwrite = FALSE,
                                             key = get_os_key(),
                                             ...){

  # Generate a package list of desired products.
  pl <- list_os_datapackages(product_id = product,
                             version_id = version,
                             key = key)

  # Initiate the download of the package list.
  dwnld <- download_os_datapackages(pl,
                                    file_name = file_name,
                                    output_dir = output_dir,
                                    overwrite = overwrite,
                                    key = key,
                                    ...)
  invisible(dwnld)
}

