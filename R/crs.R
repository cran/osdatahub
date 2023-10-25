
#' Internal function to confirm that user input for a coordinate reference
#' system is one of the acceptable types and formatted correctly.
#' @keywords internal
#' @noRd
valid_crs <- function(x){

  valid <- FALSE

  sCRS <- EPSG$epsg
  nCRS <- EPSG$epsg_number

  if(missing(x)) stop('Please provide a CRS to check.', call. = FALSE)
  stopifnot(length(x) == 1L)

  if(is.character(x)){
    x <- tolower(x)

    if(!x %in% sCRS){
      stop(paste0('CRS must be one of ', paste(sCRS, collapse = ', ')),
           call. = FALSE)
    } else{
      valid <- TRUE
    }

  } else if(is.numeric(x)){
    if(!x %in% nCRS){
      stop(paste0('CRS must be one of ', paste(unique(nCRS), collapse = ', ')),
           call. = FALSE)
    } else{
      valid <- TRUE
    }

  } else{
    stop('CRS must be a valid string or numeric value.', call. = FALSE)
  }

  return(valid)
}


#' Convert the CRS to a URI.
#'
#' Given possible user-inputs to specify a CRS, check validity and then convert
#' the CRS labels into a URI accepted by the OS Data Hub.
#' @param x (character or numeric)  The CRS for the, either in the format
#'   "epsg:xxxx" or an EPSG number. e.g. British National Grid can be supplied
#'   as "epsg:27700" or 27700. Available CRS values are: EPSG:27700, EPSG:4326,
#'   EPSG:7405, EPSG:3857, and CRS84.
#' @param returnType (character) Should the URI to the CRS be returned
#'   (\code{'uri'}), the EPSG number (\code{'number'}), or the string value
#'   (\code{'code'})? The default is \code{'uri'}.
#'
#' @returns Character string of a URI to the CRS specification.
#'
#' @keywords internal
get_crs <- function(x, returnType = c('uri', 'number', 'code')){
  if(missing(x)) stop('Please provide a CRS to check.', call. = FALSE)
  stopifnot(length(x) == 1L)

  returnType <- match.arg(returnType)

  stopifnot(valid_crs(x))

  if(is.character(x)){
    x <- tolower(x)
    url <- EPSG[EPSG$epsg == x, 'url']
    number <- EPSG[EPSG$epsg == x, 'epsg_number']
    code <- EPSG[EPSG$epsg == x, 'epsg']

  } else if(is.numeric(x)){
    url <- EPSG[EPSG$epsg_number == x, 'url']
    number <- x
    code <- EPSG[EPSG$epsg_number == x, 'epsg']

  } else{
    stop('Unknown CRS.', call. = FALSE)
  }

  if(returnType == 'uri'){
    return(url)
  } else if(returnType == 'code'){
    return(code)
  } else if(returnType == 'number'){
    return(number)
  } else{
    return(NULL)
  }
}


#' Print the currently accepted EPSG codes.
#'
#' Convenience function primarily used internally by \code{osdatahub}.
#' @param ... Not currently used.
#' @returns (Invisible) Vector of character strings.
#'
#' @examples
#' list_crs()
#'
#' @export
list_crs <- function(...){
  print(EPSG$epsg)
  invisible(EPSG$epsg)
}

