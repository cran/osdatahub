
#' Query the OS NGD Features API
#'
#' Retrieve features from a given Collection of the National Geographic
#' Database in the Ordnance Survey Data Hub.
#' @param x Object defining the query parameters, including feature IDs,
#'   extents, or spatial objects from which extents can be determined. If
#'   \code{x} is \code{NULL} or missing with other options specified by name,
#'   then the first \code{max_results} of the collection will be returned.
#' @param collection (character) The name of the NGD Collection to query
#'   (required). See \code{list_ngd_collections()}.
#' @param crs (character or numeric)  The CRS for the returned features, either
#'   in the format "epsg:xxxx" or an EPSG number. e.g. British National Grid can
#'   be supplied as "epsg:27700" or 27700. Available CRS values are: EPSG:27700,
#'   EPSG:4326, EPSG:7405, EPSG:3857, and CRS84. Defaults to CRS84.
#' @param start_datetime (datetime or string)  Selects features that have a
#'   temporal property after the given start time. If you want to query a single
#'   timestamp, provide the same value to both \code{start_datetime} and
#'   \code{end_datetime}.
#' @param end_datetime (datetime or string) Selects features that have a
#'   temporal property before the given end time. If you want to query a single
#'   timestamp, provide the same value to both \code{start_datetime} and
#'   \code{end_datetime}.
#' @param cql_filter (character) A filter query in CQL format. More information
#'   about supported CQL operators can be found at
#'   \url{https://osdatahub.os.uk/docs/ofa/technicalSpecification}.
#' @param filter_crs (character or numeric) The CRS for a given CQL query (if
#'   required), either in the format “epsg:xxxx” or an epsg number. e.g. British
#'   National Grid can be supplied as “epsg:27700” or 27700 Available CRS values
#'   are: EPSG:27700, EPSG:4326, EPSG:7405, EPSG:3857, and CRS84. Defaults to
#'   CRS84.
#' @param max_results (numeric) The maximum number of features to return.
#'   Default is 100 which is the max return per page from the Data Hub.
#' @param offset (numeric) The offset number skips past the specified number of
#'   features in the collection. Used to page through results. Default is 0.
#' @param key (character) OS API key. Default action is to search for an
#'   environment variable using \code{get_os_key()}.
#' @param returnType (character) Return the query results as the raw
#'   \code{'geojson'}, a nested \code{'list'} object containing the returns,
#'   or convert them into Simple Features and return an object of class
#'   \code{'sf'}.
#' @param ... Additional parameters (not currently used).
#'
#' @details The value of \code{x} determines the type of query that is executed
#'   against the NGD API. When \code{x} is missing or set to \code{NULL} the
#'   first n=\code{max_results} features are returned. If a character string of
#'   an OSID is supplied as \code{x}, then that one feature from the collection
#'   will be returned.
#'
#'   When \code{x} is present \code{query_ngd()} will attempt to derive an
#'   extent from it. The \code{extent_from_*} family of functions are used and
#'   can be passed to \code{query_ngd} as a more verbose option. The one
#'   exception to this, \code{extent_from_grid_ref} must be used to create an
#'   extent and query a BNG grid reference.
#'
#'   The \code{start_datetime} and \code{end_datetime} parameters specify a
#'   valid date-time with UTC time zone (Z). Leave either empty to specify an
#'   open start/end interval. Only features that have a temporal geometry
#'   ('versionavailablefromdate' or 'versionavailabletodate') that intersect the
#'   value in the datetime parameter are selected. Example
#'   '2021-12-12T13:20:50Z'.
#'
#'   More information on the structure and data in the NGD is available from:
#'   \url{https://osngd.gitbook.io/osngd/}. Technical details on the NGD API are
#'   documented on the Data Hub:
#'   \url{https://osdatahub.os.uk/docs/ofa/technicalSpecification}.
#'
#' @returns A GeoJSON string with the results of the API query, a list object,
#'   or an object of class \code{sf} based on the \code{returnType} parameter.
#'
#' @seealso [extent]
#'
#' @examplesIf has_os_key()
#' # Return the first 50 features in the collection.
#' results <- query_ngd(collection = 'bld-fts-buildingline-1', max_results = 50)
#'
#' # Return the most recent representation of a feature ID.
#' results <- query_ngd('0000013e-5fed-447d-a627-dae6fb215138',
#'                      collection = 'bld-fts-buildingline-1')
#'
#' # Use a BNG reference to define a query extent.
#' results <- query_ngd(extent_from_bng("SU3715"),
#'                      collection = 'bld-fts-buildingpart-1')
#'
#' # Add a temporal filter to query.
#' results <- query_ngd(collection = 'bld-fts-buildingline-1',
#'                      max_results = 50,
#'                      start_datetime = '2021-12-12 13:20:50')
#'
#' @import httr
#' @import geojsonsf
#' @import jsonlite
#' @export
query_ngd <- function(x, ...){

  if(missing(x)) x <- NULL  # Trick R to overload without a query option.
  UseMethod('query_ngd', x)
}


#' @noRd
#' @export
query_ngd.NULL <- function(x,
                           collection,
                           crs = 'crs84',
                           start_datetime,
                           end_datetime,
                           cql_filter,
                           filter_crs,
                           max_results = 100,
                           offset = 0,
                           key = get_os_key(),
                           returnType = c('geojson', 'list', 'sf'),
                           ...){

  # The user has not supplied information to `x`. This is a simple query to take
  # the first `max_results` from the collection. This method will only be
  # reached if the other arguments are supplied with names. Otherwise there will
  # likely be an error due to trying to convert a collection character string to
  # a filter.

  # Validate parameters.
  params <- list()

  if(missing(collection)){
    stop('Missing collection, please check `list_ngd_collections()`.',
         call. = FALSE)
  }

  stopifnot(valid_crs(crs))
  nCRS <- get_crs(crs, returnType = 'number')
  crs <- get_crs(crs)
  params[['crs']] <- crs

  returnType <- match.arg(returnType)

  # Date-time filters.
  if(!missing(start_datetime) | !missing(end_datetime)){
    if(!missing(start_datetime)){
      sdt <- as.POSIXct(start_datetime, 'GMT')
      sdt_iso <- strftime(sdt , "%Y-%m-%dT%H:%M:%SZ")
    } else{
      sdt_iso <- '..'
    }

    if(!missing(end_datetime)){
      edt <- as.POSIXct(end_datetime, 'GMT')
      edt_iso <- strftime(edt , "%Y-%m-%dT%H:%M:%SZ")
    } else{
      edt_iso <- '..'
    }

    if(!missing(start_datetime) & !missing(end_datetime)){
      if(sdt > edt){
        stop('Start time must be before end time.', .call = FALSE)
      }
    }

    params[['datetime']] <- paste(sdt_iso, edt_iso, sep = '/')
  }

  # Set filters.
  if(!missing(cql_filter)){
    if(!missing(filter_crs)){
      filter_crs <- get_crs(filter_crs)
    } else{
      filter_crs <- crs
    }

    params[['filter']] <- cql_filter
    params[['filter-crs']] <- filter_crs
  }

  # Execute query.
  res <- ngd_feature_query(key = key,
                           collection = collection,
                           nCRS = nCRS,
                           offset = offset,
                           max_results = max_results,
                           params = params,
                           returnType = returnType)

  return(res)
}


#' @rdname query_ngd
#' @export
query_ngd.character <- function(x,
                                collection,
                                crs = 'crs84',
                                key = get_os_key(),
                                returnType = c('geojson', 'list', 'sf'),
                                ...){

  # Unlike other possible queries, this one can only return the most recent
  # representation of a single feature. It is queried from the collection based
  # on OSID. Therefore, filtering options are limited and, if supplied, are
  # ignored (silently falling into `...`).

  # Validate parameters.
  params <- list()

  if(length(x) > 1L){  # Multiple OSIDs received.
    stop('Submit only one OSID to query.', call. = FALSE)
  }

  stopifnot(valid_crs(crs))
  nCRS <- get_crs(crs, returnType = 'number')
  crs <- get_crs(crs)
  params[['crs']] <- crs

  if(missing(collection)){
    stop('Missing collection, please check `list_ngd_collections()`.',
         call. = FALSE)
  }

  returnType <- match.arg(returnType)

  # Execute query.
  res <- ngd_feature_query(key = key,
                           item = x,
                           collection = collection,
                           nCRS = nCRS,
                           offset = 0,
                           max_results = 1,
                           params = params,
                           returnType = returnType)

  return(res)
}


#' @rdname query_ngd
#' @export
query_ngd.qExtent <- function(x,
                              collection,
                              crs = 'crs84',
                              start_datetime,
                              end_datetime,
                              cql_filter,
                              filter_crs,
                              max_results = 100,
                              offset = 0,
                              key = get_os_key(),
                              returnType = c('geojson', 'list', 'sf'),
                              ...){

  # Validate parameters.
  params <- list()

  stopifnot(valid_crs(crs))
  nCRS <- get_crs(crs, returnType = 'number')
  crs <- get_crs(crs)
  params[['crs']] <- crs

  if(missing(collection)){
    stop('Missing collection, please check `list_ngd_collections()`.',
         call. = FALSE)
  }

  returnType <- match.arg(returnType)

  # Date-time filters.
  if(!missing(start_datetime) | !missing(end_datetime)){
    if(!missing(start_datetime)){
      sdt <- as.POSIXct(start_datetime, 'GMT')
      sdt_iso <- strftime(sdt , "%Y-%m-%dT%H:%M:%SZ")
    } else{
      sdt_iso <- '..'
    }

    if(!missing(end_datetime)){
      edt <- as.POSIXct(end_datetime, 'GMT')
      edt_iso <- strftime(edt , "%Y-%m-%dT%H:%M:%SZ")
    } else{
      edt_iso <- '..'
    }

    if(!missing(start_datetime) & !missing(end_datetime)){
      if(sdt > edt){
        stop('Start time must be before end time.', .call = FALSE)
      }
    }

    params[['datetime']] <- paste(sdt_iso, edt_iso, sep = '/')
  }

  # Build extent filter
  bbox_filter <- paste0('INTERSECTS(geometry, ', poly_from_extent(x), ')')

  # Combine all filters.
  if(!missing(cql_filter)){
    if(!missing(filter_crs)){
      c1 <- x$crs
      c1 <- get_crs(c1)

      c2 <- get_crs(filter_crs)
      if(c1 != c2){
        stop('When passing an extent and a filter the `filter_crs` must match.',
             call. = FALSE)
      }
    } else{
      filter_crs <- get_crs(x$crs)
    }

    cql_filter <- paste0(cql_filter, ' AND ', bbox_filter)

  } else{
    cql_filter <- bbox_filter
    filter_crs <- get_crs(x$crs)
  }

  if(cql_filter != ''){
    params[['filter']] <- cql_filter
    params[['filter-crs']] <- filter_crs
  }

  # Execute query.
  res <- ngd_feature_query(key = key,
                           collection = collection,
                           nCRS = nCRS,
                           offset = offset,
                           max_results = max_results,
                           params = params,
                           returnType = returnType)

  return(res)
}


#' @rdname query_ngd
#' @export
query_ngd.geos_geometry <- function(x,
                                    collection,
                                    crs = 'crs84',
                                    start_datetime,
                                    end_datetime,
                                    cql_filter,
                                    filter_crs,
                                    max_results = 100,
                                    offset = 0,
                                    key = get_os_key(),
                                    returnType = c('geojson', 'list', 'sf'),
                                    ...){
  # Convert to an extent.
  x <- extent_from_polygon(x, crs = attr(x, 'crs'))

  # Re-execute.
  res <- query_ngd(x, collection, crs,
                   start_datetime, end_datetime,
                   cql_filter, filter_crs,
                   max_results, offset,
                   key, returnType, ...)

  return(res)
}


#' @rdname query_ngd
#' @export
query_ngd.sf <- function(x,
                         collection,
                         crs = 'crs84',
                         start_datetime,
                         end_datetime,
                         cql_filter,
                         filter_crs,
                         max_results = 100,
                         offset = 0,
                         key = get_os_key(),
                         returnType = c('geojson', 'list', 'sf'),
                         ...){
  # Convert to an extent.
  x <- extent_from_polygon(x)

  # Re-execute.
  res <- query_ngd(x, collection, crs,
                   start_datetime, end_datetime,
                   cql_filter, filter_crs,
                   max_results, offset,
                   key, returnType, ...)

  return(res)
}


#' @rdname query_ngd
#' @export
query_ngd.sfc <- function(x,
                          collection,
                          crs = 'crs84',
                          start_datetime,
                          end_datetime,
                          cql_filter,
                          filter_crs,
                          max_results = 100,
                          offset = 0,
                          key = get_os_key(),
                          returnType = c('geojson', 'list', 'sf'),
                          ...){
  # Convert to an extent.
  x <- extent_from_polygon(x)

  # Re-execute.
  res <- query_ngd(x, collection, crs,
                   start_datetime, end_datetime,
                   cql_filter, filter_crs,
                   max_results, offset,
                   key, returnType, ...)

  return(res)
}

## TODO ## Add additional query formats.

# query_ngd.bbox <- function(x, key = get_os_key(), ...){
#
# }
#
# query_ngd.raster <- function(x, key = get_os_key(), ...){
#
# }
#
# query_ngd.Extent <- function(x, key = get_os_key(), ...){
#
# }


#' Internal function for querying the NGD API
#'
#' Primary helper function for executing queries to the NGD API on the OS Data
#' Hub.
#' @param key OS API key.
#' @param item OSID of a single feature to query.
#' @param collection (character) The name of the NGD Collection to query.
#' @param nCRS Numeric version of coordinate reference system information.
#' @param offset (numeric) The offset number skips past the specified number of
#'   features in the collection.
#' @param max_results (numeric) The maximum number of features to return.
#' @param params List of queryables to filter.
#' @param returnType (character) Return the query results as the raw
#'   \code{'geojson'}, a nested \code{'list'} object containing the returns,
#'   or convert them into Simple Features and return an object of class
#'   \code{'sf'}.
#'
#' @returns A GeoJSON string with the results of the API query, a list object,
#'   or an object of class \code{sf} based on the \code{returnType} parameter.
#'
#' @examples
#' \dontrun{
#' ngd_feature_query(key, item = NULL,'bld-fts-buildingline-1',
#'                   27700, 0, 100, params, 'geojson')
#' }
#'
#' @keywords Internal
#' @noRd
ngd_feature_query <- function(key, item = NULL, collection, nCRS,
                              offset, max_results,
                              params, returnType){

  # Set user-agent.
  ua <- httr::user_agent('osdatahub-r')

  # Build base URL.
  NGD_ENDPOINT <- ENDPOINTS[ENDPOINTS$api == 'ngd', 'url']

  if(max_results <= 0){
    stop('`max_results` must be greater than 0.', call. = FALSE)
  }

  # Respect throttle limits (600 requests per minute)
  if(max_results >= 60000){
    waits <- (60 / 599)
  } else{
    waits <- 0.0
  }

  # Query loop.
  n_required <- max_results
  data <- list()

  while(n_required > 0){
    limit <- min(n_required, 100)
    offset <- max(offset, data[['numberReturned']])

    # Update the URL offset for each loop as needed.
    if(!is.null(item)){
      url <- paste(NGD_ENDPOINT, collection, 'items', item, sep = '/')
      max_results <- 1

      # Remove invalid params for an item query.
      params[['limit']] <- NULL
      params[['offset']] <- NULL
    } else{
      url <- paste(NGD_ENDPOINT, collection, 'items/', sep = '/')
      params[['limit']] <- limit
      params[['offset']] <- offset
    }

    # Update with new offset/limit.
    url <- httr::modify_url(url,
                            query = params)

    # Connect to API.
    resp <- httr::GET(url,
                      ua,
                      httr::add_headers(key = key,
                                        Accept = 'application/geo+json'))

    rc <- httr::status_code(resp)

    if (rc != 200) {
      stop(sprintf("OS NGD API failed [%s]\nResponse: %s (%s)",
                   rc,
                   RESPONSECODES[RESPONSECODES$code == rc, 'description'],
                   RESPONSECODES[RESPONSECODES$code == rc, 'explanation']),
           call. = FALSE)
    }

    if(httr::http_type(resp) != 'application/geo+json'){
      stop('API did not return geo+json type.', call. = FALSE)
    }

    # Get query results.
    resp_txt <- suppressMessages(httr::content(resp, 'text'))
    # Extract GeoJSON + Extra members with return information.
    jsonlist <- jsonlite::parse_json(resp_txt)
    # Count features
    nReturned <- jsonlist[['numberReturned']]

    # store properties
    links <- do.call(rbind.data.frame,  # Combine nested lists
                     lapply(jsonlist$links,
                            FUN = function(l){ data.frame(l) }))

    # Names of features
    if('features' %in% names(jsonlist)){
      nms <- names(jsonlist[["features"]][[1]][["properties"]])
    } else{
      nms <- names(jsonlist[['properties']])
    }

    # Collect features
    parsed <- geojsonsf::geojson_sf(resp_txt)
    parsed <- parsed[, c(nms, 'geometry')]  # sort
    attr(parsed, 'sf_column') <- 'geometry'  # persist attribute

    if(!is.null(item)){
      # Returned only 1 item (by ID).
      # So trick JSON to have extra members for consistent conversion to 'sf'.
      parsed[['numberReturned']] <- nReturned <- 1

      data[['type']] <- jsonlist$type
      data[['links']] <- links
      data[['numberReturned']] <- 1
      data[['features']] <- parsed

    } else{
      # Store results from this loop.
      if(length(data) == 0L){
        data[['type']] <- jsonlist$type
        data[['links']] <- links
        data[['numberReturned']] <- nReturned
        data[['features']] <- parsed

      } else{  # Append to existing data list.
        data[['numberReturned']] <-  sum(data[['numberReturned']],
                                         nReturned)
        data[['links']] <- rbind(data[['links']], links)
        data[['features']] <- rbind(data[['features']], parsed)
      }
    }

    if(nReturned < limit)
      break  # Query complete.
    else
      n_required <- n_required - nReturned

    Sys.sleep(waits)
  } # End query loop.

  if(returnType == 'geojson'){
    # Convert to geojson
    gj <- geojsonsf::sf_geojson(data$features)
    return(gj)
  }

  if(returnType == 'list'){
    gj <- geojsonsf::sf_geojson(data$features)
    listgj <- jsonlite::parse_json(gj)
    # add other returns
    listgj <- c('type' = list(data$type),
                'links' = list(as.list(data$links)),
                'numberReturned' = list(data$numberReturned),
                listgj)

    return(listgj)
  }

  if(returnType == 'sf'){
    # Check `sf` package is available.
    if (!requireNamespace('sf', quietly = TRUE)){
      stop('Package `sf` must be installed to use this function.',
           call. = FALSE)
    } else{

      if(nCRS == 84) nCRS <- 'CRS:84'  # Convert to a format understood by `sf`.
      crs <- sf::st_crs(nCRS)

      # Extract spatial object.
      sdf <- data[['features']]
      suppressWarnings(sf::st_crs(sdf) <- crs)

      return(sdf)
    }
  }
}


#' Retrieve OS NGD Feature Collections
#'
#' Query the osdatahub NGD Features API to gather information on available data
#' collections. An API key is not required for this query.
#' @param simple (logical) Should only the collection ID be returned? Default is
#'   \code{TRUE}. Use \code{FALSE} to return the detailed output.
#'
#' @details OS NGD themes and collections have been created to group similar
#'   geographic entities and data types, making it quicker and easier to
#'   identify the data you need. The OGC API - Features standard also references
#'   feature collections, and in the context of OS NGD datasets, this is
#'   equivalent to feature types. The following naming convention has been
#'   applied to the feature collections: theme-collection-featuretype. Short
#'   codes have been used for both the theme and collection to keep the feature
#'   collection names manageable and not overly long. An example of the short
#'   codes used is: 'bld-fts-buildingline'. For more information, see
#'   \url{https://osdatahub.os.uk/docs/ofa/technicalSpecification}.
#'
#' @returns If \code{simple} is \code{TRUE} then return a character vector of
#'   available collections identified by their shortened code, else return a
#'   \code{data.frame} with the full details.
#'
#' @examplesIf has_os_key()
#' ngd_collections <- list_ngd_collections(simple = TRUE)
#' ngd_collections[1:10]
#'
#' @export
list_ngd_collections <- function(simple = TRUE){

  if(!is.logical(simple)){
    stop('Argument must be a logical value, TRUE or FALSE.', call. = FALSE)
  }

  # Build URL.
  NGD_ENDPOINT <- ENDPOINTS[ENDPOINTS$api == 'ngd', 'url']

  # Query.
  resp <- httr::GET(NGD_ENDPOINT)

  rc <- httr::status_code(resp)

  if (rc != 200) {
    stop(sprintf("OS NGD API failed [%s]\nResponse: %s (%s)",
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

  if(simple){
    return(parsed$collections$id)
  } else{
    return(parsed$collections)
  }
}
