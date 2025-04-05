
#' Query the OS Names API
#'
#' Retrieve information from a geographic directory of identifiable places based
#' on a free text search.
#' @param x The free text search parameter.
#' @param limit (numeric) The maximum number of features to return.
#'   Default is 100.
#' @param bounds Biases the search results to a certain area. Should be of type
#'   \code{qExtent} created by the \code{extent_from_*} functions with CRS =
#'   EPSG:27700.
#' @param bbox_filter Filters the results to a certain area. Should be of type
#'   \code{qExtent} created by the \code{extent_from_*} functions with CRS =
#'   EPSG:27700.
#' @param local_type (character) Filters the results to certain local types. The
#'   available local types can be found at:
#'   \url{https://osdatahub.os.uk/docs/names/technicalSpecification}.
#' @param key (character) OS API key. Default action is to search for an
#'   environment variable using \code{get_os_key()}.
#' @param returnType (character) Return the query results as the raw
#'   \code{'geojson'}, a nested \code{'list'} object containing the returns,
#'   or convert them into Simple Features and return an object of class
#'   \code{'sf'}.
#' @param ... Additional parameters (not currently used).
#'
#' @details The OS Names API is a geographic directory containing basic
#'   information about identifiable places. Those places are divided into
#'   themes, but the name of the place is the key property used in queries.
#'   The free text search is intended to be a "fuzzy" search.
#'
#'   Within OS Names, place names aren’t unique. Extra location details are
#'   provided to help users refine their queries and accurately identify the
#'   named place they’re interested in. These details include postcode district,
#'   populated place, district/borough, county/unitary authority, European
#'   region and country. Queries can also be refined by supplying bounding boxes
#'   or local types to search.
#'
#'   Technical details on the Names API are documented on the Data Hub:
#'   \url{https://osdatahub.os.uk/docs/names/technicalSpecification}.
#'
#'
#' @returns A GeoJSON string with the results of the API query, a list object,
#'   or an object of class \code{sf} based on the \code{returnType} parameter.
#'
#' @seealso [query_nearest_names()], [extent]
#'
#' @examplesIf has_os_key()
#' # Find names places by text search.
#' results <- query_names('Buckingham Palace', limit = 5)
#'
#' # Use filters
#' results <- query_names('Southampton', local_type = 'City')
#'
#' # Limit results to a bounding box
#' extent <- extent_from_bbox(c(600000, 310200, 600900, 310900),
#'                            crs = 'EPSG:27700')
#'
#' results <- query_names('Norwich', bbox_filter = extent)
#'
#' @import httr
#' @import geojsonsf
#' @import jsonlite
#' @export
query_names <- function(x,
                        limit = 100,
                        bounds,
                        bbox_filter,
                        local_type,
                        key = get_os_key(),
                        returnType = c('geojson', 'list', 'sf'),
                        ...){
  # Query addresses based on a free text search.
  if(missing(x)){
    stop('Please provide free text to search.', call. = FALSE)
  } else{
    if(!is.character(x)){
      stop('Please provide a character string to search.', call. = FALSE)
    }

    if(length(x) > 1L){
      stop('Please provide only one search string at a time.', call. = FALSE)
    }
  }

  # Validate parameters.
  params <- list()

  # Query text.
  params[['query']] <- x

  if(limit < 1){
    stop('Limit must be greater than 0.', call. = FALSE)
  }

  returnType <- match.arg(returnType)

  # Check bounds
  if(!missing(bounds)){
    if(!inherits(bounds, 'qExtent')){
      stop("Bounds must be an extent object using `extent_from_*`",
           call. = FALSE)
    }

    if(get_crs(bounds$crs, 'number') != 27700){
      stop("Bounds must use EPSG:27700.", call. = FALSE)
    }

    params[['bounds']] <- paste0(format(bounds$bbox,
                                        scientific = FALSE),
                                 collapse=',')
  }

  # Check filter box
  if(!missing(bbox_filter)){
    if(!inherits(bbox_filter, 'qExtent')){
      stop("BBox filter must be an extent object using `extent_from_*`",
           call. = FALSE)
    }

    if(get_crs(bbox_filter$crs, 'number') != 27700){
      stop("BBox filter must use EPSG:27700.", call. = FALSE)
    }

    bbox_filter <- bbox_filter$bbox
  }

  # Add filters.
  params <- names_filter(params, local_type, bbox_filter)

  # Execute
  data <- names_query(key, endpoint = 'find', params, limit)

  # Process return data.
  output <- names_process(returnType,
                          data,
                          crs = toupper(get_crs(27700, 'code')))

  return(output)
}


#' Query the OS Names API
#'
#' Takes a pair of coordinates (X, Y) as an input to determine the closest name
#' from a geographic directory of identifiable places.
#' @param point A set of British National Grid coordinates (EPSG:27700). Can be
#'   a set of coordinates as a numeric vector, an object of class \code{geos},
#'   or an object of class \code{sf}.
#' @param radius (numeric) The search radius in metres (max. 1000). Default is
#'   100.
#' @param local_type (character) Filters the results to certain local types. The
#'   available local types can be found at:
#'   \url{https://osdatahub.os.uk/docs/names/technicalSpecification}.
#' @param key (character) OS API key. Default action is to search for an
#'   environment variable using \code{get_os_key()}.
#' @param returnType (character) Return the query results as the raw
#'   \code{'geojson'}, a nested \code{'list'} object containing the returns,
#'   or convert them into Simple Features and return an object of class
#'   \code{'sf'}.
#' @param ... Additional parameters (not currently used).
#'
#' @details The OS Names API is a geographic directory containing basic
#'   information about identifiable places. Use this function to query Names to
#'   find the nearest named place to a given point location.
#'
#'   Within OS Names, place names aren’t unique. Extra location details are
#'   provided to help users refine their queries and accurately identify the
#'   named place they’re interested in. These details include postcode district,
#'   populated place, district/borough, county/unitary authority, European
#'   region and country. Queries can also be refined by supplying bounding boxes
#'   or local types to search.
#'
#'   Technical details on the Names API are documented on the Data Hub:
#'   \url{https://osdatahub.os.uk/docs/names/technicalSpecification}.
#'
#'
#' @returns A GeoJSON string with the results of the API query, a list object,
#'   or an object of class \code{sf} based on the \code{returnType} parameter.
#'
#' @seealso [query_names()], [extent]
#'
#' @examplesIf has_os_key()
#' # Named entity nearest to a point location
#' results <- query_nearest_names(point = c(440200,449300))
#'
#' @import httr
#' @import geojsonsf
#' @import jsonlite
#' @export
query_nearest_names <- function(point,
                                radius = 100,
                                local_type,
                                key = get_os_key(),
                                returnType = c('geojson', 'list', 'sf'),
                                ...){
  if(missing(point)){
    stop('Please supply a query point location.', call. = FALSE)
  }

  # Process point location to a vector.
  if(inherits(point, 'numeric')){
    if(length(point) != 2){
      stop('Query points should only contain one coordinate pair.')
    }
    # Keep as numeric.

  } else if(inherits(point, 'geos_geometry')){
    if(geos::geos_type(point) != 'point'){
      stop('Only Point geometries can be supplied.', call. = FALSE)
    }
    point <- c(geos::geos_x(point), geos::geos_y(point))

  } else if(inherits(point, c('sf', 'sfc'))){
    # Check `sf` package is available.
    if (!requireNamespace('sf', quietly = TRUE)){
      stop('Package `sf` must be installed to use this function.',
           call. = FALSE)
    }

    if(nrow(point) > 1){
      stop('Please provide only one feature.', call. = FALSE)
    }

    if(sf::st_geometry_type(point) != 'POINT'){
      stop('Only Point geometries can be supplied.', call. = FALSE)
    }

    point <- c(sf::st_coordinates(point)[,1],
               sf::st_coordinates(point)[,2])

  } else{
    stop('The query point format is not valid.', call. = FALSE)
  }

  # check range of point coordinates
  if(point[1] < 0 | point[1] > 700000 | point[2] < 0 | point[2] > 1300000){
    stop("Invalid point location.", call. = FALSE)
  }

  returnType <- match.arg(returnType)

  if(radius < 0.01 | radius > 1000){
    stop('The query radius must be between 0.01 and 1000 metres.',
         call. = FALSE)
  }

  params <- list('point' = paste(point, collapse = ','),
                 # 'srs' = point_crs,
                 # 'output_srs' = output_crs,
                 'radius' = radius)

  # Add filters.
  params <- names_filter(params, local_type)

  # Execute
  data <- names_query(key, endpoint = 'nearest', params, limit = 1)

  # Process return data.
  output <- names_process(returnType,
                          data,
                          crs = toupper(get_crs(27700, 'code')))

  return(output)
}


#' Internal function to add filters to Names API query
#'
#' Combine potential filtering codes into the list of queryables.
#' @param params list of named queryables to modify.
#' @param local_type Type codes to filter query by.
#' @param bbox Bounding box to filter query results.
#'
#' @returns List with modified query parameters.
#'
#' @examples
#' params <- list()
#' filter <- osdatahub:::names_filter(params, c('City', 'Bay'))
#'
#' @keywords internal
#' @noRd
names_filter <- function(params, local_type, bbox){
  fq_args <- vector('character')

  if(!missing(local_type)){
    chk_lt <- local_type %in% LOCALTYPES$local_type

    if(!all(chk_lt)){
      stop(paste0("Unknown local type: ", local_type[!chk_lt], "."),
           call. = FALSE)
    }

    fq_args <- paste0('LOCAL_TYPE:',
                      local_type,
                      collapse = ' ')
  }

  if(!missing(bbox)){
    bb <- paste0('BBOX:', paste0(format(bbox, scientific = FALSE),
                                 collapse = ','))
    fq_args <- append(fq_args, bb)
  }

  # httr doesn't like multiple parameters with the same name.
  # Avoid URL encoding by wrapping in I(...)
  if(length(fq_args) > 0){
    params[['fq']] <- I(paste0(fq_args, collapse = '&fq='))
  }

  return(params)
}


#' Internal function for querying the Names API
#'
#' Primary helper function for executing queries to the Names API on the OS Data
#' Hub.
#' @param key OS API key.
#' @param endpoint API endpoint to add to the base URL.
#' @param params List of named queryables to filter.
#' @param limit Max results to return.
#'
#' @returns List with the results of the API query.
#'
#' @examples
#' \dontrun{
#' names_query(get_os_key(), endpoint, params, limit = 1)
#' }
#'
#' @keywords internal
#' @noRd
names_query <- function(key, endpoint, params, limit){

  # Build base URL.
  NAMES_ENDPOINT <- ENDPOINTS[ENDPOINTS$api == 'names', 'url']
  NAMES_ENDPOINT <- paste0(NAMES_ENDPOINT, '/', endpoint)

  # Set user-agent.
  ua <- httr::user_agent('osdatahub-r')

  # Respect throttle limits (600 requests per minute)
  if(limit >= 60000){
    waits <- (60 / 599)
  } else{
    waits <- 0.0
  }

  # Query loop.
  n_required <- limit
  offset <- 0
  numberReturned <- 0
  data <- list()

  while(n_required > 0){
    limit <- min(n_required, 100)
    offset <- max(offset, numberReturned)
    url <- NAMES_ENDPOINT

    # Update with new offset/limit.
    if(!endpoint %in% c('nearest')){
      params[['offset']] <- offset
      params[['maxresults']] <- limit
    }

    url <- httr::modify_url(url,
                            query = params)

    # Connect to API.
    resp <- httr::GET(url,
                      ua,
                      httr::add_headers(key = key))

    rc <- httr::status_code(resp)

    if (rc != 200) {
      stop(sprintf("OS Names API failed [%s] \nResponse: %s (%s)",
                   rc,
                   RESPONSECODES[RESPONSECODES$code == rc, 'description'],
                   RESPONSECODES[RESPONSECODES$code == rc, 'explanation']),
           call. = FALSE)
    }

    if(httr::http_type(resp) != 'application/json'){
      stop('API did not return json type.', call. = FALSE)
    }

    # Get query results.
    resp_txt <- suppressMessages(httr::content(resp, 'text'))
    results <- process_names_data(resp_txt)

    # Count features
    nReturned <- length(results)
    numberReturned <- numberReturned + nReturned

    # Store results from this loop.
    data <- append(data, results)

    if(nReturned < limit)
      break  # Query complete.
    else
      n_required <- n_required - nReturned

    Sys.sleep(waits)
  } # End query loop.

  return(data)
}


#' Process gazetteer data
#'
#' Parse the list of features returned from Names API into GeoJSON format.
#' @param response_text Character vector from the HTTP request.
#'
#' @details This internal function is re-used by the different Names API query
#'   functions to parse the returned JSON text string. Point features are
#'   created and the data are re-formatted as a list object with names that can
#'   be converted into GeoJSON.
#'
#' @returns List of features compatible with GeoJSON specs.
#'
#' @examples
#' \dontrun{
#' resp <- names_query(get_os_key(), endpoint, params, limit = 1)
#' process_names_data(resp)
#' }
#'
#' @keywords internal
#' @noRd
process_names_data <- function(response_text){
  # Extract JSON + Extra members with return information.
  jsonlist <- jsonlite::parse_json(response_text)
  # Count features
  nReturned <- length(jsonlist$results)

  if(nReturned == 0){
    stop('Query returned no features.', call. = FALSE)
  }

  # Names of properties.
  nms <- names(jsonlist$results[[1]][[1]])

  # Find coordinate names. Depends on CRS.
  if(all(c('X_COORDINATE', 'Y_COORDINATE') %in% nms)){
    xnm <- 'X_COORDINATE'
    ynm <- 'Y_COORDINATE'
  } else if(all(c('GEOMETRY_X', 'GEOMETRY_Y') %in% nms)){
    xnm <- 'GEOMETRY_X'
    ynm <- 'GEOMETRY_Y'
  } else if(all(c('LNG', 'LAT') %in% nms)){
    xnm <- 'LNG'
    ynm <- 'LAT'
  } else{
    stop('Expected coordinate names not found.', call. = FALSE)
  }

  # Process features to list compatible with GeoJSON.
  gj <- lapply(jsonlist$results, FUN = function(r){
    feat <- r[[1]]
    fjson <- list('type' = 'Feature',
                  'geometry' = list('type' = 'Point',
                                    'coordinates' = c(feat[[xnm]],
                                                      feat[[ynm]])),
                  'properties' = feat)
    return(fjson)
  })
}


#' Process Gazetteer data to FeatureCollection
#'
#' Create the requested output format for Names queries.
#' @param returnType String defining the type of output format.
#' @param data JSON strings of parsed Addres data.
#' @param crs String with the output CRS.
#' @details This internal function is re-used by the different Names API query
#'   functions to parse the returned JSON text string. 'FeatureCollections' are
#'   created and converted into the possible returnType.
#'
#' @returns GeoJSON, list object, or 'sf' object depending on \code{returnType}.
#'
#' @examples
#' \dontrun{
#' resp <- names_query(get_os_key(), endpoint, params, limit = 1)
#' data <- process_names_data(resp)
#' places_process('sf', data, 'EPSG:27700')
#' }
#'
#' @keywords internal
#' @noRd
names_process <- function(returnType, data, crs){

  # Convert list of GeoJSON to FeatureCollection.
  featlist <- list('type' = 'FeatureCollection',
                   'crs' = crs,  # Match format of Python
                   'features' = data)

  featcollect <- jsonlite::toJSON(featlist, auto_unbox = TRUE)

  if(returnType == 'geojson'){
    return(featcollect)
  }

  if(returnType == 'list'){
    return(featlist)
  }

  if(returnType == 'sf'){
    # Check `sf` package is available.
    if (!requireNamespace('sf', quietly = TRUE)){
      stop('Package `sf` must be installed to use this function.',
           call. = FALSE)
    } else{

      # Names of properties. Needed for sorting.
      nms <- names(featlist$features[[1]]$properties)

      # Extract spatial object.
      sdf <- geojsonsf::geojson_sf(featcollect)
      sdf <- sdf[, c(nms, 'geometry')]
      suppressWarnings(sf::st_crs(sdf) <- sf::st_crs(crs))

      return(sdf)
    }
  }
}

