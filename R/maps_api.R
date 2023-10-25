
#' Query the OS Maps API
#'
#' Retrieve pre-rendered tiles from the web maps service of the Maps API in the
#' Ordnance Survey Data Hub.
#' @param x Object defining the query extent. Should be of type \code{qExtent}
#'   created by the \code{extent_from_*} functions.
#' @param layer (character) The name of the layer to query. See details.
#' @param zoom (numeric) The zoom level of the tiles to return. If omitted, a
#'   suitable zoom level will be estimated. See details.
#' @param output_dir (character) Path to the directory where the downloaded
#'   tiles will be saved.
#' @param overwrite Boolean. Should existing files be overwritten? Default is
#'   \code{FALSE}.
#' @param key (character) OS API key. Default action is to search for an
#'   environment variable using \code{get_os_key()}.
#' @param ... Additional parameters (not currently used).
#'
#' @details The OS Maps API serves pre-rendered raster tiles and is available in
#'   two projections; British National Grid and Web Mercator. This function
#'   provides basic access to download these tiles to your local machine.
#'
#'   Alternatively, you can request the maps using the Open Geospatial
#'   Consortium Web Map Tile Service (WMTS) standard or RESTful ZXY for easy
#'   access/visualisation in most GIS software and web mapping applications.
#'   More information on the Maps API is available from:
#'   \url{https://osdatahub.os.uk/docs/wmts/technicalSpecification}.
#'
#'   The parameter \code{x} currently only accepts a query extent created from
#'   \code{extent_from_*} family of functions. The coordinate reference system
#'   of this extent must match the coordinate reference system of the returned
#'   tiles (i.e. only EPSG:27700 and EPSG:3857 are accepted).
#'
#'   The available layers are Road, Outdoor, Light in both 27700 and 3857, plus
#'   Leisure in 27700. These should be specified as combined strings to the
#'   \code{layer} argument, e.g. 'Road_27700'.
#'
#'   The zoom levels available vary based on the projection of the tile matrix
#'   set. EPSG:3857 is from 7 to 20 and EPSG:27700 is from 0 to 13. See the
#'   technical specifications for more information on the scale and resolution:
#'   \url{https://osdatahub.os.uk/docs/wmts/technicalSpecification}
#'
#' @returns A list of file paths to the downloaded image tiles, their bounding
#'   boxes, and coordinate reference system information.
#'
#' @seealso [extent]
#'
#' @examplesIf has_os_key()
#' # Define an extent.
#' OS_ext <- extent_from_bng('SU3715')
#'
#' # Download tiles.
#' imgTile <- query_maps(OS_ext,
#'                       layer = 'Light_27700',
#'                       output_dir = tempdir())
#'
#' # The tiles can be merged together and georeferenced for spatial applications.
#'
#' @export
query_maps <- function(x,
                       layer = c('Road_27700', 'Road_3857',
                                 'Outdoor_27700', 'Outdoor_3857',
                                 'Light_27700', 'Light_3857',
                                 'Leisure_27700'),
                       zoom,
                       output_dir,
                       overwrite = FALSE,
                       key = get_os_key(),
                       ...){

  UseMethod('query_maps', x)
}


#' @rdname query_maps
#' @export
query_maps.qExtent <- function(x,
                               layer = c('Road_27700', 'Road_3857',
                                         'Outdoor_27700', 'Outdoor_3857',
                                         'Light_27700', 'Light_3857',
                                         'Leisure_27700'),
                               zoom,
                               output_dir,
                               overwrite = FALSE,
                               key = get_os_key(),
                               ...){

  # Check projection.
  crs <- get_crs(x$crs, 'number')

  if(!crs %in% c(27700, 3857)){
    stop('Query extent must be in EPSG:27700 or EPSG:3857.', call. = FALSE)
  }

  match.arg(layer)
  lyr_crs <- as.numeric(sub('.*_', '', layer))

  if(crs != lyr_crs){
    stop('Query extent CRS must match Maps layer CRS.', call. = FALSE)
  }

  # Check output directory.
  if(missing(output_dir)){
    stop('Please provide a path to an output directory.', call. = FALSE)
  }

  if(!dir.exists(output_dir)){
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }

  # Get the vector of bounding box values.
  bbox <- as.numeric(x$bbox)

  # if(lyr_crs == 3857){
  #   # Use conversion to lon/lat
  #   llmin <- sm2ll(bbox[1], bbox[2])
  #   llmax <- sm2ll(bbox[3], bbox[4])
  #   bbox <- c(llmin, llmax)
  # }

  # Check zoom level.
  if(missing(zoom)){
    # Estimate zoom based on extent.

    if(lyr_crs == 27700){
      zooms <- 0:13

    } else if(lyr_crs == 3857){
      zooms <- 7:20

    } else{
      stop('Could not estimate zoom level.', call. = FALSE)
    }

    # Get tile counts.
    total_tiles <- vector('numeric', length = length(zooms))
    min_tile_x <- vector('numeric', length = length(zooms))
    min_tile_y <- vector('numeric', length = length(zooms))
    max_tile_x <- vector('numeric', length = length(zooms))
    max_tile_y <- vector('numeric', length = length(zooms))

    for(i in 1:length(zooms)){
      z <- zooms[i]

      minTile <- xy_tile(bbox[1], bbox[2], zoom = z, crs = lyr_crs)
      maxTile <- xy_tile(bbox[3], bbox[4], zoom = z, crs = lyr_crs)

      total_tiles[i] <- (maxTile[1] - minTile[1] + 1) *
        (minTile[2] - maxTile[2] + 1)
      min_tile_x[i] <- minTile[1]
      min_tile_y[i] <- minTile[2]
      max_tile_x[i] <- maxTile[1]
      max_tile_y[i] <- maxTile[2]
    }

    # Collect results and select zoom based on tiles to download.
    df <- data.frame('zoom' = zooms,
                     min_tile_x, min_tile_y,
                     max_tile_x, max_tile_y,
                     total_tiles)
    # Change number here to adjust zoom.
    zoom_select <- df[which.max(df$total_tiles > 4), ]

    zoom <- zoom_select$zoom
    min_tile_x <- zoom_select$min_tile_x
    min_tile_y <- zoom_select$min_tile_y
    max_tile_x <- zoom_select$max_tile_x
    max_tile_y <- zoom_select$max_tile_y

  } else{
    # Zoom has been supplied by the user.
    # Get tiles within bbox.
    minTile <- xy_tile(bbox[1], bbox[2], zoom = zoom, crs = lyr_crs)
    maxTile <- xy_tile(bbox[3], bbox[4], zoom = zoom, crs = lyr_crs)

    min_tile_x <- minTile[1]
    min_tile_y <- minTile[2]
    max_tile_x <- maxTile[1]
    max_tile_y <- maxTile[2]
  }

  if(lyr_crs == 27700){
    if(zoom > 13 | zoom < 0){
      stop('Zoom level must be between 0 and 13.', call. = FALSE)
    }

  } else if(lyr_crs == 3857){
    if(zoom > 20 | zoom < 7){
      stop('Zoom level must be between 7 and 20.', call. = FALSE)
    }
  }

  # Generate grid of tiles.
  x_tiles <- min_tile_x:max_tile_x
  y_tiles <- min_tile_y:max_tile_y

  tile_grid <- expand.grid(x = x_tiles, y = y_tiles)

  ## Download tiles.
  output_list <- vector('list', length = nrow(tile_grid))
  # Build base URL.
  MAPS_ENDPOINT <- ENDPOINTS[ENDPOINTS$api == 'maps', 'url']

  for(i in 1:nrow(tile_grid)){
    tile <- tile_grid[i,]

    file_name <- paste(layer, zoom, tile$x, tile$y, sep = '_')
    file_name <- paste0(file_name, '.png')
    output_path <- file.path(output_dir, file_name)

    # update url
    url <- MAPS_ENDPOINT
    url <- gsub(pattern = "layer", url, replacement = layer, fixed = TRUE)
    url <- gsub(pattern = "{z}", url, replacement = zoom, fixed = TRUE)
    url <- gsub(pattern = "{x}", url, replacement = tile$x, fixed = TRUE)
    url <- gsub(pattern = "{y}", url, replacement = tile$y, fixed = TRUE)

    tryCatch({
      if(file.exists(output_path) & !overwrite){
        message(paste0(file_name, ' already exists. Set `overwrite` to TRUE.'))

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
      }

    },
    warning = function(w) print(w),
    error = function(e) print(paste('Download error:', e)))

    # Get extent of the downloaded tile.
    bbox <- tile_bbox(tile$x, tile$y, zoom, crs)

    output_list[[i]] <- list('file_path' = output_path,
                             'x_tile' = tile$x, 'y_tile' = tile$y,
                             'bbox' = bbox,
                             'crs' = paste0('EPSG:', crs))
  } # end tile loop.

  return(output_list)
}


#' Get tile numbers from xy coordinate position.
#' @param x The easting or x-coordinate of the position.
#' @param y The northing or y-coordinate of the position.
#' @param zoom (numeric) The zoom level appropriate to the CRS.
#' @param crs (numeric) Should the coordinates be 4326, 27700 or 3857?
#'
#' @returns Numeric vector with tile X,Y numbers.
#'
#' @examples
#' osdatahub:::xy_tile(337297, 503695, 7, 27700)
#'
#' @keywords internal
#' @noRd
xy_tile <- function(x, y, zoom, crs){
  if(crs == 3857){
    # Implements slippy map spec from https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    ll <- sm2ll(x, y)  # convert to lon/lat
    returnTile <- xy_tile(ll[1], ll[2], zoom = zoom, crs = 4326)

    x_tile <- returnTile[1]
    y_tile <- returnTile[2]

  } else if(crs == 27700){
    # Based on https://github.com/OrdnanceSurvey/tile-name-derivation
    originX <- -238375.0
    originY <- 1376256.0
    tileMeters <- 256 * (896 / (2**zoom))

    x_tile <- floor((x - originX) / tileMeters)
    y_tile <- floor((originY - y) / tileMeters)

  } else if(crs == 4326){
    # coordinates must have been converted into lon/lat.
    lat_rad <- y * pi / 180
    n <- 2.0^zoom

    x_tile <- floor((x + 180.0) / 360.0 * n)
    y_tile <- floor((1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n)

  } else{
    stop('Invalid CRS.', call. = FALSE)
  }

  return(c(x_tile, y_tile))
}


#' Get an xy coordinate position from tile numbers.
#' @param x_tile The tile number in the horizontal domain.
#' @param y_tile The tile number in the vertical domain.
#' @param zoom The zoom level appropriate to the CRS.
#' @param crs EPSG code for the CRS of the tiles. Valid options are 3857, 27700.
#'
#' @returns Numeric vector with X,Y coordinates of upper-left corner.
#'
#' @examples
#' osdatahub:::tile_xy(321, 486, 7, 27700)
#'
#' @keywords internal
#' @noRd
tile_xy <- function(x_tile, y_tile, zoom, crs){
  if(crs == 3857){
    # Based on: https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    # Get lon/lat of the tile
    n <- 2.0^zoom

    lon_r <- (((x_tile / n) * 2) - 1) * pi
    lon <- lon_r * 180 / pi

    lat_r <- (1 - ((y_tile / n) * 2)) * pi
    lat_r <- atan(sinh(lat_r))
    lat <- lat_r * 180 / pi

    # Convert lat/lon to Mercator
    merc <- ll2sm(lon, lat)
    x <- merc[1]
    y <- merc[2]

  } else if(crs == 27700){
    # Based on https://github.com/OrdnanceSurvey/tile-name-derivation
    originX <- -238375.0
    originY <- 1376256.0
    tileMeters <- 256 * (896 / (2**zoom))

    x <- originX + (x_tile * tileMeters)
    y <- originY - (y_tile * tileMeters)

  } else{
    stop('Invalid CRS.', call. = FALSE)
  }

  return(c(x, y))
}


#' Get a bounding box given a tile x,y number
#' @param x The easting or longitude of the position.
#' @param y The northing or latitude of the position.
#' @param zoom The zoom level appropriate to the CRS.
#' @param crs Should tile matrix be 27700 or 3857?
#'
#' @returns Numeric vector of a bounding box
#'
#' @examples
#' osdatahub:::tile_bbox(321, 486, 7, 27700)
#'
#' @keywords internal
#' @noRd
tile_bbox <- function(x, y, zoom, crs){

    # lower-left position
    ll <- tile_xy(x, y + 1, zoom, crs)
    # Upper-right position
    ur <- tile_xy(x + 1, y, zoom, crs)

  return(c(ll, ur))
}
