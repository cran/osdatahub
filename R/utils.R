
#' Convert Spherical Mercator (EPSG:3857) coordinates to WGS84
#' @keywords internal
#' @noRd
sm2ll <- function(x, y){
  # Based on `contextily`: https://github.com/geopandas/contextily/blob/main/contextily/tile.py
  rMajor <- 6378137  # Approx. equatorial radius, WGS84
  shift <- pi * rMajor

  lon <- x / shift * 180
  lat <- y / shift * 180
  lat <- 180 / pi * (2 * atan(exp(lat * pi / 180)) - pi / 2)

  return(c('lon' = lon, 'lat' = lat))
}


#' Convert WGS84 coordinates to Spherical Mercator (EPSG:3857)
#' @keywords internal
#' @noRd
ll2sm <- function(lon, lat){
  # Based on http://dotnetfollower.com/wordpress/2011/08/javascript-how-to-convert-latitude-and-longitude-to-mercator-coordinates/
  rMajor <- 6378137  # Approx. equatorial radius, WGS84
  shift <- pi * rMajor

  x <- lon * shift / 180
  y <- log(tan((90 + lat) * pi / 360)) / (pi / 180)
  y <- y * shift / 180;

  return(c('x' = x, 'y' = y))
}
