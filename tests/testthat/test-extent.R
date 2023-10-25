test_that("extents from bbox works", {
  expect_error(extent_from_bbox())
  expect_error(extent_from_bbox(NULL))
  expect_error(extent_from_bbox(''))

  expect_error(extent_from_bbox(c(1, 2, 3)))

  bbox <- c(600000, 310200, 600900, 310800)
  expect_equal(extent_from_bbox(bbox, crs = 'epsg:27700')$wkt,
               "POLYGON ((600000 310200, 600900 310200, 600900 310800, 600000 310800, 600000 310200))")

  bbox <- as.data.frame(matrix(bbox, ncol = 4, byrow = TRUE))
  expect_equal(extent_from_bbox(bbox, crs = 'epsg:27700')$wkt,
               "POLYGON ((600000 310200, 600900 310200, 600900 310800, 600000 310800, 600000 310200))")

  expect_error(extent_from_bbox(bbox, crs = 'INVALID'))
  expect_error(extent_from_bbox(rbind(bbox, bbox), crs = 'epsg:27700'))
})


test_that("extents from polygon works", {
  expect_error(extent_from_polygon())
  expect_error(extent_from_polygon(NULL))
  expect_error(extent_from_polygon(''))

  poly <- "POLYGON ((600000 310200, 600900 310200, 600900 310800, 600000 310800, 600000 310200))"
  expect_equal(extent_from_polygon(poly, crs = 'epsg:27700')$wkt,
               "POLYGON ((600000 310200, 600900 310200, 600900 310800, 600000 310800, 600000 310200))")

  expect_error(extent_from_polygon(c(poly, poly), crs = 'epsg:27700'))
  expect_error(extent_from_polygon(poly, crs = 'INVALID'))
})


test_that("extents from geojson works", {
  expect_error(extent_from_geojson())
  expect_error(extent_from_geojson(NULL))
  expect_error(extent_from_geojson(''))

  poly <- "{\"type\":\"Polygon\",\"coordinates\":[[[600000.0,310200.0],[600900.0,310200.0],[600900.0,310800.0],[600000.0,310800.0],[600000.0,310200.0]]]}"
  expect_equal(extent_from_geojson(poly, crs = 'epsg:27700')$wkt,
               "POLYGON ((600000 310200, 600900 310200, 600900 310800, 600000 310800, 600000 310200))")

  expect_error(extent_from_polygon(c(poly, poly), crs = 'epsg:27700'))
  expect_error(extent_from_geojson(poly, crs = 'INVALID'))
})


test_that("extents from radius works", {
  expect_error(extent_from_radius())
  expect_error(extent_from_radius(NULL))
  expect_error(extent_from_radius(''))

  point <- c(441317, 112165)
  distance <- 200

  expect_error(extent_from_radius(point))
  expect_error(extent_from_radius(point, NULL))
  expect_error(extent_from_radius(point, ''))

  expect_equal(as.numeric(extent_from_radius(point, distance)$bbox),
               c(441117, 111965, 441517, 112365))

  point <- "POINT (441317 112165)"
  expect_equal(as.numeric(extent_from_radius(point, distance)$bbox),
               c(441117, 111965, 441517, 112365))

  expect_error(extent_from_radius(c(point, point), distance))
  expect_error(extent_from_radius(point, distance, crs='epsg:4326'))
})


test_that("extents from BNG ref works", {
  expect_error(extent_from_bng())
  expect_error(extent_from_bng(NULL))
  expect_error(extent_from_bng(''))

  bng <- 'SU3715'
  expect_equal(extent_from_bng(bng)$wkt,
               "POLYGON ((437000 115000, 438000 115000, 438000 116000, 437000 116000, 437000 115000))")

  expect_error(extent_from_bng(c(bng, bng)))
})

