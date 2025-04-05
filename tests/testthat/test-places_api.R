test_that("we can catch Places errors", {
  skip_if_offline()

  expect_error(query_places())
  expect_error(query_places(NULL))
  expect_error(query_places('Text query', output_crs = 'invalid-crs'))
  expect_error(query_places('Text query', limit = 0))
  # expect_error(query_places('Text query', limit = 101))

  expect_error(query_nearest_places(point=c(1,2,3)))
  expect_error(query_nearest_places(point=c(0,0), point_crs = 'invalid-crs'))
  expect_error(query_nearest_places(point=c(0,0), point_crs = 'CRS84', radius = 0))

  expect_error(query_postcode_places())
  expect_error(query_postcode_places(NULL))

  expect_error(query_uprn_places())
  expect_error(query_uprn_places(NULL))
})

with_mock_api({
  test_that("we can query the Places API", {

    extent <- extent_from_bbox(c(600000, 310200, 600900, 310900), crs = 'EPSG:27700')
    res <- query_places(extent, limit = 10)

    expect_s3_class(res, 'json')
    expect_equal(length(res), 1L)

    res <- query_places(extent, limit = 100, classification_code = c('CL02', 'CI03'))

    expect_s3_class(res, 'json')
    expect_equal(length(res), 1L)

    skip_if_not_installed('sf')
    res <- query_places(extent, limit = 10, returnType = 'sf')

    expect_s3_class(res, 'sf')
    expect_equal(nrow(res), 10L)

    res <- query_places(extent, limit = 100, classification_code = c('CL02', 'CI03'), returnType = 'sf')

    expect_s3_class(res, 'sf')
    expect_equal(nrow(res), 2L)
  })

  test_that("we can query the Places API by text", {
    res <- query_places('Ordnance Survey, Adanac Drive, SO16', minmatch = 0.5)

    expect_s3_class(res, 'json')
    expect_equal(length(res), 1L)

    skip_if_not_installed('sf')
    res <- query_places('Ordnance Survey, Adanac Drive, SO16', minmatch = 0.5, returnType = 'sf')

    expect_s3_class(res, 'sf')
    expect_equal(nrow(res), 1L)
  })

  test_that("we can query the nearest Places", {
    pt <- c(437292.4, 115541.9)

    res <- query_nearest_places(pt, point_crs = 'EPSG:27700')

    expect_s3_class(res, 'json')
    expect_equal(length(res), 1L)

    skip_if_not_installed('sf')
    res <- query_nearest_places(pt, point_crs = 'EPSG:27700', returnType = 'sf')

    expect_s3_class(res, 'sf')
    expect_equal(nrow(res), 1L)
  })

  test_that("we can query Places by postcode", {
    res <- query_postcode_places(postcode = 'SO16 0AS')

    expect_s3_class(res, 'json')
    expect_equal(length(res), 1L)

    skip_if_not_installed('sf')
    res <- query_postcode_places(postcode = 'SO16 0AS', returnType = 'sf')

    expect_s3_class(res, 'sf')
    expect_equal(nrow(res), 1L)
  })

  test_that("we can query Places by uprn", {
    res <- query_uprn_places(uprn = 200010019924)

    expect_s3_class(res, 'json')
    expect_equal(length(res), 1L)

    skip_if_not_installed('sf')
    res <-  query_uprn_places(uprn = 200010019924, returnType = 'sf')

    expect_s3_class(res, 'sf')
    expect_equal(nrow(res), 1L)
  })
})
