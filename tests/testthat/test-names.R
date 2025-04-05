test_that("we can catch Names errors", {
  skip_if_offline()

  expect_error(query_names())
  expect_error(query_names(NULL))
  expect_error(query_names('Text query', limit = 0))

  expect_error(query_names('Buckingham Palace', local_type = 'invalid'))

  expect_error(query_nearest_names(point=c(1,2,3)))
  expect_error(query_nearest_names(point=c(0,0), radius = 1001))
  expect_error(query_nearest_names(point=c(700001, 1300000)))
})

with_mock_api({
  test_that("we can query the Names API", {
    res <- query_names('Buckingham Palace', limit = 10)

    expect_s3_class(res, 'json')
    expect_equal(length(res), 1L)

    extent <- extent_from_bbox(c(600000, 310200, 600900, 310900),
                               crs = 'EPSG:27700')

    res <- query_names('Norwich', bbox_filter = extent)

    expect_s3_class(res, 'json')
    expect_equal(length(res), 1L)
  })

  test_that("we can query the nearest Names", {
    res <- query_nearest_names(point = c(440200,449300))

    expect_s3_class(res, 'json')
    expect_equal(length(res), 1L)
  })
})
