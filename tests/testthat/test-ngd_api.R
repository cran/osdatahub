test_that("we can get NGD collections", {
  skip_if_offline()

  expect_error(list_ngd_collections('invalid'))

  res <- list_ngd_collections(TRUE)
  expect_type(res, 'character')

  res <- list_ngd_collections(FALSE)
  expect_s3_class(res, 'data.frame')
})


test_that("we can catch NGD errors", {
  skip_if_offline()

  expect_error(query_ngd())
  expect_error(query_ngd(NULL))
  expect_error(query_ngd(NULL, collection='invalid-name'))
  expect_error(query_ngd(NULL, collection='bld-fts-buildingline-1', crs='invalid-crs'))
})


with_mock_api({
  test_that("we can query the NGD", {
    res <- query_ngd(collection='bld-fts-buildingline-1', max_results = 10)

    expect_s3_class(res, 'json')
    expect_equal(length(res), 1L)

    skip_if_not_installed('sf')
    res <- query_ngd(collection='bld-fts-buildingline-1', max_results = 10, returnType='sf')

    expect_s3_class(res, 'sf')
    expect_equal(nrow(res), 10L)
  })

  test_that("we can get one NGD feature", {
    res <- query_ngd('0000013e-5fed-447d-a627-dae6fb215138', collection = 'bld-fts-buildingline-1')

    expect_s3_class(res, 'json')
    expect_equal(length(res), 1L)

    skip_if_not_installed('sf')
    res <- query_ngd('0000013e-5fed-447d-a627-dae6fb215138', collection = 'bld-fts-buildingline-1', returnType='sf')

    expect_s3_class(res, 'sf')
    expect_equal(nrow(res), 1L)
  })
})


