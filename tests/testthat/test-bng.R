test_that("grid reference conversion works", {
  expect_error(bng_to_geom())
  expect_error(bng_to_geom(NULL))
  expect_error(bng_to_geom(''))
  expect_error(bng_to_geom(1234))

  expect_equal(bng_to_geom('SV'),
               "POLYGON ((0 0, 100000 0, 100000 100000, 0 100000, 0 0))")
  expect_equal(bng_to_geom('TL63'),
               "POLYGON ((560000 230000, 570000 230000, 570000 240000, 560000 240000, 560000 230000))")
  expect_equal(bng_to_geom('NS2468'),
               "POLYGON ((224000 668000, 225000 668000, 225000 669000, 224000 669000, 224000 668000))")
})

test_that("invalid grid refs are found", {
  expect_error(valid_grid_ref())
  expect_error(valid_grid_ref(NULL))
  expect_error(valid_grid_ref(''))
  expect_error(valid_grid_ref('A'))
  expect_error(valid_grid_ref(c('a','b')))
  expect_error(valid_grid_ref('AAAAAAAAAAAAAA'))
})

