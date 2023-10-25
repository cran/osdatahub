test_that("error on empty API key", {
  expect_error(set_os_key())
  expect_error(set_os_key(''))
  expect_error(set_os_key(NULL))
})

test_that("error on invalid API keys", {
  expect_error(set_os_key(c('a', 'b')))
})
