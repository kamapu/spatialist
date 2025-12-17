context("masking windows")

test_that("mask_window() is working", {
  expect_is(mask_window(), "matrix")
  expect_is(mask_window(option = "bottom"), "matrix")
  expect_is(mask_window(option = "left"), "matrix")
  expect_is(mask_window(option = "right"), "matrix")
  expect_is(mask_window(option = "topleft"), "matrix")
  expect_is(mask_window(option = "topright"), "matrix")
  expect_is(mask_window(option = "bottomleft"), "matrix")
  expect_is(mask_window(option = "bottomright"), "matrix")
  expect_is(mask_window(window = matrix(data = rep(1, 25), ncol = 5)), "matrix")
})

test_that("error messages are thrown by mask_window()", {
  expect_error(mask_window(dim = c(4, 5)))
  expect_error(mask_window(option = "abajo"))
})
