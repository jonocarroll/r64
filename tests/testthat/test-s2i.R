context("s2i")

test_that("s2i works", {
  expect_identical(s2i("hello"), c(72L, 69L, 76L, 76L, 79L))
})
