context("address_to_bytes")

test_that("address_to_bytes works", {
  expect_identical(address_to_bytes(0), c(0L, 0L))
  expect_identical(address_to_bytes(0x1ff), c(255L, 1L))
  expect_identical(address_to_bytes(0xff01), c(1L, 255L))
})
