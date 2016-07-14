context("Bases to Genotypes")

test_that("basesToGenotypes returns matrix", {
  expect_true(is.matrix(basesToGenotypes(matrix(
    sample(0:1, size = 80, replace = TRUE), ncol = 8
  ), ploidy = 4)))
})

test_that("basesToGenotypes returns right sized matrix", {
  expect_equal(dim(basesToGenotypes(matrix(
    sample(0:1, size = 80, replace = TRUE), ncol = 8
  ), ploidy = 4)), c(10, 2))
  expect_equal(dim(basesToGenotypes(matrix(
    sample(0:1, size = 24, replace = TRUE), ncol = 8
  ), ploidy = 4)), c(3, 2))
  expect_equal(dim(basesToGenotypes(matrix(
    sample(0:1, size = 180, replace = TRUE), ncol = 18
  ), ploidy = 6)), c(10, 3))
  expect_equal(dim(basesToGenotypes(matrix(
    sample(0:1, size = 8000, replace = TRUE), ncol = 800
  ), ploidy = 8)), c(10, 100))
})

test_that("basesToGenotypes values are from 0 to ploidy", {
  expect_equal(names(table(basesToGenotypes(matrix(
    sample(0:1, size = 8000, replace = TRUE), ncol = 800
  ), ploidy = 4))), as.character(0:4))
  expect_equal(names(table(basesToGenotypes(matrix(
    sample(0:1, size = 8000, replace = TRUE), ncol = 800
  ), ploidy = 8))), as.character(0:8))
  expect_equal(names(table(basesToGenotypes(matrix(
    sample(0:1, size = 8000, replace = TRUE), ncol = 800
  ), ploidy = 2))), as.character(0:2))
})