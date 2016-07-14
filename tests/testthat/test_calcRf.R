context("Calculate Recombination")

test_that("calcRec returns matrix", {
  expect_true(is.matrix(calcRec(matrix(
    sample(0:4, size = 8000, replace = TRUE), ncol = 800
  ), ploidy = 4)))
})

test_that("calcRec returns square matrix", {
  dimension <- dim(calcRec(matrix(
    sample(0:4, size = 8000, replace = TRUE), ncol = 800
  ), ploidy = 4))
  expect_true(dimension[1] == dimension[2])
})

test_that("calcRec returns values from 0 to 0.5", {
  mat <- calcRec(matrix(sample(
    0:4, size = 8000, replace = TRUE
  ), ncol = 800), ploidy = 4)
  expect_true(min(mat) <= 0.5)
  expect_true(min(mat) >= 0)
  expect_true(max(mat) <= 0.5)
  expect_true(max(mat) >= 0.5)
})

test_that("calcRec returns only zeros on diagonal", {
  mat <- calcRec(matrix(sample(
    0:4, size = 8000, replace = TRUE
  ), ncol = 800), ploidy = 4)
  expect_true(all(diag(mat) == 0))
})

test_that("calcRec returns symmetric matrix", {
  mat <- calcRec(matrix(sample(
    0:4, size = 8000, replace = TRUE
  ), ncol = 800), ploidy = 4)
  expect_true(isSymmetric(mat))
})

test_that("calcRec returns correctly sized matrix", {
  mat <- matrix(sample(0:4, size = 8000, replace = TRUE), ncol = 800)
  expect_equal(dim(calcRec(mat, ploidy = 4)), c(10, 10))
})