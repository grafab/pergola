context("Shuffle input")

test_that("shuffleInput returns matrix", {
  expect_true(is.matrix(shuffleInput(matrix(1:80, ncol = 8))))
})

test_that("shuffleInput returns right sized matrix", {
  expect_equal(dim(shuffleInput(matrix(1:80, ncol = 8))), c(10, 8))
  expect_equal(dim(shuffleInput(matrix(1:24, ncol = 8))), c(3, 8))
  expect_equal(dim(shuffleInput(matrix(1:180, ncol = 18), ploidy = 6)), c(10, 18))
  expect_equal(dim(shuffleInput(matrix(1:8000, ncol = 800), ploidy = 8)), c(10, 800))
})

test_that("shuffleInput shuffles only within samples", {
  expect_equal(sort(rowSums(shuffleInput(
    matrix(1:80, ncol = 8)
  )[, 1:4])), sort(rowSums(matrix(1:80, ncol = 8)[, 1:4])))
  expect_equal(sort(rowSums(shuffleInput(
    matrix(1:24, ncol = 8)
  )[, 1:4])), sort(rowSums(matrix(1:24, ncol = 8)[, 1:4])))
  expect_equal(sort(rowSums(shuffleInput(
    matrix(1:180, ncol = 18), ploidy = 6
  )[, 1:6])), sort(rowSums(matrix(1:180, ncol = 18)[, 1:6])))
  expect_equal(sort(rowSums(shuffleInput(
    matrix(1:8000, ncol = 800), ploidy = 8
  )[, 1:8])), sort(rowSums(matrix(1:8000, ncol = 800)[, 1:8])))
})