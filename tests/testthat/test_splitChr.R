context("Split Chromosomes")

test_that("splitChr stops when no rownames are provided", {
  mat <- calcRec(matrix(sample(0:4, size = 8000, replace = TRUE), ncol = 800), ploidy = 4)
    expect_error(splitChr(mat))
})

test_that("splitChr returns data frame with columns name split dup", {
  mat <- calcRec(matrix(sample(0:4, size = 8000, replace = TRUE), ncol = 800), ploidy = 4)
  rownames(mat) <- LETTERS[1:10]
  split <- splitChr(mat)
  expect_true(is.data.frame(split))
  expect_equal(names(split), c("names", "split", "dup"))
})

test_that("splitChr returns correct number of markers", {
  mat <- calcRec(matrix(sample(0:4, size = 8000, replace = TRUE), ncol = 800), ploidy = 4)
  rownames(mat) <- LETTERS[1:10]
  expect_equal(nrow(splitChr(mat)), 10)
})