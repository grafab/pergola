context("Sort Leafs")

test_that("sortLeafs returns correct data frame", {
  data("simTetra")
  simTetra <- basesToGenotypes(simTetra, ploidy = 4)
  rf <- calcRec(simTetra, ploidy = 4)
  split <- splitChr(rf, nchr = 7)
  sorted <- sortLeafs(rf, split)
  expect_true(is.data.frame(sorted))
  expect_equal(nrow(sorted), nrow(split))
  expect_equal(names(sorted), c("names", "split", "dup", "order"))
  expect_true(all(!duplicated(sorted$order[sorted$order>0])))
})

test_that("sortLeafs creates correct order", {
  correct <- c(15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 37, 36, 
    35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 
    19, 18, 17, 16, 54, 53, 52, 51, 50, 49, 48, 47, 46, 45, 44, 43, 
    42, 41, 40, 39, 38, 74, 73, 72, 71, 70, 69, 68, 67, 66, 65, 64, 
    63, 62, 61, 60, 59, 58, 57, 56, 55, 75, 76, 77, 78, 79, 80, 81, 
    82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 115, 
    114, 113, 112, 111, 110, 109, 108, 107, 106, 105, 104, 103, 102, 
    101, 100, 99, 98, 97, 131, 130, 129, 128, 127, 126, 125, 124, 
    123, 122, 121, 120, 119, 118, 117, 116)
  data("simTetra")
  simTetra <- basesToGenotypes(simTetra, ploidy = 4)
  rf <- calcRec(simTetra, ploidy = 4)
  split <- splitChr(rf, nchr = 7)
  sorted <- sortLeafs(rf, split)
  sorted2 <- sortLeafs(rf, split,method = "endlink")
  expect_equal(sorted$order, correct)
  expect_equal(sorted2$order, 1:131)
})

