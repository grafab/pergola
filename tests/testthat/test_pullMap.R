context("Pull Map")

test_that("pullMap returns correct data frame", {
  data("simTetra")
  simTetra <- basesToGenotypes(simTetra, ploidy = 4)
  rf <- calcRec(simTetra, ploidy = 4)
  split <- splitChr(rf, nchr = 7)
  sorted <- sortLeafs(rf, split)
  map <- pullMap(rf, sorted)
  expect_true(is.list(map))
  expect_equal(length(map), 7)
  expect_equal(sapply(map, length), c(15, 22, 17, 20, 22, 19, 16))
  expect_true(all(!duplicated(lapply(map, names))))
})