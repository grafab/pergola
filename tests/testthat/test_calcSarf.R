context("Calculate SARF")

test_that("CalcSarf returns correct values", {
  data("simTetra")
  simTetra <- basesToGenotypes(simTetra, ploidy = 4)
  rf <- calcRec(simTetra, ploidy = 4)
  expect_true(is.numeric(calcSarf(rf)))
  expect_equal(calcSarf(rf), 13.67916963)
  expect_equal(calcSarf(rf, n = 2), 36.96114754)
  expect_equal(calcSarf(rf, n = 3), 67.16071729)
  expect_equal(calcSarf(rf,ord = c(1:100)), 10.89730579)
})