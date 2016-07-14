context("Map Manipulations Switch and Swap")

test_that("switchChrs returns correct map", {
  data("simTetra")
  simTetra <- basesToGenotypes(simTetra, ploidy = 4)
  rf <- calcRec(simTetra, ploidy = 4)
  split <- splitChr(rf, nchr = 7)
  sorted <- sortLeafs(rf, split)
  map <- pullMap(rf, sorted)
  map2 <- map
  map2[1] <- map[2]
  map2[2] <- map[1]
  map2[3] <- map[7]
  map2[[4]] <- rev(max(map2[[4]]) - map2[[4]])
  map2[6] <- map[3]
  map2[7] <- map[6]
  maps <- switchChrs(map = map, comp = map2)
  maps2 <- switchChrs(map = map2, comp = map)
  expect_true(is.list(maps))
  expect_true(is.list(maps2))
  expect_equal(length(maps), 7)
  expect_equal(length(maps2), 7)
  expect_equal(names(map[[4]]), names(maps2[[4]]))
  expect_equal(names(map[[5]]), names(maps2[[5]]))
  expect_equal(names(map2[[4]]), names(maps[[4]]))
  expect_equal(names(map2[[5]]), names(maps[[5]]))
  expect_equal(sapply(maps, length), c(15, 22, 17, 20, 22, 19, 16))
  expect_equal(sapply(maps2, length), c(22, 15, 16, 20, 22, 17, 19))
  expect_true(all(!duplicated(lapply(map, names))))
  expect_true(all(!duplicated(lapply(maps, names))))
})

test_that("swapChrs returns correct map", {
  data("simTetra")
  simTetra <- basesToGenotypes(simTetra, ploidy = 4)
  rf <- calcRec(simTetra, ploidy = 4)
  split <- splitChr(rf, nchr = 7)
  sorted <- sortLeafs(rf, split)
  map <- pullMap(rf, sorted)
  map2 <- map
  map2[1] <- map[2]
  map2[2] <- map[1]
  map2[3] <- map[7]
  map2[[4]] <- rev(max(map2[[4]]) - map2[[4]])
  map2[6] <- map[3]
  map2[7] <- map[6]
  maps <- swapChrs(map = map, comp = map2)
  maps2 <- swapChrs(map = map2, comp = map)
  expect_true(is.list(maps))
  expect_true(is.list(maps2))
  expect_equal(length(maps), 7)
  expect_equal(length(maps2), 7)
  expect_equal(names(map[[4]]), rev(names(maps2[[4]])))
  expect_equal(names(map[[5]]), names(maps2[[5]]))
  expect_equal(names(map2[[4]]), rev(names(maps[[4]])))
  expect_equal(names(map2[[5]]), names(maps[[5]]))
  expect_equal(sapply(maps, length), c(22, 15, 16, 20, 22, 17, 19))
  expect_equal(sapply(maps2, length), c(15, 22, 17, 20, 22, 19, 16))
  expect_true(all(!duplicated(lapply(map, names))))
  expect_true(all(!duplicated(lapply(maps, names))))
  
})
