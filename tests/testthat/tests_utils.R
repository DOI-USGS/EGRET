context("EGRET utils")
test_that("axis functions work", {
  
})

test_that("nObservations returns correct numbers", {
  expect_equal(nObservations(Arkansas_eList), 8401)
  expect_equal(nObservations(Choptank_eList), 11688)
})

test_that("nSamples returns correct numbers", {
  expect_equal(nSamples(Arkansas_eList), 254)
  expect_equal(nSamples(Choptank_eList), 606)
})

context("plot method for egret objects")

test_that("plot method for egret objects work", {
  graphics.off()
  dev_start <- dev.cur()
  eList <- Choptank_eList
  plot(eList)
  
  expect_true(dev_start + 1 == dev.cur())
})

test_that("plot.egret passes correct arguments", {
  graphics.off()
  dev_start <- dev.cur()
  eList <- Choptank_eList
  plot(eList, logScaleConc = TRUE)
  
  expect_true(dev_start + 1 == dev.cur())
})

test_that("plot.egret passes correct arguments", {
  expect_error(plot(eList, col='blue'))
})
