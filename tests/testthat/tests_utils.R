context("EGRET utils")
test_that("axis functions work", {
  
})

test_that("nDischarge returns correct numbers", {
  expect_equal(nDischarge(Arkansas_eList), 8401)
  expect_equal(nDischarge(Choptank_eList), 11688)
})

test_that("nObservations returns correct numbers", {
  expect_equal(nObservations(Arkansas_eList), 254)
  expect_equal(nObservations(Choptank_eList), 606)
})

test_that("nCensored returns correct numbers", {
  expect_equal(nCensoredVals(Arkansas_eList), 115)
  expect_equal(nCensoredVals(Choptank_eList), 1)
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
