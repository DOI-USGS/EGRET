context("EGRET utils")
test_that("axis functions work", {
  
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

