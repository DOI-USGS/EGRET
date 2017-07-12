context("EGRET utils")
test_that("axis functions work", {
  #logPretty, generalAxis
  axis1 <- logPretty1(0.7, 990000)
  expect_is(axis1, "numeric")
  expect_equal(range(axis1), c(1e-1, 1e6))
  
  axis3 <- logPretty3(3,60)
  expect_is(axis3, "numeric")
  expect_equal(range(axis3), c(2, 100))
  
  q <- Choptank_eList$Daily$Q
  genAx <- generalAxis(x = q, max = max(q), min = min(q), 
                       units = getInfo(Choptank_eList)$param.units)
  expect_is(genAx, "list")
  expect_is(genAx$ticks, "numeric")
  expect_equal(genAx$bottom, 0)
  expect_equal(genAx$top, 250)
  expect_equal(range(genAx$ticks), c(0,250))
})