context("EGRET calculation functions")

test_that("calculateMonthly returns expected type/format", {
  results <- calculateMonthlyResults(Arkansas_eList)
  expect_is(results, "data.frame")
  expect_gt(nrow(results), 1)
  expect_is(results$Q, "numeric")
})

test_that("flowDuration returns expected type, and arguments change output", {
  #using < 1 year span so results change with different centerDate
  fd1 <- flowDuration(Arkansas_eList, span = 45, centerDate = "10-15")
  expect_is(fd1, "numeric")
  fd2 <- flowDuration(Arkansas_eList, span = 45,centerDate = "01-15")
  expect_false(any(fd1 == fd2))
  
  #output with different units
  fd_30days <- flowDuration(Choptank_eList,"06-25", qUnit=1,span=30)
  fd_30_kcfs <- flowDuration(Choptank_eList,"06-25", qUnit=3,span=30)
  fd_30_cms <- flowDuration(Choptank_eList,"06-25",span=30)
  expect_equal(fd_30days, fd_30_kcfs*1000)
  expect_equal(fd_30_cms*3.28^3, fd_30days, tolerance = 0.001)
})

test_that("makeAnnualSeries works", {
  annualSeries <- makeAnnualSeries(Choptank_eList)
  expect_is(annualSeries, "array")
  expect_equal(dim(annualSeries), c(3,8,34))
  expect_is(annualSeries[1,1,], "numeric")
})

test_that("makeAugmentedSample returns expected types and Sample columns", {
  choptank_augmented <- makeAugmentedSample(Choptank_eList)
  expect_true(all(c("rResid", "rObserved") %in% 
                    names(choptank_augmented$Sample)))
  expect_is(getSample(choptank_augmented)$rResid, "numeric")
  expect_is(getSample(choptank_augmented)$rObserved, "numeric")
  expect_false(anyNA(getSample(choptank_augmented)$rObserved))
})

test_that("fluxBiasStat returns expected data frame", {
  fbs <- fluxBiasStat(getSample(Choptank_eList))
  expect_equal(ncol(fbs), 3)
  expect_is(fbs, "data.frame")
  expect_false(anyNA(fbs))
})
