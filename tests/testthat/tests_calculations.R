context("EGRET calculation functions")

test_that("calculateMonthly", {
  results <- calculateMonthlyResults(Arkansas_eList)
  expect_is(results, "data.frame")
  expect_gt(nrow(results), 1)
  expect_is(results$Q, "numeric")
})

test_that("flowDuration", {
  fd1 <- flowDuration(Arkansas_eList, span = 45, centerDate = "10-15")
  expect_is(fd1, "numeric")
  fd2 <- flowDuration(Arkansas_eList, span = 45,centerDate = "01-15")
  expect_false(any(fd1 == fd2))
  
  fd_30days <- flowDuration(Choptank_eList,"06-25", qUnit=1,span=30)
  fd_30_kcfs <- flowDuration(Choptank_eList,"06-25", qUnit=3,span=30)
  expect_false(any(fd_30days == fd_30_kcfs))
})

test_that("makeAnnualSeries", {
  
  #is everything right with this function?
})

test_that("makeAugmentedSample", {
  choptank_augmented <- makeAugmentedSample(Choptank_eList)
  expect_true(all(c("rResid", "rObserved") %in% 
                    names(choptank_augmented$Sample)))
  expect_is(getSample(choptank_augmented)$rResid, "numeric")
  expect_is(getSample(choptank_augmented)$rObserved, "numeric")
  expect_false(anyNA(getSample(choptank_augmented)$rObserved))
})

test_that("flux bias statistic", {
  fbs <- fluxBiasStat(getSample(Choptank_eList))
  expect_equal(ncol(fbs), 3)
  expect_is(fbs, "data.frame")
  expect_false(anyNA(fbs))
})
