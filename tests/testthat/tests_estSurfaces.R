context("testing estSurfaces")

test_that("estSurfaces gives expected results with default args", {
  skip_on_cran()
  
  # Uses original and "stale" versions of Arkansas data created in `tests/helper-originaldata.R`
  
  surfaces_Ar <- estSurfaces(eList_orig_Ar)
  
  # test that dimensions returned are correct
  expect_equal(nrow(surfaces_Ar), 14) # num logQ 
  expect_equal(ncol(surfaces_Ar[1,,]), 3) # yHat, SE, and ConcHat
  expect_equal(nrow(surfaces_Ar[10,,]), 385) # time dimension is correct
  
  # test some yHat values 
  expect_equal(signif(surfaces_Ar[1,1,1], 4), -2.886)
  expect_equal(signif(surfaces_Ar[1,50,1], 4), -3.366)
  expect_equal(signif(surfaces_Ar[8,1,1], 4), -2.287)
  
  # test some SE values 
  expect_equal(signif(surfaces_Ar[1,1,2], 4), 0.9013)
  expect_equal(signif(surfaces_Ar[5,100,2], 4), 0.7)
  expect_equal(signif(surfaces_Ar[12,19,2], 4), 0.5216)
  
  # test some ConcHat values 
  expect_equal(signif(surfaces_Ar[2,62,3], 4), 0.06336)
  expect_equal(signif(surfaces_Ar[9,63,3], 4), 0.07361)
  expect_equal(signif(surfaces_Ar[6,98,3], 4), 0.07991)
  
})


test_that("estSurfaces gives expected results with user-specified args", {
  skip_on_cran()
  
  # Uses original and "stale" versions of Choptank data created in `tests/helper-originaldata.R`
  
  surfaces_Ar <- estSurfaces(eList_orig_Ar, windowY = 5, windowQ = 0.5, windowS = 1,
                             minNumObs = 50, minNumUncen = 25, edgeAdjust = FALSE)
  
  # test that dimensions returned are correct
  expect_equal(nrow(surfaces_Ar), 14) # num logQ 
  expect_equal(ncol(surfaces_Ar[1,,]), 3) # yHat, SE, and ConcHat
  expect_equal(nrow(surfaces_Ar[10,,]), 385) # time dimension is correct
  
  # test some yHat values 
  expect_equal(signif(surfaces_Ar[1,1,1], 4), -4.735)
  expect_equal(signif(surfaces_Ar[1,50,1], 4), -5.163)
  expect_equal(signif(surfaces_Ar[8,1,1], 4), -2.143)
  
  # test some SE values 
  expect_equal(signif(surfaces_Ar[1,1,2], 4), 1.061)
  expect_equal(signif(surfaces_Ar[5,100,2], 4), 1.016)
  expect_equal(signif(surfaces_Ar[12,19,2], 4), 0.4478)
  
  # test some ConcHat values 
  expect_equal(signif(surfaces_Ar[2,62,3], 4), 0.02236)
  expect_equal(signif(surfaces_Ar[9,63,3], 4), 0.06279)
  expect_equal(signif(surfaces_Ar[6,98,3], 4), 0.09860)
  
})
