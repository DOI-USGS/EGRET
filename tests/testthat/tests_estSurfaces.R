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
  expect_equal(surfaces_Ar[1,1,1], -2.8859272183)
  expect_equal(surfaces_Ar[1,50,1], -3.3663536796)
  expect_equal(surfaces_Ar[8,1,1], -2.2874716935)
  
  # test some SE values 
  expect_equal(surfaces_Ar[1,1,2], 0.9012848883)
  expect_equal(surfaces_Ar[5,100,2], 0.6999714816)
  expect_equal(surfaces_Ar[12,19,2], 0.5216116210)
  
  # test some ConcHat values 
  expect_equal(surfaces_Ar[2,62,3], 0.0633582807)
  expect_equal(surfaces_Ar[9,63,3], 0.0736049109)
  expect_equal(surfaces_Ar[6,98,3], 0.0799091333)
  
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
  expect_equal(surfaces_Ar[1,1,1], -4.7345368054)
  expect_equal(surfaces_Ar[1,50,1], -5.1625803732)
  expect_equal(surfaces_Ar[8,1,1], -2.1432899668)
  
  # test some SE values 
  expect_equal(surfaces_Ar[1,1,2], 1.0608780805)
  expect_equal(surfaces_Ar[5,100,2], 1.0162851948)
  expect_equal(surfaces_Ar[12,19,2], 0.4478360954)
  
  # test some ConcHat values 
  expect_equal(surfaces_Ar[2,62,3], 0.0223554843)
  expect_equal(surfaces_Ar[9,63,3], 0.0627927472)
  expect_equal(surfaces_Ar[6,98,3], 0.0986004193)
  
})
