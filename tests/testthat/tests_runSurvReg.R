context("testing runSurvReg")

test_that("runSurvReg returns expected values with defaults", {
  
  # Uses original and "stale" versions of Arkansas data created in `tests/helper-originaldata.R`
  
  # get subset of data to test survival regression
  i <- c(23, 51, 86, 3)
  yrestpts <- sample_orig_Ar$DecYear[i]
  logqestpts <- sample_orig_Ar$LogQ[i]
  
  # get info about sample data frame
  yrstart <- min(sample_orig_Ar$DecYear)
  yrend <- max(sample_orig_Ar$DecYear)
  
  surv <- runSurvReg(yrestpts, logqestpts, yrstart, yrend, sample_orig_Ar)
  
  # test format of results
  expect_equal(dim(surv), c(4,3))
  expect_null(names(surv))
  expect_is(surv, "matrix")
  
  # check values from each column
  expect_equal(surv[1,1], -2.8592687416)
  expect_equal(surv[3,2], 0.7004624715)
  expect_equal(surv[4,3], 0.1019059051)
  
})

test_that("runSurvReg returns expected values with user args", {
  
  # Uses original and "stale" versions of Choptank data created in `tests/helper-originaldata.R`
  
  # get subset of data to test survival regression
  i <- c(19, 171, 512, 303, 11)
  yrestpts <- sample_orig_Ch$DecYear[i]
  logqestpts <- sample_orig_Ch$LogQ[i]
  
  # get info about sample data frame
  yrstart <- min(sample_orig_Ch$DecYear)
  yrend <- max(sample_orig_Ch$DecYear)
  
  surv <- runSurvReg(yrestpts, logqestpts, yrstart, yrend, sample_orig_Ch,
                     windowQ = 0.5, minNumObs = 150)
  
  # test format of results
  expect_equal(dim(surv), c(5,3))
  expect_null(names(surv))
  expect_is(surv, "matrix")
  
  # check values from each column
  expect_equal(surv[5,1], 0.0155175200)
  expect_equal(surv[2,2], 0.1936220144)
  expect_equal(surv[3,3], 0.6526830391)
  
})
