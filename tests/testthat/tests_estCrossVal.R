context("testing estCrossVal")

test_that("estCrossVal adds correct, new columns", {
  
  # Choptank_eList has model results already, but the model 
  # hasn't been run since it was originally saved.
  eList <- Choptank_eList
  info_stale <- getInfo(eList)
  daily_stale <- getDaily(eList)
  sample_stale <- getSample(eList)
  
  info_orig <- info_stale[, 1:(which(names(info_stale) == "bottomLogQ") - 1)]
  daily_orig <- daily_stale[, 1:(which(names(daily_stale) == "yHat") - 1)]
  sample_orig <- sample_stale[, 1:(which(names(sample_stale) == "yHat") - 1)]
  surfaces_orig <- NA
  eList_orig <- mergeReport(info_orig, daily_orig, sample_orig, surfaces_orig)
  
  # execute cross validation
  numDays <- length(daily_orig$DecYear)
  DecLow <- daily_orig$DecYear[1]
  DecHigh <- daily_orig$DecYear[numDays]
  sample_crossval <- estCrossVal(numDays,DecLow,DecHigh,sample_orig)
  
  # estCrossVal adds three columns to Sample
  new_sample_cols <- setdiff(names(sample_crossval), names(sample_orig))
  expect_equal(sort(new_sample_cols), sort(c("yHat", "SE", "ConcHat")))
  
  # test that no original columns were lost in estCrossVal
  expect_true(all(names(sample_orig) %in% names(sample_crossval)))
  
  # verify that values of yHat, SE, and ConcHat are what they should be
  expect_equal(mean(sample_crossval$yHat), 0.0475113943)
  expect_equal(mean(sample_crossval$SE), 0.2629914096)
  expect_equal(mean(sample_crossval$ConcHat), 1.1272620557)
  
})


