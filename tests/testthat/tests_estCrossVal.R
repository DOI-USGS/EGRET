context("testing estCrossVal")

test_that("estCrossVal adds correct, new columns", {
  
  # Uses original and "stale" versions of Choptank data created in `tests/helper-originaldata.R`
  
  # execute cross validation
  DecLow <- range(daily_orig_Ch$DecYear)[1]
  DecHigh <- range(daily_orig_Ch$DecYear)[2]
  sample_crossval <- estCrossVal(DecLow,DecHigh,sample_orig_Ch)
  
  # estCrossVal adds three columns to Sample
  new_sample_cols <- setdiff(names(sample_crossval), names(sample_orig_Ch))
  expect_equal(sort(new_sample_cols), sort(c("yHat", "SE", "ConcHat")))
  
  # test that no original columns were lost in estCrossVal
  expect_true(all(names(sample_orig_Ch) %in% names(sample_crossval)))
  
  # verify that values of yHat, SE, and ConcHat are what they should be
  expect_equal(mean(sample_crossval$yHat), 0.0475113943)
  expect_equal(mean(sample_crossval$SE), 0.2629914096)
  expect_equal(mean(sample_crossval$ConcHat), 1.1272620557)
  
})


