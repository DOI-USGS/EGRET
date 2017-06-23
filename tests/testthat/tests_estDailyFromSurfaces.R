context("testing estDailyFromSurfaces")

test_that("estDailyFromSurfaces returns expected values", {
  
  skip_on_cran()
  
  # Uses original and "stale" versions of Choptank data created in `tests/helper-originaldata.R`
  
  # everything but daily has been modeled/altered in modelEstimation
  eList_Ch_estsurf <- mergeReport(info_stale_Ch, daily_orig_Ch, sample_stale_Ch, surfaces_stale_Ch)
  daily_est <- estDailyFromSurfaces(eList_Ch_estsurf)
  
  # estDailyFromSurfaces adds six columns to Daily
  new_daily_cols <- setdiff(names(daily_est), names(daily_orig_Ch))
  expect_equal(sort(new_daily_cols), sort(c("yHat", "SE", "ConcDay", "FluxDay", "FNConc", "FNFlux")))
  
  # test that no original columns were lost in estDailyFromSurfaces
  expect_true(all(names(daily_orig_Ch) %in% names(daily_est)))
  
  # verify that values of new columns are what they should be
  expect_equal(mean(daily_est$yHat), 0.1203179856)
  expect_equal(mean(daily_est$SE), 0.2689238161)
  expect_equal(mean(daily_est$ConcDay), 1.1977873668)
  expect_equal(mean(daily_est$FluxDay), 366.0845349588)
  expect_equal(mean(daily_est$FNConc), 1.2004191122)
  expect_equal(mean(daily_est$FNFlux), 362.7069812715)
  
})
