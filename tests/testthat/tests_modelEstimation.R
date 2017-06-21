context("testing modelEstimation")

test_that("modelEstimation produces correct values with default args", {
  skip_on_cran()
  
  # Choptank_eList has model results already, but the model 
  # hasn't been run since it was originally saved.
  eList <- Choptank_eList
  info_stale <- getInfo(eList)
  daily_stale <- getDaily(eList)
  sample_stale <- getSample(eList)
  
  info_orig <- info_stale[, 1:(which(names(info_stale) == "bottomLogQ") - 1)]
  daily_orig <- daily_stale[, 1:(which(names(daily_stale) == "Q30") - 1)]
  sample_orig <- sample_stale[, 1:(which(names(sample_stale) == "yHat") - 1)]
  surfaces_orig <- NA
  eList_orig <- mergeReport(info_orig, daily_orig, sample_orig, surfaces_orig)
  
  eList_modeled <- modelEstimation(eList_orig)
  info_modeled <- getInfo(eList_modeled)
  daily_modeled <- getDaily(eList_modeled)
  sample_modeled <- getSample(eList_modeled)
  surfaces_modeled <- getSurfaces(eList_modeled)
  
  ## INFO ##
  
  # columns retained in info are unaltered after running modelEstimation
  orig_cols_info <- 1:which(names(info_modeled) == "bottomLogQ") - 1
  expect_true(all(names(info_orig) == names(info_modeled[, orig_cols_info])))
  expect_equal(sapply(info_orig, '[[', 1), sapply(info_modeled[, orig_cols_info], '[[', 1))
  
  # defaults show up in INFO
  expect_equal(info_modeled[['windowY']], 7)
  expect_equal(info_modeled[['windowQ']], 2)
  expect_equal(info_modeled[['windowS']], 0.5)
  
  ## DAILY ##
  
  # daily data is unaltered after running modelEstimation
  expect_equal(mean(daily_modeled[['Date']]), mean(daily_orig[['Date']]))
  expect_equal(mean(daily_modeled[['Q']]), mean(daily_orig[['Q']]))
  expect_equal(mean(daily_modeled[['Julian']]), mean(daily_orig[['Julian']]))
  expect_equal(mean(daily_modeled[['Month']]), mean(daily_orig[['Month']]))
  expect_equal(mean(daily_modeled[['Day']]), mean(daily_orig[['Day']]))
  expect_equal(mean(daily_modeled[['DecYear']]), mean(daily_orig[['DecYear']]))
  expect_equal(mean(daily_modeled[['MonthSeq']]), mean(daily_orig[['MonthSeq']]))
  expect_equal(daily_modeled[['Qualifier']], daily_orig[['Qualifier']])
  expect_equal(mean(daily_modeled[['i']]), mean(daily_orig[['i']]))
  expect_equal(mean(daily_modeled[['LogQ']]), mean(daily_orig[['LogQ']]))
  expect_true(is.na(mean(daily_modeled[['Q7']])))
  expect_equal(mean(daily_modeled[['Q7']], na.rm=TRUE), mean(daily_orig[['Q7']], na.rm=TRUE))
  # expect_true(is.na(mean(daily_modeled[['Q30']])))
  # expect_equal(mean(daily_modeled[['Q30']], na.rm=TRUE), mean(daily_orig[['Q30']], na.rm=TRUE))
  
  # daily modeled values come out correctly with defaults
  expect_equal(mean(daily_modeled[['yHat']]), 0.1189346162)
  expect_equal(mean(daily_modeled[['SE']]), 0.2680897030)
  expect_equal(mean(daily_modeled[['ConcDay']]), 1.1976723069)
  expect_equal(mean(daily_modeled[['FluxDay']]), 366.4582148138)
  expect_equal(mean(daily_modeled[['FNConc']]), 1.1982113316)
  expect_equal(mean(daily_modeled[['FNFlux']]), 363.0914843289)
  
  ## SAMPLE ##
  
  # columns retained in sample are unaltered after running modelEstimation
  orig_cols_sample <- 1:which(names(sample_modeled) == "yHat") - 1
  # expect_true(all(names(sample_orig) == names(sample_modeled[, orig_cols_sample])))
  # expect_equal(sapply(sample_orig, '[[', 1), sapply(sample_modeled[, orig_cols_sample], '[[', 1))
  
  # sample new columns are correct
  expect_equal(mean(sample_modeled[['yHat']]), 0.0475113943)
  expect_equal(mean(sample_modeled[['SE']]), 0.2629914096)
  expect_equal(mean(sample_modeled[['ConcHat']]), 1.1272620557)
  
  ## SURFACES ##
  
  # modelEstimation adds surfaces values
  expect_true(is.na(surfaces_orig))
  expect_true(all(names(eList_modeled) %in% c("INFO", "Daily", "Sample", "surfaces")))
  expect_equal(nrow(surfaces_modeled), 14)
  
  # surface estimations are correct
  summary_surface <- summary(surfaces_modeled)
  expect_equal(summary_surface[['Min.']], -2.3170200930)
  expect_equal(summary_surface[['1st Qu.']], 0.1806983451)
  expect_equal(summary_surface[['Median']], 0.3121496809)
  expect_equal(summary_surface[['Mean']], 0.4082161671)
  expect_equal(summary_surface[['3rd Qu.']], 0.7356174773)
  expect_equal(summary_surface[['Max.']], 3.4505378638)
  
})

test_that("modelEstimation window params work", {
  skip_on_cran()
  
  # Arkansas_eList has model results already, but the model 
  # hasn't been run since it was originally saved.
  eList <- Arkansas_eList
  info_stale <- getInfo(eList)
  daily_stale <- getDaily(eList)
  sample_stale <- getSample(eList)
  
  info_orig <- info_stale[, 1:(which(names(info_stale) == "bottomLogQ") - 1)]
  daily_orig <- daily_stale[, 1:(which(names(daily_stale) == "Q30") - 1)]
  sample_orig <- sample_stale[, 1:(which(names(sample_stale) == "yHat") - 1)]
  surfaces_orig <- NA
  eList_orig <- mergeReport(info_orig, daily_orig, sample_orig, surfaces_orig)
  
  eList_modeled <- modelEstimation(eList_orig, windowY = 5,
                                   windowQ = 3, windowS = 0.25)
  info_modeled <- getInfo(eList_modeled)
  daily_modeled <- getDaily(eList_modeled)
  sample_modeled <- getSample(eList_modeled)
  surfaces_modeled <- getSurfaces(eList_modeled)
  
  ## INFO ##
  
  # info shows over-ridden default args
  expect_equal(info_modeled[['windowY']], 5)
  expect_equal(info_modeled[['windowQ']], 3)
  expect_equal(info_modeled[['windowS']], 0.25)
  
  ## DAILY ##
  
  # daily modeled values come out correctly with user args
  expect_equal(mean(daily_modeled[['yHat']]), -3.4706448494)
  expect_equal(mean(daily_modeled[['SE']]), 0.6752682594)
  expect_equal(mean(daily_modeled[['ConcDay']]), 0.0443946602)
  expect_equal(mean(daily_modeled[['FluxDay']]), 205342.7065510541)
  expect_equal(mean(daily_modeled[['FNConc']]), 0.0445119162)
  expect_equal(mean(daily_modeled[['FNFlux']]), 200124.8625001870)
  
  ## SAMPLE ##

  # sample new columns are correct
  expect_equal(mean(sample_modeled[['yHat']]), -3.5152636876)
  expect_equal(mean(sample_modeled[['SE']]), 0.6791096992)
  expect_equal(mean(sample_modeled[['ConcHat']]), 0.0421790128)
  
  ## SURFACES ##
  
  # surface estimations are correct
  summary_surface <- summary(surfaces_modeled)
  expect_equal(summary_surface[['Min.']], -8.7775151764)
  expect_equal(summary_surface[['1st Qu.']], -3.1610429534)
  expect_equal(summary_surface[['Median']], 0.0360327282)
  expect_equal(summary_surface[['Mean']], -1.0061587398)
  expect_equal(summary_surface[['3rd Qu.']], 0.5875311446)
  expect_equal(summary_surface[['Max.']], 1.3902387681)
  
})
