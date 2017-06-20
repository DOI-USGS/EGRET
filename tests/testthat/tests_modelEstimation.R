context("testing modelEstimation")

test_that("modelEstimation defaults works", {
  skip_on_cran()
  
  eList_MO <- Missouri_eList
  info_orig <- getInfo(eList_MO)
  daily_orig <- getDaily(eList_MO)
  sample_orig <- getSample(eList_MO)
  surfaces_orig <- getSurfaces(eList_MO)
  
  eList_MO_modeled <- modelEstimation(eList_MO)
  info <- getInfo(eList_MO_modeled)
  daily <- getDaily(eList_MO_modeled)
  sample <- getSample(eList_MO_modeled)
  surfaces <- getSurfaces(eList_MO_modeled)
  
  ## INFO ##
  
  # info retained columns are unaltered after running modelEstimation
  end_orig_cols_info <- which(names(info) == "bottomLogQ") - 1
  expect_true(all(names(info_orig) == names(info[,1:end_orig_cols_info])))
  expect_equal(sapply(info_orig, '[[', 1), sapply(info[,1:end_orig_cols_info], '[[', 1))
  
  # defaults show up in INFO
  expect_equal(info[['windowY']], 7)
  expect_equal(info[['windowQ']], 2)
  expect_equal(info[['windowS']], 0.5)
  
  ## DAILY ##
  
  # daily data is unaltered after running modelEstimation
  expect_equal(mean(daily[['Date']]), mean(daily_orig[['Date']]))
  expect_equal(mean(daily[['Q']]), mean(daily_orig[['Q']]))
  expect_equal(mean(daily[['Julian']]), mean(daily_orig[['Julian']]))
  expect_equal(mean(daily[['Month']]), mean(daily_orig[['Month']]))
  expect_equal(mean(daily[['Day']]), mean(daily_orig[['Day']]))
  expect_equal(mean(daily[['DecYear']]), mean(daily_orig[['DecYear']]))
  expect_equal(mean(daily[['MonthSeq']]), mean(daily_orig[['MonthSeq']]))
  expect_equal(mean(daily[['waterYear']]), mean(daily_orig[['waterYear']]))
  expect_equal(daily[['Qualifier']], daily_orig[['Qualifier']])
  expect_equal(mean(daily[['i']]), mean(daily_orig[['i']]))
  expect_equal(mean(daily[['LogQ']]), mean(daily_orig[['LogQ']]))
  expect_true(is.na(mean(daily[['Q7']])))
  expect_equal(mean(daily[['Q7']], na.rm=TRUE), mean(daily_orig[['Q7']], na.rm=TRUE))
  expect_true(is.na(mean(daily[['Q30']])))
  expect_equal(mean(daily[['Q30']], na.rm=TRUE), mean(daily_orig[['Q30']], na.rm=TRUE))
  
  # daily modeled values come out correctly with defaults
  expect_equal(mean(daily[['yHat']]), -0.0400111287)
  expect_equal(mean(daily[['SE']]), 0.5821201142)
  expect_equal(mean(daily[['ConcDay']]), 1.2539802527)
  expect_equal(mean(daily[['FluxDay']]), 319335.4938188065)
  expect_equal(mean(daily[['FNConc']]), 1.2587667435)
  expect_equal(mean(daily[['FNFlux']]), 320531.3099870029)
  
  ## SAMPLE ##
  
  # sample retained columns are unaltered after running modelEstimation
  end_orig_cols_sample <- which(names(sample) == "yHat") - 1
  expect_true(all(names(sample_orig) == names(sample[,1:end_orig_cols_sample])))
  expect_equal(sapply(sample_orig, '[[', 1), sapply(sample[,1:end_orig_cols_sample], '[[', 1))
  
  # sample new columns are correct
  expect_true(is.na(mean(sample[['yHat']])))
  expect_equal(mean(sample[['yHat']], na.rm=TRUE), 0.0654536184)
  expect_true(is.na(mean(sample[['SE']])))
  expect_equal(mean(sample[['SE']], na.rm=TRUE), 0.5675788062)
  expect_true(is.na(mean(sample[['ConcHat']])))
  expect_equal(mean(sample[['ConcHat']], na.rm=TRUE), 1.3775047467)
  
  ## SURFACES ##
  
  # modelEstimation adds surfaces values
  expect_true(is.na(surfaces_orig))
  expect_true(all(names(eList_MO_modeled) %in% c("INFO", "Daily", "Sample", "surfaces")))
  expect_equal(nrow(surfaces), 14)
  
  # surface estimations are correct
  summary_surface <- summary(surfaces)
  expect_equal(summary_surface[['Min.']], -5.4647085811)
  expect_equal(summary_surface[['1st Qu.']], 0.2224051001)
  expect_equal(summary_surface[['Median']], 0.5149727385)
  expect_equal(summary_surface[['Mean']], 0.4809767574)
  expect_equal(summary_surface[['3rd Qu.']], 0.8496063413)
  expect_equal(summary_surface[['Max.']], 3.3714962000)
  
})

test_that("modelEstimation window params work", {
  skip_on_cran()
  
  eList_MO <- Missouri_eList
  eList_MO_modeled <- modelEstimation(Missouri_eList, windowY = 5,
                                   windowQ = 3, windowS = 0.25)
  
  info <- getInfo(eList_MO_modeled)
  daily <- getDaily(eList_MO_modeled)
  sample <- getSample(eList_MO_modeled)
  surfaces <- getSurfaces(eList_MO_modeled)
  
  ## INFO ##
  
  # defaults show up in INFO
  expect_equal(info[['windowY']], 5)
  expect_equal(info[['windowQ']], 3)
  expect_equal(info[['windowS']], 0.25)
  
  ## DAILY ##
  
  # daily modeled values come out correctly with defaults
  expect_equal(mean(daily[['yHat']]), -0.0231696639)
  expect_equal(mean(daily[['SE']]), 0.5445577557)
  expect_equal(mean(daily[['ConcDay']]), 1.2706431618)
  expect_equal(mean(daily[['FluxDay']]), 333863.6197739398)
  expect_equal(mean(daily[['FNConc']]), 1.2831578117)
  expect_equal(mean(daily[['FNFlux']]), 339500.9932348605)
  
  ## SAMPLE ##

  # sample new columns are correct
  expect_true(is.na(mean(sample[['yHat']])))
  expect_equal(mean(sample[['yHat']], na.rm=TRUE), 0.0780289972)
  expect_true(is.na(mean(sample[['SE']])))
  expect_equal(mean(sample[['SE']], na.rm=TRUE), 0.5307910086)
  expect_true(is.na(mean(sample[['ConcHat']])))
  expect_equal(mean(sample[['ConcHat']], na.rm=TRUE), 1.3869976229)
  
  ## SURFACES ##
  
  # surface estimations are correct
  summary_surface <- summary(surfaces)
  expect_equal(summary_surface[['Min.']], -3.2646182801)
  expect_equal(summary_surface[['1st Qu.']], 0.3045067904)
  expect_equal(summary_surface[['Median']], 0.5462399674)
  expect_equal(summary_surface[['Mean']], 0.6296340119)
  expect_equal(summary_surface[['3rd Qu.']], 0.9179736941)
  expect_equal(summary_surface[['Max.']], 4.6060525904)
  
})
