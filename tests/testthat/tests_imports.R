context("EGRET retrieval tests")

test_that("External Daily tests", {
  testthat::skip_on_cran()
  DailyNames <- c("Date","Q","Julian","Month","MonthSeq","waterYear",  
                   "Day","DecYear","Qualifier","i","LogQ","Q7","Q30")
  Daily <- readNWISDaily('01594440',
                          '00060', 
                         '1985-01-01', 
                         '1985-03-31')
  expect_that(all(names(Daily) %in% DailyNames),is_true())
  expect_is(Daily$Date, 'Date')
  expect_is(Daily$Q, 'numeric')
  DailySuspSediment <- readNWISDaily('01594440',
                                     '80154', 
                                     '1985-01-01', 
                                     '1985-03-31',convert=FALSE)
  
  expect_is(DailySuspSediment$Date, 'Date')
  expect_is(DailySuspSediment$Q, 'numeric')
  
})

test_that("External NWIS Sample tests", {
  testthat::skip_on_cran()
  
  SampleNames <- c("Date","ConcLow","ConcHigh","Uncen","ConcAve","Julian","Month",   
                   "Day","DecYear","MonthSeq","waterYear","SinDY","CosDY")
  
  Sample_01075 <- readNWISSample('01594440',
                                 '01075', 
                                 '1985-01-01', 
                                 '1985-03-31')
  expect_that(all(names(Sample_01075) %in% SampleNames),is_true())
  
  Sample_All2 <- readNWISSample('05114000',
                                c('00915','00931'), 
                                '1985-01-01', 
                                '1985-03-31')
  expect_that(all(names(Sample_All2) %in% SampleNames),is_true())
  
  Sample_Select <- readNWISSample('05114000',
                                  c('00915','00931'), 
                                  '', '')
  
  expect_that(all(names(Sample_Select) %in% SampleNames),is_true())
  
  expect_is(Sample_Select$Date, 'Date')
  expect_is(Sample_Select$ConcAve, 'numeric')
  expect_that(nrow(Sample_Select) > nrow(Sample_All2),is_true())
  
})

test_that("External WQP Sample tests", {
  testthat::skip_on_cran()
  
  SampleNames <- c("Date","ConcLow","ConcHigh","Uncen","ConcAve","Julian","Month",   
                   "Day","DecYear","MonthSeq","waterYear","SinDY","CosDY")
  
  Sample_Chloride <- readWQPSample('USGS-01594440',
                                'Chloride', 
                                '', '')
  
  expect_that(all(names(Sample_Chloride) %in% SampleNames),is_true())
    
})

test_that("External INFO tests", {
  testthat::skip_on_cran()
  requiredColumns <- c("shortName", "paramShortName","constitAbbrev",
                       "drainSqKm","paStart","paLong")
  
  INFO <- readNWISInfo('05114000','00010',interactive=FALSE)
  expect_that(all(requiredColumns %in% names(INFO)),is_true())
  
  nameToUse <- 'Specific conductance'
  pcodeToUse <- '00095'
  
  INFO_WQP <- readWQPInfo('USGS-04024315',pcodeToUse,interactive=FALSE)
  expect_that(all(requiredColumns %in% names(INFO_WQP)),is_true())
  
  INFO2 <- readWQPInfo('WIDNR_WQX-10032762',nameToUse,interactive=FALSE)
  expect_that(all(requiredColumns %in% names(INFO2)),is_true())
  
  
})