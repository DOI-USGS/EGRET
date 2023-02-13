context("EGRET retrieval tests")

test_that("External Daily tests", {
  testthat::skip_on_cran()
  DailyNames <- c("Date","Q","Julian","Month","MonthSeq","waterYear",  
                   "Day","DecYear","Qualifier","i","LogQ","Q7","Q30")
  Daily <- readNWISDaily('01594440',
                          '00060', 
                         '1985-01-01', 
                         '1985-03-31')
  expect_true(all(names(Daily) %in% DailyNames))
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
  
  expect_warning(Sample_01075 <- readNWISSample('01594440',
                                 '01075', 
                                 '1985-01-01', 
                                 '1985-03-31'))
  
  expect_true(all(names(Sample_01075) %in% SampleNames))
  
  expect_warning(Sample_All2 <- readNWISSample('05114000',
                                c('00915','00931'), 
                                '1985-01-01', 
                                '1985-03-31'))
  
  expect_true(all(names(Sample_All2) %in% SampleNames))
  
  Sample_Select <- expect_warning(readNWISSample('05114000',
                                  c('00915','00931'), 
                                  '', ''))
  
  expect_true(all(names(Sample_Select) %in% SampleNames))
  
  expect_is(Sample_Select$Date, 'Date')
  expect_is(Sample_Select$ConcAve, 'numeric')
  expect_true(nrow(Sample_Select) > nrow(Sample_All2))
  
})

test_that("External WQP Sample tests", {
  testthat::skip_on_cran()
  
  SampleNames <- c("Date","ConcLow","ConcHigh","Uncen","ConcAve","Julian","Month",   
                   "Day","DecYear","MonthSeq","waterYear","SinDY","CosDY")
  
  Sample_All <- readWQPSample('WIDNR_WQX-10032762','Specific conductance', '', '')

  expect_true(all(SampleNames %in% names(Sample_All)))
    
})

test_that("External INFO tests", {
  testthat::skip_on_cran()
  requiredColumns <- c("shortName", "paramShortName","constitAbbrev",
                       "drainSqKm","paStart","paLong")
  
  INFO <- readNWISInfo('05114000','00010',interactive=FALSE)
  expect_true(all(requiredColumns %in% names(INFO)))
  
  # nameToUse <- 'Specific conductance'
  # pcodeToUse <- '00095'
  # 
  # INFO_WQP <- readWQPInfo('USGS-04024315',pcodeToUse,interactive=FALSE)
  # expect_true(all(requiredColumns %in% names(INFO_WQP)))
  # 
  # INFO2 <- readWQPInfo('WIDNR_WQX-10032762',nameToUse,interactive=FALSE)
  # expect_true(all(requiredColumns %in% names(INFO2)))
  # 
  # 
})

test_that("User tests", {
  
  filePath <- system.file("extdata", package="EGRET")
  fileName <- 'ChoptankRiverFlow.txt'
  ChopData <- readDataFromFile(filePath,fileName, separator="\t")
  
  expect_equal(ncol(ChopData), 2)
  fileNameDaily <- "ChoptankRiverFlow.txt"
  Daily_user <- readUserDaily(filePath,fileNameDaily,separator="\t",verbose=FALSE)
  
  DailyNames <- c("Date","Q","Julian","Month","MonthSeq","waterYear",  
                  "Day","DecYear","Qualifier","i","LogQ","Q7","Q30")
  expect_true(all(names(Daily_user) %in% DailyNames))
  
  fileNameSample <- 'ChoptankRiverNitrate.csv'
  Sample_user <- readUserSample(filePath,fileNameSample, separator=";",verbose=FALSE)
  
  SampleNames <- c("Date","ConcLow","ConcHigh","Uncen","ConcAve","Julian","Month",   
                   "Day","DecYear","MonthSeq","waterYear","SinDY","CosDY")

  expect_true(all(names(Sample_user) %in% SampleNames))
  
})


test_that("processQWData", {
  testthat::skip_on_cran()
  rawWQP <- dataRetrieval::readWQPqw('WIDNR_WQX-10032762','Specific conductance', '2012-01-01', '2012-12-31')
  Sample2 <- processQWData(rawWQP, pCode=FALSE)
  expect_true(all(unique(Sample2$qualifier) %in% c("","<")))
})
