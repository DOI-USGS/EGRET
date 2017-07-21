context("Flow normalization")

test_that("setupYears", {
  testthat::skip_on_cran()
  
  eList <- Choptank_eList
  Daily <- getDaily(eList)
  #Dec - Feb
  AnnualResults <- setupYears(Daily, 4, 10)
  expect_equal(nrow(AnnualResults), 32)
  
  expect_equal(signif(AnnualResults$DecYear[1], digits = 7), 1979.917)
  expect_equal(signif(AnnualResults$FNConc[1], digits = 7), 1.066795)
  expect_equal(signif(AnnualResults$FNFlux[1], digits = 7), 264.495)
  expect_equal(signif(AnnualResults$Conc[1], digits = 7), 0.9007685)
  expect_equal(signif(AnnualResults$Flux[1], digits = 7), 359.9743)
  expect_equal(signif(AnnualResults$Q[1], digits = 7), 5.02106)
  
  AnnualResults_waterYear <- setupYears(Daily)
  expect_equal(nrow(AnnualResults_waterYear), 32)
  
  expect_equal(signif(AnnualResults_waterYear$DecYear[1], digits = 7), 1980.249)
  expect_equal(signif(AnnualResults_waterYear$FNConc[1], digits = 7), 1.002724)
  expect_equal(signif(AnnualResults_waterYear$FNFlux[1], digits = 7), 291.2176)
  expect_equal(signif(AnnualResults_waterYear$Conc[1], digits = 7), 0.9485403)
  expect_equal(signif(AnnualResults_waterYear$Flux[1], digits = 7), 316.0491)
  expect_equal(signif(AnnualResults_waterYear$Q[1], digits = 7), 4.251937)
  
  AnnualResults_calendarYear <- setupYears(Daily, paLong = 12, paStart = 1)
  expect_equal(nrow(AnnualResults_calendarYear), 31)
  
  expect_equal(signif(AnnualResults_calendarYear$DecYear[1], digits = 7), 1980.5)
  expect_equal(signif(AnnualResults_calendarYear$FNConc[1], digits = 7), 0.9994875)
  expect_equal(signif(AnnualResults_calendarYear$FNFlux[1], digits = 7), 292.0164)
  expect_equal(signif(AnnualResults_calendarYear$Conc[1], digits = 7), 0.9979275)
  expect_equal(signif(AnnualResults_calendarYear$Flux[1], digits = 7), 284.0979)
  expect_equal(signif(AnnualResults_calendarYear$Q[1], digits = 7), 3.621384)
  
  eList <- Choptank_eList
  expect_equal(as.numeric(signif(eList$Daily$FNConc[1], digits = 6)), 0.972757)
  
  eList <- setUpEstimation(eList)
  sampleSegStart <- c(1980,1990,2000)
  flowSegStart <- c(1980,1985,1992)
  flowSegEnd <- c(1994,2004,2011)
  dateInfo <- data.frame(sampleSegStart,
                         flowSegStart,
                         flowSegEnd)
  eList <- flexFN(eList, dateInfo)
  expect_equal(as.numeric(signif(eList$Daily$FNConc[1], digits = 6)), 1.0283)
  ar_calendarYear_fn <- setupYears(eList$Daily, paLong = 12, paStart = 1)
  expect_equal(signif(ar_calendarYear_fn$FNConc[1], digits = 7), 1.0216445)
  expect_equal(signif(ar_calendarYear_fn$FNFlux[1], digits = 7), 271.6512)
})