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
  expect_equal(signif(ar_calendarYear_fn$FNConc[1], digits = 7), 1.021644)
  expect_equal(signif(ar_calendarYear_fn$FNFlux[1], digits = 7), 271.6512)
})

test_that("setupYears", {
  testthat::skip_on_cran()
  
  eList <- Choptank_eList
  
  tC <- tableChange(eList)
  expect_null(tC)
  
  tCS <- tableChangeSingle(eList)
  expect_equal(ncol(tCS), 6)
  expect_equal(tCS$Year1, c(1981,1981,1981,1981,1981,1981,1986,1986,1986,1986,1986,1991,
                            1991,1991,1991,1996,1996,1996,2001,2001,2006))
  
  expect_equal(tCS$Year2, c(1986,1991,1996,2001,2006,2011,1991,1996,2001,2006,2011,1996,
                            2001,2006,2011,2001,2006,2011,2006,2011,2011))
  
  expect_equal(tCS$`change[mg/L]`[1], 0.039)
  expect_equal(tCS$`slope[mg/L/yr]`[1], 0.0079)
  expect_equal(tCS$`change[%]`[1], 3.9)
  expect_equal(tCS$`slope [%/yr]`[1], 0.79)
  
  tR <- tableResults(eList)
  expect_true(all(names(tR) %in% c("Year","Discharge [cms]",    
                                   "Conc [mg/L]","FN Conc [mg/L]",    
                                   "Flux [10^6kg/yr]","FN Flux [10^6kg/yr]")))
  
  expect_equal(tR$Year[1], 1980)
  expect_equal(tR$`Discharge [cms]`[1], 4.25)
  expect_equal(tR$`Conc [mg/L]`[1], 0.949)
  expect_equal(tR$`FN Conc [mg/L]`[1], 1.003)
  expect_equal(tR$`Flux [10^6kg/yr]`[1], 0.1154)
  expect_equal(tR$`FN Flux [10^6kg/yr]`[1], 0.106)
  
  tR_2 <- tableResults(eList, fluxUnit = 'kgDay', qUnit = 'cms')
  expect_true(all(names(tR_2) %in% c("Year","Discharge [cms]",    
                                   "Conc [mg/L]","FN Conc [mg/L]",    
                                   "Flux [kg/day]","FN Flux [kg/day]")))
  
  eList <- Choptank_eList
  tFC <- tableFlowChange(eList, istat=5, yearPoints=c(1985,1990,1995,2001,2005,2009))
  expect_true(all(names(tFC) %in% c("year1","year2","change[cfs]","slope[cfs/yr]",
                                    "change[%]","slope[%/yr]")))
  expect_equal(tFC$`change[cfs]`[1], 7.5)
  expect_equal(tFC$`slope[cfs/yr]`[1], 1.5)
  expect_equal(tFC$`change[%]`[1], 6.5)
  expect_equal(tFC$`slope[%/yr]`[1], 1.3)
  
  printReturn <- printSeries(eList, 5)
  expect_true(all(names(printReturn) %in% c("years","qActual","qSmooth")))
  
  expect_equal(printReturn$qActual[2], 78.3)
  expect_equal(printReturn$qSmooth[2], 109)
  
  expect_equal(setSeasonLabelByUser(), "Water Year")
  expect_equal(setSeasonLabelByUser(paStartInput = 12,paLongInput = 3), "Season Consisting of Dec Jan Feb")
})
  