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
  
  # eList <- setUpEstimation(eList)
  # sampleSegStart <- c(1980,1990,2000)
  # flowSegStart <- c(1980,1985,1992)
  # flowSegEnd <- c(1994,2004,2011)
  # dateInfo <- data.frame(sampleSegStart,
  #                        flowSegStart,
  #                        flowSegEnd)
  # eList <- flexFN(eList, dateInfo)
  # expect_equal(as.numeric(signif(eList$Daily$FNConc[1], digits = 6)), 1.0283)
  # ar_calendarYear_fn <- setupYears(eList$Daily, paLong = 12, paStart = 1)
  # expect_equal(signif(ar_calendarYear_fn$FNConc[1], digits = 7), 1.021644)
  # expect_equal(signif(ar_calendarYear_fn$FNFlux[1], digits = 7), 271.6512)
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
  

test_that("getSurfaceEstimates",{
  eList <- Choptank_eList
  Daily_orig <- eList$Daily
  Daily_new <- Daily_orig[,c("Date","Q","Julian","Month","Day",
                             "DecYear","MonthSeq","Qualifier","i",
                             "LogQ","Q7","Q30")]
  eList <- as.egret(eList$INFO, Daily_new, eList$Sample, eList$surfaces)
  
  daily_stuff <- EGRET:::getSurfaceEstimates(eList)
  expect_equal(ncol(daily_stuff), 16)
  
  expect_true(all(c("yHat","SE","ConcDay","FluxDay") %in% names(daily_stuff)))
  expect_equal(round(head(daily_stuff$yHat),3), 
               c(-0.165,-0.180,-0.304,
                 -0.464, -0.487,-0.368))
  expect_equal(round(head(daily_stuff$SE),3), 
               c(0.221,0.221,0.217,0.212,0.211,0.215))
  expect_equal(round(head(daily_stuff$ConcDay),3), 
               c(0.875,0.860,0.764,0.654,0.638,0.721))
  expect_equal(round(head(daily_stuff$FluxDay),3), 
               c(143.356,149.347,181.347,217.747,224.883,199.328))
  
})

test_that("bin_Qs",{
  eList <- Choptank_eList
  Daily_orig <- eList$Daily
  allLogQsByDayOfYear <- EGRET:::bin_Qs(Daily_orig)
  expect_length(allLogQsByDayOfYear, 366)
  # This is 364 instead of 366 because 59 and 60 are special
  expect_equal(364,sum(32 == sapply(allLogQsByDayOfYear, length))) 
  expect_equal(round(head(allLogQsByDayOfYear[[1]]),3), 
               c(1.041,0.425,1.118,0.830,1.773,-0.009))
})

# test_that("flexFN",{
#   eList <- Choptank_eList
#   eList <- setUpEstimation(eList)
#   daily_1 <- eList$Daily
#   sampleSegStart <- c(1980,1985,2000)
#   flowSegStart <- c(1980,1990,2000)
#   flowSegEnd <- c(1990,2000,2010)
#   dateInfo <- data.frame(sampleSegStart, flowSegStart, flowSegEnd)
#   eList <- flexFN(eList, dateInfo)
#   daily_2 <- eList$Daily
#   expect_true(!identical(daily_1, daily_2))
#   expect_equal(round(head(daily_2$FNConc),3), 
#                c(0.999, 0.988, 0.965, 0.945, 0.970, 1.000))
#   expect_equal(round(head(daily_2$FNFlux),3), 
#                c(104.815, 107.577, 118.194, 130.000, 122.589, 110.149))
#   
# })

test_that("getConcFluxFromSurface",{
  
  eList <- Choptank_eList
  localDaily <- eList$Daily
  # Calculate "flow-normalized" concentration and flux:
  allLogQsByDayOfYear <- EGRET:::bin_Qs(localDaily)
  
  concFlux_list <- EGRET:::getConcFluxFromSurface(eList, allLogQsByDayOfYear, localDaily)
  expect_length(concFlux_list, 3)
  expect_type(concFlux_list, "list")
  expect_equal(round(head(concFlux_list[["allFluxReplicated"]]),3), 
               c(143.356,100.241,88.592,47.434,88.592,92.699))
  expect_equal(round(head(concFlux_list[["allConcReplicated"]]),3), 
               c(0.875,1.024,1.065,1.212,1.065,1.052))
  expect_equal(round(head(concFlux_list[["allDatesReplicated"]]),3), 
               c(1979.75,1979.75,1979.75,1979.75,1979.75,1979.75))
})

# test_that("subFN",{
#   eList <- Choptank_eList
#   d1 <- eList$Daily
#   
#   flowNormYears <- c(1985:2002,2006:2010)
#   temp_daily <- subFN(eList, flowNormYears)
#   expect_equal(19, ncol(temp_daily))
#   expect_equal(round(head(temp_daily$FNConc),3), 
#                c(1.094,0.892,1.100,0.924,1.087,0.936))
#   expect_equal(round(head(temp_daily$FNFlux),3), 
#                c(72.567 ,139.213,68.180,133.852,80.925,128.181))
#   
#   expect_silent(plotFluxHist(eList, flowNormYears =  c(1985:2002,2006:2010)))
# 
#   
# })
  