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

test_that("dateInfo",{
  dateInfoA <- makeDateInfo(windowSide = 7,
                            surfaceStart = "1980-10-01", 
                            surfaceEnd = "1981-09-30",
                            firstQDate0 = "1970-10-01",
                            lastQDate0 = "2017-09-30")
  expect_equal(nrow(dateInfoA), 1)
  expect_equal(dateInfoA[["flowNormStart"]][1], as.Date("1973-10-01"))
  expect_equal(dateInfoA[["flowNormEnd"]][1], as.Date("1988-09-30"))
  expect_equal(dateInfoA[["flowStart"]][1], as.Date("1980-10-01"))
  expect_equal(dateInfoA[["flowEnd"]][1], as.Date("1981-09-30"))

  dateInfoB <- makeDateInfo(windowSide = 7, 
                             surfaceStart = "1980-10-01", 
                             surfaceEnd = "2017-09-30", 
                             firstQDate0 = "1970-10-01", 
                             lastQDate0 = "2017-09-30")
  expect_equal(dateInfoB[["flowNormStart"]][1], as.Date("1973-10-01"))
  expect_equal(dateInfoB[["flowNormStart"]][37], as.Date("2002-10-01"))
  expect_equal(dateInfoB[["flowNormEnd"]][1], as.Date("1988-09-30"))
  expect_equal(dateInfoB[["flowNormEnd"]][37], as.Date("2017-09-30"))
  expect_equal(dateInfoB[["flowStart"]][1], as.Date("1980-10-01"))
  expect_equal(dateInfoB[["flowStart"]][37], as.Date("2016-10-01"))
  expect_equal(dateInfoB[["flowEnd"]][1], as.Date("1981-09-30"))
  expect_equal(dateInfoB[["flowEnd"]][37], as.Date("2017-09-30"))
  
  dateInfoC <- makeDateInfo(windowSide = 7, 
                            surfaceStart = "1980-02-01", 
                            surfaceEnd = "1984-01-31", 
                            firstQDate0 = "1970-10-01", 
                            lastQDate0 = "2017-09-30")
  expect_equal(nrow(dateInfoC), 4)
  expect_equal(dateInfoC[["flowNormStart"]][1], as.Date("1973-02-01"))
  expect_equal(dateInfoC[["flowNormStart"]][4], as.Date("1976-02-01"))
  expect_equal(dateInfoC[["flowNormEnd"]][1], as.Date("1988-01-31"))
  expect_equal(dateInfoC[["flowNormEnd"]][4], as.Date("1991-01-31"))
  expect_equal(dateInfoC[["flowStart"]][1], as.Date("1980-02-01"))
  expect_equal(dateInfoC[["flowStart"]][4], as.Date("1983-02-01"))
  expect_equal(dateInfoC[["flowEnd"]][1], as.Date("1981-01-31"))
  expect_equal(dateInfoC[["flowEnd"]][4], as.Date("1984-01-31"))
  
  dateInfoD <- makeDateInfo(windowSide = 50, 
                            surfaceStart = "1980-02-01", 
                            surfaceEnd = "1984-01-31", 
                            firstQDate0 = "1970-10-01", 
                            lastQDate0 = "2017-09-30")
  expect_equal(nrow(dateInfoD), 4)
  expect_equal(dateInfoD[["flowNormStart"]][1], as.Date("1970-10-01"))
  expect_equal(dateInfoD[["flowNormStart"]][4], as.Date("1970-10-01"))
  # expect_equal(dateInfoD[["flowNormEnd"]][1], as.Date("2017-09-30"))
  # expect_equal(dateInfoD[["flowNormEnd"]][4], as.Date("2017-09-30"))
  expect_equal(dateInfoD[["flowStart"]][1], as.Date("1980-02-01"))
  expect_equal(dateInfoD[["flowStart"]][4], as.Date("1983-02-01"))
  expect_equal(dateInfoD[["flowEnd"]][1], as.Date("1981-01-31"))
  expect_equal(dateInfoD[["flowEnd"]][4], as.Date("1984-01-31"))
  
  
})

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

test_that("flexFN",{
  skip_on_cran()
  
  eList <- Choptank_eList

  eList <- setUpEstimation(eList)
  flowNormStart <- c("1979-10-01","1990-01-01","1992-10-10")
  flowNormEnd <- c("1995-06-06","2004-03-03","2011-09-29")
  flowStart <- c("1979-10-01","1995-06-07","2004-03-04")
  flowEnd <- c("1995-06-06","2004-03-03","2011-09-29")
  dateInfo <- data.frame(flowNormStart,
                         flowNormEnd,
                         flowStart,
                         flowEnd,
                         stringsAsFactors = FALSE)
  newEList <- flexFN(eList, dateInfo)
  
  expect_true("segmentInfo" %in% names(attributes(newEList$INFO)))
  segmentInfo <- attr(newEList$INFO, "segmentInfo")
  expect_equal(segmentInfo, dateInfo)
  expect_true(!(all(eList$Daily$FNFlux == newEList$Daily$FNFlux)))
  
})
  

test_that("runPairs",{
  skip_on_cran()
  
  eList <- Choptank_eList
  year1 <- 1985
  year2 <- 2010

  #Option 1:
  pairOut_1 <- runPairs(eList, year1, year2, windowSide = 0)

  pairOut_1_orig <- runPairs(eList, year1, year2, windowSide = 0, oldSurface = TRUE)
  
  expect_equal(attr(pairOut_1, "dateInfo"), attr(pairOut_1_orig, "dateInfo"))
  expect_equal(attr(pairOut_1, "yearPair"), attr(pairOut_1_orig, "yearPair"))
  expect_true(all(names(pairOut_1) %in% c("TotalChange","CQTC","QTC","x10","x11","x20","x22")))
  
  expect_equal(round(pairOut_1$TotalChange[1], digits = 4), 0.429)
  
  expect_equal(round(attr(pairOut_1, "Other")[["PercentChangeConc"]][["Total Percent Change"]],digits = 2),
               42.27)
  expect_equal(round(attr(pairOut_1, "Other")[["PercentChangeConc"]][["CQTC Percent"]],digits = 2),
               42.27)
  expect_equal(round(attr(pairOut_1, "Other")[["PercentChangeConc"]][["QTC Percent"]],digits = 2),
               0)
  expect_equal(round(attr(pairOut_1, "Other")[["PercentChangeFlux"]][["Total Percent Change"]],digits = 2),
               29.44)
  expect_equal(round(attr(pairOut_1, "Other")[["PercentChangeFlux"]][["CQTC Percent"]],digits = 2),
               29.44)
  expect_equal(round(attr(pairOut_1, "Other")[["PercentChangeFlux"]][["QTC Percent"]],digits = 2),
               0)
  
  # Option 2:
  pairOut_2 <- runPairs(eList, year1, year2, windowSide = 7)

  expect_true(all(names(pairOut_2) %in% c("TotalChange","CQTC","QTC","x10","x11","x20","x22")))
  expect_equal(round(pairOut_2$TotalChange[1], digits = 4), 0.3968)
  
  expect_equal(round(pairOut_1$TotalChange[1], digits = 4), 0.429)
  
  expect_equal(round(attr(pairOut_2, "Other")[["PercentChangeConc"]][["Total Percent Change"]],digits = 2),
               38.56)
  expect_equal(round(attr(pairOut_2, "Other")[["PercentChangeConc"]][["CQTC Percent"]],digits = 2),
               41.7)
  expect_equal(round(attr(pairOut_2, "Other")[["PercentChangeConc"]][["QTC Percent"]],digits = 2),
               -3.13)
  expect_equal(round(attr(pairOut_2, "Other")[["PercentChangeFlux"]][["Total Percent Change"]],digits = 2),
               50.68)
  expect_equal(round(attr(pairOut_2, "Other")[["PercentChangeFlux"]][["CQTC Percent"]],digits = 2),
               31.94)
  expect_equal(round(attr(pairOut_2, "Other")[["PercentChangeFlux"]][["QTC Percent"]],digits = 2),
               18.74)
  
  # Option 3:
  pairOut_3 <- runPairs(eList, year1, year2,
                        windowSide = 0, flowBreak = TRUE,
                        Q1EndDate = "1990-09-30")
  expect_true(all(names(pairOut_3) %in% c("TotalChange","CQTC","QTC","x10","x11","x20","x22")))
  expect_equal(round(pairOut_3$TotalChange[1], digits = 4), 0.4081)
  
  expect_equal(round(attr(pairOut_3, "Other")[["PercentChangeConc"]][["Total Percent Change"]],digits = 2),
               39.77)
  expect_equal(round(attr(pairOut_3, "Other")[["PercentChangeConc"]][["CQTC Percent"]],digits = 2),
               41.81)
  expect_equal(round(attr(pairOut_3, "Other")[["PercentChangeConc"]][["QTC Percent"]],digits = 2),
               -2.04)
  expect_equal(round(attr(pairOut_3, "Other")[["PercentChangeFlux"]][["Total Percent Change"]],digits = 2),
               45.81)
  expect_equal(round(attr(pairOut_3, "Other")[["PercentChangeFlux"]][["CQTC Percent"]],digits = 2),
               31.89)
  expect_equal(round(attr(pairOut_3, "Other")[["PercentChangeFlux"]][["QTC Percent"]],digits = 2),
               13.91)
  
  # Option 4:
  pairOut_4 <- runPairs(eList, year1, year2,
                        windowSide = 7, flowBreak = TRUE,
                        Q1EndDate = "1990-09-30")
  
  expect_true(all(names(pairOut_4) %in% c("TotalChange","CQTC","QTC","x10","x11","x20","x22")))
  expect_equal(round(pairOut_4$TotalChange[1], digits = 4), 0.3995)
  
  expect_equal(round(attr(pairOut_4, "Other")[["PercentChangeConc"]][["Total Percent Change"]],digits = 2),
               38.93)
  expect_equal(round(attr(pairOut_4, "Other")[["PercentChangeConc"]][["CQTC Percent"]],digits = 2),
               41.81)
  expect_equal(round(attr(pairOut_4, "Other")[["PercentChangeConc"]][["QTC Percent"]],digits = 2),
               -2.88)
  expect_equal(round(attr(pairOut_4, "Other")[["PercentChangeFlux"]][["Total Percent Change"]],digits = 2),
               50.44)
  expect_equal(round(attr(pairOut_4, "Other")[["PercentChangeFlux"]][["CQTC Percent"]],digits = 2),
               31.89)
  expect_equal(round(attr(pairOut_4, "Other")[["PercentChangeFlux"]][["QTC Percent"]],digits = 2),
               18.55)
  
})

test_that("runSeries", {

  skip_on_cran()
  
  eList <- Choptank_eList

  #Option 1:
  seriesOut_1 <- runSeries(eList,  windowSide = 0)
  seriesOut_1_orig <- runSeries(eList,  windowSide = 0, oldSurface = TRUE)
  expect_equal(class(seriesOut_1),"egret")
  expect_equal(class(seriesOut_1_orig),"egret")
  expect_true(attr(seriesOut_1, "runSeries"))
  expect_true(attr(seriesOut_1_orig, "runSeries"))
  expect_equal(round(attr(seriesOut_1$surfaces, "LogQ")[1], digits = 2), -4.66)
  expect_equal(round(attr(seriesOut_1$surfaces, "Year")[1], digits = 2), 1979.69)
  
  
  # Option 2:
  seriesOut_2 <- runSeries(eList, windowSide = 7, oldSurface = TRUE)
  expect_equal(class(seriesOut_2),"egret")
  expect_true(attr(seriesOut_2, "runSeries"))

  # Option 3:
  seriesOut_3 <- runSeries(eList,
                         windowSide = 0,
                         flowBreak = TRUE,
                         Q1EndDate = "1990-09-30")
  expect_equal(class(seriesOut_3),"egret")
  expect_true(attr(seriesOut_3, "runSeries"))
  expect_equal(round(attr(seriesOut_3$surfaces, "LogQ")[1], digits = 2), -4.66)
  expect_equal(round(attr(seriesOut_3$surfaces, "Year")[1], digits = 2), 1979.69)
  
  # Option 4:
  seriesOut_4 <- runSeries(eList,
                        windowSide = 7, flowBreak = TRUE,
                        Q1EndDate = "1990-09-30")
  expect_equal(class(seriesOut_4),"egret")
  expect_true(attr(seriesOut_4, "runSeries"))
  expect_equal(round(attr(seriesOut_4$surfaces, "LogQ")[1], digits = 2), -4.66)
  expect_equal(round(attr(seriesOut_4$surfaces, "Year")[1], digits = 2), 1979.69)
  
})

test_that("runGroups", {
  
  skip_on_cran()
  
  eList <- Choptank_eList
  
  groupOut_1 <- runGroups(eList,  windowSide = 0,
                          group1firstYear = 1980, group1lastYear = 1990,
                          group2firstYear = 1995, group2lastYear = 2005)
  
  expect_true(all(names(groupOut_1) %in% c("TotalChange","CQTC","QTC","x10",        
                                       "x11","x20","x22")))
  expect_true(all(round(groupOut_1$TotalChange, digits = 3) %in% c(0.226,0.022)))
  expect_true(all(round(groupOut_1$CQTC, digits = 3) %in% c(0.226,0.022)))
  expect_true(all(round(groupOut_1$QTC, digits = 3) %in% c(0,0)))
  expect_true(all(round(groupOut_1$x10, digits = 3) %in% c(1.019,0.116)))
  expect_true(all(round(groupOut_1$x11, digits = 3) %in% c(1.019,0.116)))
  expect_true(all(round(groupOut_1$x20, digits = 3) %in% c(1.245,0.138)))
  expect_true(all(round(groupOut_1$x22, digits = 3) %in% c(1.245,0.138)))
  
  # Option 2: Use sliding window.
  #                In each case it is a 15 year window (15 = 1 + 2*7)
  groupOut_2 <- runGroups(eList,  windowSide = 7,
                          group1firstYear = 1980, group1lastYear = 1990,
                          group2firstYear = 1995, group2lastYear = 2005)

  expect_true(all(names(groupOut_2) %in% c("TotalChange","CQTC","QTC","x10",        
                                           "x11","x20","x22")))
  expect_true(all(round(groupOut_2$TotalChange, digits = 3) %in% c(0.200,0.039)))
  expect_true(all(round(groupOut_2$CQTC, digits = 3) %in% c(0.226,0.022)))
  expect_true(all(round(groupOut_2$QTC, digits = 3) %in% c(-0.025,0.017)))
  expect_true(all(round(groupOut_2$x10, digits = 3) %in% c(1.019,0.116)))
  expect_true(all(round(groupOut_2$x11, digits = 3) %in% c(1.034,0.107)))
  expect_true(all(round(groupOut_2$x20, digits = 3) %in% c(1.245,0.138)))
  expect_true(all(round(groupOut_2$x22, digits = 3) %in% c(1.234,0.146)))
  # Option 3: Flow normalization is based on splitting the flow record at 1990-09-30
  #                But in years before the break it uses all flow data from before the break,
  #                and years after the break uses all flow data after the break
  groupOut_3 <- runGroups(eList,  windowSide = 0,
                          group1firstYear = 1980, group1lastYear = 1990,
                          group2firstYear = 1995, group2lastYear = 2005,
                          flowBreak = TRUE, 
                          Q1EndDate = "1990-09-30")
  expect_true(all(round(groupOut_3$TotalChange, digits = 3) %in% c(0.207,0.037)))
  expect_true(all(round(groupOut_3$CQTC, digits = 3) %in% c(0.226,0.022)))
  expect_true(all(round(groupOut_3$QTC, digits = 3) %in% c(-0.019,0.015)))
  expect_true(all(round(groupOut_3$x10, digits = 3) %in% c(1.019,0.116)))
  expect_true(all(round(groupOut_3$x11, digits = 3) %in% c(1.031,0.107)))
  expect_true(all(round(groupOut_3$x20, digits = 3) %in% c(1.245,0.138)))
  expect_true(all(round(groupOut_3$x22, digits = 3) %in% c(1.238,0.144)))
  expect_true(all(names(groupOut_3) %in% c("TotalChange","CQTC","QTC","x10",        
                                           "x11","x20","x22")))
  
  # Option 4: Flow normalization is based on splitting the flow record at 1990-09-30
  #                but before the break uses a 15 year window of years before the break
  #                after the break uses a 15 year window of years after the break
  groupOut_4 <- runGroups(eList,  windowSide = 7,
                          group1firstYear = 1980, group1lastYear = 1990,
                          group2firstYear = 1995, group2lastYear = 2005,
                          flowBreak = TRUE, 
                          Q1EndDate = "1990-09-30")
  expect_true(all(names(groupOut_4) %in% c("TotalChange","CQTC","QTC","x10",        
                                           "x11","x20","x22")))
  expect_true(all(round(groupOut_4$TotalChange, digits = 3) %in% c(0.203,0.040)))
  expect_true(all(round(groupOut_4$CQTC, digits = 3) %in% c(0.226,0.022)))
  expect_true(all(round(groupOut_4$QTC, digits = 3) %in% c(-0.023,0.018)))
  expect_true(all(round(groupOut_4$x10, digits = 3) %in% c(1.019,0.116)))
  expect_true(all(round(groupOut_4$x11, digits = 3) %in% c(1.031,0.107)))
  expect_true(all(round(groupOut_4$x20, digits = 3) %in% c(1.245,0.138)))
  expect_true(all(round(groupOut_4$x22, digits = 3) %in% c(1.234,0.147)))
  
})

test_that("stitch", {
  
  skip_on_cran()
  
  eList <- Choptank_eList

  surfaceStart <- "1986-10-01"
  surfaceEnd <- "2012-09-30"

  # Surface skips a few years:
  sample1StartDate <- "1986-10-01"
  sample1EndDate <- "1992-09-30"
  sample2StartDate <- "1996-10-01"
  sample2EndDate <- "2012-09-30"

  surface_skip <- stitch(eList,
                           sample1StartDate, sample1EndDate,
                           sample2StartDate, sample2EndDate,
                           surfaceStart, surfaceEnd)

  expect_true(all(names(attributes(surface_skip)) %in%
                    c("dim","surfaceIndex","Year","LogQ",          
                      "surfaceStart","surfaceEnd","sample1StartDate",
                      "sample1EndDate","sample2StartDate","sample2EndDate")))
  
  expect_equal(attr(surface_skip, "surfaceStart"), as.Date(surfaceStart))
  expect_equal(attr(surface_skip, "sample1EndDate"), sample1EndDate)
  expect_true(!(dim(surface_skip)[2] == dim(eList$surfaces)[2]))
  expect_true(dim(surface_skip)[1] == dim(eList$surfaces)[1])
  expect_true(dim(surface_skip)[3] == dim(eList$surfaces)[3])
  
  # Surface overlaps a few years:
  sample1StartDate <- "1986-10-01"
  sample1EndDate <- "1996-09-30"
  sample2StartDate <- "1992-10-01"
  sample2EndDate <- "2012-09-30"

  surface_overlap <- stitch(eList,
                           sample1StartDate, sample1EndDate,
                           sample2StartDate, sample2EndDate)
  
  expect_equal(attr(surface_overlap, "sample1EndDate"), sample1EndDate)
  expect_true(!(dim(surface_overlap)[2] == dim(eList$surfaces)[2]))
  expect_true(dim(surface_overlap)[1] == dim(eList$surfaces)[1])
  expect_true(dim(surface_overlap)[3] == dim(eList$surfaces)[3])
})
  