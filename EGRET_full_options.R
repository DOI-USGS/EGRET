library(EGRET)
library(EGRETci)

############################
# NWIS example
siteNumber <- "01491000" #Choptank River at Greensboro, MD
startDate <- "" # Get earliest date
endDate <- "" # Get latest date
parameter_cd<-"00631" #5 digit USGS code
Sample <- readNWISSample(siteNumber,parameter_cd,startDate,endDate)
#Gets earliest date from Sample record:
#This is just one of many ways to assure the Daily record
#spans the Sample record
startDate <- min(as.character(Sample$Date)) 
# Gather discharge data:
Daily <- readNWISDaily(siteNumber,"00060",startDate,endDate)
# Gather site and parameter information:
# Here user must input some values for
# the default (interactive=TRUE)
INFO <- readNWISInfo(siteNumber,"00060", interactive = FALSE)
INFO$shortName <- "Choptank River near Greensboro, MD"
############################

# Merge discharge with sample data:
eList <- mergeReport(INFO, Daily, Sample)

############################
# Basic WRTDS:
eList <- modelEstimation(eList,  
                         windowY = 7, windowQ = 2, windowS = 0.5)

AnnualResults <- setupYears(eList$Daily)
summer_results <- setupYears(eList$Daily, paLong = 3, paStart = 6)
errorStats(eList)

##############################
# WRTDS Bootstrap Test
caseSetUp <- trendSetUp(eList,
                        year1 = 1985, 
                        year2 = 2005,
                        nBoot = 50, 
                        bootBreak = 39,
                        blockLength = 200)

eBoot <- wBT(eList,
              caseSetUp,
              jitterOn = TRUE)

########################################
# # Confidence intervals
# # SO SLOW...do this when you are ready!
# CIAnnualResults <- ciCalculations(eList, 
#                                   nBoot = 100,
#                                   blockLength = 200,
#                                   widthCI = 90)
# plotConcHistBoot(eList, CIAnnualResults)
########################################

########################################
# Kalman:
eListK <- WRTDSKalman(eList)
dailyBootOut <- genDailyBoot(eListK, 
                             nBoot = 25, 
                             nKalman = 20, rho = 0.9)
monthPcts <- makeMonthPI(dailyBootOut, eListK)
annualPcts <- makeAnnualPI(dailyBootOut, eListK)
dailyPcts <- makeDailyPI(dailyBootOut, eListK)

##################################################
# Assess trends on 2 specific years:
pairResults <- runPairs(eList, year1 = 1985, year2 = 2010, 
                         windowSide = 7, flowBreak = TRUE, 
                         Q1EndDate = "1995-05-31", wall = TRUE,
                         sample1EndDate = "1995-05-31", 
                         QStartDate = "1979-10-01", 
                         QEndDate = "2010-09-30")
bootPairOut <- runPairsBoot(eList, 
                            pairResults, 
                            nBoot = 100) 


##################################################
# Assess trends on 2 groups of years:
groupResults <- runGroups(eList, 
                          group1firstYear = 1995, group1lastYear = 2004, 
                          group2firstYear = 2005, group2lastYear = 2010,
                          windowSide = 7, wall = TRUE, 
                          sample1EndDate = "2004-10-30",
                          verbose = FALSE)
bootGroupsOut <- runGroupsBoot(eList, groupResults, nBoot = 100)

##################################################
#  Create a time series of flow-normalized concentrations and flow-normalized flux values
eListOut <- runSeries(eList, windowSide = 7, verbose = FALSE)
plotConcHist(eListOut)
plotFluxHist(eListOut)
tableChange(eListOut, yearPoints = c(1985, 1995, 2010))
