# EGRET <img src="man/figures/egret-02.png" alt="EGRET" style="width:90px;height:auto;" align="right" class="logo" />

[![CRAN
version](http://www.r-pkg.org/badges/version/EGRET)](https://cran.r-project.org/package=EGRET)
[![](http://cranlogs.r-pkg.org/badges/EGRET)](https://cran.r-project.org/package=EGRET)
[![](http://cranlogs.r-pkg.org/badges/grand-total/EGRET)](https://cran.r-project.org/package=EGRET)

Exploration and Graphics for RivEr Trends (`EGRET`): An R-package for
the analysis of long-term changes in water quality and streamflow,
including the water-quality method Weighted Regressions on Time,
Discharge, and Season (WRTDS).

Look for new and improved documentation here:
<https://doi-usgs.github.io/EGRET/>

The link for the official USGS publication user guide is here:

<https://pubs.usgs.gov/tm/04/a10/>

A companion package [`EGRETci`](https://doi-usgs.github.io/EGRETci/)
implements a set of approaches to the analysis of uncertainty associated
with WRTDS trend analysis.

If you are familiar with the traditional `EGRET` workflow, check out the
\[Overview and
Updates\](<https://doi-usgs.github.io/EGRET/articles/Overview.html> to
see how all the latest updates relate.

Recent introduction to WRTDS and the `EGRET` package at the 12th
National Monitoring Conference April 19, 2021:

<iframe width="560" height="315" src="https://www.youtube.com/embed/d58h3nIc1cc" frameborder="0" allowfullscreen>
</iframe>

[New capabilities](https://www.youtube.com/watch?v=ThxdHxrw5qk)

## Package Installation

To install the EGRET package, you must be using R 3.0 or greater and run
the following command:

``` r
install.packages("EGRET")
```

## Background:

Evaluating long-term changes in river conditions (water quality and
discharge) is an important use of hydrologic data. To carry out such
evaluations, the hydrologist needs tools to facilitate several key steps
in the process: acquiring the data records from a variety of sources,
structuring it in ways that facilitate the analysis, routines that will
process the data to extract information about changes that may be
happening, and graphical techniques that can display findings about
change. The R package `EGRET` (Exploration and Graphics for RivEr
Trends) was developed for carrying out each of these steps in an
integrated manner. It is designed to accept easily data from three
sources: U.S. Geological Survey hydrologic data, Water Quality Portal
Data (currently including U.S. Environmental Protection Agency (EPA)
STORET data, and USDA STEWARDS data), and user-supplied flat files. The
`EGRET` package has components oriented towards the description of
long-term changes in streamflow statistics (high flow, average flow, and
low flow) as well as changes in water quality. For the water-quality
analysis, it uses Weighted Regressions on Time, Discharge and Season
(WRTDS) to describe long-term trends in both concentration and flux.
`EGRET` also creates a wide range of graphical presentations of the
water-quality data and of the WRTDS results. The following report serves
as a user guide, providing detailed guidance on installation and use of
the software, documentation of the analysis methods used, as well as
guidance on some of the kinds of questions and approaches that the
software can facilitate.

`EGRET` includes statistics and graphics for streamflow history, water
quality trends, and the statistical modeling algorithm Weighted
Regressions on Time, Discharge, and Season (WRTDS). Please see the
official EGRET User Guide for more information on the `EGRET` package:

<https://doi.org/10.3133/tm4A10> The best ways to learn about the WRTDS
approach is to read the User Guide and two journal articles. These
articles are available, for free, from the journals in which they were
published. The first relates to nitrate and total phosphorus data for 9
rivers draining to Chesapeake Bay. The URL is:

<https://onlinelibrary.wiley.com/doi/full/10.1111/j.1752-1688.2010.00482.x>.

The second is an application to nitrate data for 8 monitoring sites on
the Mississippi River or its major tributaries. The URL is:

<https://pubs.acs.org/doi/abs/10.1021/es201221s>

For a thorough discussion of the generalized flow normalization method
implemented in the EGRET enhancements, see the paper: “Tracking changes
in nutrient delivery to western Lake Erie: Approaches to compensate for
variability and trends in streamflow”
(<doi:10.1016/j.jglr.2018.11.012>).

## Sample Workflow

WRTDS on the Choptank River at Greensboro MD, for Nitrate:

``` r
library(EGRET)

############################
# Gather discharge data:
siteID <- "01491000" #Choptank River at Greensboro, MD
startDate <- "" #Gets earliest date
endDate <- "2011-09-30"
# Gather sample data:
parameter_cd<-"00631" #5 digit USGS code
Sample <- readNWISSample(siteID,parameter_cd,startDate,endDate)
#Gets earliest date from Sample record:
#This is just one of many ways to assure the Daily record
#spans the Sample record
startDate <- min(as.character(Sample$Date)) 
# Gather discharge data:
Daily <- readNWISDaily(siteID,"00060",startDate,endDate)
# Gather site and parameter information:

# Here user must input some values for
# the default (interactive=TRUE)
INFO<- readNWISInfo(siteID,parameter_cd)
INFO$shortName <- "Choptank River at Greensboro, MD"

# Merge discharge with sample data:
eList <- mergeReport(INFO, Daily, Sample)
```

``` r
library(EGRET)
# Sample data included in package:
eList <- Choptank_eList

boxConcMonth(eList)
```

![](man/figures/README-startPlots-1.png)

``` r
boxQTwice(eList)
```

![](man/figures/README-startPlots-2.png)

``` r
plotConcTime(eList)
```

![](man/figures/README-startPlots-3.png)

``` r
plotConcQ(eList)
```

![](man/figures/README-startPlots-4.png)

``` r
multiPlotDataOverview(eList)
```

![](man/figures/README-multiPlot-1.png)

``` r
# Run WRTDS model:
eList <- modelEstimation(eList)
#> 
#>  first step running estCrossVal may take about 1 minute
#>  estCrossVal % complete:
#> 0    1   2   3   4   5   6   7   8   9   10  
#> 11   12  13  14  15  16  17  18  19  20  
#> 21   22  23  24  25  26  27  28  29  30  
#> 31   32  33  34  35  36  37  38  39  40  
#> 41   42  43  44  45  46  47  48  49  50  
#> 51   52  53  54  55  56  57  58  59  60  
#> 61   62  63  64  65  66  67  68  69  70  
#> 71   72  73  74  75  76  77  78  79  80  
#> 81   82  83  84  85  86  87  88  89  90  
#> 91   92  93  94  95  96  97  98  99  
#> Next step running  estSurfaces with survival regression:
#> Survival regression (% complete):
#> 0    1   2   3   4   5   6   7   8   9   10  
#> 11   12  13  14  15  16  17  18  19  20  
#> 21   22  23  24  25  26  27  28  29  30  
#> 31   32  33  34  35  36  37  38  39  40  
#> 41   42  43  44  45  46  47  48  49  50  
#> 51   52  53  54  55  56  57  58  59  60  
#> 61   62  63  64  65  66  67  68  69  70  
#> 71   72  73  74  75  76  77  78  79  80  
#> 81   82  83  84  85  86  87  88  89  90  
#> 91   92  93  94  95  96  97  98  99  
#> Survival regression: Done

#eList:
plotConcTimeDaily(eList)
#> plotGenConc = TRUE requires running WRTDSKalman
#>               on eList. Switching to WRTDS concentration.
```

![](man/figures/README-runModel-1.png)

``` r
plotFluxTimeDaily(eList)
#> plotGenFlux = TRUE requires running WRTDSKalman
#>               on eList. Switching to WRTDS concentration.
```

![](man/figures/README-runModel-2.png)

``` r
plotConcPred(eList)
```

![](man/figures/README-runModel-3.png)

``` r
plotFluxPred(eList)
```

![](man/figures/README-runModel-4.png)

``` r
plotResidPred(eList)
```

![](man/figures/README-runModel-5.png)

``` r
plotResidQ(eList)
```

![](man/figures/README-runModel-6.png)

``` r
plotResidTime(eList)
```

![](man/figures/README-runModel-7.png)

``` r
boxResidMonth(eList)
```

![](man/figures/README-runModel-8.png)

``` r
boxConcThree(eList)
```

![](man/figures/README-runModel-9.png)

``` r
plotConcHist(eList)
```

![](man/figures/README-runModel-10.png)

``` r
plotFluxHist(eList)
```

![](man/figures/README-runModel-11.png)

``` r
# Multi-line plots:
date1 <- "1985-09-01"
date2 <- "1997-09-01"
date3 <- "2010-09-01"
qBottom<-0.2
qTop<-10
plotConcQSmooth(eList, date1, date2, date3, qBottom, qTop, 
                   concMax=2,legendTop = 0.85)
```

![](man/figures/README-multiPlots-1.png)

``` r
q1 <- 2
q2 <- 10
q3 <- 20
centerDate <- "07-01"
yearEnd <- 1980
yearStart <- 2010
plotConcTimeSmooth(eList, q1, q2, q3, centerDate, yearStart, yearEnd, legendTop = 0.55, legendLeft = 1990)
```

![](man/figures/README-multiPlots-2.png)

``` r
# Multi-plots:
fluxBiasMulti(eList)
```

![](man/figures/README-fluxPlots-1.png)

``` r
#Contour plots:
clevel<-seq(0,2,0.5)
yearStart <- 1980
yearEnd <- 2010

plotContours(eList, yearStart,yearEnd,qBottom=0.5,
             qTop = 20, contourLevels = clevel)
```

![](man/figures/README-contourPlots-1.png)

``` r

plotDiffContours(eList, year0 = 1990,
                 year1 = 2010,
                 qBottom = 0.5,
                 qTop = 20,
                 maxDiff = 0.6)
```

![](man/figures/README-contourPlots-2.png)

### Sample workflow for a flowHistory application for the entire record

``` r
library(EGRET)

# Flow history analysis
# Gather discharge data:
siteID <- "01491000" #Choptank River at Greensboro, MD
startDate <- "" # Get earliest date
endDate <- "" # Get latest date
Daily <- readNWISDaily(siteID, "00060", startDate, endDate)
#> GET: https://waterservices.usgs.gov/nwis/dv/?site=01491000&format=rdb,1.0&ParameterCd=00060&StatCd=00003&startDT=1851-01-01
#> There are 28082 data points, and 28082 days.
# Gather site and parameter information:
# Here user must input some values for
# the default (interactive=TRUE)
INFO <- readNWISInfo(siteID, "00060")
#> GET: https://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&format=rdb&site=01491000
#> Your site for streamflow data is:
#>  01491000 .
#> Your site name is CHOPTANK RIVER NEAR GREENSBORO, MD 
#> but you can modify this to a short name in a style you prefer. 
#> This name will be used to label graphs and tables. 
#> If you want the program to use the name given above, just do a carriage return,
#> otherwise enter the preferred short name(no quotes):
#> 
#> The latitude and longitude of the site are:  38.99719 ,  -75.78581 (degrees north and west).
#> 
#> The drainage area at this site is  113 square miles
#>  which is being stored as 292.6687 square kilometers.
#> 
#> It is helpful to set up a station abbreviation when doing multi-site studies,
#> enter a unique id (three or four characters should work). It is case sensitive.  
#> Even if you don't feel you need an abbreviation for your site you need to enter something(no quotes):
#> 
#> Your water quality data are for parameter number:
#> 00060 
#> which has the name:' Discharge, cubic feet per second '.
#> Typically you will want a shorter name to be used in graphs and tables.
#> The suggested short name is:' Stream flow, mean. daily '.
#> If you would like to change the short name, enter it here, 
#> otherwise just hit enter (no quotes):
#> The units for the water quality data are:  ft3/s .
#> It is helpful to set up a constiuent abbreviation, enter a unique id 
#> three or four characters should work something like tn or tp or NO3).
#> Even if you don't feel you need an abbreviation you need to enter something (no quotes):
#> 
#> Required concentration units are mg/l. 
#> The INFO dataframe indicates: ft3/s 
#> Flux calculations will be wrong if units are not consistent.
INFO$shortName <- "Choptank River at Greensboro, MD"
eList <- as.egret(INFO, Daily, NA, NA)

# Check flow history data:
plotFlowSingle(eList, istat = 7,qUnit = "thousandCfs")
```

![](man/figures/README-unnamed-chunk-4-1.png)

``` r
plotSDLogQ(eList)
```

![](man/figures/README-unnamed-chunk-4-2.png)

``` r
plotQTimeDaily(eList, qLower = 1, qUnit = 3)
```

![](man/figures/README-unnamed-chunk-4-3.png)

``` r
plotFour(eList, qUnit=3)
```

![](man/figures/README-plotFours-1.png)

``` r
plotFourStats(eList, qUnit=3)
```

![](man/figures/README-plotFours-2.png)

## Reporting bugs

Please consider reporting bugs and asking questions on the Issues page:
<https://github.com/DOI-USGS/EGRET/issues>

## Subscribe

Please email questions, comments, and feedback to:
<egret_comments@usgs.gov>

Additionally, to subscribe to an email list concerning updates to these
R packages, please send a request to <egret_comments@usgs.gov>.

## Package Support

The Water Mission Area of the USGS has supported the development and
maintenance of the `EGRET` R-package. Further maintenance is expected to
be stable through October 2024. Resources are available primarily for
maintenance and responding to user questions. Priorities on the
development of new features are determined by the `EGRET` development
team.

## Sunset date

Funding for `EGRET` currently expires fall 2024. Expectations are that
maintenance and customer service will continue to be supported past that
date.

## How to cite EGRET:

``` r
citation(package = "EGRET")
#> To cite EGRET in publications, please use:
#> 
#>   Hirsch, R.M., De Cicco, L.A., Murphy, J., 2024, Exploration and
#>   Graphics for RivEr Trends (EGRET), version 3.0.10,
#>   doi:10.5066/P9CC9JEX
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     author = {Robert Hirsch and Laura DeCicco and Jennifer Murphy},
#>     title = {Exploration and Graphics for RivEr Trends (EGRET)},
#>     publisher = {U.S. Geological Survey},
#>     year = {2024},
#>     url = {https://pubs.usgs.gov/tm/04/a10/},
#>   }
```

## References

See this list for WRTDS applications in print:

<https://doi-usgs.github.io/EGRET/articles/References_WRTDS.html>

# Disclaimer

This software is preliminary or provisional and is subject to revision.
It is being provided to meet the need for timely best science. The
software has not received final approval by the U.S. Geological Survey
(USGS). No warranty, expressed or implied, is made by the USGS or the
U.S. Government as to the functionality of the software and related
material nor shall the fact of release constitute any such warranty. The
software is provided on the condition that neither the USGS nor the U.S.
Government shall be held liable for any damages resulting from the
authorized or unauthorized use of the software.
