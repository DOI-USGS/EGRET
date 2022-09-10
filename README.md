# EGRET <img src="man/figures/egret-02.png" alt="EGRET" style="width:90px;height:auto;" align="right" class="logo" />

[![R build
status](https://github.com/USGS-R/EGRET/workflows/R-CMD-check/badge.svg)](https://github.com/USGS-R/EGRET/actions)
[![codecov](https://codecov.io/gh/USGS-R/EGRET/branch/main/graph/badge.svg)](https://codecov.io/gh/USGS-R/EGRET)
[![status](https://img.shields.io/badge/USGS-Research-blue.svg)](https://owi.usgs.gov/R/packages.html#research)
[![CRAN
version](http://www.r-pkg.org/badges/version/EGRET)](https://cran.r-project.org/package=EGRET)
[![](http://cranlogs.r-pkg.org/badges/EGRET)](https://cran.r-project.org/package=EGRET)
[![](http://cranlogs.r-pkg.org/badges/grand-total/EGRET)](https://cran.r-project.org/package=EGRET)

Exploration and Graphics for RivEr Trends (`EGRET`): An R-package for
the analysis of long-term changes in water quality and streamflow,
including the water-quality method Weighted Regressions on Time,
Discharge, and Season (WRTDS).

Look for new and improved documentation her:
<https://rconnect.usgs.gov/EGRET/>

The link for the official USGS publication user guide is here:

<https://pubs.usgs.gov/tm/04/a10/>

A companion package [`EGRETci`](https://usgs-r.github.io/EGRETci/)
implements a set of approaches to the analysis of uncertainty associated
with WRTDS trend analysis.

If you are familiar with the traditional `EGRET` workflow, check out the
[Overview and
Updates](https://usgs-r.github.io/EGRET/articles/Overview.html) to see
how all the latest updates relate.

Recent introduction to WRTDS and the `EGRET` package at the 12th
National Monitoring Conference April 19, 2021:

[Introduction](https://www.youtube.com/watch?v=d58h3nIc1cc)

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
variability and trends in streamflow”:

(<https://www.sciencedirect.com/science/article/pii/S0380133018302235>).

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
```

![](man/figures/README-runModel-1.png)

``` r
plotFluxTimeDaily(eList)
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
             qTop=20, contourLevels = clevel)
```

![](man/figures/README-contourPlots-1.png)

``` r
plotDiffContours(eList, year0 = 1990,
                 year1 = 2010,
                 qBottom=0.5,
                 qTop=20,
                 maxDiff=0.6)
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
Daily <- readNWISDaily(siteID,"00060",startDate,endDate)
#> There are 27281 data points, and 27281 days.
# Gather site and parameter information:
# Here user must input some values for
# the default (interactive=TRUE)
INFO<- readNWISInfo(siteID,"00060")
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
plotFlowSingle(eList, istat=7,qUnit="thousandCfs")
```

![](man/figures/README-unnamed-chunk-4-1.png)

``` r
plotSDLogQ(eList)
```

![](man/figures/README-unnamed-chunk-4-2.png)

``` r
plotQTimeDaily(eList, qLower=1,qUnit=3)
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

## Model Archive

When using the `WRTDS` model, it is important to be able to reproduce
the results in the future. The following version of R and package
dependencies were used most recently to pass the embedded tests within
this package. There is no guarantee of reproducible results using future
versions of R or updated versions of package dependencies; however, we
will make diligent efforts to test and update future modeling
environments.

``` r
sessioninfo::session_info()
#> ─ Session info ───────────────────────────────────────────────────────────────
#>  setting  value
#>  version  R version 4.2.1 (2022-06-23 ucrt)
#>  os       Windows 10 x64 (build 19044)
#>  system   x86_64, mingw32
#>  ui       RTerm
#>  language (EN)
#>  collate  English_United States.utf8
#>  ctype    English_United States.utf8
#>  tz       America/Chicago
#>  date     2022-09-10
#>  pandoc   2.18 @ C:/Program Files/RStudio/bin/quarto/bin/tools/ (via rmarkdown)
#> 
#> ─ Packages ───────────────────────────────────────────────────────────────────
#>  package       * version  date (UTC) lib source
#>  assertthat      0.2.1    2019-03-21 [1] CRAN (R 4.2.1)
#>  bit             4.0.4    2020-08-04 [1] CRAN (R 4.2.1)
#>  bit64           4.0.5    2020-08-30 [1] CRAN (R 4.2.1)
#>  class           7.3-20   2022-01-16 [2] CRAN (R 4.2.1)
#>  classInt        0.4-7    2022-06-10 [1] CRAN (R 4.2.1)
#>  cli             3.3.0    2022-04-25 [1] CRAN (R 4.2.1)
#>  colorspace      2.0-3    2022-02-21 [1] CRAN (R 4.2.1)
#>  crayon          1.5.1    2022-03-26 [1] CRAN (R 4.2.1)
#>  curl            4.3.2    2021-06-23 [1] CRAN (R 4.2.1)
#>  dataRetrieval   2.7.11.1 2022-08-31 [1] local
#>  DBI             1.1.3    2022-06-18 [1] CRAN (R 4.2.1)
#>  digest          0.6.29   2021-12-01 [1] CRAN (R 4.2.1)
#>  dotCall64       1.0-1    2021-02-11 [1] CRAN (R 4.2.1)
#>  dplyr           1.0.9    2022-04-28 [1] CRAN (R 4.2.1)
#>  e1071           1.7-11   2022-06-07 [1] CRAN (R 4.2.1)
#>  EGRET         * 3.0.7.2  2022-09-10 [1] local
#>  ellipsis        0.3.2    2021-04-29 [1] CRAN (R 4.2.1)
#>  evaluate        0.16     2022-08-09 [1] CRAN (R 4.2.1)
#>  fansi           1.0.3    2022-03-24 [1] CRAN (R 4.2.1)
#>  fastmap         1.1.0    2021-01-25 [1] CRAN (R 4.2.1)
#>  fields          14.1     2022-08-12 [1] CRAN (R 4.2.1)
#>  generics        0.1.3    2022-07-05 [1] CRAN (R 4.2.1)
#>  ggplot2         3.3.6    2022-05-03 [1] CRAN (R 4.2.1)
#>  glue            1.6.2    2022-02-24 [1] CRAN (R 4.1.3)
#>  gridExtra       2.3      2017-09-09 [1] CRAN (R 4.2.1)
#>  gtable          0.3.1    2022-09-01 [1] CRAN (R 4.2.1)
#>  highr           0.9      2021-04-16 [1] CRAN (R 4.2.1)
#>  hms             1.1.2    2022-08-19 [1] CRAN (R 4.2.1)
#>  htmltools       0.5.3    2022-07-18 [1] CRAN (R 4.2.1)
#>  httr            1.4.4    2022-08-17 [1] CRAN (R 4.2.1)
#>  jsonlite        1.8.0    2022-02-22 [1] CRAN (R 4.2.1)
#>  KernSmooth      2.23-20  2021-05-03 [2] CRAN (R 4.2.1)
#>  knitr           1.40     2022-08-24 [1] CRAN (R 4.2.1)
#>  lattice         0.20-45  2021-09-22 [2] CRAN (R 4.2.1)
#>  lifecycle       1.0.1    2021-09-24 [1] CRAN (R 4.2.1)
#>  magrittr        2.0.3    2022-03-30 [1] CRAN (R 4.1.3)
#>  maps            3.4.0    2021-09-25 [1] CRAN (R 4.2.1)
#>  Matrix          1.4-1    2022-03-23 [2] CRAN (R 4.2.1)
#>  munsell         0.5.0    2018-06-12 [1] CRAN (R 4.2.1)
#>  pillar          1.8.1    2022-08-19 [1] CRAN (R 4.2.1)
#>  pkgconfig       2.0.3    2019-09-22 [1] CRAN (R 4.2.1)
#>  proxy           0.4-27   2022-06-09 [1] CRAN (R 4.2.1)
#>  purrr           0.3.4    2020-04-17 [1] CRAN (R 4.2.1)
#>  R6              2.5.1    2021-08-19 [1] CRAN (R 4.2.1)
#>  Rcpp            1.0.9    2022-07-08 [1] CRAN (R 4.2.1)
#>  readr           2.1.2    2022-01-30 [1] CRAN (R 4.2.1)
#>  rlang           1.0.4    2022-07-12 [1] CRAN (R 4.2.1)
#>  rmarkdown       2.16     2022-08-24 [1] CRAN (R 4.2.1)
#>  rstudioapi      0.14     2022-08-22 [1] CRAN (R 4.2.1)
#>  scales          1.2.1    2022-08-20 [1] CRAN (R 4.2.1)
#>  sessioninfo     1.2.2    2021-12-06 [1] CRAN (R 4.2.1)
#>  sf              1.0-8    2022-07-14 [1] CRAN (R 4.2.1)
#>  spam            2.9-1    2022-08-07 [1] CRAN (R 4.2.1)
#>  stringi         1.7.8    2022-07-11 [1] CRAN (R 4.2.1)
#>  stringr         1.4.1    2022-08-20 [1] CRAN (R 4.2.1)
#>  survival        3.4-0    2022-08-09 [1] CRAN (R 4.2.1)
#>  tibble          3.1.8    2022-07-22 [1] CRAN (R 4.2.1)
#>  tidyselect      1.1.2    2022-02-21 [1] CRAN (R 4.2.1)
#>  tzdb            0.3.0    2022-03-28 [1] CRAN (R 4.2.1)
#>  units           0.8-0    2022-02-05 [1] CRAN (R 4.2.1)
#>  utf8            1.2.2    2021-07-24 [1] CRAN (R 4.2.1)
#>  vctrs           0.4.1    2022-04-13 [1] CRAN (R 4.2.1)
#>  viridis         0.6.2    2021-10-13 [1] CRAN (R 4.2.1)
#>  viridisLite     0.4.1    2022-08-22 [1] CRAN (R 4.2.1)
#>  vroom           1.5.7    2021-11-30 [1] CRAN (R 4.2.1)
#>  xfun            0.31     2022-05-10 [1] CRAN (R 4.2.1)
#>  xml2            1.3.3    2021-11-30 [1] CRAN (R 4.2.1)
#>  yaml            2.3.5    2022-02-21 [1] CRAN (R 4.2.0)
#> 
#>  [1] C:/Users/ldecicco/Documents/R/win-library/4.2
#>  [2] C:/Program Files/R/R-4.2.1/library
#> 
#> ──────────────────────────────────────────────────────────────────────────────
```

## Reporting bugs

Please consider reporting bugs and asking questions on the Issues page:
<https://github.com/USGS-R/EGRET/issues>

Follow `@USGS_DataSci` on Twitter for updates on USGS R packages:

[![Twitter
Follow](https://img.shields.io/twitter/follow/USGS_DataSci.svg?style=social&label=Follow%20USGS_DataSci)](https://twitter.com/USGS_DataSci)

## Subscribe

Please email questions, comments, and feedback to:
<egret_comments@usgs.gov>

Additionally, to subscribe to an email list concerning updates to these
R packages, please send a request to <egret_comments@usgs.gov>.

## Code of Conduct

We want to encourage a warm, welcoming, and safe environment for
contributing to this project. See the [code of
conduct](https://github.com/USGS-R/EGRET/blob/master/CONDUCT.md) for
more information.

## Package Support

The Water Mission Area of the USGS has supported the development and
maintenance of the `EGRET` R-package. Further maintenance is expected to
be stable through October 2022. Resources are available primarily for
maintenance and responding to user questions. Priorities on the
development of new features are determined by the `EGRET` development
team.

[![USGS](http://usgs-r.github.io/images/usgs.png)](https://www.usgs.gov/)

## Sunset date

Funding for `EGRET` currently expires fall 2022. Expectations are that
maintenance and customer service will continue to be supported past that
date.

## How to cite EGRET:

``` r
citation(package = "EGRET")
#> 
#> To cite EGRET in publications, please use:
#> 
#>   Hirsch, R.M., and De Cicco, L.A., 2015, User guide to Exploration and
#>   Graphics for RivEr Trends (EGRET) and dataRetrieval: R packages for
#>   hydrologic data (version 2.0, February 2015): U.S. Geological Survey
#>   Techniques and Methods book 4, chap. A10, 93 p., doi:10.3133/tm4A10
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @InBook{,
#>     author = {Robert M. Hirsch and Laura A. {De Cicco}},
#>     title = {User guide to Exploration and Graphics for RivEr Trends (EGRET) and dataRetrieval: R packages for hydrologic data},
#>     publisher = {U.S. Geological Survey},
#>     address = {Reston, VA},
#>     booktitle = {Techniques and Methods},
#>     institution = {U.S. Geological Survey},
#>     year = {2015},
#>     chapter = {A10},
#>     url = {https://pubs.usgs.gov/tm/04/a10/},
#>   }
```

## References

See this list for WRTDS applications in print:

<http://usgs-r.github.io/EGRET/articles/References_WRTDS.html>

## Disclaimer

This software has been approved for release by the U.S. Geological
Survey (USGS). Although the software has been subjected to rigorous
review, the USGS reserves the right to update the software as needed
pursuant to further analysis and review. No warranty, expressed or
implied, is made by the USGS or the U.S. Government as to the
functionality of the software and related material nor shall the fact of
release constitute any such warranty. Furthermore, the software is
released on condition that neither the USGS nor the U.S. Government
shall be held liable for any damages resulting from its authorized or
unauthorized use.
