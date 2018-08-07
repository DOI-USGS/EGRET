EGRET
=====

[![travis](https://travis-ci.org/USGS-R/EGRET.svg?branch=master)](https://travis-ci.org/USGS-R/EGRET) [![Build status](https://ci.appveyor.com/api/projects/status/i2hr35abwxx85vgs?svg=true)](https://ci.appveyor.com/project/ldecicco-USGS/EGRET) [![Coverage Status](https://coveralls.io/repos/github/USGS-R/EGRET/badge.svg?branch=master)](https://coveralls.io/github/USGS-R/EGRET?branch=master) [![status](https://img.shields.io/badge/USGS-Research-blue.svg)](https://owi.usgs.gov/R/packages.html#research) [![CRAN version](http://www.r-pkg.org/badges/version/EGRET)](https://cran.r-project.org/package=EGRET) [![](http://cranlogs.r-pkg.org/badges/EGRET)](https://cran.r-project.org/package=EGRET) [![](http://cranlogs.r-pkg.org/badges/grand-total/EGRET)](https://cran.r-project.org/package=EGRET) [![Research software impact](http://depsy.org/api/package/cran/EGRET/badge.svg)](http://depsy.org/package/r/EGRET)

Exploration and Graphics for RivEr Trends (`EGRET`): An R-package for the analysis of long-term changes in water quality and streamflow, including the water-quality method Weighted Regressions on Time, Discharge, and Season (WRTDS).

Please visit the wiki for more information: [EGRET Wiki](https://github.com/USGS-R/EGRET/wiki)

The link for the official USGS publication user guide is here:

<https://pubs.usgs.gov/tm/04/a10/>

Package Installation
--------------------

To install the EGRET package, you must be using R 3.0 or greater and run the following command:

``` r
install.packages("EGRET")
```

To get inter-CRAN release updates, use the command:

``` r
install.packages("EGRET",repos="https://owi.usgs.gov/R")
```

To get cutting-edge changes, install from GitHub using the `devtools` packages:

``` r
library(devtools)
install_github("USGS-R/EGRET")
```

Background:
-----------

Evaluating long-term changes in river conditions (water quality and discharge) is an important use of hydrologic data. To carry out such evaluations, the hydrologist needs tools to facilitate several key steps in the process: acquiring the data records from a variety of sources, structuring it in ways that facilitate the analysis, routines that will process the data to extract information about changes that may be happening, and graphical techniques that can display findings about change. The R package `EGRET` (Exploration and Graphics for RivEr Trends) was developed for carrying out each of these steps in an integrated manner. It is designed to accept easily data from three sources: U.S. Geological Survey hydrologic data, Water Quality Portal Data (currently including U.S. Environmental Protection Agency (EPA) STORET data, and USDA STEWARDS data), and user-supplied flat files. The `EGRET` package has components oriented towards the description of long-term changes in streamflow statistics (high flow, average flow, and low flow) as well as changes in water quality. For the water-quality analysis, it uses Weighted Regressions on Time, Discharge and Season (WRTDS) to describe long-term trends in both concentration and flux. `EGRET` also creates a wide range of graphical presentations of the water-quality data and of the WRTDS results. The following report serves as a user guide, providing detailed guidance on installation and use of the software, documentation of the analysis methods used, as well as guidance on some of the kinds of questions and approaches that the software can facilitate.

`EGRET` includes statistics and graphics for streamflow history, water quality trends, and the statistical modeling algorithm Weighted Regressions on Time, Discharge, and Season (WRTDS). Please see the official EGRET User Guide for more information on the `EGRET` package:

<https://dx.doi.org/10.3133/tm4A10> The best ways to learn about the WRTDS approach is to read the User Guide and two journal articles. These articles are available, for free, from the journals in which they were published. The first relates to nitrate and total phosphorus data for 9 rivers draining to Chesapeake Bay. The URL is:

<https://onlinelibrary.wiley.com/doi/full/10.1111/j.1752-1688.2010.00482.x>.

The second is an application to nitrate data for 8 monitoring sites on the Mississippi River or its major tributaries. The URL is:

<http://pubs.acs.org/doi/abs/10.1021/es201221s>

Sample Workflow
---------------

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
############################
```

``` r
library(EGRET)
# Sample data included in package:
eList <- Choptank_eList

############################
# Check sample data:
boxConcMonth(eList)
```

![](man/figures/unnamed-chunk-6-1.png)

``` r
boxQTwice(eList)
```

![](man/figures/unnamed-chunk-6-2.png)

``` r
plotConcTime(eList)
```

![](man/figures/unnamed-chunk-6-3.png)

``` r
plotConcQ(eList)
```

![](man/figures/unnamed-chunk-6-4.png)

``` r
multiPlotDataOverview(eList)
```

![](man/figures/unnamed-chunk-6-5.png)

``` r
############################

############################
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
############################

############################
#Check model results:

#eList:
plotConcTimeDaily(eList)
```

![](man/figures/unnamed-chunk-6-6.png)

``` r
plotFluxTimeDaily(eList)
```

![](man/figures/unnamed-chunk-6-7.png)

``` r
plotConcPred(eList)
```

![](man/figures/unnamed-chunk-6-8.png)

``` r
plotFluxPred(eList)
```

![](man/figures/unnamed-chunk-6-9.png)

``` r
plotResidPred(eList)
```

![](man/figures/unnamed-chunk-6-10.png)

``` r
plotResidQ(eList)
```

![](man/figures/unnamed-chunk-6-11.png)

``` r
plotResidTime(eList)
```

![](man/figures/unnamed-chunk-6-12.png)

``` r
boxResidMonth(eList)
```

![](man/figures/unnamed-chunk-6-13.png)

``` r
boxConcThree(eList)
```

![](man/figures/unnamed-chunk-6-14.png)

``` r

#Require Daily + INFO:
plotConcHist(eList)
```

![](man/figures/unnamed-chunk-6-15.png)

``` r
plotFluxHist(eList)
```

![](man/figures/unnamed-chunk-6-16.png)

``` r

# Multi-line plots:
date1 <- "2000-09-01"
date2 <- "2005-09-01"
date3 <- "2009-09-01"
qBottom<-100
qTop<-5000
plotConcQSmooth(eList, date1, date2, date3, qBottom, qTop, 
                   concMax=2,qUnit=1)
```

![](man/figures/unnamed-chunk-6-17.png)

``` r
q1 <- 10
q2 <- 25
q3 <- 75
centerDate <- "07-01"
yearEnd <- 2009
yearStart <- 2000
plotConcTimeSmooth(eList, q1, q2, q3, centerDate, yearStart, yearEnd)
```

![](man/figures/unnamed-chunk-6-18.png)

``` r

# Multi-plots:
fluxBiasMulti(eList)
```

![](man/figures/unnamed-chunk-6-19.png)

``` r

#Contour plots:
clevel<-seq(0,2,0.5)
maxDiff<-0.8
yearStart <- 2000
yearEnd <- 2010

plotContours(eList, yearStart,yearEnd,qBottom,qTop, 
             contourLevels = clevel,qUnit=1)
```

![](man/figures/unnamed-chunk-6-20.png)

``` r
plotDiffContours(eList, yearStart,yearEnd,
                 qBottom,qTop,maxDiff,qUnit=1)
```

![](man/figures/unnamed-chunk-6-21.png)

### Sample workflow for a flowHistory application for the entire record

``` r
library(EGRET)

# Flow history analysis
############################
# Gather discharge data:
siteID <- "01491000" #Choptank River at Greensboro, MD
startDate <- "" # Get earliest date
endDate <- "" # Get latest date
Daily <- readNWISDaily(siteID,"00060",startDate,endDate)
#> There are 25786 data points, and 25786 days.
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
############################

############################
# Check flow history data:
plotFlowSingle(eList, istat=7,qUnit="thousandCfs")
```

![](man/figures/unnamed-chunk-7-1.png)

``` r
plotSDLogQ(eList)
```

![](man/figures/unnamed-chunk-7-2.png)

``` r
plotQTimeDaily(eList, qLower=1,qUnit=3)
```

![](man/figures/unnamed-chunk-7-3.png)

``` r
plotFour(eList, qUnit=3)
```

![](man/figures/unnamed-chunk-7-4.png)

``` r
plotFourStats(eList, qUnit=3)
```

![](man/figures/unnamed-chunk-7-5.png)

``` r
############################
```

Model Archive
-------------

When using the `WRTDS` model, it is important to be able to reproduce the results in the future. The following version of R and package dependencies were used most recently to pass the embedded tests within this package. There is no guarantee of reproducible results using future versions of R or updated versions of package dependencies; however, we will make diligent efforts to test and update future modeling environments.

``` r
devtools::session_info()
#> Session info -------------------------------------------------------------
#>  setting  value                       
#>  version  R version 3.5.1 (2018-07-02)
#>  system   x86_64, mingw32             
#>  ui       RTerm                       
#>  language (EN)                        
#>  collate  English_United States.1252  
#>  tz       America/Chicago             
#>  date     2018-08-07
#> Packages -----------------------------------------------------------------
#>  package       * version date       source        
#>  assertthat      0.2.0   2017-04-11 CRAN (R 3.5.0)
#>  backports       1.1.2   2017-12-13 CRAN (R 3.5.0)
#>  base          * 3.5.1   2018-07-02 local         
#>  bindr           0.1.1   2018-03-13 CRAN (R 3.5.0)
#>  bindrcpp        0.2.2   2018-03-29 CRAN (R 3.5.0)
#>  codetools       0.2-15  2016-10-05 CRAN (R 3.5.1)
#>  compiler        3.5.1   2018-07-02 local         
#>  crayon          1.3.4   2017-09-16 CRAN (R 3.5.0)
#>  curl            3.2     2018-03-28 CRAN (R 3.5.0)
#>  dataRetrieval   2.7.4   2018-05-09 local         
#>  datasets      * 3.5.1   2018-07-02 local         
#>  devtools        1.13.6  2018-06-27 CRAN (R 3.5.0)
#>  digest          0.6.15  2018-01-28 CRAN (R 3.5.0)
#>  dotCall64       1.0-0   2018-07-30 CRAN (R 3.5.1)
#>  dplyr           0.7.6   2018-06-29 CRAN (R 3.5.0)
#>  EGRET         * 3.0.0   2018-08-07 local         
#>  evaluate        0.11    2018-07-17 CRAN (R 3.5.1)
#>  fields          9.6     2018-01-29 CRAN (R 3.5.0)
#>  foreach         1.4.4   2017-12-12 CRAN (R 3.5.0)
#>  glue            1.3.0   2018-07-17 CRAN (R 3.5.1)
#>  graphics      * 3.5.1   2018-07-02 local         
#>  grDevices     * 3.5.1   2018-07-02 local         
#>  grid            3.5.1   2018-07-02 local         
#>  hms             0.4.2   2018-03-10 CRAN (R 3.5.0)
#>  htmltools       0.3.6   2017-04-28 CRAN (R 3.5.0)
#>  httr            1.3.1   2017-08-20 CRAN (R 3.5.0)
#>  iterators       1.0.10  2018-07-13 CRAN (R 3.5.1)
#>  jsonlite        1.5     2017-06-01 CRAN (R 3.5.0)
#>  knitr           1.20    2018-02-20 CRAN (R 3.5.0)
#>  lattice         0.20-35 2017-03-25 CRAN (R 3.5.1)
#>  lubridate       1.7.4   2018-04-11 CRAN (R 3.5.0)
#>  magrittr        1.5     2014-11-22 CRAN (R 3.5.0)
#>  maps            3.3.0   2018-04-03 CRAN (R 3.5.0)
#>  Matrix          1.2-14  2018-04-13 CRAN (R 3.5.1)
#>  memoise         1.1.0   2017-04-21 CRAN (R 3.5.0)
#>  methods       * 3.5.1   2018-07-02 local         
#>  pillar          1.3.0   2018-07-14 CRAN (R 3.5.1)
#>  pkgconfig       2.0.1   2017-03-21 CRAN (R 3.5.0)
#>  plyr            1.8.4   2016-06-08 CRAN (R 3.5.0)
#>  purrr           0.2.5   2018-05-29 CRAN (R 3.5.0)
#>  R6              2.2.2   2017-06-17 CRAN (R 3.5.0)
#>  Rcpp            0.12.18 2018-07-23 CRAN (R 3.5.1)
#>  readr           1.1.1   2017-05-16 CRAN (R 3.5.0)
#>  reshape2        1.4.3   2017-12-11 CRAN (R 3.5.0)
#>  rlang           0.2.1   2018-05-30 CRAN (R 3.5.0)
#>  rmarkdown       1.10    2018-06-11 CRAN (R 3.5.0)
#>  rprojroot       1.3-2   2018-01-03 CRAN (R 3.5.0)
#>  spam            2.2-0   2018-06-19 CRAN (R 3.5.0)
#>  splines         3.5.1   2018-07-02 local         
#>  stats         * 3.5.1   2018-07-02 local         
#>  stringi         1.1.7   2018-03-12 CRAN (R 3.5.0)
#>  stringr         1.3.1   2018-05-10 CRAN (R 3.5.0)
#>  survival        2.42-3  2018-04-16 CRAN (R 3.5.1)
#>  tibble          1.4.2   2018-01-22 CRAN (R 3.5.0)
#>  tidyselect      0.2.4   2018-02-26 CRAN (R 3.5.0)
#>  tools           3.5.1   2018-07-02 local         
#>  utils         * 3.5.1   2018-07-02 local         
#>  withr           2.1.2   2018-03-15 CRAN (R 3.5.0)
#>  xml2            1.2.0   2018-01-24 CRAN (R 3.5.0)
#>  yaml            2.2.0   2018-07-25 CRAN (R 3.5.1)
```

Reporting bugs
--------------

Please consider reporting bugs and asking questions on the Issues page: <https://github.com/USGS-R/EGRET/issues>

Follow `@USGS_R` on Twitter for updates on USGS R packages:

[![Twitter Follow](https://img.shields.io/twitter/follow/USGS_R.svg?style=social&label=Follow%20USGS_R)](https://twitter.com/USGS_R)

Subscribe
---------

Please email questions, comments, and feedback to: <egret_comments@usgs.gov>

Additionally, to subscribe to an email list concerning updates to these R packages, please send a request to <egret_comments@usgs.gov>.

Code of Conduct
---------------

We want to encourage a warm, welcoming, and safe environment for contributing to this project. See the [code of conduct](https://github.com/USGS-R/EGRET/blob/master/CONDUCT.md) for more information.

Package Support
---------------

The Water Mission Area of the USGS has supported the development and maintenance of the `EGRET` R-package. Further maintenance is expected to be stable through September 2019. Resources are available primarily for maintenance and responding to user questions. Priorities on the development of new features are determined by the `EGRET` development team.

[![USGS](http://usgs-r.github.io/images/usgs.png)](https://www.usgs.gov/)

Sunset date
-----------

Funding for `EGRET` currently expires summer 2019. Expectations are that maintenance and customer service will continue to be supported past that date.

How to cite EGRET:
------------------

``` r
citation(package = "EGRET")
#> 
#> To cite EGRET in publications, please use:
#> 
#>   Hirsch, R.M., and De Cicco, L.A., 2015, User guide to
#>   Exploration and Graphics for RivEr Trends (EGRET) and
#>   dataRetrieval: R packages for hydrologic data (version 2.0,
#>   February 2015): U.S. Geological Survey Techniques and Methods
#>   book 4, chap. A10, 93 p., http://dx.doi.org/10.3133/tm4A10
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

Disclaimer
----------

This software has been approved for release by the U.S. Geological Survey (USGS). Although the software has been subjected to rigorous review, the USGS reserves the right to update the software as needed pursuant to further analysis and review. No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. Furthermore, the software is released on condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from its authorized or unauthorized use.
