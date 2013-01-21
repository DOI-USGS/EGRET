#####################################################
# 07June2012  EGRET.R
# RMH  17Jan2012  various functions modified and added for flux bias analysis
# RMH  21Jan2012  Added plotConcHist
# RMH  23Jan2012  Added plotContours
# RMH  23Jan2012  Enhancements of graphics, and added scientific notation to axes
# RMH  24Jan2012  Starting to document for packaging
# LDC  27Jan2012  Including S4 objects for flux and qUnits and some packaging docs
# RMH  31Jan2012  More documentation for packaging and adding a few more functions
# RMH  01Feb2012  Small correction to plotLogConcTime also added plotConcQSmooth and put plotContours back in
# RMH  01Feb2012  found that some of the roxygen2 documentation was missing from plotResidPred so this was added in
# RMH  09Feb2012  added another function plotLogConcQSmooth, still needs roxygen2 documentation also deleted un-necessary code about units
# RMH  09Feb2012  added the surfaces data set and finished of some roxygen2 code
# RMH  13Feb2012  merging in Smooths.R and estimateAndAggregate.R
# RMH  23Feb2012  minor adjustments to some of the plotting routines
# RMH  09Mar2012  small changes to flowDuration, plotLogFluxQ, fluxBiasStat, fluxBiasMulti
# RMH  13Mar2012  added in all the code that had been in allOther.R
# RMH  16Mar2012  changed all of the example data sets (also changed cex=0.4 to cex=0.7 in plots)
# RMH  21Mar2012  fixed small bugs in plotConcHist, plotFluxHist, and saveResults
# RMH  23Mar2012  cosmetic improvements in plotContours and plotDiffContours
# RMH  23Apr2012  fixed typos in estDailyWithoutNormalization and changes to vertical axes in 3 plots
# RMH  25Apr2012  Edited the text in descriptions to add periods at the end of sentences, also fixed the example in plotConcHist
# RMH  05Jun2012  Added several new functions to increase the capabilities of flowHistory
# RMH  07Jun2012  three bug fixes (in plotFlowSingle, plotSDLogQ, and estDailyFromSurfaces)
################################################################
#' EGRET package (except for dataRetrieval) includes WRTDS and flowHistory
#'
#' \tabular{ll}{
#' Package: \tab EGRET\cr
#' Type: \tab Package\cr
#' Version: \tab 1.2.3\cr
#' Date: \tab 2013-01-03\cr
#' License: \tab Unlimited for this package, dependencies have more restrictive licensing.\cr
#' Copyright: \tab This software is in the public domain because it contains materials
#' that originally came from the United States Geological Survey, an agency of
#' the United States Department of Interior. For more information, see the
#' official USGS copyright policy at
#' http://www.usgs.gov/visual-id/credit_usgs.html#copyright\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' Collection of functions to do WRTDS and flowHistory analysis,
#'  and produce graphs and tables of data and results from these analyses.
#'
#' @name EGRET-package
#' @docType package
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}, Laura De Cicco \email{ldecicco@@usgs.gov}
#' @references Hirsch, R. M., Moyer, D. L. and Archfield, S. A. (2010), Weighted Regressions on Time, Discharge, and Season (WRTDS), with an Application to Chesapeake Bay River Inputs. JAWRA Journal of the American Water Resources Association, 46: 857-880. doi: 10.1111/j.1752-1688.2010.00482.x
#' @keywords water-quality graphics streamflow statistics 
NULL

#' Example initial Daily dataframe
#'
#' Example data representing streamflow from the Choptank River at Greensboro, MD,  USGS data.  
#' This is the data that is initially generated from the dataRetrieval package.
#'
#' @name exDailyStart
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality streamflow data
NULL

#' Example final Daily dataframe
#'
#' Example data representing streamflowfrom the Choptank River at Greensboro, MD,  USGS data
#' as agumented by the modelEstimation process.
#'
#' @name exDailyEnd
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality streamflow data
NULL

#' Example initial Sample dataframe
#'
#' Example data representing Nitrate sample data from the Choptank River at Greensboro, MD,  USGS data.
#' This is the data that is initially generated from the dataRetrieval package.
#'
#' @name exSampleStart
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality data
NULL

#' Example final Sample dataframe
#'
#' Example data representing Nitrate sample data from the Choptank River at Greensboro, MD,  USGS data
#' as agumented by the modelEstimation process
#'
#' @name exSampleEnd
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality data
NULL

#' Example initial INFO dataframe
#'
#' Example data representing meta-data from the Choptank River at Greensboro, MD,  USGS data
#' This is the data that is initially generated from the dataRetrieval package.
#'
#' @name exINFOStart
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality data
NULL

#' Example final INFO dataframe
#'
#' Example data representing meta-data from the Choptank River at Greensboro, MD,  USGS data
#' as augmented by the functions setupYears (for WRTDS) and setPA (for flowHistory)
#'
#' @name exINFOEnd
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality data
NULL

#' Example AnnualResults dataframe
#'
#' Example data representing annual WRTDS results for Nitrate data from the Choptank River at Greensboro, MD,  USGS data
#' as output from the function setupYears
#'
#' @name exAnnualResults
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality data
NULL

#' Example annualSeries dataframe
#'
#' Example data representing annual Series of streamflow statistics from the Choptank River at Greensboro, MD,  USGS data
#' as output from the function makeAnnualSeries
#'
#' @name exannualSeries
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords streamflow data
NULL

#' Example MonthlyResults dataframe
#'
#' Example data representing monthly WRTDS results for Nitrate data from the Choptank River at Greensboro, MD,  USGS data
#' as output from the function calculateMonthlyResults
#'
#' @name exMonthlyResults
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality data
NULL

#' Example surface array
#'
#' Example data representing surfaces, from the fitted model
#' of Nitrate data for the Choptank River at Greensboro, MD,  USGS data
#' output from the function estSurfaces
#'
#' @name exsurfaces
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality data
NULL


#' Flux units included in EGRET
#'
#' Flux units included:
#' \tabular{lllll}{
#' Number \tab ObjectName \tab shortName \tab unitFactor \tab unitName  \cr
#' 1      \tab POUNDS_DAY \tab lbs/day   \tab 2.204623   \tab pounds/day\cr
#' 2      \tab TONS_DAY   \tab tons/day  \tab 0.001102  \tab tons/day   \cr
#' 3      \tab KG_DAY     \tab kg/day    \tab 1          \tab kg/day    \cr
#' 4      \tab THOUSAND_KG_DAY\tab 10^3 kg/day \tab 0.001 \tab thousands of kg/day\cr
#' 5      \tab TONS_YEAR\tab tons/yr \tab 0.402619 \tab tons/year\cr
#' 6      \tab THOUSAND_TONS_YEAR\tab 10^3 tons/yr \tab 0.000402619 \tab thousands of tons/year \cr
#' 7      \tab MILLION_TONS_YEAR\tab 10^6 tons/yr \tab 4.02619e-07 \tab millions of tons/year\cr
#' 8      \tab THOUSAND_KG_YEAR\tab 10^3 kg/yr \tab 0.36525 \tab thousands of kg/year\cr
#' 9      \tab MILLION_KG_YEAR\tab 10^6 kg/yr \tab 0.00036525 \tab millions of kg/year\cr
#' 10     \tab BILLION_KG_YEAR\tab 10^9 kg/yr \tab 3.6525e-07 \tab billions of kg/year \cr
#' 11     \tab thousandTonsDay \tab 10^3 tons/day \tab 1.102e-06 \tab thousands of tons/day \cr
#' 12     \tab millionKgDay \tab 10^6 kg/day \tab 1e-06 \tab millions of kg/day \cr
#' }
#' 
#'
#' @name fluxConst
#' @docType data
NULL
#' Flow units included in EGRET
#'
#' Flow units included:
#' \tabular{llll}{
#' Number \tab ObjectName \tab shortName \tab unitFactor \cr
#' 1      \tab cfs      \tab Cubic Feet per Second   \tab 35.31467 \cr
#' 2      \tab cms      \tab Cubic Meters per Second  \tab 1  \cr
#' 3      \tab thousandCfs \tab Thousand Cubic Feet per Second   \tab  0.03531467 \cr
#' 4      \tab thousandCms \tab Thousand Cubic Meters per Second \tab 0.001 \cr
#' 5      \tab mmDay \tab mm per day \tab   \cr
#' 6      \tab mmYear \tab mm per year \tab   \cr
#' }
#'
#' @name qConst
#' @docType data
NULL

#' Month labels included in EGRET
#'
#' Month labels
#'
#' @name monthInfo
#' @docType data
NULL