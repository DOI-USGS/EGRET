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
#' Package: \tab EGRETBeta\cr
#' Type: \tab Package\cr
#' Version: \tab 1.2.3\cr
#' Date: \tab 2012-12-31\cr
#' License: \tab This software is in the public domain because it contains materials
#' that originally came from the United States Geological Survey, an agency of
#' the United States Department of Interior. For more information, see the
#' official USGS copyright policy at
#' \code{\link{http://www.usgs.gov/visual-id/credit_usgs.html#copyright}}\cr
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

#' Data included in EGRET
#'
#' Example data representing Streamflow and Nitrate from the Choptank River at Greensboro, MD,  USGS data
#'
#' @name exDailyStart
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality streamflow data
NULL

#' Data included in EGRET
#'
#' Example data representing Streamflow and Nitrate from the Choptank River at Greensboro, MD,  USGS data
#' as agumented by the modelEstimation process
#'
#' @name exDailyEnd
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality streamflow data
NULL

#' Data included in EGRET
#'
#' Example data representing Nitrate sample data from the Choptank River at Greensboro, MD,  USGS data
#'
#' @name exSampleStart
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality data
NULL

#' Data included in EGRET
#'
#' Example data representing Nitrate sample data from the Choptank River at Greensboro, MD,  USGS data
#' as agumented by the modelEstimation process
#'
#' @name exSampleEnd
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality data
NULL

#' Data included in EGRET
#'
#' Example data representing meta-data from the Choptank River at Greensboro, MD,  USGS data
#'
#' @name exINFOStart
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality data
NULL

#' Data included in EGRET
#'
#' Example data representing meta-data from the Choptank River at Greensboro, MD,  USGS data
#' as augmented by the functions setupYears (for WRTDS) and setPA (for flowHistory)
#'
#' @name exINFOEnd
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality data
NULL

#' Data included in EGRET
#'
#' Example data representing annual WRTDS results for Nitrate data from the Choptank River at Greensboro, MD,  USGS data
#' as output from the function setupYears
#'
#' @name exAnnualResults
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality data
NULL

#' Data included in EGRET
#'
#' Example data representing annual Series of streamflow statistics from the Choptank River at Greensboro, MD,  USGS data
#' as output from the function makeAnnualSeries
#'
#' @name exannualSeries
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords streamflow data
NULL

#' Data included in EGRET
#'
#' Example data representing monthly WRTDS results for Nitrate data from the Choptank River at Greensboro, MD,  USGS data
#' as output from the function calculateMonthlyResults
#'
#' @name exMonthlyResults
#' @docType data
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality data
NULL

#' Data included in EGRET
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
#' Flux units
#'
#' @name fluxConst
#' @docType data
NULL

#' Flow units included in EGRET
#'
#' Flow units
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