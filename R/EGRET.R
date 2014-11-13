#' EGRET package includes WRTDS and flowHistory
#'
#' \tabular{ll}{
#' Package: \tab EGRET\cr
#' Type: \tab Package\cr
#' Version: \tab 2.0.0\cr
#' Date: \tab 2014-11-13\cr
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

#' Example Daily dataframe
#'
#' Example data representing streamflowfrom the Choptank River at Greensboro, MD,  USGS data
#' as agumented by the modelEstimation process.
#'
#' @name ChopDaily
#' @docType data
#' @source Data retrieved from NWIS water services (\url{http://waterservices.usgs.gov/}) via the EGRET R package.
#' The USGS site id is 01491000. The period requested was 1979-10-01 to 2011-09-30. 
#' @format Data frame with 11688 rows and 18 columns:
#' \tabular{llll}{
#' ColumnName \tab Type \tab Description \tab Units  \cr
#' Date \tab Date \tab Date \tab date \cr 
#' Q \tab number \tab Discharge in cms \tab cms \cr 
#' Julian \tab number \tab Number of days since January 1, 1850 \tab days \cr 
#' Month \tab integer \tab Month of the year [1-12] \tab months \cr 
#' Day \tab integer \tab Day of the year [1-366] \tab days \cr 
#' DecYear \tab number \tab Decimal year \tab years \cr 
#' MonthSeq \tab integer \tab Number of months since January 1, 1850 \tab months \cr 
#' Qualifier \tab string \tab Qualifing code \tab character \cr 
#' i \tab integer \tab Index of days, starting with 1 \tab days \cr 
#' LogQ \tab number \tab Natural logarithm of Q \tab numeric \cr 
#' Q7 \tab number \tab 7 day running average of Q \tab cms \cr
#' Q30 \tab number \tab 30 running average of Q \tab cms \cr
#' yHat \tab number \tab The WRTDS estimate of the log of concentration \tab numeric \cr 
#' SE \tab number \tab The WRTDS estimate of the standard error of yHat \tab numeric \cr 
#' ConcDay \tab number \tab The WRTDS estimate of concentration \tab mg/L \cr 
#' FluxDay \tab number \tab The WRTDS estimate of flux \tab kg/day \cr 
#' FNConc \tab number \tab Flow normalized estimate of concentration \tab mg/L \cr 
#' FNFlux \tab number \tab Flow Normalized estimate of flux \tab kg/day \cr 
#' }
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality streamflow data
#' @examples
#' Daily <- ChopDaily
#' with(Daily, plot(Date, LogQ, type="l"))
NULL

#' Example Sample dataframe
#'
#' Example data representing Nitrate sample data from the Choptank River at Greensboro, MD,  USGS data
#' as agumented by the modelEstimation process
#'
#' @name ChopSample
#' @docType data
#' @source Nitrate/nitrite data retrieved from National Water Quality Monitoring Council (NWQMC) Water Quality Portal
#' (\url{http://www.waterqualitydata.us/}), and discharge retrieved from NWIS water services (\url{http://waterservices.usgs.gov/}), both via the EGRET R package.
#' The USGS site id is 01491000. The period requested was 1979-10-01 to 2011-09-30. The parameter was Inorganic nitrogen (nitrate and nitrite), 
#' USGS parameter code 00631.
#' @format Data frame with 606 rows and 17 columns:
#' \tabular{llll}{
#' ColumnName \tab Type \tab Description \tab Units  \cr
#' Date \tab Date \tab Date \tab date \cr 
#' ConcLow \tab number \tab Lower limit of concentration \tab mg/L \cr
#' ConcHigh \tab number \tab Upper limit of concentration \tab mg/L \cr
#' Uncen \tab integer \tab Uncensored data (1=true, 0=false) \tab integer \cr 
#' ConcAve \tab number \tab Average concentration \tab mg/L \cr
#' Julian \tab number \tab Number of days since January 1, 1850 \tab days \cr 
#' Month \tab integer \tab Month of the year [1-12] \tab months \cr 
#' Day \tab integer \tab Day of the year [1-366] \tab days \cr 
#' DecYear \tab number \tab Decimal year \tab years \cr 
#' MonthSeq \tab integer \tab Number of months since January 1, 1850 \tab months \cr 
#' SinDY \tab number \tab Sine of DecYear \tab numeric \cr 
#' CosDY \tab number \tab Cosine of DecYear \tab numeric \cr 
#' Q \tab number \tab Discharge \tab cms \cr 
#' LogQ \tab number \tab Natural logarithm of flow \tab numeric \cr
#' yHat \tab number \tab estimate of the log of concentration \tab numeric \cr 
#' SE \tab number \tab estimate of the standard error of yHat \tab numeric \cr 
#' ConcHat \tab number \tab unbiased estimate of concentration \tab mg/L \cr
#' }
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @keywords water quality data
#' @examples
#' Sample <- ChopSample
#' with(Sample, plot(Date, ConcAve))
NULL

#' Example INFO dataframe
#'
#' Example data representing meta-data from the Choptank River at Greensboro, MD,  USGS data
#' as augmented by the functions setupYears (for WRTDS) and setPA (for flowHistory).
#'
#' @name ChopINFO
#' @docType data
#' @source Parameter information (Nitrate/nitrite, USGS parameter code 00631) is found from NWISweb (\url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/}), and station
#' information is retrieved from NWIS water services (\url{http://waterservices.usgs.gov/}), both via the EGRET R package.
#' The USGS site id is 01491000. 
#' @format Data frame with 1 row and 56 columns, some of which are described here:
#' \tabular{lll}{
#' ColumnName \tab Type \tab Description \cr
#' shortName \tab string \tab Name of site, suitable for use in graphical headings \cr 
#' staAbbrev \tab string \tab Abbreviation for station name, used in saveResults \cr 
#' paramShortName \tab string \tab Name of constituent, suitable for use in graphical headings \cr 
#' constitAbbrev \tab string \tab Abbreviation for constituent name, used in saveResults \cr 
#' drainSqKm \tab numeric \tab Drainage area in  km^2 \cr 
#' paStart \tab integer (1-12) \tab Starting month of period of analysis \cr 
#' paLong \tab integer (1-12) \tab Length of period of analysis in months \cr 
#' }
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

#' Example eList
#'
#' Example data representing data from the Choptank River at Greensboro, MD,  USGS data
#' Data is a named list of the Daily, Sample, INFO dataframes, and the surface matrix.
#'
#' @name Choptank_eList
#' @docType data
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


#' List of USGS parameter codes
#'
#' Complete list of USGS parameter codes as of September 25, 2013.
#'
#' @name parameterCdFile
#' @docType data
#' @keywords USGS parameterCd
NULL
