.onAttach <- function(libname, pkgname) {
  if (!interactive()) return()
  EGRET_version = utils::packageVersion("EGRET")
  packageStartupMessage("EGRET ", EGRET_version,"
Extended Documentation: https://rconnect.usgs.gov/EGRET")
}

#' EGRET package includes WRTDS and flowHistory
#'
#' \tabular{ll}{
#' Package: \tab EGRET\cr
#' Type: \tab Package\cr
#' License: \tab Unlimited for this package, dependencies have more restrictive licensing.\cr
#' Copyright: \tab This software is in the public domain because it contains materials
#' that originally came from the United States Geological Survey, an agency of
#' the United States Department of Interior. For more information, see the
#' official USGS copyright policy at
#' https://www.usgs.gov/visual-id/credit_usgs.html#copyright\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' Collection of functions to do WRTDS and flowHistory analysis,
#'  and produce graphs and tables of data and results from these analyses.
#'
#' @name EGRET-package
#' @docType package
#' @importFrom utils write.table
#' @importFrom utils read.delim
#' @importFrom utils URLencode
#' @importFrom utils tail
#' @importFrom stats aggregate
#' @importFrom stats lm
#' @importFrom stats median
#' @importFrom stats sd
#' @importFrom stats reshape
#' @importFrom stats filter
#' @importFrom stats rnorm
#' @importFrom stats na.omit
#' 
#' @importFrom graphics abline
#' @importFrom graphics axis
#' @importFrom graphics box
#' @importFrom graphics boxplot
#' @importFrom graphics contour
#' @importFrom graphics filled.contour
#' @importFrom graphics grconvertX
#' @importFrom graphics grconvertY
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @importFrom graphics points
#' @importFrom graphics segments
#' @importFrom graphics title
#' @importFrom stats predict
#' @importFrom stats quantile
#' @importFrom grDevices colorRampPalette
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}, Laura De Cicco \email{ldecicco@@usgs.gov}
#' @references Hirsch, R.M., and De Cicco, L.A., 2014, User guide to Exploration and Graphics for RivEr Trends 
#' (EGRET) and dataRetrieval: R packages for hydrologic data: U.S. Geological Survey Techniques and Methods book 4, 
#' chap. A10, 94 p., \doi{10.3133/tm4A10}
#' @keywords water-quality graphics streamflow statistics 
NULL

#' Example eList
#'
#' Example data representing data from the Choptank River at Greensboro, MD,  USGS data
#' Data is a named list of the Daily, Sample, INFO dataframes, and the surface matrix.
#'
#' @name Choptank_eList
#' @rdname sampleData
#' @docType data
#' @keywords water quality data
#' @examples 
#' head(Choptank_eList$Daily)
NULL

#' @name Arkansas_eList
#' @rdname sampleData
#' @examples 
#' head(Arkansas_eList$Daily)
NULL

#' Constants included with EGRET
#' 
#'\itemize{
#'  \item{fluxConst}{Flux conversion object}
#'  \item{qConst}{Flow conversion object}
#'  \item{monthInfo}{Month object}
#'}
#'
#'@aliases fluxConst qConst monthInfo concConst
#'@name Constants
#'@docType data
#'@export fluxConst qConst monthInfo concConst
#'@keywords datasets
#'@examples
#'fluxConst
#'fluxConst[['kgDay']]
#'fluxConst[['kgDay']]@unitName
#'qConst
#'qConst[['cfs']]
#'qConst[['cfs']]@qUnitName
#'concConst[['concentration']]
#'concConst[['concentration']]@shortPrefix
NULL
