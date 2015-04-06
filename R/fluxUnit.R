#' fluxUnit class
#'
#' Some details about the fluxUnit class
#'
#' \describe{
#'    \item{shortName}{A character specifying the short name.}
#'
#'    \item{unitFactor}{A numeric representing the conversion factor}
#' 
#'    \item{unitName}{A character specifying the full name.}
#' 
#'    \item{unitExpress}{An expression specifying the full name starting with Observed.}
#'    
#'    \item{unitExpressTiny}{An expression specifying the abbreviated name starting with Observed.}
#'
#'    \item{unitEstimate}{An expression specifying the full name starting with Estimated.}
#'    
#'    \item{unitEstimateTiny}{An expression specifying the abbreviated name starting with Estimated.}
#'    
#'    \item{unitUSGS}{A character specifying flux with full text.}
#'
#'    \item{shortCode}{A number for quick lookup}
#'  }
#' @name fluxUnit-class
#' @rdname fluxUnit-class
#' @exportClass fluxUnit
setClass("fluxUnit",
	representation(
		shortName    = "character",
		unitFactor   = "numeric",
		unitName     = "character",
    unitExpress  = "expression",
		unitExpressTiny = "expression",
    unitEstimate = "expression",
		unitEstimateTiny = "expression",
		unitUSGS = "character",
    shortCode    = "numeric"
	)	
)

#' qUnit class
#'
#' Some details about the qUnit class
#'
#' \describe{
#'    \item{qshortName}{A character specifying the short name.}
#'
#'    \item{qUnitFactor}{A numeric representing the conversion factor}
#' 
#'    \item{qUnitName}{A character specifying the full name.}
#' 
#'    \item{qUnitExpress}{An expression specifying the full name.}
#'    
#'    \item{qUnitTiny}{An expression specifying the abbreviated name.}
#'
#'    \item{shortCode}{A number for quick lookup}
#'  }
#' @name qUnit-class
#' @rdname qUnit-class
#' @exportClass qUnit
setClass("qUnit",
  representation(
		qShortName   = "character",
		qUnitFactor  = "numeric",
		qUnitName    = "character",
    qUnitExpress = "expression",
    qUnitTiny = "expression",
    shortCode    = "numeric"
	)	
)

#' monthLabel class
#'
#' Some details about the monthLabel class
#'
#' \describe{
#'    \item{monthAbbrev}{A character specifying the abbreviated month name.}
#'
#'    \item{monthFull}{A character specifying the full month name}
#' 
#'    \item{monthSingle}{A character specifying the single letter of the month.}
#'  }
#' @name monthLabel-class
#' @rdname monthLabel-class
#' @exportClass monthLabel
setClass("monthLabel",
  representation(
  	monthAbbrev  = "character",
		monthFull    = "character",
		monthSingle  = "character"
	)	
)
