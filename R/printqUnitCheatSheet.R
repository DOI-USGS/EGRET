#' Available Flow Unit properties
#'
#' Cheat sheet to print out pre-defined qUnit properties from qUnit class.
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
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' printqUnitCheatSheet()
printqUnitCheatSheet <- function(){
  cat("The following codes apply to the qUnit list:\n")
  numObjects <- 4
  #   numObjects <- length(qConst)
  qUnitNameList <- sapply(c(1:numObjects), function(x){qConst[[x]]@qUnitName})
  qShortCodeList <- sapply(c(1:numObjects), function(x){qConst[[x]]@shortCode})
  qNamesList <- names(qConst)
  for (i in 1:numObjects){
    cat(qShortCodeList[i],"= ", qNamesList[i], " (", qUnitNameList[i], ")\n")
  }
}