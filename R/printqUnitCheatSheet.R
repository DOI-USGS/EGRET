#' Available Flow Unit properties
#'
#' Cheat sheet to print out pre-defined qUnit properties from qUnit class.
#'
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' printqUnitCheatSheet()
printqUnitCheatSheet <- function(){
  cat("The following codes apply to the qUnit list:\n")
  numObects <- length(qConst)
  qUnitNameList <- sapply(c(1:numObects), function(x){qConst[[x]]@qUnitName})
  qShortCodeList <- sapply(c(1:numObects), function(x){qConst[[x]]@shortCode})
  qNamesList <- names(qConst)
  for (i in 1:numObects){
    cat(qShortCodeList[i],"= ", qNamesList[i], " (", qUnitNameList[i], ")\n")
  }
}