#' Available Flux Unit properties
#'
#' Cheat sheet to print out pre-defined flux unit properties from fluxUnit class
#'
#' @keywords graphics water-quality statistics
#' @export
#' @examples
#' printFluxUnitCheatSheet()
printFluxUnitCheatSheet <- function(){
  cat("The following codes apply to the fluxUnit list:\n")
  numObects <- length(fluxConst)
  fluxUnitNameList <- sapply(c(1:numObects), function(x){fluxConst[[x]]@unitName})
  fluxShortCodeList <- sapply(c(1:numObects), function(x){fluxConst[[x]]@shortCode})
  fluxNamesList <- names(fluxConst)
  for (i in 1:numObects){
    cat(fluxShortCodeList[i],"= ", fluxNamesList[i], " (", fluxUnitNameList[i], ")\n")
  }
}