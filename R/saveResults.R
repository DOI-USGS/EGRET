#'  A utility program for saving the contents of the workspace
#'   
#'  This function saves the workspace. 
#'  It assigns the file a name using the abbreviations for station and constituent.
#'
#' @param savePath character specifying the full pathname of the folder where the file is to be saved ending with the final slash
#' @param eList named list with at least the INFO dataframe
#' @keywords water-quality statistics
#' @export
#' @examples
#' eList <- Choptank_eList
#' savePath <- "~/"
#' \dontrun{saveResults(savePath, eList)
#' 
#' #To load:
#' load(paste(savePath,"Chop.nitrogen.RData",sep=""))}
saveResults<-function(savePath, eList){
  INFO <- getInfo(eList)
  saveName <- paste(savePath, INFO$shortName, ".", INFO$constitAbbrev, 
                    ".RData", sep = "")
  save.image(file=saveName)
}
