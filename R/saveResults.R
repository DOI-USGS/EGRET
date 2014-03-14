#'  A utility program for saving the contents of the workspace
#'   
#'  This function saves the workspace. 
#'  It assigns the file a name using the abbreviations for station and constituent.
#'
#' @param savePath string specifying the full pathname of the folder where the file is to be saved ending with the final slash
#' @param localINFO data frame that contains the metadata, default is INFO
#' @keywords water-quality statistics
#' @export
#' @examples
#' INFO <- ChopINFO
#' INFO$staAbbrev <- "Chop"
#' INFO$constitAbbrev <- "nitrogen"
#' Daily <- ChopDaily
#' Sample <- ChopSample
#' surfaces <- exsurfaces
#' savePath <- "~/RData/"
#' saveResults(savePath)
#' 
#' #To load:
#' load(paste(savePath,"Chop.nitrogen.RData",sep=""))
saveResults<-function(savePath, localINFO =INFO){
  saveName <- paste(savePath, localINFO$staAbbrev, ".", localINFO$constitAbbrev, 
                    ".RData", sep = "")
  save.image(file=saveName)
}