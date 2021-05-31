#'  A utility program for saving the contents of the workspace
#'   
#'  This function saves the workspace. Future versions of EGRET will not include this function,
#'  use saveRDS to save individual eList objects.
#'  It assigns the file a name using the abbreviations for station and constituent.
#'
#' @param savePath character specifying the full pathname of the folder where the file is to be saved ending with the final slash
#' @param eList named list with at least the INFO dataframe
#' @keywords water-quality statistics
#' @export
saveResults<-function(savePath, eList){
  .Deprecated("base R's saveRDS")
  INFO <- getInfo(eList)
  saveName <- paste0(savePath, INFO$shortName, ".", INFO$constitAbbrev, 
                    ".RData")
  save.image(file=saveName)
}
