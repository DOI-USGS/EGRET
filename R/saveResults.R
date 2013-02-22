#'  A utility program for saving the contents of the workspace
#'   
#'  This function saves the workspace. 
#'  It assigns the file a name using the abbreviations for station and constituent.
#'
#' @param savePath string specifying the full pathname of the folder where the file is to be saved ending with the final slash
#' @param localINFO string specifying the name of the INFO database
#' @keywords water-quality statistics
#' @export
#' @examples
#' INFO <- exINFO
#' \dontrun{saveResults("")}
saveResults<-function(savePath, localINFO =INFO){
  saveName <- paste(savePath, localINFO$staAbbrev, ".", localINFO$constitAbbrev, 
                    ".RData", sep = "")
  save.image(file=saveName)
}