#' Import metadata to create INFO data frame
#'
#' Populates INFO data frame from either NWIS (\code{readNWISInfo}),
#' Water Quality Portal (\code{readWQPInfo}), or user-supplied files (\code{readUserInfo}).
#'  
#' @param siteNumber character site number.  For \code{readNWISInfo}, this is usually an 8 digit number,
#' for \code{readWQPInfo}, it is usually a longer code. For instance, a USGS site number in the Water Quality Portal
#' would be in the form `USGS-XXXXXXXX`. If the siteNumber is left blank (an empty string), the interactive
#' option allows users to enter required information by hand, otherwise those fields are left blank.
#' @param parameterCd character USGS parameter code (a 5 digit number) or characteristic name (if using \code{readWQPInfo}). If the 
#' parameterCd is left blank (an empty string), the interactive
#' option allows users to enter required information by hand, otherwise those fields are left blank.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS web service
#' @export
#' @name INFOdataframe
#' @import dataRetrieval
#' @seealso \code{\link[dataRetrieval]{readNWISsite}}, \code{\link[dataRetrieval]{readNWISpCode}}
#' @seealso \code{\link[dataRetrieval]{whatWQPsites}}
#' @return INFO data frame. Any metadata can be stored in INFO. However, there are 8 columns that EGRET uses by name in some functions:
#' 
#' \tabular{lll}{
#' Required column \tab Used in function \tab Description \cr
#' param.units*** \tab All concentration plotting functions \tab The units as listed in this field are used
#' to create the concentration axis labels \cr
#' shortName \tab All plotting functions \tab Station short name, used to label plots \cr
#' paramShortName \tab All plotting functions \tab Parameter short name, used to label plots \cr
#' drainSqKm \tab \code{plotFlowSingle}, \code{printSeries} \tab Calculate runoff\cr
#' constitAbbrev \tab \code{saveResults} \tab Parameter abbrieviation, used to auto-name workspace\cr
#' staAbbrev \tab \code{saveResults} \tab Station abbrieviation, used to auto-name workspace\cr
#' paStart \tab Most EGRET functions \tab Starting month of period of analysis. Defaults to 10\cr
#' paLong \tab Most EGRET functions \tab Length in number of months of period of analysis. Defaults to 12\cr
#' }
#' *** Additionally, EGRET assumes that all concentrations are saved in mg/l. If some variation of
#' 'mg/l' is not found in INFO$param.units, functions that calculate flux will issue a warning. This 
#' is because the conversion from mg/l to the user-specified flux unit (e.g., kg/day) uses hard-coded conversion factors.
#'
#' @examples
#' # These examples require an internet connection to run
#' # Automatically gets information about site 05114000 and temperature
#' \dontrun{
#' INFO <- readNWISInfo('05114000','00010')
#' }
readNWISInfo <- function(siteNumber, parameterCd,interactive=TRUE){
  if (nzchar(siteNumber)){
    INFO <- dataRetrieval::readNWISsite(siteNumber)
  } else {
    INFO <- as.data.frame(matrix(ncol = 2, nrow = 1))
    names(INFO) <- c('site_no', 'shortName')    
  }
  INFO <- populateSiteINFO(INFO, siteNumber,interactive=interactive)
  
  if (nzchar(parameterCd)){
    parameterData <- dataRetrieval::readNWISpCode(parameterCd=parameterCd)
    INFO$param.nm <- parameterData$parameter_nm
    INFO$param.units <- parameterData$parameter_units
    INFO$paramShortName <- parameterData$srsname
    INFO$paramNumber <- parameterData$parameter_cd
  } 
  
  INFO <- populateParameterINFO(parameterCd, INFO, interactive=interactive)
  
  localUnits <- toupper(INFO$param.units)  
  if((parameterCd != "00060" | parameterCd != "00065") & length(grep("MG/L", localUnits)) == 0){
    if(interactive){
      cat("Required concentration units are mg/l. \n")
      cat("The INFO dataframe indicates:",INFO$param.units,"\n")
      cat("Flux calculations will be wrong if units are not consistent.")
    } 
  }
  
  INFO$paStart <- 10
  INFO$paLong <- 12
  
  return(INFO)
}

#' @rdname INFOdataframe
#' @export
#' @examples
#' # These examples require an internet connection to run
#' # Automatically gets information about site 01594440 and temperature, no interaction with user
#' nameToUse <- 'Specific conductance'
#' pcodeToUse <- '00095'
#' \dontrun{
#' INFO <- readWQPInfo('USGS-04024315',pcodeToUse)
#' 
#' INFO2 <- readWQPInfo('WIDNR_WQX-10032762',nameToUse)
#' # To adjust the label names:
#' INFO$shortName <- "Little"
#' INFO$paramShortName <- "SC"
#' }
readWQPInfo <- function(siteNumber, parameterCd, interactive=TRUE){
  
  #Check for pcode:
  pCodeLogic <- (all(nchar(parameterCd) == 5) & suppressWarnings(all(!is.na(as.numeric(parameterCd)))))

  if (pCodeLogic){
    
    siteInfo <- dataRetrieval::whatWQPsites(siteid=siteNumber, pCode=parameterCd)

    parameterData <- dataRetrieval::readNWISpCode(parameterCd = parameterCd)
    
    siteInfo$param.nm <- parameterData$parameter_nm
    siteInfo$param.units <- parameterData$parameter_units
    siteInfo$paramShortName <- parameterData$srsname
    siteInfo$paramNumber <- parameterData$parameter_cd
    siteInfo$constitAbbrev <- parameterData$parameter_cd

  } else {
    siteInfo <- dataRetrieval::whatWQPsites(siteid=siteNumber, characteristicName=parameterCd)

    siteInfo$param.nm <- parameterCd
    siteInfo$param.units <- ""
    siteInfo$paramShortName <- parameterCd
    siteInfo$paramNumber <- ""
    siteInfo$constitAbbrev <- parameterCd
  }
  
  siteInfo$station.nm <- siteInfo$MonitoringLocationName
  siteInfo$shortName <- siteInfo$station.nm 
  siteInfo$site.no <- siteInfo$MonitoringLocationIdentifier
  
  if(interactive){
    cat("Your site for data is", as.character(siteInfo$site.no),".\n")
    if (!nzchar(siteInfo$station.nm)){
      cat("No station name was listed for site: ", siteInfo$site.no, ".\n")
      cat("Please enter a station name here(no quotes): \n")
      siteInfo$station.nm <- readline()
    }
    cat("Your site name is", siteInfo$station.nm,",\n")
    cat("but you can modify this to a short name in a style you prefer. \n")
    cat("This name will be used to label graphs and tables. \n")
    cat("If you want the program to use the name given above, just do a carriage return, \n")
    cat("otherwise enter the preferred short name(no quotes):\n")
    siteInfo$shortName <- readline()
    if (!nzchar(siteInfo$shortName)) siteInfo$shortName <- siteInfo$station.nm
    
    cat("Your water quality data are for parameter number", siteInfo$paramNumber, "\n")
    cat("which has the name:'", siteInfo$param.nm, "'.\n")
    cat("Typically you will want a shorter name to be used in graphs and tables. \n")
    cat("The suggested short name is:'", siteInfo$paramShortName, "'.\n")
    cat("If you would like to change the short name, enter it here, otherwise just hit enter (no quotes):")
    shortNameTemp <- readline()
    if (nchar(shortNameTemp)>0) siteInfo$paramShortName <- shortNameTemp
    if(!pCodeLogic){
      cat("Water Quality Portal does not offer a simple method to obtain unit information.\n",
          "EGRET requires concentration units in mg/l for all functions to work correctly. \nEnter the concentration units of Sample data:\n",sep="")
      siteInfo$param.units <- readline()
    }
    cat("It is helpful to set up a constiuent abbreviation when doing multi-constituent studies,\n")
    cat("enter a unique id (three or four characters should work something like tn or tp or NO3).\n")
    cat("Even if you don't feel you need an abbreviation you need to enter something (no quotes):\n")
    siteInfo$constitAbbrev <- readline()
  }
  
  if (interactive){
    cat("It is helpful to set up a station abbreviation when doing multi-site studies, \n")
    cat("enter a unique id (three or four characters should work).\n")
    cat("Even if you don't feel you need an abbreviation for your site you need to enter something(no quotes):\n")
    siteInfo$staAbbrev <- readline()
  } else {
    siteInfo$staAbbrev <- NA
  }

  if(siteInfo$DrainageAreaMeasure.MeasureUnitCode == "sq mi"){
    siteInfo$drainSqKm <- as.numeric(siteInfo$DrainageAreaMeasure.MeasureValue) * 2.5899881 
  } else {
    siteInfo$drainSqKm <- NA #Not sure the greatest solution, too many potential units.
    
  }
  
  if(interactive){
    if(is.na(siteInfo$drainSqKm)){
      cat("No drainage area (and/or units) was listed in the WQP site file for this site.\n")
      cat("Drainage area is used to calculate runoff parameters in flow history calculations.\n")
      cat("Please enter the drainage area, you can enter it in the units of your choice.\n")
      cat("Enter the area, then enter drainage area code, \n")
      cat("1 is square miles, \n")
      cat("2 is square kilometers, \n")
      cat("3 is acres, \n")
      cat("4 is hectares.\n")
      cat("Area(no quotes):\n")
      siteInfo$drain.area.va <- readline()
      siteInfo$drain.area.va <- as.numeric(siteInfo$drain.area.va)
      cat("Unit Code (1-4, no quotes):")
      qUnit <- readline()
      qUnit <- as.numeric(qUnit)
      conversionVector <- c(2.5899881, 1.0, 0.0040468564, 0.01)
      siteInfo$drainSqKm <- siteInfo$drain.area.va * conversionVector[qUnit]
    }
    requiredColumns <- c("drainSqKm", "staAbbrev", "constitAbbrev", 
                         "param.units", "paramShortName","shortName")
    if(!all(requiredColumns %in% names(siteInfo))){
      cat("The following columns are required for all functions to work in the EGRET package:\n")
      cat(requiredColumns[!(requiredColumns %in% names(siteInfo))])
      cat("Please enter manually")
    }
  }
  
  localUnits <- toupper(siteInfo$param.units)  
  if(length(grep("MG/L", localUnits)) == 0){
    if(interactive){
      cat("Required concentration units are mg/l. \n")
      cat("The INFO dataframe indicates:",siteInfo$param.units,"\n")
      cat("Flux calculations will be wrong if units are not consistent\n")
    } 
  }
  
  siteInfo$queryTime <- Sys.time()
  siteInfo$paStart <- 10
  siteInfo$paLong <- 12
  
  return(siteInfo)
}

#' @param filePath character specifying the path to the file (used in \code{readUserInfo})
#' @param fileName character name of file to open (used in \code{readUserInfo})
#' @param hasHeader logical true if the first row of data is the column headers (used in \code{readUserInfo})
#' @param separator character that separates data cells (used in \code{readUserInfo})
#' @rdname INFOdataframe
#' @export
#' @examples
#' filePath <- system.file("extdata", package="EGRET")
#' filePath <- paste(filePath,"/",sep="")
#' fileName <- 'infoTest.csv'
#' INFO <- readUserInfo(filePath,fileName, separator=",",interactive=FALSE)
readUserInfo <- function(filePath,fileName,hasHeader=TRUE,separator=",",interactive=TRUE){
  
  totalPath <- paste(filePath,fileName,sep="")
  if(file.exists(totalPath)){
    siteInfo <- read.delim(  
      totalPath, 
      header = hasHeader,
      sep=separator,
      colClasses=c('character'),
      fill = TRUE, 
      comment.char="#")
  } else {
    message("File not found, continuing with interactive section.")
    siteInfo <- data.frame(station.nm="",
                           shortName="",
                           param.nm="",
                           paramShortName="",
                           param.units="",
                           drainSqKm="",
                           stringsAsFactors=FALSE)
  }
  
  if(interactive){

    if (!("station.nm" %in% names(siteInfo))){
      cat("No station name was listed. Please enter a station name here(no quotes): \n")
      siteInfo$station.nm <- readline()
    }
    cat("Your site name is", siteInfo$station.nm,"\n")
    
    if(!("shortName" %in% names(siteInfo))){
      cat("but you can modify this to a short name in a style you prefer. \n")
      cat("The shortName name will be used to label graphs and tables. \n")
      cat("If you want the program to use the name given above, \n")
      cat("just do a carriage return, otherwise enter the preferred short name(no quotes):\n")
      siteInfo$shortName <- readline()
      if (!nzchar(siteInfo$shortName)) siteInfo$shortName <- siteInfo$station.nm
    }
    
    if (!("param.nm" %in% names(siteInfo))){
      cat("No water quality parameter name was listed.\n")
      cat("Please enter the name here(no quotes): \n")
      siteInfo$param.nm <- readline()
    }
    
    cat("Your water quality data are for '", siteInfo$param.nm, "'.\n")

    if (!("paramShortName" %in% names(siteInfo))){
      cat("Typically you will want a shorter name to be used in graphs and tables. \n")
      cat("The suggested short name is:'", siteInfo$paramShortName, "'.\n")
      cat("If you would like to change the short name, enter it here, otherwise just hit enter (no quotes):")
      shortNameTemp <- readline()
      
      if (nchar(shortNameTemp)>0) siteInfo$paramShortName <- shortNameTemp
    }
    
    if (!("param.units" %in% names(siteInfo))){
      cat("No water quality parameter unit was listed.\n")
      cat("Please enter the units here(no quotes): \n")
      siteInfo$param.units <- readline()
    }

    if (!("constitAbbrev" %in% names(siteInfo))){
      cat("It is helpful to set up a constiuent abbreviation, \n")
      cat("enter a unique id (three or four characters should work something like tn or tp or NO3).\n")
      cat("Even if you don't feel you need an abbreviation you need to enter something (no quotes):\n")
      siteInfo$constitAbbrev <- readline()
    }
    
    if (!("staAbbrev" %in% names(siteInfo))){
      cat("It is helpful to set up a station, enter a unique id (three or four characters should work).\n")
      cat("Even if you don't feel you need an abbreviation for your site you need to enter something(no quotes):\n")
      siteInfo$staAbbrev <- readline()
    }
    
    if (!("drainSqKm" %in% names(siteInfo))){
      cat("No drainage area was listed as a column named 'drainSqKm'.\n")
      cat("Drainage area is used to calculate runoff parameters in flow history calculations.\n")
      cat("Please enter the drainage area, you can enter it in the units of your choice.\n")
      cat("Enter the area, then enter drainage area code, \n")
      cat("1 is square miles, \n")
      cat("2 is square kilometers, \n")
      cat("3 is acres, \n")
      cat("4 is hectares.\n")
      cat("Area(no quotes):\n")
      siteInfo$drain.area.va <- readline()
      siteInfo$drain.area.va <- as.numeric(siteInfo$drain.area.va)
      cat("Unit Code (1-4, no quotes):")
      qUnit <- readline()
      qUnit <- as.numeric(qUnit)
      conversionVector <- c(2.5899881, 1.0, 0.0040468564, 0.01)
      siteInfo$drainSqKm <- siteInfo$drain.area.va * conversionVector[qUnit]
    }
  } else {
    requiredColumns <- c("drainSqKm", "staAbbrev", "constitAbbrev", 
                         "param.units", "paramShortName","shortName")
    if(!all(requiredColumns %in% names(siteInfo))){
      message("The following columns are expected in the EGRET package:\n")
      message(requiredColumns[!(requiredColumns %in% names(siteInfo))])
    }
  }
  
  localUnits <- toupper(siteInfo$param.units)  
  if(length(grep("MG/L", localUnits)) == 0){
    if(interactive){
      message("Required concentration units are mg/l. \nThe INFO dataframe indicates:",siteInfo$param.units,
              "\nFlux calculations will be wrong if units are not consistent")
    } 
  }
  
  namesToNum <- c("paLong", "paStart", "drainSqKm", "bottomLogQ", "stepLogQ", "stepYear","windowY","windowQ",
                  "windowS","dec.lat.va","dec.long.va","drain.area.va")
  namesToNum <- namesToNum[which(namesToNum %in% names(siteInfo))]
  
  namesToInt <- c("nVectorYear","minNumObs","minNumUncen")
  namesToInt <- namesToInt[which(namesToInt %in% names(siteInfo))]
  
  if(length(namesToNum) > 0){
    siteInfo[,namesToNum] <- as.numeric(siteInfo[,namesToNum])
  }
  
  if(length(namesToInt) > 0){
    siteInfo[,namesToNum] <- as.numeric(siteInfo[,namesToNum])
  }
    
  siteInfo$queryTime <- Sys.time()
  if(!("paStart" %in% names(siteInfo))){
    siteInfo$paStart <- 10
  }
  if(!("paLong" %in% names(siteInfo))){
    siteInfo$paLong <- 12
  }
  return(siteInfo)
}

