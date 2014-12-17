#' Import Metadata for USGS Data
#'
#' Populates INFO data frame for EGRET study.  If either station number or parameter code supplied, imports data about a particular USGS site from NWIS web service. 
#' This function gets the data from here: \url{http://waterservices.usgs.gov/}
#' If either station number or parameter code is not supplied, the user will be asked to input data.
#' Additionally, the user will be asked for:
#' staAbbrev - station abbreviation, will be used in naming output files and for structuring batch jobs
#' constitAbbrev - constitute abbreviation
#'
#' @param siteNumber character USGS site number.  This is usually an 8 digit number
#' @param parameterCd character USGS parameter code.  This is usually an 5 digit number.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS web service
#' @export
#' @import dataRetrieval
#' @return INFO dataframe with at least param.nm, param.units, parameShortName, paramNumber
#' @examples
#' # These examples require an internet connection to run
#' # Automatically gets information about site 05114000 and temperature, no interaction with user
#' INFO <- readNWISInfo('05114000','00010',interactive=FALSE)
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
      cat("Expected concentration units are mg/l. \n")
      cat("The INFO dataframe indicates:",INFO$param.units,"\n")
      cat("Flux calculations will be wrong if units are not consistent.")
    } 
  }
  
  INFO$paStart <- 10
  INFO$paLong <- 12
  
  return(INFO)
}

#' Import Metadata for Water Quality Portal Data
#'
#' Populates INFO data frame for EGRET study. If siteNumber or parameter code (for USGS) or characteristic name 
#' (for non-USGS) is provided, the function will make a call to the Water Quality Portal to get metadata information.
#' staAbbrev - station abbreviation, will be used in naming output files and for structuring batch jobs
#' constitAbbrev - constitute abbreviation
#'
#' @param siteNumber character site number. 
#' @param parameterCd character USGS parameter code or characteristic name.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS web service WRTDS
#' @export
#' @import dataRetrieval
#' @return INFO dataframe with agency, site, dateTime, value, and code columns
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
    cat("Water Quality Portal does not offer a simple method to obtain unit information.\n",
        "EGRET expects concentration units in mg/l. \nEnter the concetration units of Sample data:\n",sep="")
    siteInfo$param.units <- readline()
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
  }
  
  if(interactive){
    if(!("drainSqKm" %in% names(siteInfo))){
      cat("No drainage area (and/or units) was listed in the WQP site file for this site.\n")
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
  } 
  
#  else {
#     warning("Please check the units for drainage area.\n The value for INFO$drainSqKm needs to be in square kilometers")
#     siteInfo$drainSqKm <- as.numeric(siteInfo$DrainageAreaMeasure.MeasureValue)
#   }
  
  localUnits <- toupper(siteInfo$param.units)  
  if(length(grep("MG/L", localUnits)) == 0){
    if(interactive){
      cat("Expected concentration units are mg/l. \n")
      cat("The INFO dataframe indicates:",siteInfo$param.units,"\n")
      cat("Flux calculations will be wrong if units are not consistent\n")
    } 
  }
  
  siteInfo$queryTime <- Sys.time()
  siteInfo$paStart <- 10
  siteInfo$paLong <- 12
  
  return(siteInfo)
}



#' Import Metadata from User-Generated File
#'
#' Populates INFO data frame for EGRET study. Accepts a user generated file with any metadata that might 
#' be important for the analysis. 
#' Additionally, EGRET analysis requires:"drainSqKm", "staAbbrev", "constitAbbrev", 
#' "param.units", "paramShortName","shortName". If interactive=TRUE, the function will ask for these
#' fields if they aren't supplied in the file.
#'
#' @param filePath character specifying the path to the file
#' @param fileName character name of file to open
#' @param hasHeader logical true if the first row of data is the column headers
#' @param separator character character that separates data cells
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS web service WRTDS
#' @export
#' @return INFO dataframe with agency, site, dateTime, value, and code columns
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
      message("Expected concentration units are mg/l. \nThe INFO dataframe indicates:",siteInfo$param.units,
              "\nFlux calculations will be wrong if units are not consistent")
    } 
  }
  
  siteInfo$queryTime <- Sys.time()
  siteInfo$paStart <- 10
  siteInfo$paLong <- 12
  
  return(siteInfo)
}

