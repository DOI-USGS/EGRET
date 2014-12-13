#' Populate Site Information Columns
#'
#' Populates INFO data frame with additional user-supplied information. Also removes fields not related to WRTDS study.
#'
#' @param INFO dataframe with value and code columns
#' @param siteNumber character USGS site number
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @return INFO dataframe
#' @export
#' @examples
#' \dontrun{
#' library(dataRetrieval)
#' INFO <- readNWISsite('01594440')
#' siteNumber <- "01594440"
#' siteINFO <- populateSiteINFO(INFO, siteNumber)
#' }
populateSiteINFO <- function(INFO, siteNumber,interactive=TRUE){
  if (nzchar(siteNumber)){
    
    if (!nzchar(INFO$site_no)) {
      INFO$site_no <- siteNumber
    }
    
    if (interactive){
      cat("Your site for streamflow data is:\n", as.character(INFO$site_no),".\n")
      if (!nzchar(INFO$station_nm)){
        cat("No station name was listed in the USGS site file for site:\n", INFO$site_no, "\nPlease enter a station name here(no quotes): \n")
        INFO$station_nm <- readline()
      }
      cat("Your site name is", INFO$station_nm,"\n")
      cat("but you can modify this to a short name in a style you prefer.","\n")
      cat("This name will be used to label graphs and tables.","\n")
      cat("If you want the program to use the name given above, just do a carriage return,\notherwise enter the preferred short name(no quotes):\n")
      INFO$shortName <- readline()
      if (!nzchar(INFO$shortName)) INFO$shortName <- INFO$station_nm
      if (!nzchar(INFO$dec_lat_va) || !nzchar(INFO$dec_long_va)){
        cat("No latitude or longitude was listed in the USGS site file for this site.\n")
        cat("Please enter a latitude and longitude in decimal degrees, positive latitudes are north, negative are south, positive longitudes are east, \nnegative longitudes are west, so for example a site in the northeastern US might look like, 40.1, -83.2\nThese only need to be sufficiently accurate to place them on a map of the study area.\n\n")
        cat("Latitude(no quotes):\n")
        INFO$dec_lat_va <- readline()
        cat("Longitude(no quotes):\n")
        INFO$dec_long_va <- readline()
      }
      cat("The latitude and longitude of the site are: ",INFO$dec_lat_va, ", ", INFO$dec_long_va, "(degrees north and west).\n\n")
      if (!nzchar(INFO$drain_area_va)){
        cat("No drainage area was listed in the USGS site file for this site.\n")
        cat("Please enter the drainage area, you can enter it in the units of your choice.","\n")
        cat("Enter the area, then enter drainage area code,","\n")
        cat("1 is square miles","\n")
        cat("2 is square kilometers","\n")
        cat("3 is acres\n4 is hectares.","\n")
        cat("Area(no quotes):\n")
        INFO$drain_area_va <- readline()
        INFO$drain_area_va <- as.numeric(INFO$drain_area_va)
        cat("Unit Code (1-4, no quotes):")
        qUnit <- readline()
        qUnit <- as.numeric(qUnit)
        conversionVector <- c(2.5899881, 1.0, 0.0040468564, 0.01)
        INFO$drainSqKm <- INFO$drain_area_va * conversionVector[qUnit]
      } else {
        INFO$drain_area_va <- as.numeric(INFO$drain_area_va)
        INFO$contrib_drain_area_va <- as.numeric(INFO$contrib_drain_area_va)
        INFO$drainSqKm <- INFO$drain_area_va * 2.5899881
      }    
      cat("The drainage area at this site is ", INFO$drain_area_va, "square miles\n which is being stored as", INFO$drainSqKm, "square kilometers.\n\n")    
    } else {
      INFO$drain_area_va <- as.numeric(INFO$drain_area_va)
      INFO$contrib_drain_area_va <- as.numeric(INFO$contrib_drain_area_va)
      INFO$drainSqKm <- INFO$drain_area_va * 2.5899881      
      INFO$shortName <- INFO$station_nm
    }    
  } else {
    if (interactive){
#       cat("The program needs to know a site number or id, please enter that here (don't use quotes) - Enter to leave blank:")
#       INFO$site_no <- readline()
      cat("Please enter a site name that will be used to label all graphs and tables(no quotes):\n")
      INFO$shortName <- readline()
#       cat("Please enter a latitude and longitude in decimal degrees, positive latitudes are north, negative are south, positive longitudes are east, \nnegative longitudes are west, so for example a site in the northeastern US might look like, 40.1, -83.2\nThese only need to be sufficiently accurate to place them on a map of the study area.\n\n")
#       cat("Latitude(no quotes):\n")
#       INFO$dec_lat_va <- readline()
#       cat("Longitude(no quotes):\n")
#       INFO$dec_long_va <- readline()
#       INFO$dec_lat_va <- as.numeric(INFO$dec_lat_va)
#       INFO$dec_long_va <- as.numeric(INFO$dec_long_va)
      cat("Please enter the drainage area, you can enter it in the units of your choice.\n")
      cat("Enter the area, then enter drainage area code, \n")
      cat("1 is square miles, \n")
      cat("2 is square kilometers, \n")
      cat("3 is acres, \n")
      cat("4 is hectares.\n")
      cat("Area(no quotes):\n")
      INFO$drain_area_va <- readline()
      INFO$drain_area_va <- as.numeric(INFO$drain_area_va)
      cat("Unit Code (1-4, no quotes)\nrepresenting \n1: sq mi \n2: sq km \n3: sq m\n4: sq 100*km):")
      qUnit <- readline()
      qUnit <- as.numeric(qUnit)
      conversionVector <- c(2.5899881, 1.0, 0.0040468564, 0.01)
      INFO$drainSqKm <- INFO$drain_area_va * conversionVector[qUnit]
      cat("The drainage area is being stored as", INFO$drainSqKm, "square kilometers.\n")
    } else {
#       INFO$site_no <- NA
      INFO$shortName <- NA
#       INFO$dec_lat_va <- NA
#       INFO$dec_long_va <- NA
      INFO$drain_area_va <- NA
      INFO$drainSqKm <- NA
    }
  }
  if (interactive){
    cat("It is helpful to set up a station abbreviation when doing multi-site studies,\n")
    cat("enter a unique id (three or four characters should work). It is case sensitive.  \n")
    cat("Even if you don't feel you need an abbreviation for your site you need to enter something(no quotes):\n")
    INFO$staAbbrev <- readline()
  } else {
    INFO$staAbbrev <- NA
  }
  return(INFO)  
}
