#' Populate Daily data frame
#'
#' Using raw data that has at least dateTime, value, code, populates the rest of the basic Daily data frame used in WRTDS
#'
#' @param rawData dataframe contains at least dateTime, value, code columns
#' @param qConvert character conversion to cubic meters per second
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords WRTDS flow
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @return A data frame 'Daily' with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' Q \tab numeric \tab Discharge in m^3/s\cr
#' Julian \tab integer \tab Number of days since Jan. 1, 1850\cr
#' Month \tab integer \tab Month of the year [1-12] \cr 
#' Day \tab integer \tab Day of the year [1-366] \cr
#' DecYear \tab numeric \tab Decimal year \cr
#' MonthSeq \tab integer \tab Number of months since January 1, 1850 \cr
#' Qualifier \tab character \tab Qualifying code \cr
#' i \tab integer \tab Index of days, starting with 1 \cr
#' LogQ \tab numeric \tab Natural logarithm of Q  \cr
#' Q7 \tab numeric \tab 7 day running average of Q \cr
#' Q30 \tab numeric \tab 30 day running average of Q \cr
#' }
#' @importFrom stats filter
#' @seealso \code{\link{readNWISDaily}}, \code{\link{readUserDaily}}
#' @export
#' @examples
#' dateTime <- as.character(seq(as.Date("2001/1/1"), 
#'          as.Date("2001/12/31"), by = "day"))
#' value <- 1:365
#' code <- rep("",365)
#' dataInput <- data.frame(dateTime, value, code, stringsAsFactors=FALSE)
#' Daily <- populateDaily(dataInput, 2)
populateDaily <- function(rawData,qConvert,interactive=TRUE){  # rawData is a dataframe with at least dateTime, value, code

  
  localDaily <- as.data.frame(matrix(ncol=2,nrow=length(rawData$value)))
  colnames(localDaily) <- c('Date','Q')
  localDaily$Date <- rawData$dateTime
  
  # need to convert to cubic meters per second to store the values
  localDaily$Q <- rawData$value/qConvert
  
  dateFrame <- populateDateColumns(rawData$dateTime)
  localDaily <- cbind(localDaily, dateFrame[,-1])
  
  localDaily$Date <- as.Date(localDaily$Date)
  
  if(length(rawData$code) != 0) localDaily$Qualifier <- rawData$code
  
  localDaily$i <- 1:nrow(localDaily)
  
  noDataValue <- -999999
  
  nd <- localDaily$Q==noDataValue
  
  localDaily$Q<-ifelse(nd,NA,localDaily$Q)
  
  zeros<-which(localDaily$Q<=0)
  
  nz<-length(zeros)
  
  if(nz>0) {

    qshift<- 0.001*mean(localDaily$Q, na.rm=TRUE) 
    if (interactive){
      
      zeroNums <- length(which(localDaily$Q == 0))

      if (zeroNums > 0){
        cat("There were", as.character(zeroNums), "zero flow days \n")
      }
      
      cat("All days had",as.character(qshift),"cms added to the discharge value.\n")
      
    }
  } else {
    qshift<-0.0
  }
  
  negNums <- length(which(localDaily$Q<0))
  if (negNums > 0) {
    cat("There were", as.character(negNums), "negative flow days \n")
    cat("Negative values are not supported in the EGRET package\n")
  }
  
  localDaily$Q<-localDaily$Q+qshift
  
  localDaily$LogQ <- log(localDaily$Q)
  
#   Qzoo<-zoo(localDaily$Q)
  
  if (length(rawData$dateTime) < 30){
    if (interactive){
      cat("This program requires at least 30 data points. You have only provided:", length(rawData$dateTime),"Rolling means will not be calculated.\n")
    }
    warning("This program requires at least 30 data points. Rolling means will not be calculated.")
  } else {
    ma <- function(x,n=7){filter(x,rep(1/n,n), sides=1)}
    
#     localDaily$Q7<-as.numeric(rollapply(Qzoo,7,mean,na.rm=FALSE,fill=NA,align="right"))
    localDaily$Q7 <- as.numeric(ma(localDaily$Q))
#     localDaily$Q30<-as.numeric(rollapply(Qzoo,30,mean,na.rm=FALSE,fill=NA,align="right"))
    localDaily$Q30 <- as.numeric(ma(localDaily$Q,30))
  }
  
  dataPoints <- nrow(localDaily)
  difference <- (localDaily$Julian[dataPoints] - localDaily$Julian[1])+1  
  if (interactive){
    cat("There are", as.character(dataPoints), "data points, and", as.character(difference), "days.\n")

    #these next two lines show the user where the gaps in the data are if there are any
    n<-nrow(localDaily)
    for(i in 2:n) {
      if((localDaily$Julian[i]-localDaily$Julian[i-1])>1) cat("\n discharge data jumps from",as.character(localDaily$Date[i-1]),"to",as.character(localDaily$Date[i]))
    }
    
    numNAs <- sum(is.na(localDaily$Q))
    if(numNAs > 0){
      cat(numNAs, "discharge values are not reported (NA's). \nMany of the EGRET functions will not work with missing discharge values.")
      if (localDaily$Julian[max(which(is.na(localDaily$Q)),na.rm = TRUE)]-
           localDaily$Julian[min(which(is.na(localDaily$Q)),na.rm = TRUE)]+1 ==  numNAs){
        cat("\nNA gap is from",as.character(localDaily$Date[min(which(is.na(localDaily$Q)),na.rm = TRUE)]),"to",
            as.character(localDaily$Date[max(which(is.na(localDaily$Q)),na.rm = TRUE)]))
      } 
    }
    
  }
  
  return (localDaily)  
}
