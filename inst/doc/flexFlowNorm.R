## ----setup, include=FALSE, message=FALSE------------------
library(rmarkdown)
options(continue=" ")
options(width=60)
library(knitr)
library(EGRET)



## ---------------------------------------------------------
library(EGRET)
eList <- Choptank_eList
eList <- setUpEstimation(eList)

nSegments <- 2
segStart <- c(1985,2000) #Sample start year for segments
segEnd <- c(2001,2010) #Sample end years for segments
dStart <- as.Date(c("1988-10-01","2002-05-15")) #Flow starting days
dEnd <- as.Date(c("1998-09-30","2009-09-30")) #Flow ending days
eList <- flexFN(eList,nSegments,segStart,segEnd,dStart,dEnd)


## ----fig.height=6, fig.width=8----------------------------
library(lubridate)
plotFluxHist(eList)

if('segmentInfo' %in% names(attributes(eList$INFO))){
  segmentINFO <- attr(eList$INFO, "segmentInfo")
  
  for(i in 1:length(segmentINFO$dStart)){
    rect(decimal_date(segmentINFO$dStart)[i], par()$usr[3], 
         decimal_date(segmentINFO$dEnd)[i], par()$usr[4],
         col= "#FF003322")    
  }
  
  abline(v=segmentINFO$segStart, col="blue")
  abline(v=segmentINFO$segEnd-1, col="blue")

  legend("topleft", legend=c("Sample segment limits",
                             "Flow segment limits"),lwd=c(1,0),
         col=c("blue","#FF003322"),lty=c(1,0),pch=c(NA,15))
}



