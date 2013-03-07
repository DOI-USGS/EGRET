dischargeLogAxis <- function(x,tinyPlot,qUnit,padPercent=5){
  
  upperMagnification <- 1+(padPercent/100)
  lowerMagnification <- 1-(padPercent/100)
  
  xMin <- lowerMagnification*min(x)
  xMax <- upperMagnification*max(x)

  xTicks <- if(tinyPlot) {
    logPretty1(xMin,xMax) 
  } else {
    logPretty3(xMin,xMax)
  }

  numXTicks<-length(xTicks)
  xLeft<-xTicks[1]
  xRight<-xTicks[numXTicks]
  xLab<-qUnit@qUnitExpress
  
  return(list(xTicks=xTicks,xLeft=xLeft,xRight=xRight,xLab=xLab))
}