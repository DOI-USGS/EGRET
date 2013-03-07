concentrationAxis <- function(ConcLow, ConcHigh, ConcAve, concMax, concMin,padPercent=5){
  
  upperMagnification <- 1+(padPercent/100)
  lowerMagnification <- 1-(padPercent/100)
  
  yLow <- ConcLow
  yHigh <- ConcHigh
  maxYHigh <- if(is.na(concMax)) upperMagnification*max(yHigh) else concMax
  minYLow <- if(is.na(concMin)) lowerMagnification*min(ConcAve) else concMin
  yTicks <- logPretty3(minYLow,maxYHigh)
  yBottom <- yTicks[1]
  yTop <- yTicks[length(yTicks)]
  return(list(yTop=yTop,yBottom=yBottom,yTicks=yTicks))

}