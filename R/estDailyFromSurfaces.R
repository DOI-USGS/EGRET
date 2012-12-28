estDailyFromSurfacesOrig<-function(localDaily = Daily, localINFO = INFO, localsurfaces = surfaces) {
  # this function uses the surfaces that have been calulated based on the sample data
  # and fills in the individual estimates using bilinear interpolation off these surfaces
  # it produces estimates of ConcDay, FluxDay, FNConc, and FNFlux
  # these are appended to the data frame Daily, which is returned
  #
  numDays<-length(localDaily$LogQ)
  # need to make sure that localDaily$i starts with 1 (in case user subsetted the data frame)   
  localDaily$i<-seq(1,numDays)
  # we need to add an extra column to Daily to handle leap year for flow normalization
  # the 366'th day of the year is treated as an extra day 365
  localDaily$Leap<-ifelse(localDaily$Day<365,localDaily$Day,365)
  # set up the grid values
  bottomLogQ<-localINFO$bottomLogQ
  stepLogQ<-localINFO$stepLogQ
  bottomYear<-localINFO$bottomYear
  stepYear<-localINFO$stepYear
  t0<-trunc((localDaily$DecYear-bottomYear)/stepYear)+1
  t1<-t0+1
  tat0<-((t0-1)*stepYear)+bottomYear
  tf<-(localDaily$DecYear - tat0)/stepYear
  q0<-trunc((localDaily$LogQ - bottomLogQ)/stepLogQ)+1
  q1<-q0+1
  qat0<-((q0-1)*stepLogQ)+bottomLogQ
  qf<-(localDaily$LogQ - qat0)/stepLogQ
  result<-array(0,dim=c(numDays,3))
  for(i in 1:numDays) {
    for(j in 1:3){
      f00<-localsurfaces[q0[i],t0[i],j]
      f01<-localsurfaces[q1[i],t0[i],j]
      f10<-localsurfaces[q0[i],t1[i],j]
      f11<-localsurfaces[q1[i],t1[i],j]
      b1<-f00
      b2<-f01-f00
      b3<-f10-f00
      b4<-f00-f10-f01+f11
      result[i,j]<-b1+b2*qf[i]+b3*tf[i]+b4*qf[i]*tf[i]
    }
  }
  localDaily$yHat<-result[,1]
  localDaily$SE<-result[,2]
  localDaily$ConcDay<-result[,3]
  localDaily$FluxDay<-result[,3]*localDaily$Q*86.40
  # next is flow normalization
  cat("\n flow normalization starting, when the numbers reach 365 it is done")
  # first we loop through the first 365 days of they year, organizing a data frame of all days
  # that are on that day of the year, the day of the iyear index is iday and the data frame is Daily1
  #
  # set up the columns for flow normalized results
  localDaily$FNConc<-array(0,numDays)
  localDaily$FNFlux<-array(0,numDays)
  for(iday in 1:365) {
    cat(" ",iday)
    Daily1<-subset(localDaily,Leap==iday)
    numDDay<-length(Daily1$Day)
    q0<-trunc((Daily1$LogQ - bottomLogQ)/stepLogQ)+1
    q1<-q0+1
    qat0<-((q0-1)*stepLogQ)+bottomLogQ
    qf<-(Daily1$LogQ - qat0)/stepLogQ
    vectorInd<-Daily1$i
    # vectorInd is a vector of indices for each of the rows in Daily1, pointing back to the index value (row number)
    # in the orginial data frame, Daily
    #   jday is the day for which we are making the flow normalized estimate		
    for(jday in 1:numDDay) {
      # note that for any date, the t variables are now all scalars and not vectors
      # time is fixed but the discharges cover the range of historical discharges that happened on
      #    this day in all years
      Year<-Daily1$DecYear[jday]
      t0<-trunc((Year-bottomYear)/stepYear)+1
      t1<-t0+1
      tat0<-((t0-1)*stepYear)+bottomYear
      tf<-(Year-tat0)/stepYear
      #   start summing for Flow Normalized Concentration and Flux
      sumConc<-0
      sumFlux<-0
      # we sum over all of the discharge values that have been observed on this day
      #   ii is the counter of these historical days we are using to form the flow normalized value
      #   for the day jday (which is also day number vectorInd[jday] in the original Daily data frame)			
      for(ii in 1:numDDay){
        f00<-localsurfaces[q0[ii],t0,3]
        f01<-localsurfaces[q1[ii],t0,3]
        f10<-localsurfaces[q0[ii],t1,3]
        f11<-localsurfaces[q1[ii],t1,3]
        b1<-f00
        b2<-f01-f00
        b3<-f10-f00
        b4<-f00-f10-f01+f11
        conc<-b1+b2*qf[ii]+b3*tf+b4*qf[ii]*tf
        sumConc<-sumConc+conc
        sumFlux<-sumFlux+conc*Daily1$Q[ii]*86.4
      }
      localDaily$FNConc[vectorInd[jday]]<-sumConc/numDDay
      localDaily$FNFlux[vectorInd[jday]]<-sumFlux/numDDay
    }
  }	
  return(localDaily)
}