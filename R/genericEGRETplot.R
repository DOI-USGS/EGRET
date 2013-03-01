genericEGRETDotPlot <- function(x,y, axes = FALSE, printTitle=TRUE,
                                xlim,xaxs="i",xlab,ylim,yaxs="i",ylab,plotTitle="",
                                pch=20,cex=0.7,cex.main=1.3,font.main=2,cex.lab=1.2,
                                tcl=0.5,xTicks,yTicks,
                                tinyPlot=FALSE,hLine=FALSE,oneToOneLine=FALSE, ...){
  
  if(tinyPlot) par(mar=c(5,4,1,1)) else par(mar=c(5,4,4,2)+0.1)
  plot(x,y,axes=axes,xlim=xlim,xaxs=xaxs,xlab=xlab,
       ylim=ylim,yaxs=yaxs,ylab=ylab,main=plotTitle,
       pch=pch,cex=cex,cex.main=cex.main,font.main=font.main,cex.lab=cex.lab)
  
  plot(x,y,axes=axes)
  box()
  if (hLine) abline(h = 0)
  if (oneToOneLine) abline(a=0,b=1)
  axis(1,tcl=tcl,at=xTicks,labels=xTicks)
  axis(2,tcl=tcl,las=1,at=yTicks,labels=yTicks)
  axis(3,tcl=tcl,at=xTicks,labels=FALSE)
  axis(4,tcl=tcl,at=yTicks,labels=FALSE)
  par(mar=c(5,4,4,2)+0.1)
}

