#' Plot monthly trend result from runPairs
#' 
#' Plot monthly trend result from runPairs. The change in concentration
#' or flux is calculated from the \code{runPairs} function. This plotting
#' function shows an arrow for each month. If the trend from year1 to year2 
#' was increasing, the arrow is red and pointing up. If the trend was decreasing,
#' the arrow is black and pointing down.
#' 
#' @param pairResults results from \code{runPairs}.
#' @param yMax numeric. Upper limit for plot. Default is \code{NA},
#' which will use the maximum of the data.
#' @param arrowFactor numeric. Scaling factor for the size of the arrows.
#' The arrows are automatically scaled to the overall trend. This scaling 
#' factor helps adjust how big/small they are. 
#' @param flux logical. \code{TRUE} is flux, \code{FALSE} is concentration.
#' Default is \code{TRUE}.
#' @param printTitle logical variable if TRUE title is printed, if FALSE title is not printed (this is best for a multi-plot figure)
#' @return Base R plot of monthly trends
#' @param concLab object of concUnit class, or numeric represented the short code, 
#' or character representing the descriptive name.
#' @param monthLab object of monthLabel class, or numeric represented the short code, 
#' or character representing the descriptive name.
#' @export
#' @examples 
#'
#' eList <- Choptank_eList
#' year1 <- 1985
#' year2 <- 2010
#' 
#' \donttest{
#' 
#' pairOut_1 <- runPairs(eList, year1, year2, windowSide = 0)
#' 
#' plotMonthTrend(pairOut_1)
#' plotMonthTrend(pairOut_1, flux = FALSE)
#' 
#' eList <- setPA(eList, paStart = 12, paLong = 3)
#' pairOut_2 <- runPairs(eList, year1, year2, windowSide = 0)
#' 
#' plotMonthTrend(pairOut_2)
#' 
#' eList <- setPA(eList, paStart = 1, paLong = 12)
#' pairOut_3 <- runPairs(eList, year1, year2, windowSide = 0)
#' 
#' plotMonthTrend(pairOut_3)
#' 
#' }
#'  
plotMonthTrend <- function(pairResults, yMax = NA,
                    arrowFactor = 1, flux = TRUE, 
                    printTitle = TRUE,
                    concLab = 1, monthLab = 1){
  
  z <- attr(pairResults, "byMonth")
  yearPairs <- attr(pairResults, "yearPair")

  if(flux){
    z1 <- as.numeric(z[which(z$Type == "Flux" & z$Year == min(z$Year)), -1:-2])
    z2 <- as.numeric(z[which(z$Type == "Flux" & z$Year == max(z$Year)), -1:-2])
  } else {
    z1 <- as.numeric(z[which(z$Type == "Conc" & z$Year == min(z$Year)), -1:-2])
    z2 <- as.numeric(z[which(z$Type == "Conc" & z$Year == max(z$Year)), -1:-2])
  }
  
  if (is.numeric(concLab)){
    concPrefix <- concConst[shortCode=concLab][[1]]    
  } else if (is.character(concLab)){
    concPrefix <- concConst[concLab][[1]]
  } else {
    concPrefix <- concLab
  }
  
  if (is.numeric(monthLab)){
    monthInfo <- monthInfo[shortCode=monthLab][[1]]    
  } else if (is.character(monthLab)){
    monthInfo <- monthInfo[monthLab][[1]]
  } else {
    monthInfo <- monthLab
  }
  
  months <- which(!is.na(z1))
  
  monthAbb <- monthInfo@monthAbbrev

  zMax <- max(c(z1, z2), na.rm = TRUE)
  yMax <- if(is.na(yMax)) zMax * 1.05 else yMax
  
  if(flux){
    ylab <- expression("Yield, kg / month / km" ^2)
    name <- "Yield, by month"
  } else {
    ylab <- paste(concPrefix@longPrefix,
                   "in , mg / L")
    name <- paste(concPrefix@longPrefix,", by month")
  }

  season <- setSeasonLabelByUser(paStartInput = yearPairs[["paStart"]],
                                 paLongInput = yearPairs[["paLong"]], 
                                 monthLab = monthLab)
  
  title <- paste0(name, "\n",
                  attr(pairResults, "Other")[["siteName"]], "\n",
                  "From ", yearPairs[["year1"]], " to ", yearPairs[["year2"]], ", ",
                  season)
  
  if(season == "Water Year"){
    monthOrder <- c(10:12, 1:9)
    z1 <- z1[monthOrder]
    z2 <- z2[monthOrder]
    monthAbb <- monthAbb[monthOrder]
  }
  
  par(las = 1, tck = 0.02, xaxs = "i", yaxs = "i")
  plot(1:12, z1, xlim = c(0.5,12.5), ylim = c(0, yMax),
       xlab = "", ylab = "",
       main = title, col = "black", axes = FALSE, cex.lab = 0.95)
  title(ylab = ylab, line=2, cex.lab=1.2)
  axis(1, at = seq(1, 12), labels = monthAbb, tick = TRUE)
  axis(2, at = NULL, labels = TRUE, tick = TRUE)
  axis(3, at = seq(1, 12), labels = FALSE, tick = TRUE)
  axis(4, at = NULL, labels = FALSE, tick = TRUE)
  abline(h=0, col = "blue", lwd = 1)
  box()
  par(new = TRUE)
  plot(1:12, z2, xlim = c(0.5,12.5), ylim = c(0, yMax),
       xlab = "", ylab = "",
       main = "", col = "red", axes = FALSE)
  
  for(m in months){
    x0 <- m
    x1 <- m
    
    y0 <- z1[m]
    y1 <- z2[m]
    
    col <- if(y1 > y0) "red" else "black"
    length <- arrowFactor * 1 * ( abs(y1 - y0) / (max(z1, na.rm = TRUE) - 0))
    angle <- 30
    lwd <- 2
    
    graphics::arrows(x0, y0, x1, y1,
           length = length,
           angle = angle,
           col = col, lwd = lwd)
  }
}
