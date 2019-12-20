# this is written for kg/month/km^2
# the first three lines need to be adjusted to fit the results
# don't change anything else
# however, if you want some different flux units
# you will need to go into the code and make the changes to do that
plotOne <- function(z, yMax = NA, arrowFactor = 0.4, 
                    name = "Change in yield, by month"){
zMax <- max(z)
yMax <- if(is.na(yMax)) zMax * 1.05 else yMax
monthAbb <- c("J","F","M","A","M","J","J","A","S","O","N","D")
par(las = 1, tck = 0.02, xaxs = "i", yaxs = "i")
plot(seq(1,12),z[1,],xlim = c(0.5,12.5), ylim = c(0, yMax),
      xlab = "", ylab = "Change in yield, kg / month / km^2",
     main = name, col = "black", axes = FALSE, cex.lab = 0.95)
axis(1, at = seq(1, 12), labels = monthAbb, tick = TRUE)
axis(2, at = NULL, labels = TRUE, tick = TRUE)
axis(3, at = seq(1, 12), labels = FALSE, tick = TRUE)
axis(4, at = NULL, labels = FALSE, tick = TRUE)
box()
par(new = TRUE)
plot(seq(1,12),z[2,],xlim = c(0.5,12.5), ylim = c(0, yMax),
     xlab = "", ylab = "",
     main = "", col = "red", axes = FALSE)
for(m in 1: 12){
  x0 <- m
  x1 <- m
  y0 <- z[1,m]
  y1 <- z[2,m]
  col <- if(y1 > y0) "red" else "black"
  length <- arrowFactor * abs(y1 - y0) 
  angle <- 30
  lwd <- 2
  arrows(x0, y0, x1, y1, length = length, angle = angle, col = col, lwd = lwd)
}
}
