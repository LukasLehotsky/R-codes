### ### ### ###
#
# Lukas Lehotsky
# 21-12-2017
#
### ### ### ###

### ### ### ###
#
# functions and variables
#
### ### ### ###

require(fGarch)

x=seq(-5,5,length=500)

lskw <- rnorm(x, mean = 0, sd = 1.1)

mskw <- rsnorm(x,mean = 0.75, sd = 2, xi =4)

lskw.dens <- density(lskw,bw = 1)

mskw.dens <- density(mskw, bw = 0.75)

density.segment <- function(dens, point, lty, col, lwd) {
  
  app <- approx(dens$x, dens$y, xout = point)  
  
  return(segments(x0 = point , x1 = point, y0 = 0, y1 = app$y, lty = lty, col = col, lwd = lwd))
}

plot.save <- function(name, width, height, pointsize, bg) {
  
  if(missing(width)) { width <- 1500 }
  
  if(missing(height)) { height <- 1000 }
  
  if(missing(pointsize)) { pointsize <- 20 }
  
  if(missing(bg)) { bg <- "white" }
  
  Cairo::CairoPNG(filename = name,width = width,height = height,pointsize = pointsize, bg = bg)
  
}

### ### ### ###
#
# plot
#
### ### ### ###


plot.save(name = "skewed.distribution.png")
plot(x,dnorm(x,mean=0,sd=sqrt(1)),
      type="n",
     # yaxt = "n",
     ylab = "Density",
     xlab = "Value")

density.segment(dens = mskw.dens, point = mean(mskw), col = "red", lty = 1, lwd = 2)

density.segment(dens = mskw.dens, point = median(mskw), col = "green4", lty = 1, lwd = 2)

density.segment(dens = lskw.dens, point = mean(lskw), col = "red", lty = 3, lwd = 2)

density.segment(dens = lskw.dens, point = median(lskw), col = "green4", lty = 3, lwd = 2)

lines(lskw.dens, col = "gray40", lty = 3, lwd = 2)

lines(mskw.dens, col = "black", lty = 1, lwd = 2)

legend(x = "topleft",
       legend=c("Mean","Median"),
       col = c("red","green4"),
       pch = c(15,15),
       lwd = 0,
       cex=1.5,bty = "n",bg = "n",
       pt.cex =2.5,pt.lwd = 3,
       pt.bg = "white", inset = 0.025
       # y.intersp = 1.25
       )


dev.off()
