# degrees of freedom

library(Cairo)

# single point plot - R^2 = NA

name <- "one_point.png"

CairoPNG(filename = name, width = 1920, height = 1080, pointsize = 20)

  par(mfrow=c(1,2), pty="s",mar=c(4.5,4.5,0.1,1))
  
  plot.2(x = 1, y=1.3, col="maroon",abline = F)
  
  plot.2(x = 2, y=2, col="maroon",abline = F)
  
dev.off()

shell.exec(name)

graphics.off()

# two-point plot - R^2 = 1

plot.points <- function(x, y, col, abline=T) {
  
  df.2 <- data.frame(x, y)
  
  plot(df.2,xlim=c(0,3),ylim=c(0,3),cex=3, pch=20, col=col)
  
  if(abline ==T ) {
  # lines(df.2, lwd = 2, lty=2, col="gray")
  abline(lm(df.2$y~df.2$x), lwd = 2, lty=2, col="gray")
  
    }
  text(x = 2.5, y = 0, label=bquote(R^2 == .(cor(x,y))))
  
  
}

name <- "two_points.png"

CairoPNG(filename = name, width = 1920, height = 1080, pointsize = 20)

  par(mfrow=c(1,2), pty="s",mar=c(4.5,4.5,0.1,1))
  
  plot.points(x = c(2,1), y=c(2.8,1.4), col="springgreen4")
  
  plot.points(x = c(2.8,0.3), y=c(1.7,1.2), col="springgreen4")

dev.off()

shell.exec(name)

graphics.off()

# more points - meaningful R^2

name <- "multiple_points.png"

CairoPNG(filename = name, width = 1920, height = 1080, pointsize = 25)

layout(matrix(c(1,2,1,3), 2, 2, byrow = TRUE), widths=c(1.5,1), heights=c(1,1))

par(pty="s",mar=c(4.5,4.5,0.1,1))

  plot.points(x = c(1,2,2.5), y=c(0.4,1, 2), col = "dodgerblue3")
  
  plot.points(x = c(2,1,2.5,0.3), y=c(2.8,1.4,2, 1), col = "dodgerblue3")
  
  plot.points(x = c(2,1,2.5,0.3,0.9,1.3,2.2,2.9), y=c(2.8,1.4,2, 1,0.3,0.98,2,1.3), col = "dodgerblue3")

dev.off()

shell.exec(name)

graphics.off()
