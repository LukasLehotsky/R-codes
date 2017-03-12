# scatterplot demonstration

setwd("C:\\Users\\Lukas\\Documents")

library(car)
library(Cairo)
library(gplots)

df <- car::Davis

# shell.exec("http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf")

# col2hex("lightseagreen")

# palette(c("#B0306055","#00008855"))

name <- "scatterplot.width-height.png"

CairoPNG(filename = name, width = 1920, height = 1080, pointsize = 20)

par(pty="s", mar=c(4.5,1,0.1,1))

plot(df$height, 
     df$weight, 
     xlim = c(0,200), 
     ylim = c(0,200), 
     cex= 3,
     pch=20, 
     col="#B0306055", 
     xlab = "Height", 
     ylab="Weight")

dev.off()

shell.exec(name)

# scatterplot with fitted linear model

name <- "scatterplot.width-height.lm.png"

CairoPNG(filename = name, width = 1920, height = 1080, pointsize = 20)

par(pty="s", mar=c(4.5,1,0.1,1))

plot(df$height, 
     df$weight, 
     xlim = c(0,200), 
     ylim = c(0,200), 
     cex=3, 
     pch=20, 
     col="#1C86EE55", 
     xlab = "Height", 
     ylab="Weight")

text(x = 175, y = 0, label=bquote(R^2 == .(cor(df$weight,df$height))))

abline(lm(df$weight ~df$height), lwd = 2, lty=2, col="gray")

dev.off()

shell.exec(name)

# scatterplot with fitted linear model without outliers

outliers <- outlierTest(lm(df$weight~df$height))

outliers <- as.numeric(names(x$rstudent))

df.no.outliers <- df[-outliers,]

name <- "scatterplot.width-height.lm.no-outliers.png"

CairoPNG(filename = name, width = 1920, height = 1080, pointsize = 20)

  par(pty="s", mar=c(4.5,1,0.1,1))
  
  plot(df.no.outliers$height, 
       df.no.outliers$weight, 
       xlim = c(0,200), 
       ylim = c(0,200), 
       cex=3, 
       pch=20, 
       col="#20B2AA55", 
       xlab = "Height", 
       ylab="Weight")
  
  abline(lm(df.no.outliers$weight ~df.no.outliers$height), lwd = 2, lty=2, col="gray")
  
  text(x = 175, y = 0, label=bquote(R^2 == .(cor(df.no.outliers$weight,df.no.outliers$height))))
  
  points(y = df$weight[outliers], x = df$height[outliers], cex=3, pch=20, col="#20B2AA55")
  
  points(y = df$weight[outliers], x = df$height[outliers], cex=5, pch=4, col="red", lwd=1.5)

dev.off()

shell.exec(name)
