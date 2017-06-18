setwd("~/Documents/Github Repos/DataScienceSpecialisation/9. Developing Data Products/Final assignment")


rm(list=ls())

l<-10
d<-3
h<-10

plane.option = "lr"

par(mfrow=c(1,2))

xa <- c(0, l, l, 0, 0, l, l, 0, l, l+d, l+d, l)
ya <- c(0, 0, h, h, h, h, h+d, h+d, 0, 0, h, h)

maxlim <- max(l+d+2, h+d+2, sqrt(l^2+h^2)+2, sqrt(d^2+h^2)+2)

par(pty="s")

plot(xa, ya, type = "n", axes=FALSE, xlab="", ylab="", xlim=c(-1,maxlim), ylim=c(-1,maxlim))
#axis(1, at = round(xx))
#axis(2, at = round(yy))
polygon(xa[1:8], ya[1:8], col="gray", border = "blue", lwd = 3)
polygon(xa[9:12], ya[9:12], col="gray", border = "blue", lwd = 3)

text(0.1, h-1, "FRONT", col = "blue", pos = 4)
text(0.1, h+d-1, "TOP", col = "blue", pos = 4)
text(l+0.1, h-1, "SIDE", col = "blue", pos = 4)

text(l/2, -0.5, paste("Length = ", l), col = "blue")
text(l+0.1, h+d/2, paste("Depth = ", d), col = "blue", pos = 4)
text(0.1, h/2, paste("Height = ", h), col = "blue", pos = 4)

if (plane.option == "lr") { xlines <- c(0, l) } else { xlines <- c(l, l+d) }
        
lines(xlines, c(0, h), lwd=3, col="red")


xb0 <- ifelse(plane.option == "lr", d, l)
yb0 <- ifelse(plane.option == "lr", sqrt(l^2+h^2), sqrt(d^2+h^2))

xlab <- ifelse(plane.option == "lr", paste("Depth = ", d), paste("Length = ",l))
ylab <- paste("Diagonal1 = ", round(yb0,1))

xb <- c(0, xb0, xb0, 0)
yb <- c(0, 0, yb0, yb0)

plot(xb, yb, type = "n", axes=FALSE, xlab="", ylab="", xlim=c(-1,max(l+d+3,h+d+3)), ylim=c(-1,max(l+d+3,h+d+3)))
polygon(xb, yb, col="gray", border = "blue", lwd = 3)

text(xb0/2, -0.5, xlab, col = "blue")
text(xb0+0.1, yb0/2, ylab, col = "red", pos = 4)

lines(c(0, xb0), c(0, yb0), lwd=3, col="blue", lty = 2)
lines(c(xb0, xb0), c(0, yb0), lwd=3, col="red")

text(xb0+0.1, yb0-1, paste("Diagonal2 = ", round(sqrt(xb0^2+yb0^2),1)), col = "blue", pos = 4)
text(0.1, 0.5, paste(round(atan(yb0/xb0)/pi*180,1),"deg"), col = "blue", pos = 4)

