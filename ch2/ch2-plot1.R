# figure 1
curve(5*x,0,5,lwd=3,xlab='x', ylab='f(x)')
curve(10-x,lwd=3,lty=1,xlab='x', ylab='f(x)')#,add=TRUE)
#leg.txt <- c("y=5x","y=10-x")
#legend("topleft", leg.txt)
curve(0.1*exp(0.5*x),0,10,lwd=3,xlab='x', ylab='f(x)')
curve(20-12*exp(-3*x),lwd=3,lty=1,xlab='x', ylab='f(x)') #lty=2,add=TRUE)
leg.txt <- c("y=0.1exp(0.5x)","y=12exp(-0.2x)")
legend("topleft", leg.txt)