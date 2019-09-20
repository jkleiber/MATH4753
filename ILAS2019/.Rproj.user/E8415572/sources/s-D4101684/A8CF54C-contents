#' Bull's eye function
#'
#' This function shows how unbiased estimators work
#'
#' @param n Sample size
#' @param iter Number of iterations
#' @param mu Population mean
#' @param sigma Population standard deviation
#' @param nadj Adjustment to `n-nadj` 1 or 0
#'
#' @return Plots a bull's eye target
#' @export
#'
#' @examples
#' bullseye()
bullseye<-function(n=30,iter=10000,mu=10,sigma=10, nadj=1){


#graphics.off()


xbar=rnorm(iter,mean=mu,sd=sigma/sqrt(n))
ssq=sigma^2*rchisq(iter,n-1)/(n-nadj)# adjsut the n


df = data.frame(xbar = xbar,ssq =ssq)
#windows()
plot(ssq~xbar,pch=21,bg=rgb(0,1,0,0.5),cex=0.7,
     xlab=expression(bar(x)),
     ylab=expression(s^2),
     col="Grey")
leg.txt = c(expression(paste(mu,",",sigma^2)),expression(paste(bar(bar(x)),",",bar(s^2))))
legend("topright", leg.txt,
      fill=c("Red","Blue") )
abline(v=mu,h=sigma^2, lwd=4, col="Red") # pop
xy=colMeans(df)
abline(v=xy[1],h=xy[2], lwd = 2, col = "Blue") # sample
points(mu,sigma^2,cex =4, pch=21,bg = "Black")
points(mu,sigma^2,cex =2, pch=21,bg = "Pink")

points(xy[1],xy[2],cex=2,pch=21,bg="Blue")
title("Practical demonstration of biased/unbiased estimators")
numerator = ifelse(nadj==1, n,n-1)
text(mu,0.8*sigma^2, substitute(plain(E)*(S^2)==frac(numerator,n)*sigma^2,list(numerator=numerator,n=n)))
}

