#' Newton Raphson Algorithm
#'
#' Find roots calculating estimates of the slope internally
#'
#' @param x0 Initial value
#' @param f function to find the roots of
#' @param delta Used to find the derivative , h
#' @param epsilon Accuracy
#' @param th1 Domain for Upper plot
#' @param th2 Domain for lower (magnified) plot
#' @param parameter Name of the parameter to be estimated
#'
#' @return invisible list of root ,x,y
#' @export
#'
#' @examples
#' root=mynewt(1.0,th2=c(1,1.4))$root
#' s12=100
#' s11=200
#' s22=400
#' #Test the answer in two ways
#' myfun(root)
mynewt=function(x0,f=myfun,delta=1e-12,epsilon=1e-12,th1=c(0,2*pi),th2=c(0,pi/2),parameter=expression(theta))
  {
    #graphics.off()  # Kill all graphics windows
    options(digits=12)
    # x0 initial value
    #f the function to be zeroed
    #delta is the increment in the derivative
    #epsilon is how close our approximation is to zero
    #th1 is the range of rotation angle for NR
    #th2 is the range for rotation angle for myfun
    fdash=function(x) (f(x+delta/2)-f(x-delta/2))/delta

    d=1000 # initial values
    i=0
    x=c() # empty vectors
    y=c()
    x[1]=x0 # assign initial guess
    y[1]=f(x[1]) # initial y value
    while(d > epsilon & i<100){ # ensures that it doesnt loop too much
      i=i+1
      x[i+1]=x[i]-f(x[i])/fdash(x[i]) # NR step
      y[i+1]=f(x[i+1]) # update y value
      d=abs(y[i+1]) # update d
    }
    #windows()
    #Cut the graphical area into two
    layout(matrix(1:2,nr=2,nc=1,byrow=TRUE),heights=c(3,4))
    curve(f(x), xlim=th1,xlab=parameter,ylab="f",main="myfun")
    abline(h=0,col="Red",lwd=2)
    # plot f with no x axis  on a reduced x range
    curve(f(x),xlim=th2,xaxt="n", xlab=parameter,ylab="f",main=  "Newton-Raphson Algorithm")
    points(x,y,col="Red",pch=19,cex=0.5)
    # Now plot the x axis
    axis(1,x,round(x,2),las=2)
    abline(h=0,col="Red")
    # Make blue segments to represent the tangents
    #make green segments for y values
    segments(x[1:(i-1)],y[1:(i-1)],x[2:i],rep(0,i-1),col="Blue",lwd=0.5)
    segments(x[2:i],rep(0,i-1),x[2:i],y[2:i],lwd=0.5,col="Green")
    # paste the root onto the last graph
    arrows(x0=x[i+1],y0=y[1],x1=x[i+1],y1=y[i+1])
    text(x[i+1],y[1],x[i+1])

    invisible(list(x=x,y=y,d=d,root=x[i+1]))

  }

