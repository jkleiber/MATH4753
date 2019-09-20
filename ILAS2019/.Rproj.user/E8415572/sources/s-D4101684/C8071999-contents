#' Multivariate Normal Distribution
#'
#' Make a zero centered sample
#'
#' @param sig11 Population variance 11
#' @param sig22 Population variance 22
#' @param sig12 Population covariance 12
#' @param rangxy How many std dev to make the range from
#'
#' @return invisible list containing the centered data
#' @export
#'
#' @examples
#' mvmat()
mvmat=function(sig11=100,sig22=400,sig12=100,rangxy=15){


  sig21=sig12
  library(mvtnorm)
  sig=matrix(c(sig11,sig12,sig21,sig22),nr=2,nc=2,byrow=TRUE)

  mat=rmvnorm(100,mean=c(10,10),sigma=sig)
  mat=mat-apply(mat,2,mean) # zero the means
  x1=mat[,1]
  x2=mat[,2]

  s11=sd(x2)
  s22=sd(x1)
  s12=cov(x1,x2)
  #windows()
  plot(x2~x1, main="MV Normal points\n Wayne Stewart", xlim=c(0-rangxy*sqrt(s11),0+rangxy*sqrt(s11)), ylim=c(0-rangxy*sqrt(s22),0+rangxy*sqrt(s22)),las=1)
  n=dim(mat)[1]
  covx=(n-1)/n*cov(mat) # biased
  library(ellipse)

  pts=ellipse(covx,centre=c(0,0))
  points(pts,type="l", col="Blue", lwd=2)
  abline(v=0,h=0)
  invisible(list(mat=mat))
}


