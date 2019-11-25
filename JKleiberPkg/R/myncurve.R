#' Normal curve plot function
#'
#' This function plots a normal probability distribution
#' and highlights the density for P(X < a)
#'
#' @param mu The mean of the distribution
#' @param sigma The standard deviation of the distribution
#' @param a The value a for which P(X < a) is calculated
#'
#' @return A plot of the probability density function and the calculation P(X < a)
#' @export
#'
#' @examples myncurve(mu = 10, sigma = 4, a = 6)
#'
myncurve = function(mu = 0, sigma = 1, a = 0){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu+3*sigma))

  # Highlight the region
  xcurve = seq(mu - 3*sigma, a, length=1000)
  ycurve = dnorm(xcurve, mu, sigma)
  polygon(c(mu-3*sigma, xcurve, a), c(0, ycurve, 0), col = "SkyBlue")

  # Compute the probability
  prob = pnorm(a, mu, sigma)
  list("probability" = prob)
}
