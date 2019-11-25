#' Confidence interval for the mean
#'
#' Function for finding the confidence interval of the mean
#'
#' @param data sample to find interval for
#' @param alpha confidence interval variable
#'
#' @return confidence interval
#'
#' @export
#' @examples myci(1:10, 0.05)
#'
myci = function(data, alpha=0.05){
  s = sd(data)
  y = mean(data)
  n = length(data)
  t = qt(1 - alpha/2, n-1)

  ci = c(y - t*s/sqrt(n), y + t*s/sqrt(n))
  ci
}
