#' Occurences in a binomial
#'
#' This function returns the probability for numbers of successes in a binomial distribution
#'
#' @param size The size of the distribution
#' @param p The probability of a trial being a success
#'
#' @return A dataframe with the probabilities of the number of successes for each possible number of successes
#' @export
#'
#' @examples obinom()
#'
obinom = function(size=10, p = 0.5) {
  prob = c()
  for (i in 1:size)
  {
    prob[i] = dbinom(i, size, p)
  }

  prob.df = data.frame(prob)
  prob.df
}
