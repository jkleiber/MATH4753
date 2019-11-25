#' Quadratic trendline function
#'
#' Finds the equation for a quadratic trendline
#'
#' @param model Linear model for finding the coefficients
#'
#' @return An equation for the quadratic trendline
#' @export
#'
#' @examples quadraticTrendline(quad.lm)
#'
quadraticTrendline <- function(model)
{
  model$coef[1] + model$coef[2]*x  + model$coef[3]*x^2
}
