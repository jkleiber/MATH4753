#' An example of a Function to be zeroed
#'
#' @param x Parameter values for which a zero is captured
#'
#' @return y value f(x)
#' @export
#'
#' @examples
#' myfun(10)
myfun=function(x) {
  (s22-s11)*sin(x)*cos(x) +s12*(cos(x)^2-sin(x)^2)
}
