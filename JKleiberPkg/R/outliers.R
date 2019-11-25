#' Outlier plot function
#'
#' This function plots data points, outliers, and potential outliers in a dot plot
#'
#' @param x The data to plot
#' @param col Color vector containing outlier color, potential outlier color, and regular color (in that order)
#'
#' @return Plots a dot plot of the data points, highlighting outliers
#' @export
#'
#' @examples plotOutliers(x)
#'
plotOutliers <- function(x, title = "", col = c("Red", "Blue", "Black"))
{
  # Find the z scores
  z = (x - mean(x)) / sd(x)

  library(lattice)

  # Color vector
  outlier_colors = ifelse(abs(z)>3, col[1],
                          ifelse(abs(z)>=2 & abs(z)<=3,col[2], col[3]))

  # Plot the regular points, the outliers, and the possible outliers
  dotplot(x, col = outlier_colors, main = title)
}
