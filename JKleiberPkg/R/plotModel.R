#' Straight line model plotting function
#'
#' Plots a straight line linear model with scatter plot
#'
#' @param data Data to plot
#' @param x_val Variable for the x axis
#' @param y_val Variable for the y axis
#' @param col Color of the plot
#' @param title Title of the plot
#'
#' @return A plot of the linear model
#' @export
#'
#' @examples plotLM(spruce.df, spruce.df$BHDiameter, spruce.df$Height, spruce.df$BHDiameter, "Height vs Diameter")
#'
#'
plotLM <- function(data, x_val, y_val, col, title)
{
  library(ggplot2)
  g = ggplot(data, aes(x=x_val,y=y_val,colour=col))
  g = g + geom_point() + geom_line() + geom_smooth(method="lm")
  g + ggtitle(title)
}
