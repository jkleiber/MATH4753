#' Violin plot function
#'
#' This function makes violin plots using ggplot2
#'
#' @param data The data frame to plot with
#' @param x_val The x axis values for the violin plots
#' @param y_val The y axis values for the violin plots
#' @param fill_col Variable to base the fill color on
#' @param title Name for the plot
#'
#' @return A violin plot of the data
#' @export
#'
#' @examples violin(ddt$RIVER, ddt$LENGTH, ddt$SPECIES, "Violin Plot")
#'
violin <- function(data, x_val, y_val, fill_col, title)
{
  library(ggplot2)
  ggplot(data, aes(x = x_val, y = y_val, fill = fill_col)) + geom_violin() + ggtitle(title)
}
