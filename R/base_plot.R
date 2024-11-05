#' Generates a basic plot
#'
#' Provides the framework for a visualization
#'
#' @param x Independent Variable
#' @param y Dependent Variable
#' @return ggplot2 object
#'
#' @examples
#' x <- rnorm(n = 100, mean = 0, sd = 1)
#' y <- rnorm(n = 100, mean = 3, sd = 1)
#'
#' @export
base_plot <- function(x,y){
  df <- data.frame(x = x, y = y)
  p <- df |>
    ggplot2::ggplot(aes(x = x, y = y)) +
    theme_bw()
  p
}
