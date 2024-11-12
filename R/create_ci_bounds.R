#' Returns a list of two vectors
#'
#' This function returns upper and lower confidence interval bounds for each value of lambda, contained in a list of two vectors.
#'
#' @param bens a matrix of incremental benefits having dimension B x L, where B is the number of bootstrap replications and L is the number of lambdas under consideration
#' @param conf_level a value between 0 and 1 specifying the percentage of confidence desired
#' @return a list of two vectors, upper and lower bounds for each lambda
#'
#' @examples
#' B <- 1000
#' N <- 20000
#' A <- rbinom(N, 1, 0.5)
#' Z <- rnorm(N, 50 + 4.5 * A, 4)
#' Y <- rnorm(N, 100 + 2 * A, 5)
#' lambdas <- c(0:1000)/500
#' df <- data.frame(A,Z,Y)
#' bens <- boot_ben(df, lambdas, N)
#' create_ci_bounds(bens, conf_level = 0.90)
#' @export

create_ci_bounds <- function(bens, conf_level = 0.95){
  tail_size <- (1 - conf_level)/2
  lower_quantile <- tail_size
  upper_quantile <- 1 - tail_size
  lowers <- apply(bens, quantile, probs = lower_quantile)
  uppers <- apply(bens, quantile, probs = upper_quantile)
  list(lower_bounds = lowers, upper_bounds = uppers)
}
