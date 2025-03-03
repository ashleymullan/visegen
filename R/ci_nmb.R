#' Computes a matrix of net incremental benefits
#'
#' Computes (via bootstrapping for each value of lambda) a matrix (of dimension B X L, where B is the number of desired bootstrap replications and L is the number of lambda constraints under consideration) of incremental net monetary benefits
#'
#' @param df a data frame containing at least columns A, Z, and Y
#' @param lambdas vector of budget constraints
#' @param B number of bootstrap repetitions, defaults to 1000
#' @param conf_level the desired nominal coverage of the CI, defaults to 95%
#' @returns
#' \item{lower_bounds: a vector of size length(lambdas) of the lower bounds of the confidence intervals for each value of lambda}
#' \item{upper_bounds: a vector of size length(lambdas) of the upper bounds of the confidence intervals for each value of lambda}
#' \item{bNMBs: a matrix of dimension B x length(lambdas) of the bootstrapped net monetary benefits}
#' @examples
#' #boostrapping parameters
#' B <- 1000
#' conf_level <- 0.95
#' #generate data
#' A <- rbinom(N, 1, 0.5)
#' Z <- rnorm(N, 50 + 4.5 * A, 4)
#' Y <- rnorm(N, 100 + 2 * A, 5)
#' #prepare data
#' df <- prep_data(A,Z,Y)
#' #set constraints
#' lambdas <- c(0:1000)/500
#' ci_nmb(df, lambdas, B, conf_level)
#' @export
ci_nmb <- function(df, lambdas, B = 1000, conf_level = 0.95){
  bNMBs <- matrix(nrow = B, ncol = length(lambdas))
  N <- nrow(df)
  for (j in 1:B){ #fills each row w/ scaled lambda vector
    sample_ids <- sample(seq(from = 1, to = N, by = 1), size = N, replace = TRUE)
    resample <- df[sample_ids,]
    a1_grp <- resample[resample$A == 1,] #isolate out rows with A = 1
    a0_grp <- resample[resample$A == 0,] #isolate out rows with A = 0
    diff_mean_z <- mean(a1_grp$Z) - mean(a0_grp$Z) #scalar: difference of group mean effects
    diff_mean_y <- mean(a1_grp$Y) - mean(a0_grp$Y) #scalar: difference of group mean costs

    bNMBs[j,] <- lambdas * diff_mean_z - diff_mean_y  #fill the row with the budget scaled effect difference - cost difference
  }

  tail_size <- (1 - conf_level)/2
  lower_quantile <- tail_size
  upper_quantile <- 1 - tail_size
  lowers <- apply(bNMBs, 2, quantile, probs = lower_quantile) #lower bound for each lambda
  uppers <- apply(bNMBs, 2, quantile, probs = upper_quantile) #upper bound for each lambda

  list(lower_bounds = lowers, upper_bounds = uppers, bNMBs = bNMBs)
}
