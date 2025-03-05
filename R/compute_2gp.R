#' computes a vector of second generation p values
#'
#' uses the formula from Blume et. al (2018) to compute a second-generation p-value for each treatment comparison at each lambda
#'
#' @param null_scale a scalar to adjust the size of your null interval, defaults to 0.15
#' @param lambdas a vector of lambda values, where lambda is an upper bound on available resources, expressed in terms of currency
#' @param ci_uppers a vector of the same length as \code{lambdas}, holding the upper CI bound of the net monetary benefits, often coming from the output of \code{ci_nmb()}
#' @param ci_lowers a vector of the same length as \code{lambdas}, holding the lower CI bound of the net monetary benefits, often coming from the output of \code{ci_nmb()}
#' @return vector of 2nd generation p-values for each lambda value
#' @examples
#' B <- 1000
#' conf_level <- 0.95
#' A <- rbinom(N, 1, 0.5)
#' Z <- rnorm(N, 50 + 4.5 * A, 4)
#' Y <- rnorm(N, 100 + 2 * A, 5)
#' lambdas <- c(0:1000)/500
#' df <- prep_data(A,Z,Y)
#' conf_ints <- ci_nmb(df, lambdas, B, conf_level)
#' null_scale <- 0.15
#' compute_2gp(null_scale, lambdas, conf_ints$lower_bounds, conf_ints$upper_bounds)
#'
#' @export
compute_2gp <- function(null_scale = 0.15, lambdas, ci_lowers, ci_uppers){
  null_interval <- c(-1 * null_scale * max(lambdas), null_scale * max(lambdas))
  length_null <- null_interval[2] - null_interval[1]
  length_intersect <- rep(NA, times = length(lambdas)) #initialize container
  length_ci <- ci_uppers - ci_lowers #for each lambda

  for(l in 1:length(lambdas)){ #loop through lambdas
    ci_lambda <- c(ci_lowers[l], ci_uppers[l]) #grab ci for that lambda

    nl <- null_interval[1]; nu <- null_interval[2]
    cl <- ci_lambda[1]; cu <- ci_lambda[2]
    length_intersect[l] <- case_when(
      nl < cl & nu < cu & nu < cl ~ 0,
      nl < cl & cl < nu & nu < cu ~ abs(nu - cl),
      nl < cl & nl < cu & cu < nu ~ abs(cu - cl),
      cl < nl & cu < nl & cu < nu ~ 0,
      cl < nl & cl < nu & nu < cu ~ abs(nu - nl),
      cl < nl & nl < cu & cu < nu ~ abs(cu - nl),
      TRUE ~ NA
    )

    #isolate and save the intersection of the two intervals
    #higher_min <- max(ci_lambda[1], null_interval[1])
    #lower_max <- min(ci_lambda[2], null_interval[2])
    #length_intersect[l] <- max(lower_max - higher_min, 0) #overrides negative
  } #end loop through lambdas

  ratios <- length_intersect / length_ci
  check_quantity <- length_ci / (2 * length_null)
  correction_terms <- ifelse(check_quantity > 1,
                             check_quantity, 1) #spits out max

  ratios * correction_terms #the 2nd gen p values for each lambda
}

