#' Prepares user's data for use in later functions
#'
#' Given a set of vectors, we construct a properly named data frame for use in other package functions
#'
#' @param A a binary vector listing treatment types for each unit of observation (where 0 is the null and 1 is the alternative)
#' @param Y a numeric vector listing the cost of the given intervention for each unit of observation
#' @param Z a numeric vector listing the effect size of the given intervention for each unit of observation
#' @return data frame consisting of the columns A,Y,Z with appropriate column names
#'
#' @export
prep_data <- function(A, Y, Z){
  df <- data.frame(A,Y,Z)
  names(df) <- c("A", "Y", "Z")
  df
}



