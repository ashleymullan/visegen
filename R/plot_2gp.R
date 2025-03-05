#' Generates a basic second-generation acceptability curve
#'
#' Uses the confidence interval method and second-generation p-values to evaluate and display conclusions about treatment optimality
#'
#' @param lambdas a vector of budget constraints
#' @param null_scale a scalar controlling the length of the null interval
#' @param confidence_interval_lows a vector of the same length as \code{lambdas}, holding the lower CI bound of the net monetary benefits, often coming from the output of \code{ci_nmb()}
#' @param confidence_interval_highs a vector of the same length as \code{lambdas}, holding the upper CI bound of the net monetary benefits, often coming from the output of \code{ci_nmb()}
#' @param p_vals a vector of the same length as \code{lambdas} holding the second-generation p-value for each lambda, often coming from the output of \code{compute_2gp()}
#' @param tol very small number, how far from the second-generation p-value benchmarks you're willing to accept
#' @return a second generation acceptability curve, a ggplot2 object
#'
#' @export
plot_2gp <- function (lambdas, null_scale = 0.15, confidence_interval_lows,
                        confidence_interval_highs, p_vals, tol = 0.000001)
{
  null_interval <- c(-1 * null_scale * max(lambdas), null_scale *
                       max(lambdas))
  df <- data.frame(l = lambdas, nl = rep(null_interval[1],
                                         times = length(lambdas)), nu = rep(null_interval[2],
                                                                            times = length(lambdas)), cl = confidence_interval_lows,
                   cu = confidence_interval_highs, pc = 1 - p_vals)
  #initial color pass
  df <- df |> mutate(code = case_when(
    cl < nl & cu <= nu ~ "control",
    cu > nu & cl > nl ~ "treatment",
    TRUE ~ NA
  ))
  #second color pass
  df <- df |> mutate(code = case_when(
    !is.na(code) ~ code,
    abs(cu - nu) > abs(cl - nl) ~ "treatment",
    TRUE ~ "control"
  ))
  #third color pass
  df <- df |> mutate(code = case_when(
    abs(pc - 1) < tol ~ code,
    abs(pc - 0.5) < tol ~ "inconclusive",
    abs(pc) < tol ~ "equivalent",
    code == "control" ~ "inconclusive, favors control",
    code == "treatment" ~ "inconclusive, favors treatment"
  ))

  cols <- c("control" = "blue4", "inconclusive, favors control" = "royalblue1",
            "treatment" = "red4", "inconclusive, favors treatment" = "red1",
            "equivalent" = "purple", "inconclusive" = "gray")
  p <- ggplot(df, aes(x = l, y = pc, color = code)) +
    geom_point(size = 1.8) +
    theme_bw() + scale_color_manual(values = cols) +
    theme(legend.position = "bottom") + labs(color = "")
  p
}
