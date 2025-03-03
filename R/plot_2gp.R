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
plot_2gp <- function(lambdas,
                     null_scale = 0.15,
                     confidence_interval_lows, confidence_interval_highs,
                     p_vals,
                     tol = 0.01){
  null_interval <- c(-1 * null_scale * max(lambdas), null_scale * max(lambdas))
  df <- data.frame(l = lambdas,
                   nl = rep(null_interval[1], times = length(lambdas)),
                   nu = rep(null_interval[2], times = length(lambdas)),
                   cl = confidence_interval_lows,
                   cu = confidence_interval_highs,
                   pc = 1 - p_vals)

  df <- df |>
    mutate(fics_code = case_when(
      abs(pc - 0) <= tol ~ "favors ctrl",
      abs(pc - 1) <= tol ~ "favors trt",
      abs(pc - 0.5) <= tol ~ "inconclusive",
      pc < (1 - tol) & pc > (0.5 + tol) ~ "inconclusive, favors trt",
      pc > (0 + tol) & pc < (0.5 - tol) ~ "inconclusive, favors ctrl"
    ))
  df <- df |>
    mutate(code = case_when(
      nl < cl & nu < cu & nu < cl ~ "favors trt",
      nl < cl & cl < nu & nu < cu ~ "2", #needs further inspection
      nl < cl & nl < cu & cu < nu ~ "favors ctrl",
      cl < nl & cu < nl & cu < nu ~ "favors trt", #favors trt
      cl < nl & cl < nu & nu < cu ~ "?", # ?
      cl < nl & nl < cu & cu < nu ~ "6", #needs further inspection
      TRUE ~ NA
    ))

  df <- df |> #handle overlap cases 2 and 6
    mutate(above = case_when(
      code %in% c("2","6") ~ abs(cu - nu),
      TRUE ~ NA
    )) |>
    mutate(below = case_when(
      code %in% c("2","6") ~ abs(cl - nl),
      TRUE ~ NA
    )) |>
    mutate(fics_code = case_when(
      code == "2" & above > below ~ "inconclusive, favors trt",
      code == "2" & above < below ~ "inconclusive, favors ctrl",
      code == "6" & above > below ~ "inconclusive, favors ctrl",
      code == "6" & above < below ~ "inconclusive, favors trt",
      TRUE ~ NA
    )) |>
    mutate(fics_code = ifelse(code %in% c("2", "6"), fics_code, code))

    p <- df |>
      ggplot(aes(x = l, y = pc)) +
      geom_point(data = df |> filter(fics_code == "inconclusive, favors ctrl"), color = "red") +
      geom_point(data = df |> filter(fics_code == "favors ctrl"), color = "green") +
      geom_point(data = df |> filter(fics_code == "favors trt"), color = "blue") +
      theme_bw()
    p
}

#####
#  if(c.int[1] < null.int[1] & c.int[2] <= null.int[2]) {whichtrt[j,1] = 1}
#  else if(c.int[2] > null.int[2] & c.int[1] >= null.int[1]) {whichtrt[j, 1] = 2}
#  #More CI below the null interval? designate it as the control:
#  else if(abs(c.int[2] - null.int[2]) < abs(c.int[1] >= null.int[1])) {whichtrt[j, 1] = 1}
#  #More CI below the null interval? designate it as the treatment:
#  else if(abs(c.int[2] - null.int[2]) >= abs(c.int[1] >= null.int[1])) {whichtrt[j, 1] = 2}
#}



#for(j in 1:length(pdeltas)){
#  ##If the 2nd-gen p-value > 0, intervention is either inconclusive, or equivalent
#  if(pdeltas[j]> 0) {
#    # If 2nd-gen p-value = 0.5, its inconclusive
#    if(pdeltas[j]== 0.5)  {whichtrt[j, 1]= 6}
#    # Intervention is inconclusive, but favoring an intervention
#    if(whichtrt[j, 1] == 1) {whichtrt[j, 1]= 3}
#    if(whichtrt[j, 1] == 2) {whichtrt[j, 1]= 4}
#    # If 2nd-gen p-value = 1, the interventions are equivalent:
#    if(pdeltas[j]== 1)  {whichtrt[j, 1]= 5}
#  }
#}
