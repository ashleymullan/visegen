#' Returns a matrix of incremental net monetary benefit
#'
#' Provides the framework for a visualization
#'
#' @param df a data frame containing at least columns A, Z, and Y
#' @param lambdas list of lambda values, where lambda is an upper bound on available resources, expressed in terms of currency
#' @param B number of bootstrap repetitions, defaults to 1000
#' @param N sample size
#' @return matrix of dimension B x length(lambdas)
#'
#' @examples
#' B <- 1000
#' N <- 20000
#' A <- rbinom(N, 1, 0.5)
#' Z <- rnorm(N, 50 + 4.5 * A, 4)
#' Y <- rnorm(N, 100 + 2 * A, 5)
#' lambdas <- c(0:1000)/500
#' df <- data.frame(A,Z,Y)
#' boot_ben(df, lambdas, N)
#'
#' @export
boot_ben <- function(df, lambdas, B = 1000, N){
  bNMBs <- matrix(nrow = B, ncol = length(lambdas))
  for (j in 1:B){ #bootstrap each row
    sample_ids <- sample(1:N, size = N, replace = TRUE)
    resample <- df[sample_ids,]
    scale <- (mean(resample$Z[resample$A == 1]) - mean(resample$Z[resample$A == 0])) - (mean(resample$Y[resample$A == 1]) - mean(resample$Y[resample$A == 0]))
    bNMBs[j,] <- lambdas * scale
  }
  bNMBs
}

#   ##Bind the sampled data together and name it:
#   dat <- data.frame(cbind(A, Z, Y))
#   names(dat) <- c("A", "Z", "Y")
#

#
#   #Construct 95% Confidence intervals (percentile method) from the bootstrap:
#   CIlow <- apply(bNMBs, 2, quantile, probs = 0.025) #over cols (get quantiles at each lambda)
#   CIhi <- apply(bNMBs, 2, quantile, probs = 0.975) #over cols (get quantiles at each lambda)
#
#   #Construct a null hypothesis:
#   null.int <- c(-nullin *max(ls), nullin * max(ls)) #nullin is parameter, scales the null length
#   length.null <- null.int[2] - null.int[1]
#
#   #Length of lambda: (introduces vector of length lambda)
#   length.inters <- matrix(nrow = length(ls), ncol = 1)
#
#   #Length of CI:  (over all bootstraps)
#   length.CI <- CIhi - CIlow
#
#   #what color's the line? whichtrt designates that.
#   whichtrt = data.frame(rep(10, 101))
#
#   #For each of the lambda values:
#   for(j in 1:length(length.inters)){
#     #Confidence interval as a list:
#     c.int <- c(CIlow[j], CIhi[j])
#
#     #Subtract the min from the max of the CI to find its length:
#     tmp1 <- max(c.int[1], null.int[1])
#     tmp2 <- min(c.int[2], null.int[2])
#     length.inters[j] <- tmp2 - tmp1 #sticks it in the container
#     #If its negative, make the CI length 0:
#     if (null.int[1] > c.int[2]) {length.inters[j] <- 0}
#     if (CIlow[j][1] > null.int[2]) {length.inters[j] <- 0}
#
#     #Using the CI to find which group is which:
#     ## whichtrt guide: 1 is control, 2 is treatment, 3 is inconclusive favoring control,
#     ## 4 is inconclusive favoring treatment, 5 is equivalent, 6 is inconclusive.
#
#     if(c.int[1] < null.int[1] & c.int[2] <= null.int[2]) {whichtrt[j,1] = 1}
#     else if(c.int[2] > null.int[2] & c.int[1] >= null.int[1]) {whichtrt[j, 1] = 2}
#     #More CI below the null interval? designate it as the control:
#     else if(abs(c.int[2] - null.int[2]) < abs(c.int[1] >= null.int[1])) {whichtrt[j, 1] = 1}
#     #More CI below the null interval? designate it as the treatment:
#     else if(abs(c.int[2] - null.int[2]) >= abs(c.int[1] >= null.int[1])) {whichtrt[j, 1] = 2}
#   } #end of loop over lambdas
#
#   #1st gen plot formula. This is a standard p-value calculation:
#   #What percentage of bootstrapped INMB's are greater than 0?
#   pCE <- (bNMBs > 0)
#
#   #Calculate the 2nd-gen p-value using Blume's formula:
#   pdeltas <- (length.inters/length.CI) * pmax(length.CI/(2 * length.null), 1)
#   #When graphing, the y-value will be 1-pdeltas.
#   #This is for the purpose of better graph comprehension.
#
#   #Making the lines stitch together:
#
#   for(j in 1:length(pdeltas)){ #start loop over lambdas
#     ##If the 2nd-gen p-value > 0, intervention is either inconclusive, or equivalent
#     if(pdeltas[j]> 0) {
#       # If 2nd-gen p-value = 0.5, its inconclusive
#       if(pdeltas[j]== 0.5)  {whichtrt[j, 1]= 6}
#       # Intervention is inconclusive, but favoring an intervention
#       if(whichtrt[j, 1] == 1) {whichtrt[j, 1]= 3}
#       if(whichtrt[j, 1] == 2) {whichtrt[j, 1]= 4}
#       # If 2nd-gen p-value = 1, the interventions are equivalent:
#       if(pdeltas[j]== 1)  {whichtrt[j, 1]= 5}
#     }
#   }
#   #For stitching lines graphically: (extends lines)
#   desig = function(nb){
#     c(min(which(whichtrt == nb))-1, which(whichtrt == nb), max(which(whichtrt == nb))+1)
#   }
#
#   #legend preparation
#   leg.cols <- c("Intervention (x = 1)" = "firebrick3", "Control (x = 0)" = "dodgerblue4")
#   maxL <- max(ls)
#
#   ##If a 1st generation graph is also wanted:
#   if(cea1 == TRUE){
#     ## Plot the average first generation p-values compared to the lambdas:
#     data.frame(ls = ls,
#                pvals = colMeans(pCE),
#                pval_comps = 1 - colMeans(pCE)) |>
#       ggplot(aes(x = ls, y = pvals)) +
#       geom_line(aes(color = "Intervention (x = 1)"), size = 1.5) +
#       geom_line(aes(y = pval_comps, color = "Control (x = 0)"),
#                 size = 1.5) + #control
#       geom_hline(yintercept = 0.5, linetype = "dashed",
#                  color = 'gray50', size = 1) +
#       ylim(c(0,1)) +
#       labs(x = expression(lambda),
#            y = "1-sided Bootstrapped p-value",
#            title = "First Generation") +
#       theme_minimal() +
#       scale_color_manual(name = "legend", values = leg.cols) +
#       theme(legend.position = "bottom",
#             #legend.position.inside = c(quantile(ls, 0.5), 0.2),
#             legend.title = element_blank(),
#             legend.key.width = unit(0.5, "cm"))
#
#     #plot(ls, colMeans(pCE), type = "l", frame.plot = FALSE, lwd = 2, col = "firebrick3", xlab = expression(lambda), ylab = "1-sided Bootstrapped p-value", main = "First Generation", ylim = c(0,1), xlim = xlimm)
#
#     # Include a line for the control as well:
#     #lines(lines(ls, 1- colMeans(pCE), col= "dodgerblue4", lwd = 2))
#
#     # Dashed line at 0.5 helps mark the intersection point:
#     #abline(0.5,0, col = "gray50", lwd = 1.5, lty  =2)
#
#     # Basic legend:
#     legend("right", c("Intervention (x = 1)", "Control (x = 0)"), col = c("firebrick3", "dodgerblue4"), text.col = "black", lty = c(1, 1), merge = TRUE, cex = 0.6)
#
#     ##Now to plot the 2CEAc:
#     #Start with a 1st gen plot, but just mask the lines by making them white.
#     plot(ls, colMeans(pCE), type = "l", frame.plot = FALSE, lwd = 2, col = "white", xlab = expression(lambda), ylab = expression(paste("1 "-" p"[delta])), ylim = c(0,1),xlim = xlimm, main = "Second Generation")
#
#     ##Draw a line per whichtrt value, using desig() to link inconclusive lines:
#     lines(ls[desig(3)], (1 - pdeltas)[desig(3)], col = "dodgerblue", lwd = 3)
#     lines(ls[whichtrt == 1], (1 - pdeltas)[whichtrt == 1], col = "dodgerblue4", lwd = 3)
#     lines(ls[desig(4)], (1 - pdeltas)[desig(4)], col = "firebrick1", lwd = 3)
#     lines(ls[whichtrt == 2], (1 - pdeltas)[whichtrt == 2], col = "firebrick3", lwd = 3)
#     lines(ls[whichtrt == 5], (1 - pdeltas)[whichtrt == 5], col = "black", lwd = 3)
#     lines(ls[whichtrt == 6], (1 - pdeltas)[whichtrt == 6], col = "forestgreen", lwd = 3)
#
#     ##If a legend is wanted for the 2CEAc, the below is generated:
#     if(puttab == T){
#       legend("bottomright",
#              c("Intervention (x = 1)", expression(paste("Inconclusive; favors intervention ")),expression(paste("Control ", "(x = 0)")), expression(paste("Inconclusive; favors control")), "Inconclusive", "Approximate equivalence"),
#              col = c("firebrick3", "firebrick1", "dodgerblue4", "dodgerblue", "forestgreen","black"), text.col = "black", merge = TRUE, lty = c(1,1,1,1,1,1),
#              lwd = c(3,3,3,3,3,3), cex = 0.66)
#     }
#   }
#   ## If no standard CEAc is wanted, the 2CEAc is just generated alone:
#   else{
#     #Instead of scientific notation, the lambdas can be defined in terms of millions:
#     if(inmil == T){
#       #Divide by a million
#       ls = ls/1000000
#       #Graph a blank 2CEAc
#       plot(ls, colMeans(pCE), type = "l", frame.plot = FALSE, lwd = 2, col = "white", xlab = expression(paste(lambda, " ($ x 1,000,000)")), ylab = expression(paste("1 "-" p"[delta])), ylim = c(0,1), xlim = xlimm, main = tit)
#     }
#     else{
#       #Graph a blank 2CEAc
#       plot(ls, colMeans(pCE), type = "l", frame.plot = FALSE, lwd = 2, col = "white", xlab = expression(lambda), ylab = expression(paste("1 "-" p"[delta])), ylim = c(0,1), xlim = xlimm, main = tit)
#     }
#
#     ##Draw a line per whichtrt value, using desig() to link inconclusive lines:
#     lines(ls[desig(3)], (1 - pdeltas)[desig(3)], col = "dodgerblue", lwd = 3)
#     lines(ls[whichtrt == 1], (1 - pdeltas)[whichtrt == 1], col = "dodgerblue4", lwd = 3)
#     lines(ls[desig(4)], (1 - pdeltas)[desig(4)], col = "firebrick1", lwd = 3)
#     lines(ls[whichtrt == 2], (1 - pdeltas)[whichtrt == 2], col = "firebrick3", lwd = 3)
#     lines(ls[whichtrt == 5], (1 - pdeltas)[whichtrt == 5], col = "black", lwd = 3)
#     lines(ls[whichtrt == 6], (1 - pdeltas)[whichtrt == 6], col = "forestgreen", lwd = 3)
#
#     ##If a legend is wanted for the 2CEAc, the below is generated:
#     if(puttab == T){
#       legend("bottomright", c("Intervention (x = 1)", expression(paste("Inconclusive; favors intervention ")),expression(paste("Control ", "(x = 0)")), expression(paste("Inconclusive; favors control")), "Inconclusive", "Approximate equivalence"), col = c("firebrick3", "firebrick1", "dodgerblue4", "dodgerblue", "forestgreen","black"), text.col = "black", merge = TRUE, lty = c(1,1,1,1,1,1), lwd = c(3,3,3,3,3,3), cex = 0.66)
#     }
#
#   }
#
# }
