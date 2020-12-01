#' @title Confidence Intervals for Linear Regression Coefficients
#'
#' @description Obtain confidence intervals for the linear regression model
#' coefficients \eqn{\beta}, estimated by the function \code{\link{regress}}.
#'
#' @param mod An object of class \code{"linreg"} returned by the function
#' \code{\link{regress}}.
#' @param alpha Number between 0 and 1 specifying the significance level
#' \eqn{\alpha} to obtain the \eqn{1-\alpha} confidence intervals. Default is
#' 0.05 for a 95\% confidence interval.
#' @param approach A specification of which approach to use to obtain the
#' confidence intervals. Possible values are \code{"asymptotic"} (default) and
#' \code{"bootstrap"} for asymptotic and bootstrap confidence intervals,
#' respectively.
#' @param B Number of bootstrap replications to use for a bootstrap confidence
#' interval. Default is 1000.
#'
#' @return A matrix with columns giving lower and upper confidence limits for
#' each parameter. These will be labelled as \code{alpha}/2 and 1-\code{alpha}/2
#' in \% (by default 2.5\% and 97.5\%).
#' @export
#' @examples
#' mod1 <- regress(mtcars$mpg, mtcars$wt)
#' coefCI(mod1) # asymptotic confidence intervals
#' coefCI(mod1, approach = "bootstrap") # bootstrap confidence intervals
#'
#' mod2 <- regress("mpg", c("wt", "disp", "hp"), data = mtcars)
#' coefCI(mod2, alpha = 0.1, approach = "asymptotic")
#' coefCI(mod2, alpha = 0.1, approach = "bootstrap", B = 500)
coefCI <- function(mod, alpha = 0.05, approach = "asymptotic", B = 1000) {

  # check if mod is of class linreg
  if(class(mod) != "linreg") {
    stop("mod has to be a `linreg` object returned by function `regress`")
  }

  # check if alpha is between 0 and 1
  if(alpha < 0 | alpha > 1) {
    stop("alpha has to be between 0 and 1")
  }

  # check if approach is "asymptotic" or "bootstrap"
  if(!(approach %in% c("asymptotic", "bootstrap"))) {
    stop("approach has to be 'asymptotic' or 'bootstrap'")
  }

  # check if B is positive
  if(B <= 0) {
    stop("Number of bootstrap replications B cannot be <= 0")
  }

  # find asymptotic confidence intervals
  if(approach == "asymptotic") {
    # model results
    mod_res <- results(mod, print = FALSE)
    # t critical value
    tcrit <- qt(1-alpha/2, df = mod_res$df2)
    # confidence interval
    CI_lower <- mod_res$results[, 1] - tcrit * mod_res$results[, 2]
    CI_upper <- mod_res$results[, 1] + tcrit * mod_res$results[, 2]
    CI <- cbind(CI_lower, CI_upper)
    colnames(CI) <- c(paste(alpha/2 * 100, "%", sep = ""),
                      paste((1-alpha/2) * 100, "%", sep = ""))
  }

  # find bootstrap confidence intervals
  if(approach == "bootstrap") {
    # matrix to store bootstrap results
    boot_coef <- matrix(nrow = B, ncol = mod$p)
    colnames(boot_coef) <- rownames(mod$coefficients)
    for(i in 1:B) {
      # sample n observations with replacement
      ind <- sample(1:mod$n, size = mod$n, replace = TRUE)
      # estimate coefficients
      coeffs <- regress(mod$y[ind], mod$x[ind, ], intercept = FALSE)
      boot_coef[i, ] <- coeffs$coefficients
    }
    # bootstrap confidence interval
    CI <- t(apply(boot_coef, 2, quantile, c(alpha/2, 1 - alpha/2)))
  }

  return(CI)

}
