#' @title F-test for Linear Regression Model
#'
#' @description Obtain F-test results for the overall significance of a linear
#' regression model fitted by the function \code{\link{regress}}.
#'
#' @param mod An object of class \code{"linreg"} returned by the function
#' \code{\link{regress}}.
#'
#' @return A \code{list} containing the following components:
#' \describe{
#'      \item{F-statistic}{F-statistic for the overall significance of the model}
#'      \item{df1}{Numerator degrees of freedom}
#'      \item{df2}{Denominator degrees of freedom}
#'      \item{p-value}{p-value for the F-test of overall significance}
#' }
#' @export
#' @examples
#' mod1 <- regress(mtcars$mpg, mtcars$wt)
#' Ftest(mod1)
#'
#' mod2 <- regress("mpg", c("wt", "disp", "hp"), data = mtcars)
#' Ftest(mod2)
Ftest <- function(mod) {

  # check if mod is of class linreg
  if(class(mod) != "linreg") {
    stop("mod has to be a `linreg` object returned by function `regress`")
  }

  # explained sum of squares for regression with intercept
  if(colnames(mod$x)[1] == "intercept") {
    SSM <- sum((mod$fitted.values - mean(mod$y))^2)
  # explained sum of squares for regression with no intercept
  } else {
    SSM <- sum(mod$fitted.values^2)
  }

  # sum of squared residuals
  SSE <- sum(mod$residuals^2)

  # degrees of freedom for regression with intercept
  if(colnames(mod$x)[1] == "intercept") {
    DFM <- mod$p - 1
  # degrees of freedom for regression with no intercept
  } else {
    DFM <- mod$p
  }

  DFE <- mod$df

  # F-statistic
  MSM <- SSM / DFM
  MSE <- SSE / DFE
  Fstat <- MSM / MSE

  # p-value
  pval <- pf(Fstat, df1 = DFM, df2 = DFE, lower.tail = FALSE)

  return(list(`F-statistic` = Fstat,
              df1 = DFM,
              df2 = DFE,
              `p-value` = pval))

}
