#' @title R-squared of Linear Regression Model
#'
#' @description Obtain R-squared of a linear regression model fitted by the
#' function \code{\link{regress}}.
#'
#' @param mod An object of class \code{"linreg"} returned by the function
#' \code{\link{regress}}.
#'
#' @return R-squared of the linear regression model \code{mod}.
#' @export
#' @examples
#' mod1 <- regress(mtcars$mpg, mtcars$wt)
#' Rsquared(mod1)
#'
#' mod2 <- regress("mpg", c("wt", "disp", "hp"), data = mtcars)
#' Rsquared(mod2)
Rsquared <- function(mod) {

  # check if mod is of class linreg
  if(class(mod) != "linreg") {
    stop("mod has to be a `linreg` object returned by function `regress`")
  }

  # sum of squared residuals
  SSE <- sum(mod$residuals^2)
  # total sum of squares for regression with intercept
  if(colnames(mod$x)[1] == "intercept") {
    SST <- sum((mod$y - mean(mod$y))^2)
  # total sum of squares for regression with no intercept
  } else {
    SST <- sum(mod$y^2)
  }

  # R-squared
  Rsq <- 1 - (SSE/mod$n) / (SST/mod$n)

  return(Rsq)

}
