#' @title Adjusted R-squared of Linear Regression Model
#'
#' @description Obtain Adjusted R-squared of a linear regression model fitted
#' by the function \code{\link{regress}}.
#'
#' @param mod An object of class \code{"linreg"} returned by the function
#' \code{\link{regress}}.
#'
#' @return Adjusted R-squared of the linear regression model \code{mod}.
#' @export
#' @examples
#' mod1 <- regress(mtcars$mpg, mtcars$wt)
#' RsquaredAdj(mod1)
#'
#' mod2 <- regress("mpg", c("wt", "disp", "hp"), data = mtcars)
#' RsquaredAdj(mod2)
RsquaredAdj <- function(mod) {

  # check if mod is of class linreg
  if(class(mod) != "linreg") {
    stop("mod has to be a `linreg` object returned by function `regress`")
  }

  # Adjusted R-squared for regression with intercept
  if(colnames(mod$x)[1] == "intercept") {
    # total sum of squares
    SST <- sum((mod$y - mean(mod$y))^2)
    AdjRsq <- 1 - mod$error.variance / (SST / (mod$n - 1))
  # Adjusted R-squared for regression with no intercept
  } else {
    Rsq <- Rsquared(mod)
    AdjRsq <- 1 - (1 - Rsq) * mod$n / (mod$df)
  }

  return(AdjRsq)

}
