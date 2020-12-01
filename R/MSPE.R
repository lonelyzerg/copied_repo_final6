#' @title Mean Square Prediction Error
#'
#' @description Obtain Mean Square Prediction Error (MSPE) of a linear
#' regression model fitted by the function \code{\link{regress}}.
#'
#' @param mod An object of class \code{"linreg"} returned by the function
#' \code{\link{regress}}.
#'
#' @return MSPE of the linear regression model \code{mod}.
#' @export
#' @examples
#' mod1 <- regress(mtcars$mpg, mtcars$wt)
#' MSPE(mod1)
#'
#' mod2 <- regress("mpg", c("wt", "disp", "hp"), data = mtcars)
#' MSPE(mod2)
MSPE <- function(mod) {

  # check if mod is of class linreg
  if(class(mod) != "linreg") {
    stop("mod has to be a `linreg` object returned by function `regress`")
  }

  # MSPE
  return(mean(mod$residuals^2))

}
