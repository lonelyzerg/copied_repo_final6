#' @title Results of Linear Regression Model
#'
#' @description Obtain summary results of a linear regression model fitted by
#' the function \code{\link{regress}}.
#'
#' @param mod An object of class \code{"linreg"} returned by the function
#' \code{\link{regress}}.
#' @param print A \code{logical}. If \code{TRUE} (default), the summary of the
#' results is printed. If \code{FALSE}, printing is suppressed. Use
#' \code{print = FALSE} if you want to assign the results to an object without
#' printing.
#'
#' @return The function \code{results} computes and prints (if \code{print = TRUE})
#' a list of summary statistics of the fitted linear regression model and returns
#' a list containing the following components:
#' \describe{
#'      \item{results}{A matrix containing columns \code{Estimate}, \code{Std. Error},
#'      \code{t value} and \code{Pr(>|t|)} with the estimated model coefficients,
#'      their standard errors, corresponding t statistics and p-values for the
#'      significance of the estimated coefficients, respectively}
#'      \item{resid.std.error}{Estimated residual standard error}
#'      \item{mspe}{Mean Square Prediction Error of the estimated model}
#'      \item{Rsquared}{R-squared of the model}
#'      \item{AdjRsquared}{Adjusted R-squared of the model}
#'      \item{Fstatistic}{F-statistic for the overall significance of the model}
#'      \item{df1}{Numerator degrees of freedom for the F-test of overall
#'      significance}
#'      \item{df2}{Denominator degrees of freedom for the F-test of overall
#'      significance}
#'      \item{pvalue}{p-value for the F-test of overall significance}
#' }
#' @export
#' @examples
#' mod1 <- regress(mtcars$mpg, mtcars$wt)
#' results(mod1)
#'
#' mod2 <- regress("mpg", c("wt", "disp", "hp"), data = mtcars)
#' results(mod2)
#'
#' # assign results without printing
#' mod2_res <- results(mod2, print = FALSE)
results <- function(mod, print = TRUE) {

  # check if mod is of class linreg
  if(class(mod) != "linreg") {
    stop("mod has to be a `linreg` object returned by function `regress`")
  }

  # standard errors of the coefficients
  beta.se <- sqrt(diag(mod$beta.variance))
  # t values for the significance of the coefficients
  tval <- mod$coefficients / beta.se
  # p-values for the significance of the coefficients
  pval <- 2*pt(abs(tval), df = mod$df, lower.tail = FALSE)

  # results table
  res <- cbind(mod$coefficients, beta.se, tval, pval)
  colnames(res) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

  # calculate MSPE
  mspe <- MSPE(mod)

  # calculate R-squared
  Rsq <- Rsquared(mod)
  # calculate adjusted R-squared
  AdjRsq <- RsquaredAdj(mod)

  # F-test
  Ftestres <- Ftest(mod)

  # print results if print is set to TRUE
  if(print == TRUE) {
    cat("Coefficients:\n")
    print(res)
    cat("\n")
    cat(paste("Residual standard error:", signif(sqrt(mod$error.variance), 5),
              "on", mod$df, "degrees of freedom\n"))
    cat(paste("Mean Square Prediction Error (MSPE):", signif(mspe, 5), "\n"))
    cat(paste("R-squared:", signif(Rsq, 5), "\n"))
    cat(paste("Adjusted R-squared:", signif(AdjRsq, 5), "\n"))
    cat(paste("F-statistic:", signif(Ftestres$`F-statistic`, 5), "on",
              signif(Ftestres$df1, 5), "and", signif(Ftestres$df2, 5),
              "DF, p-value:", signif(Ftestres$`p-value`, 5)))
  }

  # return all results in a list but not print it
  invisible(list(results = res,
                 resid.std.error = sqrt(mod$error.variance),
                 mspe = mspe,
                 Rsquared = Rsq,
                 AdjRsquared = AdjRsq,
                 Fstatistic = Ftestres$`F-statistic`,
                 df1 = Ftestres$df1,
                 df2 = Ftestres$df2,
                 pvalue = Ftestres$`p-value`))

}
