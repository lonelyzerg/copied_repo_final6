#' @title Estimating Linear Regression Model
#'
#' @description Estimate the coefficient vector \eqn{\beta} for a linear
#' regression model \eqn{y = X'\beta + \epsilon} using ordinary least squares
#' and obtain the variance-covariance matrix of the estimated \eqn{\beta} and the
#' estimate of the residual variance \eqn{\sigma^2}.
#'
#' @param y A numeric \code{vector} containing the sample data of the dependent
#' variable or, if argument \code{data} is specified, a character \code{vector}
#' of length 1 with the name of the dependent variable.
#' @param x A numeric \code{vector} or \code{matrix} containing the sample data
#' of the independent variables or, if argument \code{data} is specified, a
#' character \code{vector} with the names of the independent variables.
#' @param data An optional \code{data.frame} or \code{matrix} with column names,
#' containing the variables specified in \code{y} and \code{x}.
#' @param intercept A \code{logical}. If \code{TRUE} (default), a column of 1s
#' is added to \code{x} and the intercept coefficient is estimated along with
#' the slope coefficients.
#'
#' @return \code{regress} returns an object of class \code{"linreg"}.
#'
#' The functions \code{\link{results}} and \code{\link{residualPlots}} are used to
#' obtain and print a summary and analysis of the regression results and plot the
#' diagnostic plots of the model residuals. The functions \code{\link{coefCI}},
#' \code{\link{Ftest}}, \code{\link{MSPE}}, \code{\link{Rsquared}} and
#' \code{\link{RsquaredAdj}} extract various useful statistics of the values
#' returned by \code{regress}.
#'
#' An object of class \code{"linreg"} is a \code{list} containing the
#' following components:
#' \describe{
#'      \item{coefficients}{Estimated coefficients of the linear regression model}
#'      \item{error.variance}{Estimated residual variance}
#'      \item{beta.variance}{Estimated variance-covariance matrix of the
#'      estimated coefficients}
#'      \item{residuals}{Model residuals, that is response minus fitted values}
#'      \item{fitted.values}{The fitted values}
#'      \item{y}{The response used}
#'      \item{x}{The model matrix used}
#'      \item{n}{Number of observations used}
#'      \item{p}{Number of parameters estimated}
#'      \item{df}{The residual degrees of freedom}
#' }
#' @export
#' @examples
#' mod1 <- regress(mtcars$mpg, mtcars$wt)
#' mod1$coefficients
#' mod1$error.variance
#' mod1$beta.variance
#'
#' # alternative way to specify the same model
#' mod2 <- regress("mpg", "wt", data = mtcars)
#' mod2$coefficients
#'
#' mod3 <- regress("mpg", c("wt", "disp", "hp"), data = mtcars)
#' mod3$coefficients
#' mod3$beta.variance
#'
#' # omitting intercept
#' mod4 <- regress("mpg", c("wt", "disp", "hp"), data = mtcars, intercept = FALSE)
#' mod4$coefficients
#' mod4$beta.variance
regress <- function(y, x, data = NULL, intercept = TRUE) {

  # if data is specified
  if(!is.null(data)) {
    # make sure data is a data.frame or matrix
    if(!is.data.frame(data) & !is.matrix(data)) {
      stop("data has to be a data.frame or a matrix or NULL")
    }
    # make sure data has column names
    if(is.null(colnames(data))) {
      stop("data has no column names")
    }
    # make sure y is a character vector of length 1
    if(!is.character(y) | length(y) != 1) {
      stop("y has to be a character vector of length 1")
    }
    # make sure x is a character vector
    if(!is.character(x)) {
      stop("x has to be a character vector")
    }
    # check if data contains variables with names given in x and y
    if(!all(x %in% colnames(data)) | !(y %in% colnames(data))) {
      if(!(y %in% colnames(data))) {
        stop(paste("there is no variable", y, "in the data"))
      } else{
        ind <- which(!(x %in% colnames(data)))[1]
        stop(paste("there is no variable", x[ind], "in the data"))
      }
    }
    # extract variables from the data frame by name
    y <- data[, y]
    x <- data[, x]
  }

  # make sure data formats are appropriate
  y <- as.vector(y)
  x <- as.matrix(x)

  # make sure all variables are numeric
  if(!is.numeric(y) | !is.numeric(x)) {
    stop("all variables must be numeric")
  }

  # make sure y and x have the same number of observations
  if(length(y) != nrow(x)) {
    stop("number of observations in y and x are different")
  }

  # remove observations with missing values if there are any
  if(any(is.na(y)) | any(is.na(x))) {
    ind_missing <- which(!complete.cases(cbind(y, x)))
    y <- y[-ind_missing]
    x <- as.matrix(x[-ind_missing, ])
    warning(paste(length(ind_missing),
                  "observation(s) have been removed due to missing values"))
  }

  # if intercept needs to be estimated add a column of 1s to x
  if(intercept == TRUE) {
    intercept <- rep(1, nrow(x))
    x <- cbind(intercept, x)
  }

  # if x has missing column names, replace with "beta"
  if(is.null(colnames(x))) {
    colnames(x) <- "beta1"
  } else if(any(colnames(x) == "")) {
    ind <- which(colnames(x) == "")
    colnames(x)[ind] <- paste("beta", 1:length(ind), sep = "")
  }

  # number of observations
  n <- length(y)
  # number of parameters to estimate
  p <- dim(x)[2]
  # degrees of freedom
  df <- n - p

  # estimate beta
  beta.hat <- solve(t(x) %*% x) %*% t(x) %*% y

  # fitted values
  y.hat <- x %*% beta.hat

  # residuals
  resid <- y - y.hat

  # estimate residual variance (sigma^2)
  sigma2.hat <- as.numeric((1 / df) * t(resid) %*% resid)

  # estimate variance of the estimated beta coefficients
  var.beta <- sigma2.hat * solve(t(x) %*% x)

  # return all estimated values as a list of class "linreg"
  result <- list(coefficients = beta.hat,
                 error.variance = sigma2.hat,
                 beta.variance = var.beta,
                 residuals = resid,
                 fitted.values = y.hat,
                 y = y,
                 x = x,
                 n = n,
                 p = p,
                 df = df)
  class(result) <- "linreg"

  return(result)

}
