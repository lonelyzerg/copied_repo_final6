#' @title Residual Plots for Linear Regression Model
#'
#' @description Obtain diagnostic residual plots for the linear regression model
#' estimated by the function \code{\link{regress}}. Four plots (selectable by
#' \code{which}) are available: (1) a plot of residuals against fitted values,
#' (2) a Normal Q-Q plot of residuals, (3) a histogram of residuals and (4) a
#' density plot of residuals. By default, the first three are provided in a
#' single figure.
#'
#' @param mod An object of class \code{"linreg"} returned by the function
#' \code{\link{regress}}.
#' @param which A number (1, 2, 3 or 4) indicating which plot is required.
#' If \code{NULL} (the default), the first three plots are returned in a single
#' figure. See the Description above for which number corresponds to which plot.
#'
#' @export
#' @examples
#' mod1 <- regress(mtcars$mpg, mtcars$wt)
#' residualPlots(mod1)
#'
#' mod2 <- regress("mpg", c("wt", "disp", "hp"), data = mtcars)
#' residualPlots(mod2)
#' residualPlots(mod2, 4)
residualPlots <- function(mod, which = NULL) {

  # check if mod is of class linreg
  if(class(mod) != "linreg") {
    stop("mod has to be a `linreg` object returned by function `regress`")
  }

  # check if which is correctly specified
  if(!is.null(which)) {
    if(!(which %in% 1:4)) {
      stop("`which` has to be one of: NULL, 1, 2, 3, 4")
    }
  }

  # residuals vs fitted-values
  p1 <- ggplot2::ggplot(mapping = ggplot2::aes(x = mod$fitted.values, y = mod$residuals)) +
    ggplot2::geom_point() +
    # add horizontal line at zero
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    # add smoothing line to see the relationship
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Fitted values", y = "Residuals",
                  title = "Residuals vs Fitted")

  # qq-plot of residuals
  p2 <- ggplot2::ggplot(mapping = ggplot2::aes(sample = mod$residuals)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line() +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
                  title = "Normal Q-Q Plot of Residuals")

  # histogram of residuals
  p3 <- ggplot2::ggplot(mapping = ggplot2::aes(x = mod$residuals)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Residuals", y = "Count", title = "Histogram of Residuals")

  # density plot of residuals
  p4 <- ggplot2::ggplot(mapping = ggplot2::aes(x = mod$residuals)) +
    ggplot2::geom_density() +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Residuals", y = "Density",
                  title = "Density plot of Residuals")

  # if plot is not specified, return the first 3 plots in one figure
  if(is.null(which)) {
    ggpubr::ggarrange(p1, # first row with scatterplot
      ggpubr::ggarrange(p2, p3, ncol = 2), # second row with qqplot and histogram
      nrow = 2)
  # otherwise return the specified plot
  } else if(which == 1) {
    p1
  } else if(which == 2) {
    p2
  } else if(which == 3) {
    p3
  } else if(which == 4) {
    p4
  }

}
