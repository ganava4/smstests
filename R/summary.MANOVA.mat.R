#' Summarize a one-way MANOVA
#'
#' Summarizes the results produced by the \code{MANOVA.mat} function
#'
#' @param x an object of class MANOVA.mat
#' @param test The name of the test statistic to be used (the four tests
#' implemented in \code{summary.manova}). Pillai's test is the default. Partial
#' matching is used so the name can be abbreviated.
#' @param long a logical variable indicating whether a long output is desired
#' (\code{TRUE}) or not (\code{FALSE}, the default)
#'
#' @examples
#' data(skulls)
#' res.MANOVA <- MANOVA.mat(skulls, group = Period)
#' # Long output, Wilks' test
#' summary.MANOVA.mat(res.MANOVA, test = "Wilks", long = TRUE)
#'
#' @export summary.MANOVA.mat
summary.MANOVA.mat <- function(x, test = c("Pillai", "Wilks", "Hotelling-Lawley",
                                           "Roy"), long = FALSE)
{
  stopifnot(inherits(x, "MANOVA.mat"))
  cat(" One-factor Multivariate Analysis of Variance with extra matrix info\n")
  #
  cat("\n Data:", x$data.name, "\n")
  cat(" Variables:", x$variables, "\n")
  cat(" Factor:", x$group, "\n")
  cat(" Levels:", x$x.mnv$xlevels[[1]], "\n")
  if (long == TRUE) {
    cat("\nBetween-Sample Matrix of Sum of Squares and Crossed Products, B\n")
    print(x$B)
    cat("\nWithin Sample Matrix of Total Sum of Squares and Crossed Products, W\n")
    print(x$W)
    cat("\nTotal Sample Matrix of Sum of Squares and Crossed Products, T\n")
    print(x$T)
  }
  cat("\n                      One-Way MANOVA\n")
  summ.MANOVA <- summary(x$x.mnv, test = test)
  print(summ.MANOVA)
}
