#' @title Multiple two-sample Levene tests for the comparison of variation in
#'        multivariate data
#'
#' @author Jorge Navarro Alberto, \email{ganava4@@gmail.com}
#'
#' @references Manly, B.F.J., Navarro Alberto, J.A. and Gerow, K. (2024)
#' \emph{Multivariate Statistical Methods. A Primer}. 5th Edn. CRC Press.
#'
#' @description
#' Performs multiple two-sample Levene tests, based on two-sample t-tests
#' applied to absolute differences around medians for more than one response
#' vector, with corrected significance levels using any of the adjustment
#' methods for multiple comparisons offered by \code{\link[stats]{p.adjust}}.
#' This function includes the argument \code{alternative =} useful to specify
#' the type of alternative, either one-sided (lower-/ upper-tail) or two-sided.
#'
#' @param x a data frame with one two-level factor and \emph{p} response
#' variables.
#' @param group two-level factor defining groups. It must be one of the columns
#' in \code{x}.
#' @param level1 a character string identifying Sample 1. The string must be one
#' of the factor levels in \code{group}.
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of \code{"two.sided"} (default), \code{"greater"} or
#' \code{"less"}. You can specify just the initial letter.
#' @param var.equal a logical variable indicating whether to treat the two
#' variances as being equal. If \code{TRUE} then the pooled variance is used to
#' estimate the variance otherwise the Welch (or Satterthwaite) approximation to
#' the degrees of freedom is used.
#' @param P.adjust p-value correction method, a character string. Can be
#' abbreviated. See 'Details'.
#'
#' @details
#' This function focuses on the univariate Levene test for the comparison of
#' mean values for two samples, when more than one variable is involved in the
#' data analysis, so that type one error rates ("false significances") in the
#' series of Levene tests are adjusted according to the number of response
#' variables analyzed. The pairwise comparisons between the two levels in
#' \code{group} with corrections for multiple testing are made over more than
#' one response vector.
#'
#' The methods implemented in \code{P.adjust} are the same as those contained in
#' the \code{p.adjust.methods}: \code{"bonferroni"}, \code{"holm"},
#' \code{"hochberg"}, \code{"hommel"}, \code{"BH"}, (Benjamini-Hochberg) or its
#' alias \code{"fdr"} (False Discovery Rate), and \code{"BY"} (Benjamini &
#' Yekutieli). The default pass-through option (\code{"none"}) is also included.
#'
#' @return Returns an object of class \code{"Levene.t"}, a list containing the
#' following components:
#' \tabular{lllllllll}{
#'    \code{ name} \tab A character string describing the function. \cr
#'    \code{medians} \tab A list containing two vectors of length \emph{p},
#'    being \emph{p} the number of response variables. \code{medians1} and
#'    \code{medians2} store the medians for samples 1 (corresponding to
#'    \code{level1}) and 2, respectively.  \cr
#'    \code{absdev.median} \tab a list containing two data frames,
#'    \code{abs.dev.median1} and \code{abs.dev.median2}, corresponding to the
#'    absolute deviation around sample medians 1 and 2, respectively \cr
#'    \code{means.absdev} \tab a list containing two vectors of length \emph{p},
#'    (\code{means.absdev1} and \code{means.absdev1}), corresponding to the
#'    mean absolute deviations around medians for variables 1,...,\emph{p}, in
#'    samples 1 and 2, respectively. \cr
#'    \code{vars.absdev} \tab a list containing two vectors of length \emph{p},
#'    (\code{vars.absdev1} and \code{vars.absdev1}), corresponding to the
#'    variances of absolute deviations around medians for variables 1,...,
#'    \emph{p}, in samples 1 and 2, respectively. \cr
#'    \code{t.list} \tab A list containing \emph{p} vectors of length 3, each
#'    vector containing the t-statistic, the degrees of freedom and the adjusted
#'    p-value for the test, respectively. \cr
#'    \code{alternative} \tab a character string specifying the alternative
#'    hypothesis chosen. \cr
#'    \code{var.equal} \tab a logical variable indicating whether the two
#'    variances were treated as being equal \code{TRUE} or not \code{FALSE}.
#'    \cr
#'    \code{P.adjust} \tab a character string indicating the correction method
#'    chosen \cr
#'    \code{group} \tab a character string specifying the name of the two-level
#'    factor defining groups. \cr
#'    \code{levels.group} \tab a vector of length two showing the two levels in
#'    factor \code{group}. \cr
#'    \code{data.name} \tab a character string giving the name of the data. \cr
#'    \code{data} \tab the data frame analyzed. \cr
#' }
#'
#' The extractor function \code{\link[smstests]{summary.Levene.t}} returns an
#' annotated output of the Levene tests (or, equivalently, the two-sample
#' t-tests applied to the absolute differences around medians).
#'
#' @examples
#' data(sparrows)
#' res.Levene.t <- Levenetests2s.mv(sparrows, Survivorship, "S",
#'                                 alternative = "less", var.equal = TRUE,
#'                                 P.adjust = "bonferroni")
#' summary.Levene.t(res.Levene.t)
#'
#' @importFrom stats p.adjust t.test median var
#' @export Levenetests2s.mv
Levenetests2s.mv <- function(x, group, level1, alternative = "two.sided",
                             var.equal = FALSE, P.adjust = "none")
{
  group <- deparse(substitute(group))
  fac <- x[, names(x) %in% c(group)]
  fac <- droplevels(fac)
  levels.group <- as.character(c(unique(fac[fac == level1]),
                                 unique(fac[fac != level1])))
  df <- x[, !names(x) %in% c(group)]
  # Levene's tests based on absolute differences around means using t-tests
  matlevel1 <- df[fac == levels.group[1], ]
  matlevel2 <- df[fac == levels.group[2], ]
  vecmedian1 <- apply(matlevel1, 2, median)
  names(vecmedian1) <- names(df)
  vecmedian2 <- apply(matlevel2, 2, median)
  names(vecmedian2) <- names(df)
  medians <- list(medians1 = vecmedian1, medians2 = vecmedian2)
  matabsdev1 <- abs(matlevel1 -
                      matrix(rep(vecmedian1, nrow(matlevel1)),
                             nrow = nrow(matlevel1), byrow = TRUE))
  matabsdev2 <- abs(matlevel2 -
                      matrix(rep(vecmedian2, nrow(matlevel2)),
                             nrow = nrow(matlevel2), byrow = TRUE))
  absdev.median <- list(abs.dev.median1 = as.data.frame(matabsdev1),
                        abs.dev.median2 = as.data.frame(matabsdev2))
  matabsdev.all <- rbind(matabsdev1, matabsdev2)
  matabsdev.all <- as.data.frame(cbind(fac, matabsdev.all))
  names(matabsdev.all)[2:(ncol(df)+1)] <- names(df)
  means.absdev1 <- colMeans(matabsdev1)
  means.absdev2 <- colMeans(matabsdev2)
  means.absdev <- list(means.absdev1 = means.absdev1,
                       means.absdev2 = means.absdev2)
  vars.absdev1 <-  apply(matabsdev1, 2, var)
  vars.absdev2 <-  apply(matabsdev2, 2, var)
  vars.absdev <- list(vars.absdev1 = vars.absdev1,
                      vars.absdev2 = vars.absdev2)
  t.list <- vector(mode = 'list', length = ncol(df))
  P.value.adj <- rep(NULL, ncol(df))
  for (i in 1:ncol(df)) {
    tstat <- t.test(matabsdev.all[fac == levels.group[1], (i + 1)],
                    matabsdev.all[fac == levels.group[2], (i + 1)],
                    alternative = alternative,
                    var.equal = var.equal)$statistic
    df.t <- t.test(matabsdev.all[fac == levels.group[1], (i + 1)],
                   matabsdev.all[fac == levels.group[2], (i + 1)],
                   alternative = alternative,
                   var.equal = var.equal)$parameter
    P.value <- t.test(matabsdev.all[fac == levels.group[1], (i + 1)],
                      matabsdev.all[fac == levels.group[2], (i + 1)],
                      alternative = alternative,
                      var.equal = var.equal)$p.value
    P.value.adj[i] <- P.value
    t.list[[i]] <- c(tstat, df.t, P.value = P.value)
  }
  if (P.adjust != "none") {
    for (i in 1:ncol(df)) {
      t.list[[i]]["P.value"] <- p.adjust(P.value.adj, method = P.adjust)[i]
    }
  }
  names(t.list) <- names(df)
  results.Lev <- list(name = "Multiple Levene tests for multivariate data",
                      medians = medians, absdev.median = absdev.median,
                      means.absdev = means.absdev,
                      vars.absdev = vars.absdev, t.list = t.list,
                      alternative = alternative, var.equal = var.equal,
                      P.adjust = P.adjust, group = group,
                      levels.group = levels.group,
                      data.name = deparse(substitute(x)), data = x)
  class(results.Lev) <- "Levene.t"
  return(results.Lev)
}
