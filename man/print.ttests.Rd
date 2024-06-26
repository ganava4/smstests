% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.ttests2s.mv.R
\name{print.ttests}
\alias{print.ttests}
\title{Print method for a "ttests" object}
\usage{
\method{print}{ttests}(x)
}
\arguments{
\item{x}{an object of class \code{"ttests"}}
}
\description{
Prints an object of class "ttests", produced by
\code{\link[smstests]{ttests2s.mv}}
}
\examples{
data(sparrows)
ttests.sparrows <- ttests2s.mv(sparrows, group = Survivorship, level1 = "S",
                              var.equal = TRUE, P.adjust = "holm")
print(ttests.sparrows)

}
