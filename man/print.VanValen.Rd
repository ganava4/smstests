% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.VanValen.R
\name{print.VanValen}
\alias{print.VanValen}
\title{Print method for a "VanValen" object}
\usage{
\method{print}{VanValen}(x)
}
\arguments{
\item{x}{an object of class \code{"VanValen"}}
}
\description{
Prints an object of class "VanValen", produced by
\code{\link[smstests]{VanValen}}
}
\examples{
data(sparrows)
res.VanValen <- VanValen(sparrows, "Survivorship", "S",
                         alternative = "less", var.equal = TRUE)
print(res.VanValen)

}
