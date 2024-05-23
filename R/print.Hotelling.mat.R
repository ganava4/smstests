#' Print method for a "Hotelling.mat" object
#'
#' Prints an object of class "T2", produced by
#' \code{\link[smstests]{Hotelling.mat}}
#'
#' @param x an object of class \code{"T2"}
#'
#' @examples
#' data(sparrows)
#' results.T2 <- Hotelling.mat(sparrows, group = Survivorship, level1 = "S")
#' print(results.T2)
#'
#' @export print.T2
# Print method
print.T2 <- function(x) {
  cl <- oldClass(x)
  oldClass(x) <- cl[cl != "T2"]
  NextMethod("print")
  invisible(x)
}
