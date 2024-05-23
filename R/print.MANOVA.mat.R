#' Print method for a "MANOVA.mat" object
#'
#' Prints an object of class "MANOVA.mat", produced by
#' \code{\link[smstests]{MANOVA.mat}}
#'
#' @param x an object of class \code{"MANOVA.mat"}
#'
#' @examples
#' data(skulls)
#' res.MANOVA <- MANOVA.mat(skulls, group = Period)
#' # Brief output
#' print(res.MANOVA)
#'
#' @export print.MANOVA.mat
print.MANOVA.mat <- function(x) {
  cl <- oldClass(x)
  oldClass(x) <- cl[cl != "MANOVA.mat"]
  NextMethod("print")
  invisible(x)
}
