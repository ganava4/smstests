#' Print method for a "ttests" object
#'
#' Prints an object of class "ttests", produced by
#' \code{\link[smstests]{ttests2s.mv}}
#'
#' @examples
#' data(sparrows)
#' ttests.sparrows <- ttests2s.mv(sparrows, group = Survivorship, level1 = "S",
#'                               var.equal = TRUE, P.adjust = "holm")
#' print(ttests.sparrows)
#'
#' @param x an object of class \code{"ttests"}
#' @export print.ttests
print.ttests <- function(x) {
  cl <- oldClass(x)
  oldClass(x) <- cl[cl != "ttests"]
  NextMethod("print")
  invisible(x)
}
