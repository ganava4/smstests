#' Print method for a BoxM.F object
#'
#' Prints an object of class "BoxM.F", , produced by
#' \code{\link[smstests]{BoxM.F}}
#'
#' @param x an object of class \code{"BoxM.F"}
#'
#' @examples
#' data(skulls)
#' resBoxM.F <- BoxM.F(skulls, Period)
#' print(resBoxM.F)
#'
#' @export print.BoxM.F
# Print method
print.BoxM.F <- function(x) {
  cl <- oldClass(x)
  oldClass(x) <- cl[cl != "BoxM.F"]
  NextMethod("print")
  invisible(x)
}
