# Print method dor Penrose.dist
#'
#' Prints an object of class "Penrose.dist"
#'
#' @param x an object of class \code{"Penrose.dist"}
#'
#' @examples
#' data(skulls)
#' res.Penrose <- Penrose.dist(skulls, Period)
#' print(res.Penrose)
#'
#' @export print.Penrose.dist
print.Penrose.dist <- function(x) {
  cl <- oldClass(x)
  oldClass(x) <- cl[cl != "Penrose.dist"]
  NextMethod("print")
  invisible(x)
}
