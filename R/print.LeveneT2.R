#' Print method for a "LeveneT2" object
#'
#' Prints an object of class "LeveneT2"
#'
#' @param x an object of class \code{"LeveneT2"}
#'
#' @examples
#' data(sparrows)
#' LeveneT2.sparrows <- LeveneT2(sparrows, group = Survivorship, level1 = "S",
#'                               var.equal = TRUE)
#' print(LeveneT2.sparrows)
#'
#' @export print.LeveneT2
# Print method
print.LeveneT2 <- function(x) {
  cl <- oldClass(x)
  oldClass(x) <- cl[cl != "LeveneT2"]
  NextMethod("print")
  invisible(x)
}
