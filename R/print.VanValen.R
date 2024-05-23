#' Print method for a "VanValen" object
#'
#' Prints an object of class "VanValen", produced by
#' \code{\link[smstests]{VanValen}}
#'
#' @param x an object of class \code{"VanValen"}
#'
#' @examples
#' data(sparrows)
#' res.VanValen <- VanValen(sparrows, "Survivorship", "S",
#'                          alternative = "less", var.equal = TRUE)
#' print(res.VanValen)
#'
#' @export print.VanValen
#'
print.VanValen <- function(x) {
  cl <- oldClass(x)
  oldClass(x) <- cl[cl != "VanValen"]
  NextMethod("print")
  invisible(x)
}
