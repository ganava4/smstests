#' Print method for a "Levene.t" object
#'
#' Prints an object of class "Levene.t", , produced by
#' \code{\link[smstests]{Levenetests2s.mv}}
#'
#' @param x an object of class \code{"Levene.t"}
#'
#' @examples
#' data(sparrows)
#' res.Levene.t <- Levenetests2s.mv(sparrows, Survivorship, "S",
#'                                alternative = "less", var.equal = TRUE,
#'                                P.adjust = "bonferroni")
#' print(res.Levene.t)

#' @export print.Levene.t

print.Levene.t <- function(x) {
  cl <- oldClass(x)
  oldClass(x) <- cl[cl != "Levene.t"]
  NextMethod("print")
  invisible(x)
}
