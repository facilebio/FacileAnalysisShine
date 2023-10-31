# A place for random utility functions

#' A wrapper to bscols that suppresswarnings.
#'
#' I am using bscols to create multiple rows, and in this case it always
#' throws a warning, which I want to suppress
#'
#' @noRd
#' @importFrom crosstalk bscols
bscols. <- function(..., suppress_warning = TRUE) {
  wrap. <- if (suppress_warning) suppressWarnings else identity
  wrap.(bscols(...))
}

#' Convenience wrapper to require specified packages
#'
#' @noRd
#' @param pkg A character vector of packages to require
#' @param quietly defaults to true
#' @param ... passed into [requireNamespace()]
reqpkg <- function(pkg, quietly = TRUE, ...) {
  assert_character(pkg)
  for (p in pkg) {
    if (!requireNamespace(p, ..., quietly = quietly)) {
      stop("'", p, "' package required, please install it.", call. = FALSE)
    }
  }
}

#' Create an empty plot
#' https://stackoverflow.com/a/56788210
#' @noRd
#' @export
empty_plot <- function(title = NULL) {
  plotly::plotly_empty(type = "scatter", mode = "markers") |> 
    plotly::config(displayModeBar = FALSE) |> 
    plotly::layout(title = list(text = title, yref = "paper", y = 0.5))
} 
