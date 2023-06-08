#' @noRd
#' @export
is_ttest.ReactiveFacileAnalysisResult <- function(x, ...) {
  is_ttest(faro(x))
}

#' @noRd
#' @export
is_anova.ReactiveFacileAnalysisResult <- function(x, ...) {
  is_anova(faro(x))
}