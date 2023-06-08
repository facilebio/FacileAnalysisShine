#' @noRd
#' @export
initialized.FacileLinearModelDefinition <- function(x, ...) {
  !is(x, "FacileFailedModelDefinition") && !is(x, "IncompleteModelDefintion")
}
