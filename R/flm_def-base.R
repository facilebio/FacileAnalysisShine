#' @noRd
#' @export
initialized.FacileLinearModelDefinition <- function(x, ...) {
  !is(x, "FacileFailedModelDefinition") && !is(x, "IncompleteModelDefintion")
}

#' @export
#' @rdname flm_def
#' @importFrom FacileShine active_samples
#' @importFrom FacileViz unselected
flm_def.ReactiveFacileDataStore <- function(x, covariate, numer = NULL,
                                            denom = NULL, batch = NULL,
                                            block = NULL,
                                            on_missing = c("warning", "error"),
                                            ..., samples = active_samples(x),
                                            custom_key = user(x)) {
  samples <- collect(samples, n = Inf)
  if (unselected(numer)) numer <- NULL
  if (unselected(denom)) denom <- NULL
  if (unselected(batch)) batch <- NULL
  if (unselected(block)) block <- NULL
  
  flm_def(samples, covariate = covariate, numer = numer, denom = denom,
          batch = batch, block = block, on_missing = on_missing,
          custom_key = custom_key, ...)
}
