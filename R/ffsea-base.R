#' @noRd
#' @export
initialized.FacileFseaAnalysisResult <- function(x, ...) {
  is(result(x), "SparrowResult")
}

#' @noRd
#' @export
#' @examples
#' gdb <- sparrow::getMSigGeneSetDb("h", "human", id.type = "entrez")
#' dge.ttest <- FacileData::exampleFacileDataSet() |>
#'   FacileData::filter_samples(indication == "CRC") |>
#'   flm_def(covariate = "sample_type", numer = "tumor", denom = "normal",
#'           batch = "sex") |>
#'   fdge(method = "voom")
#' gsea.ttest <- ffsea(dge.ttest, gdb, methods = c("cameraPR", "fgsea"))
#'
#' viz(gsea.ttest, name = "HALLMARK_ANGIOGENESIS")
#' if (interactive()) {
#'   shine(gsea.ttest)
#' }
shine.FacileFseaAnalysisResult <- function(x, user = Sys.getenv("USER"),
                                           title = "FSEA Results",
                                           viewer = "browser", ...) {
  frunGadgetServer(ffseaViewServer, ffseaViewUI, x, aresult = x, title = title,
                   viewer = viewer, ...)
}
