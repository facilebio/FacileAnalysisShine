#' @noRd
#' @export
initialized.FacilePcaAnalysisResult <- function(x, ...) {
  stat.table <- tidy(x)
  is.data.frame(stat.table) &&
    nrow(stat.table) == nrow(samples(x))
}

# Interactivity and vizualization over FacilePcaAnalysisResult =================

#' @noRd
#' @export
shine.FacilePcaAnalysisResult <- function(x, dims = 2, user = Sys.getenv("USER"),
                                          title = "PCA Results",
                                          viewer = "browser", ...) {
  frunGadgetServer(fpcaViewServer, fpcaViewUI, x, pcares = x, title = title,
                   viewer = viewer, ...)
}
