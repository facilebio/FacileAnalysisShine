#' @noRd
#' @export
initialized.FacileDgeAnalysisResult <- function(x, ...) {
  stat.table <- tidy(x)
  is.data.frame(stat.table) &&
    is.numeric(stat.table[["pval"]]) &&
    is.numeric(stat.table[["padj"]])
}

#' @noRd
#' @export
shine.FacileDgeAnalysisResult <- function(x, user = Sys.getenv("USER"),
                                          title = "Differential Expression Results",
                                          viewer = "browser", ...) {
  # frunGadget(fdgeView, fdgeViewUI, x, dgeres = x, title = title,
  #            viewer = viewer, ...)
  frunGadgetServer(fdgeViewServer, fdgeViewUI, x, dgeres = x, title = title,
                   viewer = viewer, ...)
}
