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
#' @examples
#' if (interactive()) {
#' 
#' afds <- FacileData::an_fds()
#' dge.ttest <- afds |>
#'   flm_def("cell_abbrev", numer = "CNT", denom = "IC") |> 
#'   fdge(method = "voom")
#' shine(dge.ttest)
#' 
#' # Limit view on subset of genes
#' fsome <- tidy(dge.ttest) |> dplyr::sample_n(20)
#' shine(dge.ttest, features = fsome)
#' 
#' dge.anova <- afds |>
#'   flm_def("cell_abbrev") |> 
#'   fdge(method = "voom")
#' shine(dge.anova)
#' }
shine.FacileDgeAnalysisResult <- function(x, features = NULL, 
                                          user = Sys.getenv("USER"),
                                          title = "Differential Expression Results",
                                          viewer = "browser", 
                                          ...) {
  frunGadgetServer(fdgeViewServer, fdgeViewUI, x, dgeres = x, title = title,
                   viewer = viewer, feature_subset = features, ...)
}
