#' Gadget to run both DGE and GSEA
#'
#' @export
#' @examples
#' \dontrun{
#' gdb <- sparrow::getMSigGeneSetDb("h", "human", id.type = "entrez")
#' efds <- FacileData::exampleFacileDataSet()
#' xs <- FacileData::filter_samples(efds, indication == "CRC")
#' fdgeseaGadget(xs, gdb)
#' }
fdgeseaGadget <- function(x, gdb = NULL, title = "DGE and GSEA",
                          height = 800, width = 1000, viewer = "browser", ...) {
  assert_multi_class(x, c("FacileDataStore", "facile_frame"))
  assert_class(gdb, "GeneSetDb")
  frunGadgetServer(fDgeSeaAnalysisServer, fDgeSeaAnalysisUI, x,
                   gdb = reactive(gdb),
                   title = title, height = height, width = widh,
                   viewer = viewer, ...)
}

#' An analysis module that combines differential expression with GESA.
#'
#' This module enables the interactive configuration and execution of
#' "paired" differential expression and feature set enrichment.
#'
#' Although the focus of the analysis workflow in this package is based on
#' singular units of analysis, there are times like this where we almost always
#' want to perform two anlayses together, differential expression and GSEA
#' on the same contrast. This shiny-module presents an interface to both.
#'
#' @section Development Thougts:
#' This also gives us the opportunity to exercise different methods of
#' interaction between independant analysis results. In the DGE and GSEA
#' scenario, for instance, we might want the linked brushing that happens within
#' the `fdge` volcano to do a "significant gene set search" in the fgsea result.
#'
#' More specifically, maybe the volcano plot in the `dge_view` module should
#' be able to broadcast the feature brushing in such a way that the "listener"
#' in the `ffsea_vew` module can react to it. Similarly for the `ffsea_vew`
#' module should be able to broadcast the features that are brushed within
#' it to the `dge_vew` volcano and statistics tables ... or not?
#'
#' @export
fDgeSeaAnalysisServer <- function(id, rfds, ..., gdb = NULL, debug = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  shiny::moduleServer(id, function(input, output, session) {
    fdge_res <- fdgeAnalysisServer("fdge", rfds, debug = debug, ...)
    
    ffsea_res <- ffseaAnalysisServer("ffsea", rfds, fdge_res$main,
                                     gdb = gdb, debug = debug, ...)
    shiny::hideTab("atabs", target = "ffseatab")
    
    # toogle UI logic ..........................................................
    # ffsea stuf is visible only when a dge result is live
    shiny::observe({
      res. <- req(FacileAnalysis::faro(fdge_res))
      show <- is(res., "FacileDgeAnalysisResult")
      if (show) {
        shiny::showTab("atabs", target = "ffseatab")
      }
    })
    
    # wrap up ....................................................................
    vals <- list(
      main = list(dge = fdge_res, fsea = ffsea_res),
      .ns = session$ns)
    
    class(vals) <- c(
      # This is a serious whopper of a name -- are you being serious?
      "ReactiveFacileMultiAnalysisResultContainer"
    )
    
    vals
  })
}

#' @export
#' @noRd
fDgeSeaAnalysisUI <- function(id, ..., debug = FALSE) {
  ns <- shiny::NS(id)
  box. <- shinydashboard::box
  shiny::tagList(
    shinydashboard::tabBox(
      id = ns("atabs"), width = 12,
      shiny::tabPanel(
        title = "Differential Expression", value = "fdgetab",
        fdgeAnalysisUI(ns("fdge")),
        shiny::tags$div(style = "clear: both")),
      shiny::tabPanel(
        title = "GSEA", value = "ffseatab",
        ffseaAnalysisUI(ns("ffsea")),
        shiny::tags$div(style = "clear: both"))
    ))
}
