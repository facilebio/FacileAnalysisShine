#' Full interactive executation of GSEA, soup to nuts.
#'
#' For now, feature set enrichment analysis are only performed downstream of
#' a `FacileAnalysisResult`. This requires that we have an ffsea method defined
#' for the specific class of the result, ie. `ffsea.FacilePcaAnalysisResult`.
#'
#' @rdname interactive-ffsea
#'
#' @export
#'
#' @param x A FacileAnalysisResult that has an implemented `ffsea.*` method
#' @param gdb A `GeneSetDb` to use for the FSEA.
#' @examples
#' if (interactive()) {
#' gdb <- sparrow::getMSigGeneSetDb("h", species = "human", id.type = "ensembl")
#' dge.ttest <- FacileData::an_fds() |> 
#'   flm_def("cell_abbrev", numer = "CNT", denom = "IC") |> 
#'   fdge(method = "voom")
#' gsea <- ffsea(dge.ttest, gdb, methods = "cameraPR")
#' shine(gsea)
#' 
#' gsea2 <- ffseaGadget(dge.ttest, gdb)
#' }
ffseaGadget <- function(x, gdb, title = "Feature Set Enrichment Analysis",
                        height = 800, width = 1000, viewer = "browser", ...,
                        debug = FALSE) {
  assert_class(x, "FacileAnalysisResult")
  assert_class(gdb, "GeneSetDb")
  rgdb <- reactive(gdb)
  
  frunGadgetServer(ffseaAnalysisServer, ffseaAnalysisUI, x, 
                   aresult = x, gdb = rgdb,
                   title = title, height = height, width = width,
                   viewer = viewer, ..., retval = "faro", debug = debug)
}

#' A moodule that encapsulates configuring and running ffsea, and a view to
#' interact with the results.
#'
#' @noRd
#' @export
ffseaAnalysisServer <- function(id, rfds, aresult, gdb, ..., debug = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  assert_multi_class(aresult, c("ReactiveFacileAnalysisResult", "FacileAnalysisResult"))
  assert_class(gdb, "reactive") # NOTE: should this be a ReactiveGeneSetDb?
  
  shiny::moduleServer(id, function(input, output, session) {
    res <- ffseaRunServer("run", rfds, aresult, gdb, ..., debug = debug)
    view <- ffseaViewServer("view", rfds, res, ..., debug = FALSE)
    
    # Only show the view UI when there is an FfseaAnalysisResult ready
    observe({
      toggleElement("viewbox", condition = initialized(res))
    })
    
    vals <- list(
      main = res,
      view = view,
      .ns = session$ns)
    class(vals) <- c("ReactiveFacileFseaAnalysisResultContainer",
                     "ReactiveFacileAnalysisResultContainer")
    vals
  })
}

#' @noRd
#' @export
ffseaAnalysisUI <- function(id, ...) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$div(
      id = ns("runbox"),
      shinydashboard::box(
        title = "Configure Feature Set Analysis",
        width = 12,
        ffseaRunUI(ns("run")))),
    shinyjs::hidden(
      shiny::tags$div(
        id = ns("viewbox"),
        shinydashboard::box(
          title = "Feature Set Analysis Results",
          solidHeader = TRUE, width = 12, 
          ffseaViewUI(ns("view"), debug = debug)))))
}

# Run ==========================================================================

#' @section Custom Run Configuration:
#' The options presented to the user for running a feature set enrichment
#' analysis on a `FacilePcaAnalysisResult` will be different than the ones
#' made available for an enrichment analysis over a `FacileTtestAnalysisResult`
#' or even a `FacileAnovaAnalysisResult`.
#'
#' As such, each type of result should define a UI that accepts the appropriate
#' parameters for its corresponding `ffsea.*` method, and a server function
#' that extract and invokes the function.
#'
#' @rdname interactive-ffsea
#' @export
#' @importFrom sparrow GeneSetDb
#' @importFrom sparrow.shiny GeneSetDb.ReactiveGeneSetDb
#' @param aresult A `FacileAnalysisResult` that has a `ffsea.*` method defined.
#' @param gdb A `reactive(GeneSetDb)` object
ffseaRunServer <- function(id, rfds, aresult, gdb, ..., debug = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  assert_multi_class(aresult, c("ReactiveFacileAnalysisResult", "FacileAnalysisResult"))
  assert_class(gdb, "reactive") # NOTE: should this be a ReactiveGeneSetDb?
  
  shiny::moduleServer(id, function(input, output, session) {
    ares <- reactive({
      req(initialized(aresult))
      faro(aresult)
    })
    
    # When the AnalysisResult changes, update the runopts UI based on the specific
    # subclass of the AnalysisResult we have at play.
    #
    # Because the GeneSetDb knobs to subset collection and specify geneset size
    # are buried in the run options menu, we pass this down to the runOpts
    # module.
    runopts <- callModule(ffseaRunOpts, "runopts", rfds, aresult = aresult,
                          gdb = gdb, ..., debug = debug)
    
    
    # Updates the set-enrichment methods when the analysis result changes.
    available_methods <- reactive({
      ares. <- req(ares())
      ffsea_methods(ares.)
    })
    
    observeEvent(available_methods(), {
      methods <- req(available_methods())
      choices <- split(methods[["method"]], methods[["type"]])
      # Sub groups of length 1 break out of the grouping structure, one way
      # to fix that if they exist is outlined here:
      # https://github.com/rstudio/shiny/issues/1938#issuecomment-363942532
      choices <- lapply(choices, function(xc) {
        if (length(xc) == 1L) list(xc) else xc
      })
      # only pre-select first rank-based method, if not ranks based method is
      # applicable (unlikely), this should will evaluate to NULL anyway
      selected <- choices[["ranks"]][1L]
      
      # rename 'ora' group to "Over Represented"
      ora.idx <- which(names(choices) == "ora")
      if (length(ora.idx)) names(choices)[ora.idx] <- "over representation"
      opts <- NULL
      shinyWidgets::updatePickerInput(
        session, "ffsea_methods", selected = selected,
        choices = choices, choicesOpt = opts)
    })
    
    runnable <- reactive({
      !unselected(input$ffsea_methods) &&
        initialized(ares()) &&
        initialized(runopts)
    })
    
    observe({
      runnable. <- runnable()
      ftrace("runnable: ", as.character(runnable.))
      shinyjs::toggleState("runbtn", condition = runnable.)
    })
    
    observe({
      ftrace("Run button pressed: ", as.character(input$runbtn))
    })
    
    fsea_res <- eventReactive(input$runbtn, {
      req(runnable())
      
      gdb.args <- list(
        x = ares(),
        fsets = GeneSetDb(runopts$gdb),
        # min.gs.size = runopts$gdb$min.gs.size(),
        # max.gs.size = runopts$gdb$max.gs.size(),
        #
        # Note that we don't set min/max sizes anymore because they were
        # pre-specified in the universe, and depending on what features exist
        # in the object under test, further filtering might happen which may
        # be surprising
        min.gs.size = 2,
        max.gs.size = Inf)
      
      methods <- list(methods = input$ffsea_methods)
      method.args <- runopts$args()
      
      args <- c(gdb.args, methods, method.args)
      
      shiny::withProgress({
        do.call(ffsea, args)
      }, message = "Running Enrichment Analysis")
    })
    
    vals <- list(
      faro = fsea_res,
      .ns = session$ns)
    
    # TODO: fix class hierarchy
    classes <- c("ReactiveFacileFseaAnalysisResult",
                 "ReactiveFacileAnalysisResult",
                 "FacileFseaAnalysisResult")
    class(vals) <- classes
    vals
  })
}

#' @noRd
#' @export
ffseaRunUI <- function(id, ..., debug = FALSE) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shinyWidgets::pickerInput(
          ns("ffsea_methods"), "Methods", choices = NULL, multiple = TRUE)),
      shiny::column(
        width = 1,
        shiny::tags$div(
          style = "padding-top: 1.7em",
          ffseaRunOptsUI(ns("runopts"), width = "350px"))),
      shiny::column(
        width = 1, 
        shiny::actionButton(
          ns("runbtn"), "Run", style = "margin-top: 1.7em"))
    )
  )
}

# View =========================================================================

#' Responsible for the shiny view of a FacileFseaAnalysisResult
#'
#' @noRd
#' @export
#' @param rfds the reactive facile data store
#' @param ares The `FacileFseaAnalysisResult`
ffseaViewServer <- function(id, rfds, aresult, ..., debug = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  assert_multi_class(
    aresult, 
    c("ReactiveFacileFseaAnalysisResult", "FacileFseaAnalysisResult"))
  
  shiny::moduleServer(id, function(input, output, session) {
    state <- reactiveValues(
      gsview_select = tibble(assay_name = character(), feature_id = character()),
      set_select = tibble(collection = character(), name = character())
    )
  
    ares <- reactive({
      req(initialized(aresult))
      faro(aresult)
    })
  
    mgc <- reactive({
      res <- req(ares())
      mgres <- result(res)
      # TODO: validate() isn't working here, only works inside a
      # `output$xxx <- render*({})` block, which is generating outpout into the
      # shiny app
      validate(
        need(is(mgres, "SparrowResult"), "SparrowResult can't be found")
      )
      sparrow.shiny::SparrowResultContainer(mgres)
    })
  
    gs_result_filter <- callModule(
      sparrow.shiny::mgResultFilter,
      "mg_result_filter", mgc)
  
    # Overview Tab ...............................................................
    output$gseaMethodSummary <- shiny::renderUI({
      mgc. <- req(mgc())
      shiny::tagList(
        shiny::tags$h4("GSEA Analyses Overview"),
        sparrow.shiny::summaryHTMLTable.sparrow(
          mgc.$sr, mgc.$methods,
          gs_result_filter$fdr(),
          p.col = "padj.by.collection")
      )
    })
  
    # GSEA Results Tab ...........................................................
    gs_viewer <- callModule(
      sparrow.shiny::geneSetContrastView,
      "geneset_viewer",
      mgc, maxOptions = 500, feature_table_filter = "top", server = TRUE)
  
    # A table of GSEA statistics/results for the given method and fdr threshold
    # The table is wired to the gs_viewer so that row clicks can signal updates
    # to the contrast viewer
    gs_table_browser <- callModule(
      sparrow.shiny::mgTableBrowser,
      "mg_table_browser",
      mgc,
      method=gs_result_filter$method,
      fdr=gs_result_filter$fdr,
      server=TRUE)
    # clicks on gsea result table update the contrast view
    observeEvent(gs_table_browser$selected(), {
      .mgc <- req(mgc())
      geneset <- req(gs_table_browser$selected())
      sparrow.shiny::updateActiveGeneSetInContrastView(
        session, gs_viewer, geneset, .mgc)
    })
  
    # A table of other genesets that brushed genes in the contrast viewer
    # belong to. This table is also wired to the contrast viewer, so that
    # a click on a row of the table will update the contrast view, too.
    other_genesets_gsea <- callModule(
      sparrow.shiny::mgGeneSetSummaryByGene,
      "other_genesets_gsea",
      mgc, features = gs_viewer$selected,
      method = gs_result_filter$method,
      fdr = gs_result_filter$fdr)
  
    vals <- list(
      selected_features = reactive(state$gsview_select),
      selected_sets = reactive(state$set_select),
      .ns = session$ns)
  
    vals
  })
}

#' @noRd
#' @export
ffseaViewUI <- function(id, rmd = FALSE, ..., debug = FALSE) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # wellPanel(mgResultFilterUI(ns("mg_result_filter"))),

    shiny::tags$div(
      style="margin-bottom: 10px; padding: 5px; background-color: white",
      title='GSEA Results',
      shiny::uiOutput(ns("gseaMethodSummary"))),

    shiny::fluidRow(
      shiny::column(
        width = 5,
        style = "padding: 0",
        sparrow.shiny::mgResultFilterUI(ns("mg_result_filter")),
        shiny::wellPanel(
          sparrow.shiny::geneSetContrastViewUI(ns("geneset_viewer")))),
      shiny::column(
        width = 7,
        sparrow.shiny::mgTableBrowserUI(ns("mg_table_browser")))),

    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::tags$h4("Other Gene Sets with Selected Genes"),
        sparrow.shiny::mgGeneSetSummaryByGeneUI(ns("other_genesets_gsea"))))
  )
}
