#' This module will retun a list of arguments to configure the ffsea method
#' for the specific subclass of an AnalysisResult that `ares` belongs to.
#' The list that is returned here will not include `ares` itself.
#'
#' @noRd
#' @export
#' @param ares a reactive that contains a FacileAnalysisResult
#' @param gdb A `reactive(GeneSetDb)` object
#' @return An `FfseaRunOptions` list, where `$args()` is a reactive list, with
#'   name=value pairs set to the arguments to run the appropriate `ffsea.*`
#'   method for `aresult`.
ffseaRunOpts <- function(input, output, session, rfds, aresult, gdb, ...,
                         debug = FALSE) {
  ns <- session$ns
  state <- reactiveValues(
    aclass = "__initializing__",
    default_args = "__initializing__")

  ares <- reactive({
    req(initialized(aresult))
    faro(aresult)
  })

  # Enable user to configure the GeneSetDb used for testing
  rgdb <- callModule(
    sparrow.shiny::reactiveGeneSetDb,
    "gdb", gdb, debug = debug, ...)

  observeEvent(ares(), {
    ares. <- req(ares())
    aclass <- class(ares.)[1L]

    # always reset to default params when a new analysis result is provided,
    # but we may want to change this later.
    if (TRUE || state$aclass != aclass) {
      rm.ui <- names(state$default_args)
      state$aclass <- aclass
      uistuff <- renderFfseaRunOptsUI(ares., ns)
      state$default_args <- uistuff$args
      output$ui <- shiny::renderUI(uistuff$ui)
    }
  }, priority = 5)

  # register the values of the default values in server code without having
  # to expand the dropdownButton.
  observeEvent(state$default_args, {
    args <- state$default_args
    req(!unselected(args))
    # for (arg in names(args)) {
    #   outputOptions(output, arg, suspendWhenHidden = FALSE)
    # }
    shiny::outputOptions(output, "ui", suspendWhenHidden = FALSE)
  })

  # extracts the values from the UI for this object
  args <- reactive({
    dargs <- state$default_args
    req(!unselected(dargs))
    sapply(names(dargs), function(name) input[[name]], simplify = FALSE)
  })

  # 1. remove the UI already there
  # 2. create the new one
  vals <- list(
    args = args,
    aclass = reactive(state$aclass),
    gdb = rgdb,
    .state = state,
    .ns = session$ns)
  class(vals) <- "FfseaRunOptions"
  vals
}

#' Creates the UI panel for the FacileAnalysisResult-specific ffsea options.
#'
#' This function creates the UI `tagList` for the options required to run
#' `ffsea` over a specific analysis result. The values are initialized to
#' their defaults. The `tagList` and list of `args` are returned.
#'
#' @noRd
#' @export
#' @return a list with `$ui` for the tagList of interface components and
#'   `$args`, which is a list of name/value pairs for the default arguments
#'   of the ffsea.* function implementation.
ffseaRunOptsUI <- function(id, width = "350px", ..., debug = FALSE) {
  ns <- shiny::NS(id)

  shinyWidgets::dropdown(
    ns("opts"),
    icon = shiny::icon("sliders"),
    status = "primary",
    # circle = FALSE,
    width = width,
    shiny::tags$div(
      id = ns("genesetdbconfig"),
      shiny::tags$h4(
        shiny::tags$span("Gene Set Selection",
                         style = "background: #fff; padding: 0 10px 0 0"),
        style = "border-bottom: 1px solid #000; line-height: 0.1em; margin-bottom: 13px"),
      sparrow.shiny::reactiveGeneSetDbUI(ns("gdb"))),
    shiny::tags$div(
      id = ns("ffseaRunOptsContainer"),
      # style = "height: 400px",
      shiny::uiOutput(ns("ui"))))
}

# Helper Functions =============================================================

#' @noRd
#' @export
initialized.FfseaRunOptions <- function(x, ...) {
  args <- x$args()
  args.set <- is.list(args) &&
    length(args) > 0L &&
    all(sapply(args, function(arg) !(is.null(arg) || is.na(arg))))
  args.set && initialized(x$gdb)
}

#' @noRd
#' @export
#' @importFrom shiny tagList
#' @param x the FacileAnalysisResult to buil a UI for
#' @param ns the namespace object to use for ui id generation
renderFfseaRunOptsUI <- function(x, ns, ...) {
  UseMethod("renderFfseaRunOptsUI", x)
}

#' @noRd
#' @export
renderFfseaRunOptsUI.FacileTtestAnalysisResult <- function(x, ns, ...) {
  args <- default_ffsea_args(x)
  # We can rank by either logFC or t-statistics (if they are available)
  rank.opts <- intersect(c("logFC", "t"), colnames(tidy(x)))

  # NOTE: We may want to update this and even have the UI update with the number
  # of features that pass these filtering criteria, and what have you.

  ui <- shiny::tagList(
    shiny::tags$div(
      id = ns("rankoptsbox"),
      style = "border-bottom: 1px solid #000; line-height: 0.1em; margin-bottom: 13px",
      shiny::tags$h4(
        shiny::tags$span(
          style = "background: #fff; padding: 0 10px 0 0",
          "Rank Encirhment Options")),
      tags$p("Configure how genes are ranked"),
      shiny::selectInput(
        ns("rank_by"), "Rank By", choices = rank.opts,
        selected = rank.opts[1]),
      shiny::checkboxInput(ns("signed"), "Signed Ranks", value = args$signed)),
    shiny::tags$div(
      id = ns("oraoptsbox"),
      shiny::tags$h4(
        shiny::tags$span(
          "Over Representation Options",
          style = "background: #fff; padding: 0 10px 0 0"),
        style = "border-bottom: 1px solid #000; line-height: 0.1em; margin-bottom: 13px"),
      shiny::tags$p("Coriteria for selecting genes of interest"),
      shiny::numericInput(
        ns("min_logFC"), "Min absolute logFC", value = args$min_logFC,
        min = 0, max = 10, step = 0.5),
      shiny::numericInput(
        ns("max_padj"), "Max FDR", value = args$max_padj,
        min = 0, max = 1, step = 0.05)))

  list(args = args, ui = ui)
}

#' @noRd
#' @export
renderFfseaRunOptsUI.FacileAnovaAnalysisResult <- function(x, ns, ...) {
  args <- default_ffsea_args(x)
  ui <- shiny::tagList(
    shiny::numericInput(
      ns("max_padj"), "Max FDR", value = args$max_padj,
      min = 0, max = 1, step = 0.05))
  list(args = args, ui = ui)
}

#' @noRd
#' @export
renderFfseaRunOptsUI.FacilePcaAnalysisResult <- function(x, ns, ...) {
  args <- default_ffsea_args(x)
  dims <- intersect(names(x$percent_var), colnames(tidy(x)))
  labels <- sprintf("%s (%.02f%% variance)", dims, x$percent_var[dims] * 100)
  choices <- setNames(dims, labels)
  ui <- shiny::tagList(
    shiny::selectInput(
      ns("dim"), "Test Dimension", choices = choices,
      selected = choices[1L]),
    shiny::checkboxInput(ns("signed"), "Signed Ranks", value = args$signed))
  list(args = args, ui = ui)
}

