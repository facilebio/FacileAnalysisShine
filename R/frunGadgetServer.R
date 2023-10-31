frunGadgetServer <- function(modServer, analysisUI, x, user = Sys.getenv("USER"),
                             title = "Facile Analysis Gadget",
                             height = 800, width = 1000, viewer = "dialog", ...,
                             with_sample_filter = is(x, "FacileDataStore"),
                             retval = "x",
                             debug = FALSE) {
  bs4dash <- getOption("facile.bs4dash")
  options(facile.bs4dash = FALSE)
  on.exit(options(facile.bs4dash = bs4dash))
  
  assert_function(modServer)
  assert_function(analysisUI)
  
  viewer <- gadget_viewer(viewer, title, width, height)
  
  fds. <- fds(x)
  restrict_samples <- NULL

  if (is(x, "facile_frame")) {
    samples. <- x
    sample.filter <- FALSE
    restrict_samples <- samples.
  } else if (is(x, "FacileDataStore")) {
    samples. <- collect(samples(x), n = Inf)
    sample.filter <- TRUE
  } else if (is(x, "FacileAnalysisResult")) {
    # ugh, this isn't going to work -- I'm writing this in to fire up a
    # ffseaGadget, whose needs to be a FacileAnalysisResult.
    samples. <- collect(samples(x), n = Inf)
    sample.filter <- FALSE
    restrict_samples <- samples.
  } else {
    stop("What in the world?")
  }
  
  sample.filter <- sample.filter || with_sample_filter
  xtra_covariates <- setdiff(colnames(samples.), c("dataset", "sample_id"))
  # if (length(xtra_covariates)) {
  #   xtra_covariates <- as.EAVtable(samples.)
  # } else {
  #   xtra_covariates <- NULL
  # }
  if (length(xtra_covariates)) {
    xtra_covariates <- samples.
  } else {
    xtra_covariates <- NULL
  }
  
  assert_class(fds., "FacileDataStore")
  assert_class(samples., "facile_frame")
  
  ui.content <- analysisUI("analysis", ..., debug = debug)
  if (sample.filter) {
    ui.content <- shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::wellPanel(
          FacileShine::facileSampleFiltersSelectInput("rfds", debug = debug))),
      shiny::column(
        width = 9,
        ui.content))
  }
  
  ui <- miniUI::miniPage(
    shinyjs::useShinyjs(),
    shinyWidgets::useSweetAlert(),
    if (interactive()) miniUI::gadgetTitleBar(title) else NULL,
    miniUI::miniContentPanel(ui.content),
    NULL)
  
  server <- function(input, output, session) {
    rfds <- FacileShine::facileDataStoreServer(
      "rfds", reactive(fds.), samples_subset = reactive(samples.),
      with_filters = sample.filter)
    
    analysis <- modServer("analysis", rfds, ..., debug = debug)
    
    if (!is.null(xtra_covariates)) {
      observe({
        req(initialized(rfds))
        rfds$.state$esample_annotation <- xtra_covariates
      })
    }
    
    observeEvent(input$done, {
      annotation <- list(
        samples = FacileShine:::.empty_sample_annotation_tbl(),
        features = FacileShine:::.empty_feature_annotation_tbl())
      
      if (is(x, "FacileAnalysisResult") && retval != "faro")   {
        if (retval == "x") {
          result. <- x
        } else {
          result. <- analysis[[retval]]
          if (is(result., "reactive")) result. <- result.()
        }
      } else {
        if (is(analysis, "ReactiveFacileAnalysisResultContainer")) {
          result. <- failWith(list(), unreact(faro(analysis)))
          attr(result., "INTERACTED") <- list(
            annotation = annotation)
          class(result.) <- classify_as_gadget(result.)
        } else if (is(analysis, "ReactiveFacileMultiAnalysisResultContainer")) {
          # like the fDgeGseaModule
          results. <- analysis$main
          assert_list(results., names = "unique")
          result. <- lapply(results., function(res) {
            assert_class(res, "ReactiveFacileAnalysisResult")
            unreact(faro(res))
          })
          class(result.) <- "FacileMultiAnalysisResult"
        } else {
          stop("Unexpected `analysis` return type: ", class(analysis)[1L])
        }
      }
      
      stopApp(invisible(result.))
    })
    
    observeEvent(input$cancel, {
      stopApp(invisible(NULL))
    })
  }
  
  if (interactive()) {
    shiny::runGadget(ui, server, viewer = viewer, stopOnCancel = FALSE)
  } else {
    # Running in an Rmd with runtime: shiny?
    opts <- list(height = height)
    shiny::shinyApp(ui = ui, server = server, options = opts)
  }
}

#' Extracts interactions over an object that happened in a gadget
#'
#' These are stored and returned as a named list
#'
#' @export
#' @param x any object
#' @return a named list of interactions recorded from running [frunGadgetServer()]
interacted <- function(x, ...) {
  out <- attr(x, "INTERACTED", exact = TRUE)
  if (is.null(out)) {
    warning("No metadata from gadget interaction found in x")
    out <- list()
  }
  out
}

#' @export
#' @importFrom FacileShine annotation
#' @noRd
annotation.FacileAnalysisGadgetResult <- function(x, type = NULL, ...) {
  annos <- interacted(x)[["annotation"]]
  if (is.list(annos) && !is.null(type)) {
    assert_choice(type, names(annos))
    annos <- annos[[type]]
  }
  annos
}


#' @noRd
#' @importFrom shiny browserViewer dialogViewer paneViewer
gadget_viewer <- function(type = c("dialog", "browser", "pane"),
                          title = "Facile Gadget",
                          width = 800, height = 600, minHeight = height,
                          ...) {
  type <- match.arg(type)
  switch(type,
         dialog = dialogViewer(title, width, height),
         browser = browserViewer(),
         pane = paneViewer(minHeight = minHeight))
}
