#' @noRd
fpcaRunOptionsUI <- function(id, width = "300px", ..., debug = FALSE) {
  ns <- shiny::NS(id)
  shinyWidgets::dropdown(
    inputId = ns("opts"),
    icon = shiny::icon("sliders"),
    status = "primary", circle = FALSE,
    width = width,
    
    shinyWidgets::pickerInput(
      ns("features"),
      label = "Feature Class",
      choices = NULL,
      multiple = TRUE,
      options = list(
        `selected-text-format`= "count",
        `count-selected-text` = "{0} classes chosen"
      )),
    shiny::numericInput(ns("pcs"), label = "Number of PCs",
                        value = 10, min = 2, max = 30, step = 1),
    shiny::numericInput(ns("ntop"), label = "Number of genes",
                        value = 500, min = 50, max = 5000, step = 500),
    shiny::numericInput(ns("priorcount"), label = "Prior Count",
                        value = 5, min = 1, max = 10, step = 1))
}

#' @noRd
#' @param rfds the current reactivefaciledatastore
#' @param assaySelect the FacileShine::assaySelect module which defines the
#'   current assay the pca will be run over.
#' @param asamples a reactive data.frame that enumerates the active samples the
#'   PCA will be run on
fpcaRunOptions <- function(input, output, session, assaySelect, asamples,
                           ...) {
  
  # Set the maximum number of PCs such that they do not exceed number of samples
  # Running PCA is also disabled if there are less than three samples.
  observeEvent(asamples(), {
    asamples. <- req(asamples())
    nsamples <- nrow(asamples.)
    value <- if (req(input$pcs) > nsamples) nsamples else NULL
    updateNumericInput(session, "pcs", value = value, max = nsamples)
  })
  
  observeEvent(assaySelect$assay_info(), {
    ainfo <- req(assaySelect$assay_info())
    features. <- assaySelect$features()
    ftypes <- unique(features.[["meta"]])
    default_pcoding <- "protein_coding" %in% ftypes
    if (default_pcoding) {
      selected <- "protein_coding"
    } else {
      selected <- ftypes
    }
    updatePickerInput(session, "features", choices = ftypes, selected = selected)
  })
  
  feature_universe <- reactive({
    filter(assaySelect$features(), .data$meta %in% input$features)
  })
  
  vals <- list(
    npcs = reactive(input$pcs),
    ntop = reactive(input$ntop),
    priorcount = reactive(input$priorcount),
    feature_universe = feature_universe)
  class(vals) <- "FpcaRunOptions"
  vals
}

#' @noRd
initialized.FpcaRunOptions <- function(x, ...) {
  npcs <- x$npcs()
  ntop <- x$ntop()
  priorcount <- x$priorcount()
  
  !unselected(npcs) && is.integer(npcs) &&
    !unselected(ntop) && is.integer(ntop) &&
    nrow(x$feature_universe()) >= npcs
}
