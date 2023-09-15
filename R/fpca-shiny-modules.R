#' Perform a PCA analysis on a (subset) of a FacileDataStore
#'
#' Interactively runs PCA on a FacileDataStore. If you want to run PCA on a
#' subset of samples, pass in a facile_frame sample descriptor
#'
#' @export
#' @examples
#' if (interactive()) {
#' # run tumor vs normal comparisons vs each, then run compare9) on the results
#' efds <- FacileData::exampleFacileDataSet()
#' pca.crc <- efds |>
#'   FacileData::filter_samples(indication == "CRC") |>
#'   fpcaGadget()
#' report(pca.crc)
#' shine(pca.crc)
#' }
fpcaGadget <- function(x, title = "Principal Components Analysis",
                       viewer = "browser", ...) {
  assert_multi_class(x, c("FacileDataStore", "facile_frame"))
  frunGadgetServer(fpcaAnalysisServer, fpcaAnalysisUI, x, title = title,
                   viewer = viewer, ...)
}

# Embeddable Analysis Module ===================================================

#' @noRd
#' @export
fpcaAnalysisServer <- function(id, rfds, ..., debug = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  shiny::moduleServer(id, function(input, output, session) {
    pca <- fpcaRunServer("pca", rfds, ..., debug = debug)
    view <- fpcaViewServer("view", rfds, pca, ..., debug = debug)
    
    # Only show view widgets when there is a pca result
    observe({
      res. <- req(faro(pca))
      show <- is(res., "FacilePcaAnalysisResult")
      ftrace("Toggling PCA viewbox to: ", if (show) "show" else "hide")
      shinyjs::toggleElement("viewbox", condition = show)
    })
    
    vals <- list(
      main = pca,
      view = view,
      .ns = session$ns)
    class(vals) <- c("ReactiveFacilePcaAnalysisResultContainer",
                     "ReactiveFacileAnalysisResultContainer")
    vals
  })
}

#' @noRd
#' @export
#' @importFrom shinyjs hidden
fpcaAnalysisUI <- function(id, ..., debug = FALSE) {
  ns <- NS(id)
  shiny::tagList(
    shiny::tags$div(
      id = ns("runbox"), 
      fpcaRunUI(ns("pca"), debug = debug)),
    shinyjs::hidden(
      shiny::tags$div(
        id = ns("viewbox"), 
        fpcaViewUI(ns("view"), debug = debug))
    )
  )
}

# Run PCA ======================================================================

#' Minimal shiny module to run fpca
#'
#' @export
fpcaRunServer <- function(id, rfds, ..., debug = FALSE) {
  # Provide user with inputs to control:
  # 1. Assay to run PCA on
  # 2. Number of PCs to calculate
  # 3. Number of top varying features to keep
  assert_class(rfds, "ReactiveFacileDataStore")
  shiny::moduleServer(id, function(input, output, session) {
  
    batch <- FacileShine::batchCorrectConfigServer("batch", rfds, debug = debug)
    assay <- FacileShine::assaySelectServer("assay", rfds)

    runopts <- callModule(fpcaRunOptions, "runopts", rfds, assay)
    
    runnable <- reactive({
      req(initialized(rfds) && initialized(runopts))
    })
    
    observe({
      shinyjs::toggleState("run", condition = runnable())
    })
    
    result <- eventReactive(input$run, {
      req(runnable())
      samples. <- rfds$active_samples()
      assay_name <- assay$selected()
      
      features. <- runopts$feature_universe()
      pcs <- runopts$npcs()
      ntop <- runopts$ntop()
      pcount <- runopts$priorcount()
      
      batch. <- name(batch$batch)
      main. <- name(batch$main)

      ftrace("running fpca")
      out <- shiny::withProgress({
        these <- unreact(samples.)
        fpca(these, dims = pcs, ntop = ntop, assay_name = assay_name,
             features = features., filter = "variance",
             batch = batch., main = main., prior.count = pcount,
             custom_key = user(rfds))
      }, message = "Performing PCA")
      ftrace("PCA ran on ", nrow(these), " samples")
      ftrace("PCA Output on ", nrow(tidy(out)), " samples")
      out
    })
    
    vals <- list(
      faro = result,
      feature_universe = runopts$feature_universe,
      .ns = session$ns)
    class(vals) <- c("ReactiveFacilePcaAnalysisResult",
                     "ReactiveFacileAnalysisResult",
                     "FacilePcaAnalysisResult")
    vals
  })
}

#' @noRd
#' @export
fpcaRunUI <- function(id, width_opts = "200px", ..., debug = FALSE) {
  ns <- NS(id)
  # 1. Assay to run PCA on
  # 2. Number of PCs to calculate
  # 3. Number of top varying features to
  out <- shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 3, 
        FacileShine::assaySelectInput(ns("assay"), label = "Assay", choices = NULL)),
      shiny::column(
        width = 4, 
        FacileShine::batchCorrectConfigUI(ns("batch"), direction = "horizontal")),
      shiny::column(
        width = 1,
        shiny::tags$div(
          style = "padding-top: 1.7em",
          fpcaRunOptionsUI(ns("runopts"), width = "300px"))),
      shiny::column(
        width = 1, 
        shiny::actionButton(ns("run"), "Run"), style = "margin-top: 1.7em")
    ))
}

# Visualize PCA ================================================================

#' @noRd
#' @export
fpcaViewServer <- function(id, rfds, pcares, ...,
                     feature_selection = session$ns("features"),
                     sample_selection = session$ns("samples"),
                     debug = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  assert_multi_class(
    pcares, 
    c("ReactiveFacilePcaAnalysisResult", "FacilePcaAnalysisResult"))
  shiny::moduleServer(id, function(input, output, session) {
    state <- reactiveValues(
      # store brushed samples from scatterplot
      color_aes = NULL,
      shape_aes = NULL,
      facet_aes = NULL,
      hover = NULL,
      scatter_select = tibble(
        assay_name = character(), 
        feature_id = character()))
    
    pca <- reactive({
      req(initialized(pcares))
      faro(pcares)
    })
    
    pcs_calculated <- reactive({
      pca. <- req(pca())
      names(pca.$percent_var)
      out <- tibble(
        name = names(pca.$percent_var),
        variance = unname(pca.$percent_var))
      out$index <- as.integer(sub("^[^0-9]*", "", out$name))
      out$label <- sprintf("%s (%0.1f%%)", out$name, out$variance * 100)
      out
    })
    
    # When a new fpca result is produced, we may want to reset a few things.
    # Trigger that UI restting/cleanup work here.
    observeEvent(pca(), {
      pca. <- req(pca())
      components <- req(pcs_calculated())
      pcs <- setNames(components$index, components$label)
      
      shiny::updateSelectInput(session, "xaxis", choices = pcs, selected = pcs[1])
      shiny::updateSelectInput(session, "yaxis", choices = pcs, selected = pcs[2])
      shiny::updateSelectInput(session, "zaxis", choices = c("---", pcs), 
                               selected = "")
      
      shiny::updateSelectInput(session, "loadingsPC", 
                               choices = pcs, selected = pcs[1L])
    }, priority = 5) # upping priority so some withProgress things hide quick
    
    assay_name. <- reactive(param(req(pca()), "assay_name"))
    samples. <- reactive(samples(req(pca())))
    
    batch_corrected <- reactive({
      batch <- param(req(pca()), "batch")
      !is.null(batch)
    })
    
    aes <- FacileShine::categoricalAestheticMapServer(
      "aes", rfds, color = TRUE, shape = TRUE, group = FALSE, facet = TRUE,
      hover = TRUE, ..., debug)    
    
    # Color Mapping ..............................................................
    # This will be painful. Let's enable the PCA plot to be colored by a
    # categorical covariate, or one of the features picked from the loadings on
    # the feature table
    
    # Color covariate was selected from the categoricalAestheticMap module
    observeEvent(aes$map(), {
      aes.map <- aes$map()
      acolor <- aes.map$color
      ashape <- aes.map$shape
      ahover <- aes.map$hover
      afacet <- aes.map$facet
      if (!identical(state$color_aes, acolor)) state$color_aes <- acolor
      if (!identical(state$shape_aes, ashape)) state$shape_aes <- ashape
      if (!identical(state$facet_aes, afacet)) state$facet_aes <- afacet
      if (!identical(state$hover, ahover)) state$hover <- ahover
      
    })
    
    # A gene was selected from the PC loadings table
    observeEvent(input$loadings_rows_selected, {
      selected <- input$loadings_rows_selected
      if (!is.null(selected)) {
        dat <- pc.loadings()
        feature <- paste0("feature:", dat[["feature_id"]][selected])
        if (!identical(state$color_aes, feature)) {
          state$color_aes <- feature
          # TODO: update selected color selection in categoricalAestheticMap
          # `aes` module
        }
      }
    })
    
    # Render PCA Plot ............................................................
    pcaviz <- reactive({
      pca. <- req(pca())
      pc.calcd <- pcs_calculated()
      req(nrow(pc.calcd) >= 2)
      axes <- intersect(c(input$xaxis, input$yaxis, input$zaxis), pc.calcd$index)
      axes <- as.integer(axes)
      req(length(axes) >= 2)
      
      acolor <- state$color_aes
      ashape <- state$shape_aes
      afacet <- state$facet_aes
      ahover <- state$hover
      
      if (identical(substr(acolor, 1L, 8L), "feature:")) {
        feature_id <- sub("feature:", "", state$color_aes)
        current.cols <- colnames(pca.$result)
        pca.$result <- with_assay_data(pca.$result, features = feature_id,
                                       assay_name = param(pca., "assay_name"),
                                       normalized = TRUE,
                                       batch = param(pca., "batch"),
                                       main = param(pca., "main"))
        added <- setdiff(colnames(pca.$result), current.cols)
        acolor <- added
      }
      
      viz(pca., axes, color_aes = acolor, shape_aes = ashape, hover = ahover,
          facet_aes = afacet, width = NULL, height = 550)
    })
    
    output$pcaplot <- plotly::renderPlotly({
      plot(req(pcaviz()))
    })
    
    feature.ranks <- reactive({
      req(pca()) |>
        signature(signed = TRUE, ntop = 50) |>
        tidy()
    })
    
    # Loadings =================================================================
    pc.loadings <- reactive({
      pc <- paste0("PC", input$loadingsPC)
      req(feature.ranks()) |>
        filter(dimension == pc) |>
        select(symbol, score) |> 
        mutate(absScore = abs(score))
    })
    
    observe({
      ldat <- req(pc.loadings())
      shinyjs::toggleState(
        "loadingsdl",
        condition = is(ldat, "data.frame") && nrow(ldat) > 0)
    })
    
    output$loadingsdl <- shiny::downloadHandler(
      filename = function() {
        req(feature.ranks(), assay_name.())
        name <- paste0("pca-loadings-", assay_name.(), ".csv")
      },
      content = function(file) {
        dat <- req(feature.ranks()) |> 
          select(dimension, symbol, feature_id, score)
        write.csv(dat, file, row.names = FALSE)
      }
    )
    
    output$loadings <- DT::renderDT({
      dtopts <- list(deferRender = TRUE, scrollY = 450,
                     # scroller = TRUE,
                     pageLength = 15,
                     lengthMenu = c(15, 30, 50))
      
      pc.dat <- pc.loadings()
      num.cols <- colnames(pc.dat)[sapply(pc.dat, is.numeric)]
      
      dt <- DT::datatable(
        pc.dat, filter = "top",
        style = "bootstrap",
        class = "display", width = "100%", rownames = FALSE,
        selection = "single",
        options = dtopts)
      DT::formatRound(dt, num.cols, 3)
    }, server = TRUE)
  })
}

#' @noRd
#' @export
fpcaViewUI <- function(id, ..., debug = FALSE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h4(
      shiny::tags$span(
        "PCA Result", style = "background: #fff; padding: 0 10px 0 0"),
      style = "border-bottom: 1px solid #000; line-height: 0.1em; margin-bottom: 13px"),
    shiny::fluidRow(
      shiny::column(
        width = 7,
        shiny::fluidRow(
          shiny::column(4, shiny::selectInput(ns("xaxis"), "X axis", choices = NULL)),
          shiny::column(4, shiny::selectInput(ns("yaxis"), "Y axis", choices = NULL)),
          shiny::column(4, shiny::selectInput(ns("zaxis"), "Z axis", choices = NULL))),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::wellPanel(
              FacileShine::categoricalAestheticMapInput(
                ns("aes"),
                color = TRUE, shape = TRUE, facet = TRUE, hover = TRUE,
                group = FALSE)))),
        waiter::withWaiter(plotly::plotlyOutput(ns("pcaplot")))),
      shiny::column(
        width = 5,
        shiny::tags$h4("Feature Loadings"),
        # shiny::fluidRow(
        #   shiny::column(
        #     width = 4,
        #     shiny::tags$div(
              shiny::selectInput(
                ns("loadingsPC"), 
                "Principal Component",
                choices = NULL),
        # )),
          # shiny::column(
            # width = 8,
            shinyjs::disabled(
              shiny::downloadButton(ns("loadingsdl"), "Download Loadings"))
          # )),
        ,
        # shinyWidgets::addSpinner(
        waiter::withWaiter(DT::DTOutput(ns("loadings"), height = "650px")),
        spin = getOption("FacileShine.spinner_type"),
        color = getOption("FacileShine.spinner_color")
        # )
      )))
}
