#' Fully interactive differential expression analysis
#'
#' Assembles a shiny UI to define all of the bits required to perform a
#' differential expression analysis over a predefined set of samples.
#'
#' An interactive differential expression analysis is divided into three
#' steps, each of which provides its own shiny module and interface. The
#' minimal input to this analysis is a pre-defined subset of samples to act on.
#'
#' These steps are:
#'
#' 1. Model matrix definition. The functionality is provided by the
#'    [flm_def()] function, and the shiny interface by the
#'    [flmDefRun()] module.
#' 2. Differential expression analysis. The functionality is
#'    defined by the [fdge()] function, and the shiny interface by the
#'    [fdgeRun()] module.
#' 3. Results display. The interactive display of the results is provided
#'    by the [fdgeView()] module.
#'
#' @rdname fdge-shiny
#' @export
#'
#' @section Interactive Gadget:
#'
#' @examples
#' if (interactive()) {
#' # run tumor vs normal comparisons vs each, then run compare() on the results
#' options(facile.log.level.fshine = "trace")
#' efds <- FacileData::exampleFacileDataSet()
#' dge.crc <- efds |>
#'   FacileData::filter_samples(indication == "CRC") |>
#'   fdgeGadget(viewer = "pane")
#' dge.blca <- efds |>
#'   FacileData::filter_samples(indication == "BLCA") |>
#'   fdgeGadget(viewer = "pane")
#' dge.comp <- compare(dge.crc, dge.blca)
#' }
fdgeGadget <- function(x, title = "Differential Expression Analysis",
                       height = 800, width = 1000, viewer = "browser", ...) {
  assert_multi_class(x, c("FacileDataStore", "facile_frame"))
  frunGadgetServer(fdgeAnalysisServer, fdgeAnalysisUI, x, title = title,
                   height = height, width = width, viewer = viewer, ...)
}

#' @rdname fdge-shiny
#' @section Analysis Module:
#' Wrapper module to perform and interact with a differential expression result.
#'
#' This module can be embedded within a shiny app, or called from a gadget.
#'
#' @export
#' @return a `ReactiveFacileDgeAnalysisResult`, the output from [fdge()]
fdgeAnalysisServer <- function(id, rfds, ..., debug = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  shiny::moduleServer(id, function(input, output, session) {
    model <- flmDefRunServer("model", rfds, ..., debug = debug)
    dge <- fdgeRunServer("dge", rfds, model, ..., debug = debug)
    view <- fdgeViewServer("view", rfds, dge, ..., 
                           feature_selection = session$ns("volcano"),
                           sample_selection = session$ns("samples"),
                           debug = debug)
    
    # Only show the view UI when there is (at least) a defined model. When
    # done this way, the showSpinner() around the view's output datatable works
    # like a "wait, processing DGE" while it's running.
    observe({
      res. <- req(faro(dge))
      show <- is(res., "FacileDgeAnalysisResult")
      shinyjs::toggleElement("viewbox", condition = show)
    })
    
    vals <- list(
      main = dge,
      model = model,
      view = view,
      .ns = session$ns)
    class(vals) <- c("ReactiveFacileDgeAnalysisResultContainer",
                     "ReactiveFacileAnalysisResultContainer")
    vals
  })
}


#' @noRd
#' @export
#' @importFrom shiny
#'   fluidRow
#'   NS
#'   tagList
#'   tags
#'   wellPanel
fdgeAnalysisUI <- function(id, ..., debug = FALSE,
                           bs4dash = isTRUE(getOption("facile.bs4dash"))) {
  ns <- shiny::NS(id)
  box. <- shinydashboard::box
  shiny::tagList(
    shiny::tags$div(
      id = ns("modelbox"),
      box.(title = "Model Definition", width = 12,
           flmDefRunUI(ns("model"), debug = debug))),
    shiny::tags$div(
      id = ns("dgebox"),
      box.(title = "Testing Parameters", width = 12,
           fdgeRunUI(ns("dge"), debug = debug))),
    shinyjs::hidden(
      shiny::tags$div(
        id = ns("viewbox"),
        box.(title = NULL, #"Testing Results",
             solidHeader = TRUE,
             width = 12,
             fdgeViewUI(ns("view"), debug = debug)))))
}

# Run ==========================================================================

#' Shiny module to configure and run fdge over a predefined model.
#'
#' @export
#' 
#' @param id namespace for module
#' @param model A linear model definition. Can be either an "innert"
#'   `FacileLinearModelDefinition` that is returned from a call to
#'   [flm_def()], or the `ReactiveDGEModelDefinition` object returned
#'   from the [flmDefRun()] module.
#' @param ... passed into [fdge()]
#' @return A list of stuff. `$result` holds the `FacileDgeAnalysisResult`
#'   wrapped in a `reactive()`.
#' @export
fdgeRunServer <- function(id, rfds, model, ..., debug = FALSE, 
                          .reactive = TRUE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  assert_class(model, "FacileLinearModelDefinition")
  
  shiny::moduleServer(id, function(input, output, session) {
    assay <- FacileShine::assaySelectServer("assay", rfds)
    runopts <- callModule(fdgeRunOptions, "runopts", rfds, model, assay, ...)
    
    runnable <- reactive({
      initialized(model) && initialized(assay) && initialized(runopts)
    })
    
    observe({
      shinyjs::toggleState("run", condition = runnable())
    })
    
    dge <- eventReactive(input$run, {
      req(runnable())
      assay_name. <- assay$selected()
      flm <- unreact(faro(model))
      
      shinyjs::disable("run")
      
      result <- shiny::withProgress({
        tryCatch(
          fdge(flm, assay_name = assay_name.,
               method = runopts$dge_method(),
               with_sample_weights = runopts$sample_weights(),
               treat_lfc = runopts$treat_lfc(),
               filter_universe = runopts$feature_filter$universe()
               #, filter = runopts$feature_filter$method(),
               # filter_min_count = runopts$feature_filter$min_count(),
               # filter_min_total_count = runopts$feature_filter$min_total_count(),
               # filter_min_expr = runopts$feature_filter$min_expr()
        ), error = function(e) geterrmessage())
      }, message = "Performing differential expression")
      if (is.character(result)) {
        shinyWidgets::sendSweetAlert(
          session, 
          "Error in differential analysis",
          text = result, type = "error")
      }
      
      shinyjs::enable("run")

      result
    })
    
    if (debug) {
      output$debug <- shiny::renderText({
        dge. <- req(dge())
        format(dge.)
      })
    }
    
    vals <- list(
      faro = dge,
      .ns = session$ns)
    
    # Since `model` can be a reactive version of a FacileLinearModelDefinition, I
    # don't know how to get the Ttest or ANOVA state of that model without
    # being in a reactive context and, therefore, appending it to the class
    # of the outgoing result.
    class(vals) <- c("ReactiveFacileDgeAnalysisResult",
                     "ReactiveFacileAnalysisResult",
                     "FacileDgeAnalysisResult")
    vals    
  })
}

#' @noRd
#' @export
fdgeRunUI <- function(id, ..., debug = FALSE) {
  ns <- shiny::NS(id)
  out <- shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 3, 
        FacileShine::assaySelectInput(ns("assay"), choices = NULL)),
      shiny::column(
        width = 1,
        shiny::tags$div(
          style = "padding-top: 1.7em",
          fdgeRunOptionsUI(ns("runopts"), width = "300px"))),
      shiny::column(
        width = 1,
        shiny::actionButton(ns("run"), "Run", style = "margin-top: 1.7em"))
    ))

  if (debug) {
    out <- shiny::tagList(
      out,
      shiny::tags$hr(),
      shiny::verbatimTextOutput(ns("debug"), placeholder = TRUE))
  }

  out
}

#' Runs fdge over a model definition created from a shiny gadget.
#'
#' @noRd
#' @export
fdge.ReactiveFacileLinearModelDefinition <- function(x, assay_name = NULL,
                                                     method = NULL,
                                                     filter = "default",
                                                     with_sample_weights = FALSE,
                                                     treat_lfc = NULL, ...) {
  fdge(faro(x), assay_name = assay_name, method = method, filter = filter,
       with_sample_weights = with_sample_weights, treat_lfc = treat_lfc, ...)
}


# View =========================================================================

#' Shiny module that interacts with result of fdgeRun module
#'
#' Provides an interactive view over the result of running the [fdgeRun()]
#' module.
#'
#' @export
#' @param dgeres The result from calling [fdge()], or [fdgeRun()].
fdgeViewServer <- function(id, rfds, dgeres, ...,
                           feature_subset = NULL,
                           feature_selection = NULL,
                           sample_selection = NULL,
                           debug = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  assert_multi_class(dgeres, c("ReactiveFacileDgeAnalysisResult", "FacileDgeAnalysisResult", "reactive"))

  shiny::moduleServer(id, function(input, output, session) {
    # if (!shiny::is.reactive(feature_subset)) {
    #   message("Reactivating")
    #   feature_subset <- shiny::reactive(feature_subset)
    # }
    
    state <- reactiveValues(
      # store brushed values from volcano plot here so that it can be reset
      # externally. When a new DGE is run, I want to turn off the volcano, and
      # blow out the selection. If we rely on just the reactive nature of
      # event_data("plotly_selected") then the selection would be stuck when
      # user turns off the volcano option
      feature_subset = feature_subset,
      volcano_select = tibble(assay_name = character(), feature_id = character()),
      boxplot_select = tibble(dataset = character(), sample_id = character())
    )
    
    if (is.reactive(feature_subset)) {
      observe({
        state$feature_subset <- feature_subset() 
      })
    }
        
    if (is.null(feature_selection)) {
      feature_selection <- session$ns("volcano")
    }
    if (is.null(sample_selection)) {
      sample_selection <- session$ns("samples")
    }
    
    # The FacileDGEResult object
    dge <- reactive({
      if (is(dgeres, "reactive")) {
        dgeres <- req(dgeres())
      }
      req(initialized(dgeres))
      faro(dgeres)
    })
  
    # When a new fdge result is produced, we may want to reset a few things.
    # Trigger that UI restting/cleanup work here.
    observeEvent(dge(), {
      # Hide volcanobox completely if new dge is not a ttest
      shinyjs::toggleElement("volcanobox", condition = is_ttest(dge()))
      # New results should turn off the volcano toggleSwitch if it is a ttest
      shinyWidgets::updateSwitchInput(session, "volcanotoggle", value = FALSE)
    }, priority = 5) # upping priority so some withProgress things hide quick
  
    # When the volcano boxplot is turned off (toggle is set to FALSE), nuke
    # any active selection
    observeEvent(input$volcanotoggle, {
      if (input$volcanotoggle) {
        shinyjs::show("volcanoplotdiv")
      } else {
        state$volcano_select <- tibble(feature_id = character())
        shinyjs::hide("volcanoplotdiv")
      }
    }, priority = 5)
  
    assay_name. <- reactive(param(req(dge()), "assay_name"))
    samples. <- reactive(samples(req(dge())))
    model. <- reactive(model(req(dge())))
    covariate. <- reactive(param(req(model.()), "covariate"))
  
    batch_corrected <- reactive({
      mod <- req(model.())
      batch <- param(mod, "batch")
      !is.null(batch)
    })
  
    # Two versions of the result table are stored:
    # 1. `dge.stats.all`: always the full table; and
    # 2. `dge.stats`: the subset of (1) which corresponds to the currently
    #    selected features in (1) that are brushed from the volcano. If no
    #    brushing is active, then (2) == (1)
    dge.stats.all <- reactive({
      dge. <- req(dge())
      
      ranked <- result(ranks(dge.))
      if (is_ttest(dge.)) {
        out <- dplyr::select(ranked, symbol, feature_id, logFC, FDR = padj, pval)
      } else {
        out <- dplyr::select(ranked, symbol, feature_id, F, FDR = padj, pval)
      }
      if (!is.null(state$feature_subset) && nrow(state$feature_subset) > 0L) {
        out <- dplyr::semi_join(out, state$feature_subset, by = "feature_id")
      }

      out
    })
  
    observe({
      shinyjs::toggleElement(
        "dlbuttonbox", 
        condition = is(dge.stats.all(), "data.frame"))
    })
  
    # Visualization of DGE results -----------------------------------------------
  
    output$volcano <- plotly::renderPlotly({
      plt <- NULL
      show <- input$volcanotoggle
      if (is_ttest(dge()) && show) {
        dat. <- req(dge.stats.all())
        if (debug) {
          # reduce volcano size
          dat. <- bind_rows(head(dat., 100), tail(dat., 100))
          dat. <- distinct(dat, feature_id, .keep_all = TRUE)
        }
        dat.$yaxis <- -log10(dat.$pval)
        fplot <- FacileViz::fscatterplot(
          dat., c("logFC", "yaxis"),
          xlabel = "logFC", ylabel = "-log10(pval)",
          width = NULL, height = NULL,
          hover = c("symbol", "logFC", "FDR"),
          webgl = TRUE, event_source = feature_selection,
          key = "feature_id")
        plt <- plot(fplot)
      }
      plt
    })
  
    # Responds to selection events on the volcano plot. If no selection is active,
    # this is NULL, otherwise its the character vector of the "feature_id" values
    # that are brushed in the plot.
    observeEvent({
      plotly::event_data("plotly_selected", source = feature_selection)
    } , {
      selected <- plotly::event_data(
        "plotly_selected", 
        source = feature_selection)
      if (is.null(selected)) {
        selected <- character()
      } else {
        selected <- selected$key
      }
      if (!setequal(selected, state$volcano_select$feature_id)) {
        state$volcano_select <- tibble(
          assay_name = rep(assay_name.(), length(selected)),
          feature_id = selected)
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
    dge.stats <- reactive({
      stats. <- req(dge.stats.all())
      selected <- state$volcano_select
      if (nrow(selected)) {
        stats. <- filter(stats., feature_id %in% selected$feature_id)
      }
      stats.
    })
  
    # Visualization of selected genes across samples -----------------------------
  
    # The element in the statistics table that is selected, this will be an
    # assay/feature_id 1-row tibble, or NULL
    selected_result <- reactive({
      out <- input$stats_rows_selected
      aname <- isolate(assay_name.())
      if (!is.null(out)) {
        dat <- req(dge.stats())
        out <- tibble(assay = aname, feature_id = dat[["feature_id"]][out],
                      symbol = dat[["symbol"]][out])
      }
      out
    })
  
    # Keeps track of whether or not the model and fdge result used a "batch" term.
    # and shows/hides the UI element to view batch-corrected expression data
    # accordingly
    observe({
      enable_toggle <- batch_corrected()
      if (!enable_toggle) {
        shinyWidgets::updateSwitchInput(session, "batch_correct", value = FALSE)
      }
      shinyjs::toggleElement(
        "batch_correct_container", 
        condition = enable_toggle)
    })
  
    featureviz <- eventReactive({
      selected_result()
      input$batch_correct
    }, {
      dge. <- req(dge())
      bc <- batch_corrected() && input$batch_correct
      feature <- req(selected_result())
      viz(dge., feature, event_source = sample_selection, batch_correct = bc)
    })
  
    output$boxplot <- plotly::renderPlotly({
      # plt <- tryCatch(featureviz(), error = function(e) NULL)
      # if (is.null(plt)) empty_plot() else plot(plt)
      plot(req(featureviz()))
    })
  
    output$boxplotdl <- shiny::downloadHandler(
      filename = function() {
        feature <- req(selected_result())
        req(nrow(feature) == 1L)
        symbol <- feature$symbol
        name <- "expression"
        if (!is.na(symbol) && !unselected(symbol)) {
          name <- paste(name, symbol, sep = "_")
        }
        name <- paste(name, feature[["feature_id"]], sep = "_")
        if (batch_corrected() && input$batch_correct) {
          name <- paste(name, "batch-corrected", sep = "_")
        }
        paste0(name, ".csv")
      },
      content = function(file) {
        dat <- req(featureviz()) |>
          FacileViz::input_data() |>
          select(dataset, sample_id, feature_id, feature_name, value) |>
          with_sample_covariates()
        write.csv(dat, file, row.names = FALSE)
      }
    )
  
    # Responds to sample-selection events on the boxplot. If no selection is
    # active, this is NULL, otherwise it's <dataset>__<sample_id>-pastd
    # compound key
    observeEvent({
      plotly::event_data("plotly_selected", source = sample_selection)
    }, {
      selected <- plotly::event_data(
        "plotly_selected", source = sample_selection)
      if (is.null(selected)) {
        selected <- tibble(dataset = character(), sample_id = character())
      } else {
        selected <- featureviz() |>
          FacileViz::input_data() |>
          slice(as.integer(selected$key)) |>
          select(dataset, sample_id)
      }
      current <- paste(state$boxplot_select$dataset,
                       state$boxplot_select$sample_id)
      snew <- paste(selected$dataset, selected$sample_id)
      if (!setequal(current, snew)) {
        state$boxplot_select <- selected
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
    # Stats Table ----------------------------------------------------------------
  
    output$statsdl <- shiny::downloadHandler(
      filename = function() {
        res <- req(isolate(dge()))
        test_type <- if (is_ttest(res)) "ttest" else "anova"
        is.selected <- if (nrow(state$volcano_select)) "subset" else "all"
        sprintf(
          "DGE-%s_%s_%s_%s.csv",
          param(res, "method"),
          test_type,
          name(res),
          is.selected)
      },
      content = function(file) {
        .fdge <- req(isolate(dge()))
        .result <- req(dge.stats())
        if ("padj" %in% colnames(.result)) {
          .result <- rename(.result, FDR = "padj")
        }
        c.order <- c("symbol", "name", "feature_id", "meta", "AveExpr",
                     "FDR", "pval")
        if (is_ttest(.fdge)) {
          c.order <- c(c.order, "logFC", "CI.L", "CI.R", "B", "t")
        } else {
          c.order <- c(c.order, "F")
        }
        c.order <- c(c.order, "assay")
        c.order <- intersect(c.order, colnames(.result))
        .result <- select(.result, !!c.order)
        write.csv(.result, file, row.names = FALSE)
      }
    )
  
    output$stats <- DT::renderDT({
      # Triggered when new result comes in
      res <- req(dge())
      dat <- req(dge.stats())
      num.cols <- colnames(dat)[sapply(dat, is.numeric)]
  
      dtopts <- list(
        deferRender = TRUE, 
        scrollY = 600,
        # scroller = TRUE,
        pageLength = 15,
        lengthMenu = c(15, 30, 50))
  
      dtable <- dat |>
        DT::datatable(
          filter = "top",
          # extensions = "Scroller",
          style = "bootstrap",
          class = "display", width = "100%", rownames = FALSE,
          selection = list(mode = "single", selected = 1, target = "row"),
          options = dtopts) |>
        DT::formatRound(num.cols, 3)
      dtable
    }, server = TRUE)
  
    vals <- list(
      x = dge,
      selected_features = reactive(state$volcano_select),
      selected_samples = reactive(state$boxplot_select),
      .ns = session$ns)
    vals
  })
}

#' @noRd
#' @export
#' @param rmd set to `TRUE` if you are embedding the widged in a
#'   `runtime: shiny` Rmd.
fdgeViewUI <- function(id, rmd = FALSE, ..., debug = FALSE) {
  ns <- shiny::NS(id)
  box <- shinydashboard::box

  # if (rmd) {
  #   withSpinner <- identity 
  # } else {
  #   withSpinner <- function(x) {
  #     shinyWidgets::addSpinner(
  #       x,
  #       spin = getOption("FacileShine.spinner_type"),
  #       color = getOption("FacileShine.spinner_color"))
  #   }
  # }

  volcano.box <- shiny::tags$div(
    style = "margin-top: 10px",
    id = ns("volcanobox"),
    box(
      width = 12, style = "padding-top: 10px",
      shinyWidgets::switchInput(
        ns("volcanotoggle"), 
        size = "mini",
        onLabel = "Yes", offLabel = "No", value = FALSE,
        inline = TRUE),
      shiny::tags$span("Volcano Plot", style = "font-weight: bold"),
      shiny::tags$div(
        id = ns("volcanoplotdiv"),
        shinycssloaders::withSpinner(plotly::plotlyOutput(ns("volcano"))))))

  boxplot.box <- box(
    width = 12,
    id = ns("boxplotbox"),
    shinyjs::hidden(
      shiny::tags$div(
        id = ns("batch_correct_container"),
        shinyWidgets::switchInput(
          ns("batch_correct"), size = "mini",
          onLabel = "Yes", offLabel = "No", value = TRUE,
          inline = TRUE),
        shiny::tags$span("Batch Correction", style = "font-weight: bold"))),
    shinycssloaders::withSpinner({
      shinyjqui::jqui_resizable(plotly::plotlyOutput(ns("boxplot")))
    }))

  shiny::fluidRow(
    # Plots Column
    shiny::column(
      width = 5,
      id = ns("vizcolumn"),
      shiny::tags$div(
        style = "margin-top: 150px",
        shiny::tags$p(shiny::HTML("&nbsp;"))),
      boxplot.box,
      shiny::downloadButton(ns("boxplotdl"), "Download Data"),
      volcano.box),
    # Stats Table Column
    shiny::column(
      width = 7,
      id = ns("statscolumn"),
      # style = "padding: 2px; margin: 5px",
      box(
        width = 12,
        id = ns("statbox"),
        title = "Statistics",
        shiny::tags$div(
          id = ns("dlbuttonbox"),
          style = "padding: 0 0 1em 0; float: right;",
          shiny::downloadButton(ns("statsdl"), "Download")),
        shiny::tags$div(style = "clear: right;"),
        shinycssloaders::withSpinner(DT::DTOutput(ns("stats"))))))
  
}
