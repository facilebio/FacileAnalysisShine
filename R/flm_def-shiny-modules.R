#' A shiny module that generates a linear model definition via `flm_def`.
#'
#' ```
#' model_info <- fds |>
#'   FacileData::filter_samples(indication == "BLCA") |>
#'   flm_def(covariate = "sample_type", numer = "tumor", denom = "normal",
#'           batch = "sex")
#' ```
#'
#' @export
#' @importFrom shiny renderText
#' @importFrom shinyjs toggleElement
#' @return A `ReactiveFacileLinearModelDefinition` object, the output from
#'   [flm_def()].
flmDefRunServer <- function(id, rfds, default_covariate = NULL,
                            ..., debug = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  shiny::moduleServer(id, function(input, output, session) {

    testcov <- FacileShine::categoricalSampleCovariateSelectServer(
      "testcov", rfds, include1 = FALSE, default_covariate = default_covariate,
      with_none = FALSE)
    
    numer <- FacileShine::categoricalSampleCovariateLevelsSelectServer(
      "numer", testcov)
    denom <- FacileShine::categoricalSampleCovariateLevelsSelectServer(
      "denom", testcov)
    
    
    # the "batch" covariate is what I'm calling the extra/batch-level
    # covariates. the entry selected in the testcov is removed from the
    # available elemetns to select from here
    batchcov <- FacileShine::categoricalSampleCovariateSelectServer(
      "batchcov", rfds, include1 = FALSE, exclude = testcov$covariate,
      with_none = FALSE)
    
    model <- reactive({
      samples. <- FacileShine::active_samples(rfds)
      shiny::validate(
        shiny::need(
          nrow(samples.) >= 3L, "Need at least 3 samples for a linear model")
      )
      
      testcov. <- name(testcov)
      req(!unselected(testcov.))
      
      numer. <- numer$values()
      denom. <- denom$values()
      batch. <- name(batchcov)
      
      is.anova <- !unselected(testcov.) &&
        unselected(numer.) && unselected(denom.)
      is.ttest <- !unselected(testcov.) &&
        !unselected(numer.) && !unselected(denom.) &&
        !setequal(numer., denom.)
      if (is.ttest) {
        req(all(c(numer., denom.) %in% testcov$levels()))
      }
      
      is_sane <- is.anova || is.ttest
      
      if (is_sane) {
        out <- flm_def(samples., testcov., numer = numer., denom = denom.,
                batch = batch.)
      } else {
        out <- NULL
      } 

      out
    })
    
    status. <- reactive({
      req(initialized(rfds))
      model. <- model()
      status(model., with_warnings = TRUE)
    })
    
    observeEvent(status.(), {
      s <- status.()
      iserr <- is(s, "FacileAnalysisStatusError")
      shinyjs::toggleElement("messagebox", condition = !iserr)
      if (iserr) {
        shinyWidgets::sendSweetAlert(session, "Error building model",
                                     text = s, type = "error")
      }
    })
    
    output$message <- renderText({
      model. <- model()
      if (is.null(model.)) {
        msg <- "Undefined model"
      } else {
        msg <- status.()
      }
      msg
    })
    
    if (debug) {
      output$debug <- shiny::renderText({
        model. <- req(model())
        format(model.)
      })
    }
    
    vals <- list(
      faro = model,
      testcov = testcov,
      numer = numer,
      denom = denom,
      batchcov = batchcov,
      .ns = session$ns)
    
    class(vals) <- c("ReactiveFacileLinearModelDefinition",
                     "ReactiveFacileAnalysisResult",
                     "FacileLinearModelDefinition")
    vals    
  })
}

#' @noRd
#' @export
flmDefRunUI <- function(id, ..., debug = FALSE) {
  ns <- shiny::NS(id)

  out <- shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 2,
        FacileShine::categoricalSampleCovariateSelectInput(
          ns("testcov"),
          label = "Group to test",
          multiple = FALSE)),
      shiny::column(
        width = 4,
        FacileShine::categoricalSampleCovariateLevelsSelectInput(
          ns("numer"),
          label = "Numerator",
          multiple = TRUE)),
      shiny::column(
        width = 4,
        FacileShine::categoricalSampleCovariateLevelsSelectInput(
          ns("denom"),
          label = "Denominator",
          multiple = TRUE)),
      shiny::column(
        width = 2,
        FacileShine::categoricalSampleCovariateSelectInput(
          ns("batchcov"),
          label = "Control for",
          multiple = TRUE))),
    shinyjs::hidden(
      shiny::wellPanel(
        id = ns("messagebox"), 
        shiny::textOutput(ns("message"))))
  )

  if (debug) {
    out <- shiny::tagList(
      out,
      shiny::verbatimTextOutput(ns("debug"), placeholder = TRUE))
  }
  
  out
}
