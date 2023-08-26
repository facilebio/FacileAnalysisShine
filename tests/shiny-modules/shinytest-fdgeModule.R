devtools::load_all(".")
afds <- FacileData::an_fds()
user <- Sys.getenv("USER")
options(facile.log.level.fshine = "trace")

# debug(FacileData:::biocbox.facile_frame)
# debug(FacileData:::.fetch_assay_data)

# All in one module ============================================================
shiny::shinyApp(
  ui = shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::wellPanel(
          FacileShine::facileSampleFiltersSelectInput("rfds", debug = debug))),
      shiny::column(
        width = 9,
        shiny::tags$h2("fdgeAnalysis"),
        fdgeAnalysisUI("analysis")))),
  server = function(input, output) {
    rfds <- FacileShine::facileDataStoreServer("rfds", reactive(afds))
    analysis <- fdgeAnalysisServer("analysis", rfds)
  }
)

if (FALSE) {
  flm <- flm_def(samples(efds), "cell_label")
  res <- fdge(flm, method = "voom")
}