devtools::load_all(".")

datadir <- "~/workspace/facilebio/data"
# afds <- FacileData::an_fds()
user <- Sys.getenv("USER")
options(facile.log.level.fshine = "trace")

# debug(FacileData:::biocbox.facile_frame)
# debug(FacileData:::.fetch_assay_data)

# All in one module ============================================================
shiny::shinyApp(
  ui = shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::wellPanel(
          FacileShine::facileDataSetSelectInput("fdslist", debug = debug),
          FacileShine::facileSampleFiltersSelectInput("rfds", debug = debug))),
      shiny::column(
        width = 9,
        shiny::tags$h2("fdgeAnalysis"),
        fdgeAnalysisUI("analysis")))),
  server = function(input, output) {
    fdslist <- FacileShine::facileDataSetSelectServer(
      "fdslist", reactive(datadir))
    # rfds <- FacileShine::facileDataStoreServer("rfds", reactive(afds))
    rfds <- FacileShine::facileDataStoreServer(
      "rfds", fdslist$path,
      user = user)
    analysis <- fdgeAnalysisServer("analysis", rfds)
  }
)

if (FALSE) {
  flm <- flm_def(samples(efds), "cell_label")
  res <- fdge(flm, method = "voom")
}