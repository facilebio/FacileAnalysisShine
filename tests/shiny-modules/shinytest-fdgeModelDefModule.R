options(facile.log.level.fshine = "trace")
devtools::load_all(".")

efds <- FacileData::exampleFacileDataSet()
pdata <- FacileData::samples(efds) |>
  FacileData::with_sample_covariates()

user <- Sys.getenv("USER")

ignore <- names(pdata)[sapply(pdata, is.numeric)]
ignore <- NULL
options(facile.log.level.fshine = "trace")


shiny::shinyApp(
  ui = shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::wellPanel(
          FacileShine::facileSampleFiltersSelectInput("rfds"))),
      shiny::column(
        width = 9,
        shiny::tags$h2("fdgeModelDef"),
        flmDefRunUI("model", debug = TRUE)))),
  
  server = function(input, output) {
    rfds <- FacileShine::facileDataStoreServer(
      "rfds", shiny::reactive(efds),
      ignore_sample_covariates = shiny::reactive(ignore))
    model <- flmDefRunServer("model", rfds, debug = TRUE)
  }
)
