devtools::load_all(".")
afds <- FacileData::an_fds()
user <- Sys.getenv("USER")
options(facile.log.level.fshine = "trace")

gdb <- sparrow::getMSigGeneSetDb("h", "human", "ensembl")

dge.res <- afds |> 
  FacileData::filter_samples(cell_abbrev %in% c("CNT", "DCT")) |> 
  flm_def("cell_abbrev", "DCT", "CNT") |> 
  fdge(assay_name = "scrnaseq", method = "voom")

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
        shiny::tags$h2("ffseaAnalysis"),
        ffseaAnalysisUI("ffsea")))),
  server = function(input, output) {
    rfds <- FacileShine::facileDataStoreServer("rfds", reactive(afds))
    ffsea <- ffseaAnalysisServer("ffsea", rfds, dge.res, shiny::reactive(gdb))
  }
)
