# Things fail when there is multi-modal data stored in the faciledataset, and
# the assay you run the analysis on isn't available on all of the samples you
# sent into the analysis.
#
# This is because `initialized.FacilePcaAnalysisResult` tests to see that the
# number of rows in the output from `tidy(fpca(samples)) == nrow(samples)`, and
# if some rows in `samples` don't have data from the assay under test, then we
# get hosed
# efds <- FacileData::exampleFacileDataSet()e

devtools::load_all(".")
efds <- FacileData::FacileDataSet("~/workspace/facilebio/data/BulkKPMPDataSet")
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
        shiny::tags$h2("fpcaAnalysis"),
        fpcaAnalysisUI("analysis")))),
  server = function(input, output) {
    rfds <- FacileShine::facileDataStoreServer("rfds", reactive(efds))
    analysis <- fpcaAnalysisServer("analysis", rfds)
  }
)

if (FALSE) {
  asamples <- samples(efds) |> collect(n = Inf)
  kpca <- fpca(asamples, "scrnaseq")
}