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




library(FacileData)

devtools::load_all(".")
options(facile.log.level.fshine = "trace")
options(facile.log.level.fanalysis = "trace")

efds <- FacileData::exampleFacileDataSet()
gdb <- sparrow::getMSigGeneSetDb("h", "human", "entrez")

pca.crc <- efds |>
  FacileData::filter_samples(indication == "CRC") |>
  fpca()
pca.gsea <- ffseaGadget(pca.crc, gdb)

if (FALSE) {
  pca.gsea <- ffsea(pca.crc, gdb, dim = 1, methods = c("cameraPR", "fgsea"))
  shine(pca.gsea)
}


ttest.res <- efds |>
  FacileData::filter_samples(indication == "CRC") |>
  flm_def(covariate = "sample_type",
                 numer = "tumor", denom = "normal", batch = "sex") |>
  fdge(method = "voom")
ttest.gsea <- ffseaGadget(ttest.res, gdb)

stage.anova <- efds |>
  FacileData::filter_samples(indication == "BLCA") |>
  flm_def(covariate = "stage", batch = "sex") |>
  fdge(method = "voom")
anova.gsea <- ffseaGadget(stage.anova, gdb)

