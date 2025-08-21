library(FacileBiocData)
devtools::load_all(".")
yf <- facilitate(example_bioc_data("DGEList"), organism = "Homo sapiens")
xpca <- fpca(yf)
shine(xpca)

