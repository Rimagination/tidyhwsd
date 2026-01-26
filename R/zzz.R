# Package-level cache
.tidyhwsd_cache <- new.env(parent = emptyenv())

# avoid CMD check notes on NSE columns
utils::globalVariables(c("LAYER", "HWSD2_SMU_ID"))

# HWSD v2.0 endpoints
server <- function() {
  fao_base <- "https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org"
  list(
    mdb = file.path(fao_base, "HWSD/HWSD2_DB.zip"),
    grid = file.path(fao_base, "HWSD/HWSD2_RASTER.zip")
  )
}
