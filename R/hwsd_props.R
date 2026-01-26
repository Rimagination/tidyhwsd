#' List available HWSD v2.0 properties
#'
#' Returns a tibble of property names present in the bundled \code{hwsd2} table,
#' with type info and short descriptions to help selection.
#'
#' @return tibble with columns \code{property}, \code{type}, \code{description}
#' @export
#' @examples
#' hwsd_props()
hwsd_props <- function() {
  if (is.null(.tidyhwsd_cache$hwsd2)) {
    .tidyhwsd_cache$hwsd2 <- tidyhwsd::hwsd2
  }
  hwsd2 <- .tidyhwsd_cache$hwsd2

  # Property descriptions based on HWSD v2.0 documentation
  descriptions <- c(
    ID = "Internal row identifier",
    HWSD2_SMU_ID = "HWSD v2.0 Soil Mapping Unit ID",
    NSC_MU_SOURCE1 = "National Soil Classification source 1",
    NSC_MU_SOURCE2 = "National Soil Classification source 2",
    WISE30s_SMU_ID = "WISE30s Soil Mapping Unit ID",
    HWSD1_SMU_ID = "HWSD v1.x Soil Mapping Unit ID",
    COVERAGE = "Geographic coverage code",
    SEQUENCE = "Sequence number within SMU",
    SHARE = "Share of soil unit within SMU (%)",
    NSC = "National Soil Classification code",
    WRB_PHASES = "WRB phase qualifiers",
    WRB4 = "WRB 4th edition classification",
    WRB2 = "WRB 2nd edition classification",
    FAO90 = "FAO90 soil classification",
    ROOT_DEPTH = "Reference depth for rooting (cm)",
    PHASE1 = "Soil phase 1",
    PHASE2 = "Soil phase 2",
    ROOTS = "Obstacles to roots",
    IL = "Impermeable layer presence",
    SWR = "Soil water regime",
    DRAINAGE = "Drainage class",
    AWC = "Available water capacity (mm/m)",
    ADD_PROP = "Additional properties",
    LAYER = "Soil layer code (D1-D7)",
    TOPDEP = "Top depth of layer (cm)",
    BOTDEP = "Bottom depth of layer (cm)",
    COARSE = "Coarse fragments >2mm (%vol)",
    SAND = "Sand content 50-2000 um (%wt)",
    SILT = "Silt content 2-50 um (%wt)",
    CLAY = "Clay content <2 um (%wt)",
    TEXTURE_USDA = "USDA texture class",
    TEXTURE_SOTER = "SOTER texture class",
    BULK = "Bulk density (g/cm3)",
    REF_BULK = "Reference bulk density (g/cm3)",
    ORG_CARBON = "Organic carbon (%wt)",
    PH_WATER = "pH in water",
    TOTAL_N = "Total nitrogen (%wt)",
    CN_RATIO = "Carbon to nitrogen ratio",
    CEC_SOIL = "CEC of whole soil (cmol/kg)",
    CEC_CLAY = "CEC of clay fraction (cmol/kg)",
    CEC_EFF = "Effective CEC (cmol/kg)",
    TEB = "Total exchangeable bases (cmol/kg)",
    BSAT = "Base saturation (%)",
    ALUM_SAT = "Aluminum saturation (%)",
    ESP = "Exchangeable sodium percentage (%)",
    TCARBON_EQ = "Total calcium carbonate equivalent (%)",
    GYPSUM = "Gypsum content (%)",
    ELEC_COND = "Electrical conductivity (dS/m)"
  )

  props <- setdiff(names(hwsd2), c("HWSD2_SMU_ID", "LAYER"))
  types <- vapply(hwsd2[props], function(x) class(x)[1], character(1))

  # Match descriptions
  desc <- descriptions[props]
  desc[is.na(desc)] <- "No description available"

  tibble::tibble(
    property = props,
    type = types,
    description = as.character(desc)
  )
}
