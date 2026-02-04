#' List available HWSD v2.0 properties
#'
#' Returns a tibble of property names present in the bundled \code{hwsd2_layers}
#' component table, with type info, descriptions, default aggregation rules, and
#' rounding precision. The \code{agg} and \code{precision} columns are meant to be
#' edited and passed into \code{hwsd_compose()} or \code{hwsd_extract(props = ...)}.
#'
#' @return tibble with columns \code{property}, \code{type}, \code{description},
#'   \code{agg} (default aggregation method), and \code{precision} (rounding step).
#' @export
#' @examples
#' props <- hwsd_props()
#' head(props)
#'
#' # Override aggregation rule for one variable
#' props$agg[props$property == "PH_WATER"] <- "dominant"
hwsd_props <- function() {
  if (is.null(.tidyhwsd_cache$hwsd2_layers)) {
    .tidyhwsd_cache$hwsd2_layers <- tidyhwsd::hwsd2_layers
  }
  hwsd2_layers <- .tidyhwsd_cache$hwsd2_layers

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
    ROOT_DEPTH = "Rootable soil depth class (1-4)",
    PHASE1 = "Soil phase 1",
    PHASE2 = "Soil phase 2",
    ROOTS = "Obstacles to roots (class code)",
    IL = "Impermeable layer (class code)",
    SWR = "Soil water regime (class code)",
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
    PH_WATER = "pH in water (-log(H+))",
    TOTAL_N = "Total nitrogen (g/kg)",
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

  props <- setdiff(names(hwsd2_layers), c("HWSD2_SMU_ID", "LAYER"))
  types <- vapply(hwsd2_layers[props], function(x) class(x)[1], character(1))

  drop_vars <- c(
    "ID", "HWSD2_SMU_ID", "HWSD1_SMU_ID", "WISE30s_SMU_ID",
    "LAYER", "SEQUENCE", "SHARE"
  )
  source_vars <- grep("SOURCE", names(hwsd2_layers), value = TRUE)
  categorical_vars <- c(
    "WRB_PHASES", "WRB4", "WRB2", "FAO90",
    "DRAINAGE", "TEXTURE_USDA", "TEXTURE_SOTER",
    "NSC", "PHASE1", "PHASE2", "ROOTS", "IL", "SWR", "ADD_PROP",
    "COVERAGE",
    source_vars
  )

  agg <- rep("weighted_mean", length(props))
  agg[types %in% c("character", "factor")] <- "weighted_mode"
  agg[props %in% categorical_vars] <- "weighted_mode"
  agg[props %in% drop_vars] <- "drop"

  precision <- rep(NA_real_, length(props))
  precision[types %in% c("numeric", "integer")] <- 1
  precision[props %in% c("PH_WATER")] <- 0.1
  precision[props %in% c("TOTAL_N")] <- 0.01
  precision[props %in% c("ORG_CARBON")] <- 0.001
  precision[props %in% c("BULK", "REF_BULK")] <- 0.05
  precision[props %in% c("TCARBON_EQ", "GYPSUM")] <- 0.1

  # Match descriptions
  desc <- descriptions[props]
  desc[is.na(desc)] <- "No description available"

  tibble::tibble(
    property = props,
    type = types,
    description = as.character(desc),
    agg = agg,
    precision = precision
  )
}
