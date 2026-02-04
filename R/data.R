#' HWSD v2.0 component table (all components, layers D1-D7)
#'
#' A compressed data frame containing HWSD v2.0 components keyed by
#' SMU_ID, soil layer, sequence, and share. Used to aggregate gridded SMU indices
#' into soil properties on demand.
#'
#' @format A data frame with columns \code{HWSD2_SMU_ID}, \code{LAYER}, and
#'   40+ soil attribute fields.
"hwsd2_layers"
