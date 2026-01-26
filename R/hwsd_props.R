#' List available HWSD v2.0 properties
#'
#' Returns a tibble of property names present in the bundled \code{hwsd2} table,
#' with simple type info to help selection.
#' @return tibble with columns \code{property}, \code{type}
#' @export
hwsd_props <- function() {
  if (is.null(.tidyhwsd_cache$hwsd2)) {
    .tidyhwsd_cache$hwsd2 <- tidyhwsd::hwsd2
  }
  hwsd2 <- .tidyhwsd_cache$hwsd2

  props <- setdiff(names(hwsd2), c("HWSD2_SMU_ID", "LAYER"))
  types <- vapply(hwsd2[props], function(x) class(x)[1], character(1))

  tibble::tibble(
    property = props,
    type = types
  )
}
