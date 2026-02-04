#' Synthesize HWSD v2.0 attributes using per-variable aggregation rules
#'
#' @inheritParams hwsd_extract
#' @param props A tibble/data.frame from \code{hwsd_props()} (or a named vector of
#'   aggregation methods). Modify the \code{agg} column to control synthesis. If a
#'   \code{precision} column is present, it is used to round numeric outputs.
#' @return Same as \code{hwsd_extract()}.
#' @examples
#' \dontrun{
#' props <- hwsd_props()
#' # Share-weighted synthesis with default rules
#' pt_syn <- hwsd_compose(
#'   coords = c(110, 40),
#'   param = c("SAND", "PH_WATER"),
#'   layer = "D1",
#'   ws_path = "D:/data/HWSD2",
#'   props = props
#' )
#' }
#' @export
hwsd_compose <- function(
  coords = NULL,
  bbox = NULL,
  param = "ALL",
  layer = "D1",
  path = tempdir(),
  ws_path = file.path(tempdir(), "ws_db"),
  internal = TRUE,
  tiles_deg = Inf,
  cores = 1,
  verbose = FALSE,
  props = hwsd_props(),
  output = "wide"
) {
  output <- match.arg(output, c("wide", "long"))

  spec <- .resolve_agg_spec(props, param)

  .hwsd_extract_impl(
    coords = coords,
    bbox = bbox,
    param = spec$param,
    layer = layer,
    path = path,
    ws_path = ws_path,
    internal = internal,
    tiles_deg = tiles_deg,
    cores = cores,
    verbose = verbose,
    agg_spec = spec$agg_spec,
    precision = spec$precision,
    dominant_by = "seq1_then_share",
    normalize_share = TRUE,
    share_tol = 1,
    output = output
  )
}

.resolve_agg_spec <- function(props, param) {
  if (is.null(props)) {
    props <- hwsd_props()
  }

  if (is.vector(props) && !is.list(props)) {
    if (is.null(names(props))) {
      cli::cli_abort("`props` must be a named vector or a data.frame with columns 'property' and 'agg'.")
    }
    props_df <- data.frame(
      property = names(props),
      agg = as.character(props),
      stringsAsFactors = FALSE
    )
  } else if (is.data.frame(props)) {
    if (!all(c("property", "agg") %in% names(props))) {
      cli::cli_abort("`props` must contain columns 'property' and 'agg'.")
    }
    cols <- c("property", "agg")
    if ("precision" %in% names(props)) {
      cols <- c(cols, "precision")
    }
    props_df <- props[, cols]
  } else {
    cli::cli_abort("`props` must be a named vector or a data.frame with columns 'property' and 'agg'.")
  }

  props_df$property <- as.character(props_df$property)
  props_df$agg <- as.character(props_df$agg)

  valid_methods <- c("dominant", "weighted_mean", "weighted_mode", "drop")
  if (any(!props_df$agg %in% valid_methods & !is.na(props_df$agg))) {
    bad <- unique(props_df$agg[!props_df$agg %in% valid_methods & !is.na(props_df$agg)])
    cli::cli_abort("Invalid aggregation method(s) in `props`: {bad}.")
  }

  request_all <- any(tolower(param) == "all")
  if (request_all) {
    param_use <- props_df$property[!is.na(props_df$agg) & props_df$agg != "drop"]
  } else {
    param_use <- param
  }

  missing <- setdiff(param_use, props_df$property)
  if (length(missing) > 0) {
    cli::cli_abort("Properties not found in `props`: {missing}.")
  }

  agg_spec <- props_df$agg[match(param_use, props_df$property)]
  names(agg_spec) <- param_use

  precision <- NULL
  if ("precision" %in% names(props_df)) {
    precision <- props_df$precision[match(param_use, props_df$property)]
    names(precision) <- param_use
  }

  drop_idx <- is.na(agg_spec) | agg_spec == "drop"
  if (any(drop_idx)) {
    dropped <- names(agg_spec)[drop_idx]
    agg_spec <- agg_spec[!drop_idx]
    param_use <- param_use[!drop_idx]
    if (!is.null(precision)) {
      precision <- precision[!drop_idx]
    }
    if (length(dropped) > 0) {
      cli::cli_warn("Dropping fields with agg='drop': {dropped}.")
    }
  }

  if (length(param_use) == 0) {
    cli::cli_abort("No valid parameters after applying aggregation rules.")
  }

  list(param = param_use, agg_spec = agg_spec, precision = precision)
}
