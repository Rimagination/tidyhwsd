#' Extract HWSD v2.0 attributes (dominant component)
#'
#' @param coords A point c(lon, lat), matrix (2 cols), or data.frame with lon/lat.
#' @param bbox A bbox c(lon_min, lat_min, lon_max, lat_max); `sf` bbox also accepted.
#' @param param Character vector of property names; `"ALL"` selects all available
#'   attributes. For `bbox` queries, non-numeric columns are automatically dropped
#'   when `param = "ALL"`.
#' @param layer Soil layer code ("D1"-"D7"). For `bbox`, must be length 1.
#' @param path Output path when writing raster (used if `internal = FALSE`).
#' @param ws_path Path to HWSD index grid; will be downloaded if missing.
#' @param internal If `TRUE`, return in-memory raster; if `FALSE`, write to `path`.
#' @param tiles_deg Optional tiling size (degrees) for large bboxes; when finite and
#'   smaller than extent, tiles are processed and mosaicked.
#' @param cores Number of cores for tiling (uses `parallel::mclapply` on non-Windows).
#' @param verbose Show progress messages.
#' @param output Output shape for point queries: `"wide"` or `"long"`.
#' @return Tibble with columns `lon`, `lat`, and one column per requested parameter
#'   (wide) or long table for point queries; `terra::SpatRaster` (or file path if
#'   `internal=FALSE`) for bbox queries.
#'
#' @details
#' Dominant component selection follows: prefer SEQUENCE = 1 if present; within
#' SEQUENCE = 1 choose the largest SHARE; if no SEQUENCE = 1 exists, choose the
#' largest SHARE (ties break by smallest SEQUENCE when available).
#' For share-weighted synthesis, use \code{hwsd_compose()}.
#' @examples
#' \dontrun{
#' # Dominant component (default behavior)
#' pt <- hwsd_extract(
#'   coords = c(110, 40),
#'   param = c("SAND", "PH_WATER"),
#'   layer = "D1",
#'   ws_path = "D:/data/HWSD2"
#' )
#'
#' # Share-weighted synthesis using default rules
#' props <- hwsd_props()
#' pt_syn <- hwsd_compose(
#'   coords = c(110, 40),
#'   param = c("SAND", "PH_WATER"),
#'   layer = "D1",
#'   ws_path = "D:/data/HWSD2",
#'   props = props
#' )
#' }
#' @export
hwsd_extract <- function(
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
  output = "wide"
) {
  output <- match.arg(output, c("wide", "long"))

  .hwsd_extract_impl(
    coords = coords,
    bbox = bbox,
    param = param,
    layer = layer,
    path = path,
    ws_path = ws_path,
    internal = internal,
    tiles_deg = tiles_deg,
    cores = cores,
    verbose = verbose,
    agg_spec = NULL,
    precision = NULL,
    dominant_by = "seq1_then_share",
    normalize_share = TRUE,
    share_tol = 1,
    output = output
  )
}

.hwsd_extract_impl <- function(
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
  agg_spec = NULL,
  precision = NULL,
  dominant_by = "seq1_then_share",
  normalize_share = TRUE,
  share_tol = 1,
  output = "wide",
  cat_levels_map = NULL
) {

  # handle input modes
  if (!is.null(coords) && !is.null(bbox)) {
    cli::cli_abort("Provide either `coords` or `bbox`, not both.")
  }
  if (is.null(coords) && is.null(bbox)) {
    cli::cli_abort("Provide `coords = c(lon, lat)` or `bbox = c(lon_min, lat_min, lon_max, lat_max)`.")
  }

  if (!is.null(coords)) {
    # accept numeric vector (single point), matrix, or data.frame
    coords_df <- NULL
    if (is.numeric(coords)) {
      if (length(coords) != 2) {
        cli::cli_abort("`coords` numeric input must be length 2: c(lon, lat). For multiple points use a matrix or data.frame with lon/lat columns.")
      }
      coords_df <- data.frame(
        lon = coords[1],
        lat = coords[2]
      )
    } else if (is.matrix(coords)) {
      if (ncol(coords) != 2) {
        cli::cli_abort("`coords` matrix must have 2 columns: lon, lat.")
      }
      coords_df <- data.frame(lon = coords[, 1], lat = coords[, 2])
    } else if (is.data.frame(coords)) {
      if (!all(c("lon", "lat") %in% names(coords))) {
        cli::cli_abort("`coords` data.frame must have columns `lon` and `lat`.")
      }
      coords_df <- data.frame(lon = coords$lon, lat = coords$lat)
    } else {
      cli::cli_abort("`coords` must be numeric (length 2), matrix (2 cols), or data.frame with lon/lat.")
    }

    mode <- "point"
    location <- as.numeric(unlist(coords_df[1, ]))
  } else {
    if (inherits(bbox, "bbox")) {
      bbox <- unname(bbox[c("xmin", "ymin", "xmax", "ymax")])
    }
    location <- as.numeric(bbox)
    if (length(location) != 4) {
      cli::cli_abort("`bbox` must be length 4: c(lon_min, lat_min, lon_max, lat_max).")
    }
    mode <- "bbox"
  }

  if (length(layer) != 1 && (mode == "bbox" || output == "wide")) {
    cli::cli_abort("`layer` must be length 1 for bbox or wide output. Use `output = 'long'` for multiple layers.")
  }

  # ensure grid exists
  grid_bil <- file.path(ws_path, "HWSD2.bil")
  grid_tif <- file.path(ws_path, "HWSD2.tif")
  grid_file <- NULL
  if (file.exists(grid_bil)) grid_file <- grid_bil
  if (is.null(grid_file) && file.exists(grid_tif)) grid_file <- grid_tif
  if (is.null(grid_file)) {
    ws_path <- hwsd_download(ws_path = ws_path, verbose = verbose)
    grid_file <- grid_bil
  }
  ids_rast <- terra::rast(grid_file)
  if (is.null(names(ids_rast)) || any(names(ids_rast) == "")) {
    names(ids_rast) <- "HWSD2"
  }

  # load and cache component table
  if (is.null(.tidyhwsd_cache$hwsd2_layers)) {
    .tidyhwsd_cache$hwsd2_layers <- tidyhwsd::hwsd2_layers
  }
  hwsd2_layers <- .tidyhwsd_cache$hwsd2_layers

  available <- names(hwsd2_layers)
  request_all <- any(tolower(param) == "all")

  if (request_all) {
    param <- setdiff(available, c("HWSD2_SMU_ID", "LAYER", "SEQUENCE", "SHARE"))
  }

  if (mode == "bbox" && request_all) {
    param <- param[
      vapply(
        param,
        function(par) is.numeric(hwsd2_layers[[par]]),
        logical(1)
      )
    ]
  }

  if (length(param) == 0) {
    cli::cli_abort("No valid parameters selected for the requested output.")
  }

  if (any(!(param %in% available))) {
    cli::cli_abort("One or more soil parameters are not valid for HWSD v2.0.")
  }

  needed_cols <- unique(c("HWSD2_SMU_ID", "LAYER", "SEQUENCE", "SHARE", param))
  needed_cols <- intersect(available, needed_cols)

  categorical_cols <- character(0)
  if (!is.null(agg_spec) && length(agg_spec) > 0) {
    categorical_cols <- names(agg_spec)[agg_spec == "weighted_mode"]
  }
  non_numeric_cols <- names(hwsd2_layers)[
    !vapply(hwsd2_layers, is.numeric, logical(1))
  ]
  categorical_cols <- unique(c(
    intersect(param, non_numeric_cols),
    intersect(param, categorical_cols)
  ))

  if (is.null(cat_levels_map)) {
    cat_levels_map <- list()
    if (length(categorical_cols) > 0) {
      meta_maps <- .hwsd_meta_maps()
      for (col in categorical_cols) {
        x <- hwsd2_layers[[col]]
        if (is.factor(x)) {
          x <- as.character(x)
        }
        if (identical(col, "DRAINAGE")) {
          cat_levels_map[[col]] <- .hwsd_drainage_levels(x)
          next
        }
        meta_key <- .hwsd_meta_key(col)
        if (!is.null(meta_key) && !is.null(meta_maps[[meta_key]])) {
          cat_levels_map[[col]] <- .hwsd_levels_from_meta(meta_maps[[meta_key]], x)
          next
        }
        if (is.numeric(x) || is.integer(x)) {
          vals <- sort(unique(x))
          vals <- vals[is.finite(vals)]
          vals <- vals[!is.na(vals)]
          cats <- as.character(vals)
        } else {
          cats <- unique(as.character(x))
          cats <- cats[!is.na(cats)]
        }
        cat_levels_map[[col]] <- data.frame(
          ID = seq_along(cats),
          label = cats,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
    }
  }

  collapse_fun <- if (is.null(agg_spec)) {
    function(df) collapse_group_dominant(df, dominant_by = dominant_by)
  } else {
    function(df) collapse_group_spec(
      df,
      agg_spec = agg_spec,
      dominant_by = dominant_by,
      normalize_share = normalize_share,
      share_tol = share_tol
    )
  }

  apply_precision <- function(df) {
    if (is.null(precision) || length(precision) == 0) {
      return(df)
    }
    for (col in names(precision)) {
      if (!col %in% names(df)) {
        next
      }
      step <- precision[[col]]
      if (!is.finite(step) || is.na(step) || step <= 0) {
        next
      }
      if (is.numeric(df[[col]])) {
        df[[col]] <- round(df[[col]] / step) * step
      }
    }
    df
  }

  # tiling for large bbox
  if (mode == "bbox" &&
    is.finite(tiles_deg) &&
    tiles_deg > 0 &&
    (location[3] - location[1] > tiles_deg ||
      location[4] - location[2] > tiles_deg)) {
    xbreaks <- seq(location[1], location[3], by = tiles_deg)
    ybreaks <- seq(location[2], location[4], by = tiles_deg)
    if (tail(xbreaks, 1) < location[3]) xbreaks <- c(xbreaks, location[3])
    if (tail(ybreaks, 1) < location[4]) ybreaks <- c(ybreaks, location[4])

    tiles <- expand.grid(
      ix = seq_len(length(xbreaks) - 1),
      iy = seq_len(length(ybreaks) - 1)
    )

    tile_fun <- function(row_id) {
      tile <- tiles[row_id, ]
      .hwsd_extract_impl(
        bbox = c(
          xbreaks[tile$ix],
          ybreaks[tile$iy],
          xbreaks[tile$ix + 1],
          ybreaks[tile$iy + 1]
        ),
        param = param,
        layer = layer,
        path = path,
        ws_path = ws_path,
        internal = TRUE,
        tiles_deg = Inf,
        cores = 1,
        verbose = verbose,
        agg_spec = agg_spec,
        dominant_by = dominant_by,
        normalize_share = normalize_share,
        share_tol = share_tol,
        output = "wide",
        precision = precision,
        cat_levels_map = cat_levels_map
      )
    }

    rasters <- if (cores > 1 && .Platform$OS.type != "windows") {
      parallel::mclapply(seq_len(nrow(tiles)), tile_fun, mc.cores = cores)
    } else {
      lapply(seq_len(nrow(tiles)), tile_fun)
    }

    mosaic <- do.call(terra::mosaic, rasters)
    names(mosaic) <- param

    # Restore factor levels using global category map (stable across tiles)
    if (length(param) > 0 && !is.null(cat_levels_map)) {
      for (j in seq_along(param)) {
        if (!is.null(cat_levels_map[[param[j]]])) {
          levels(mosaic[[j]]) <- cat_levels_map[[param[j]]]
        }
      }
    }

    if (internal) {
      return(mosaic)
    } else {
      outfile <- file.path(path, sprintf("hwsd_%s.tif", layer))
      terra::writeRaster(mosaic, outfile, overwrite = TRUE)
      return(invisible(outfile))
    }
  }

  # point workflow (single or multiple)
  if (mode == "point") {
    if (!exists("coords_df")) {
      coords_df <- data.frame(lon = location[1], lat = location[2])
    }

    coords_mat <- cbind(coords_df$lon, coords_df$lat)
    pixel_vals <- tryCatch(
      terra::extract(ids_rast, coords_mat, ID = FALSE),
      error = function(e) terra::extract(ids_rast, coords_mat)
    )
    if (is.data.frame(pixel_vals) || is.matrix(pixel_vals)) {
      if (ncol(pixel_vals) >= 2) {
        smu_ids <- pixel_vals[, 2]
      } else {
        smu_ids <- pixel_vals[, 1]
      }
    } else {
      smu_ids <- as.vector(pixel_vals)
    }

    coords_df$HWSD2_SMU_ID <- smu_ids

    unique_ids <- unique(smu_ids)
    unique_ids <- unique_ids[!is.na(unique_ids)]

    subset_data <- hwsd2_layers |>
      dplyr::filter(HWSD2_SMU_ID %in% unique_ids, LAYER %in% layer) |>
      dplyr::select(dplyr::any_of(needed_cols))

    agg <- subset_data |>
      dplyr::group_by(HWSD2_SMU_ID, LAYER) |>
      dplyr::group_modify(~ collapse_fun(.x)) |>
      dplyr::ungroup()

    agg <- agg |>
      dplyr::select(dplyr::any_of(c("HWSD2_SMU_ID", "LAYER", param)))
    agg <- apply_precision(agg)

    if (output == "long") {
      res <- dplyr::left_join(coords_df, agg, by = "HWSD2_SMU_ID")
      return(tibble::as_tibble(res))
    }

    layer_val <- as.character(layer[1])
    agg_layer <- agg |>
      dplyr::filter(as.character(LAYER) == layer_val)

    if (nrow(agg_layer) == 0) {
      attrs <- as.data.frame(matrix(NA, nrow = nrow(coords_df), ncol = length(param)))
      names(attrs) <- param
    } else {
      match_idx <- match(smu_ids, agg_layer$HWSD2_SMU_ID)
      attrs <- agg_layer[match_idx, param, drop = FALSE]
    }

    res <- dplyr::bind_cols(coords_df[, c("lon", "lat")], attrs)
    return(tibble::as_tibble(res))
  }

  # bbox workflow
  extent <- terra::ext(c(location[1], location[3], location[2], location[4]))
  cropped <- terra::crop(ids_rast, extent)

  if (terra::ncell(cropped) == 0) {
    cli::cli_abort("Bounding box does not overlap with the HWSD v2.0 grid.")
  }

  ids_vec <- terra::values(cropped, mat = FALSE)
  unique_ids <- unique(ids_vec)
  unique_ids <- unique_ids[!is.na(unique_ids)]

  subset_data <- hwsd2_layers |>
    dplyr::filter(HWSD2_SMU_ID %in% unique_ids, LAYER %in% layer) |>
    dplyr::select(dplyr::any_of(needed_cols))

  agg <- subset_data |>
    dplyr::group_by(HWSD2_SMU_ID, LAYER) |>
    dplyr::group_modify(~ collapse_fun(.x)) |>
    dplyr::ungroup()

  agg <- agg |>
    dplyr::select(dplyr::any_of(c("HWSD2_SMU_ID", "LAYER", param)))
  agg <- apply_precision(agg)

  param_mat <- matrix(NA_real_, nrow = length(ids_vec), ncol = length(param))
  levels_list <- vector("list", length(param))

  for (j in seq_along(param)) {
    column <- agg[[param[j]]]

    if (!is.null(cat_levels_map[[param[j]]])) {
      cats <- cat_levels_map[[param[j]]]
      key_col <- if ("code" %in% names(cats)) "code" else "label"
      col_vals <- as.character(column)
      if (key_col == "code") {
        col_vals <- .hwsd_normalize_codes(col_vals)
      }
      ids <- match(col_vals, as.character(cats[[key_col]]))
      lookup <- stats::setNames(ids, agg$HWSD2_SMU_ID)
      param_mat[, j] <- lookup[as.character(ids_vec)]
      levels_list[[j]] <- cats
    } else if (is.numeric(column)) {
      column[column < 0] <- NA
      lookup <- stats::setNames(column, agg$HWSD2_SMU_ID)
      param_mat[, j] <- lookup[as.character(ids_vec)]
    } else {
      f_col <- factor(column)
      levels_list[[j]] <- data.frame(
        ID = seq_along(levels(f_col)),
        label = levels(f_col),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      vals <- as.integer(f_col)
      lookup <- stats::setNames(vals, agg$HWSD2_SMU_ID)
      param_mat[, j] <- lookup[as.character(ids_vec)]
    }
  }

  ws_stack <- terra::rast(
    lapply(seq_len(length(param)), function(i) cropped)
  )
  ws_stack <- terra::setValues(ws_stack, param_mat)
  names(ws_stack) <- param

  # Assign levels to categorical layers
  for (j in seq_along(param)) {
    if (!is.null(levels_list[[j]])) {
      levels(ws_stack[[j]]) <- levels_list[[j]]
    }
  }

  if (internal) {
    return(ws_stack)
  } else {
    outfile <- file.path(path, sprintf("hwsd_%s.tif", layer))
    terra::writeRaster(ws_stack, outfile, overwrite = TRUE)
    return(invisible(outfile))
  }
}
