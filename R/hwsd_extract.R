#' Extract HWSD v2.0 attributes (point or bbox)
#'
#' Extract soil properties from the HWSD v2.0 database for point locations or
#' bounding box regions.
#'
#' @param coords Point coordinates. Accepts one of the following:
#'   \itemize{
#'     \item Numeric vector `c(lon, lat)` for a single point
#'     \item Matrix with 2 columns (lon, lat) for multiple points
#'     \item Data frame with `lon` and `lat` columns
#'   }
#' @param bbox Bounding box as `c(lon_min, lat_min, lon_max, lat_max)`.
#'   Objects of class `bbox` (e.g., from `sf::st_bbox()`) are also accepted.
#'   Provide either `coords` or `bbox`, not both.
#' @param param Character vector of property names (e.g., `"SAND"`, `"PH_WATER"`).
#'   Use `"ALL"` to select all available attributes (numeric-only for bbox queries).
#'   See `hwsd_props()` for available properties.
#' @param layer Soil layer code, one of `"D1"`, `"D2"`, `"D3"`, `"D4"`, `"D5"`,
#'   `"D6"`, or `"D7"` (from top to bottom).
#' @param path Output path when writing raster (used if `internal = FALSE`).
#' @param ws_path Path to the HWSD v2.0 index grid directory. If the grid is
#'   missing, it will be downloaded automatically. You can set the `WS_PATH`
#'   environment variable in `~/.Renviron` for persistence.
#' @param internal If `TRUE`, return in-memory raster; if `FALSE`, write to `path`.
#' @param tiles_deg Optional tiling size (degrees) for large bboxes. When finite
#'   and smaller than extent, tiles are processed and mosaicked for better
#'   performance.
#' @param cores Number of cores for parallel tiling (uses `parallel::mclapply`
#'   on non-Windows systems).
#' @param verbose If `TRUE`, show progress messages.
#'
#' @return For point queries: a tibble with columns `longitude`, `latitude`,
#'   `parameter`, and `value`. For bbox queries: a `terra::SpatRaster`
#'   (or file path if `internal = FALSE`).
#'
#' @examples
#' \dontrun{
#' # Single point query
#' pt <- hwsd_extract(
#'   coords = c(110, 40),
#'   param = c("SAND", "PH_WATER"),
#'   layer = "D1",
#'   ws_path = "~/data/HWSD2"
#' )
#'
#' # Multiple points using a data frame
#' sites <- data.frame(lon = c(120, 121.5), lat = c(30, 31.2))
#' pts <- hwsd_extract(
#'   coords = sites,
#'   param = "SAND",
#'   layer = "D1",
#'   ws_path = "~/data/HWSD2"
#' )
#'
#' # Bounding box query with tiling
#' sand <- hwsd_extract(
#'   bbox = c(70, 18, 140, 54),
#'   param = "SAND",
#'   layer = "D1",
#'   ws_path = "~/data/HWSD2",
#'   tiles_deg = 5,
#'   cores = 4
#' )
#' }
#'
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
  verbose = FALSE
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

  # load and cache attribute table
  if (is.null(.tidyhwsd_cache$hwsd2)) {
    .tidyhwsd_cache$hwsd2 <- tidyhwsd::hwsd2
  }
  hwsd2 <- .tidyhwsd_cache$hwsd2 |>
    dplyr::filter(LAYER == layer)

  available <- names(hwsd2)
  request_all <- any(tolower(param) == "all")

  if (request_all) {
    param <- available[!available %in% c("HWSD2_SMU_ID", "LAYER")]
  }

  if (mode == "bbox" && request_all) {
    param <- param[
      vapply(
        param,
        function(par) is.numeric(hwsd2[[par]]),
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
      hwsd_extract(
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
        verbose = verbose
      )
    }

    rasters <- if (cores > 1 && .Platform$OS.type != "windows") {
      parallel::mclapply(seq_len(nrow(tiles)), tile_fun, mc.cores = cores)
    } else {
      lapply(seq_len(nrow(tiles)), tile_fun)
    }

    mosaic <- do.call(terra::mosaic, rasters)
    names(mosaic) <- param

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
    # reuse coords_df constructed above
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

    results <- lapply(seq_len(nrow(coords_df)), function(i) {
      smu_id <- smu_ids[i]
      vals <- if (!is.na(smu_id)) {
        hwsd2 |>
          dplyr::filter(HWSD2_SMU_ID == smu_id)
      } else {
        NULL
      }

      rows <- lapply(param, function(par) {
        val <- NA
        if (!is.null(vals) && nrow(vals) > 0 && par %in% names(vals)) {
          col <- vals[[par]]
          if (is.numeric(col)) {
            val <- col[1]
            # HWSD v2.0 uses negative values for NoData (e.g. -9)
            if (!is.na(val) && val < 0) val <- NA_real_
          } else {
            val <- as.character(col[1])
          }
        }
        tibble::tibble(
          longitude = coords_df$lon[i],
          latitude = coords_df$lat[i],
          parameter = par,
          value = val
        )
      })
      dplyr::bind_rows(rows)
    })

    return(dplyr::bind_rows(results))
  }

  # bbox workflow
  extent <- terra::ext(c(location[1], location[3], location[2], location[4]))
  cropped <- terra::crop(ids_rast, extent)

  if (terra::ncell(cropped) == 0) {
    cli::cli_abort("Bounding box does not overlap with the HWSD v2.0 grid.")
  }

  ids_vec <- terra::values(cropped, mat = FALSE)
  param_mat <- matrix(NA_real_, nrow = length(ids_vec), ncol = length(param))

  for (j in seq_along(param)) {
    column <- hwsd2[[param[j]]]
    if (!is.numeric(column)) {
      cli::cli_abort("Parameter {.val {param[j]}} is not numeric and cannot be rasterized.")
    }
    # HWSD v2.0 uses negative values for NoData (e.g. -9)
    column[column < 0] <- NA
    lookup <- stats::setNames(column, hwsd2$HWSD2_SMU_ID)
    param_mat[, j] <- lookup[as.character(ids_vec)]
  }

  ws_stack <- terra::rast(
    lapply(seq_len(length(param)), function(i) cropped)
  )
  ws_stack <- terra::setValues(ws_stack, param_mat)
  names(ws_stack) <- param

  if (internal) {
    return(ws_stack)
  } else {
    outfile <- file.path(path, sprintf("hwsd_%s.tif", layer))
    terra::writeRaster(ws_stack, outfile, overwrite = TRUE)
    return(invisible(outfile))
  }
}
