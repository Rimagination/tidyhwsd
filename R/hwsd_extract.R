#' Extract HWSD v2.0 attributes (point or bbox)
#'
#' @param location A point c(lon, lat) or bbox c(lon_min, lat_min, lon_max, lat_max);
#'   `sf` bbox objects are also accepted.
#' @param param Character vector of property names; `"ALL"` selects all available
#'   attributes (numeric-only for bbox).
#' @param layer Soil layer code ("D1"–"D7").
#' @param path Output path when writing raster (used if `internal = FALSE`).
#' @param ws_path Path to HWSD index grid; will be downloaded if missing.
#' @param internal If `TRUE`, return in-memory raster; if `FALSE`, write to `path`.
#' @param tiles_deg Optional tiling size (degrees) for large bboxes; when finite and
#'   smaller than extent, tiles are processed and mosaicked.
#' @param cores Number of cores for tiling (uses `parallel::mclapply` on non-Windows).
#' @param verbose Show progress messages.
#' @return tibble for point queries; `terra::SpatRaster` (or file path if `internal=FALSE`) for bbox queries.
#' @export
hwsd_extract <- function(
  location,
  param = "ALL",
  layer = "D1",
  path = tempdir(),
  ws_path = file.path(tempdir(), "ws_db"),
  internal = TRUE,
  tiles_deg = Inf,
  cores = 1,
  verbose = FALSE
) {
  # handle sf bbox
  if (inherits(location, "bbox")) {
    location <- unname(location[c("xmin", "ymin", "xmax", "ymax")])
  }

  if (!(length(location) %in% c(2, 4))) {
    cli::cli_abort("location must be a point (lon, lat) or bbox (lon_min, lat_min, lon_max, lat_max).")
  }

  location <- as.numeric(location)

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

  if (length(location) == 4 && request_all) {
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
  if (length(location) == 4 &&
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
        location = c(
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

  # point workflow
  if (length(location) == 2) {
    p <- sf::st_as_sf(
      data.frame(lon = location[1], lat = location[2]),
      coords = c("lon", "lat"),
      crs = 4326
    )

    pixel_id <- terra::extract(ids_rast, p)
    smu_id <- pixel_id[[1]]
    if (length(smu_id) == 0 || is.na(smu_id)) {
      cli::cli_abort("Location falls outside the HWSD v2.0 grid.")
    }

    values <- hwsd2 |>
      dplyr::filter(HWSD2_SMU_ID == smu_id)

    rows <- lapply(param, function(par) {
      tibble::tibble(
        longitude = location[1],
        latitude = location[2],
        parameter = par,
        value = values[[par]]
      )
    })

    return(dplyr::bind_rows(rows))
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
