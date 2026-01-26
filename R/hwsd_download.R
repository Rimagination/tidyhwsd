#' Download HWSD v2.0 index grid
#'
#' Downloads the HWSD v2.0 gridded index file to a desired output path
#' for use with \code{hwsd_extract()}.
#'
#' @param ws_path Path where the HWSD v2.0 grid will be stored.
#' @param verbose Show progress messages.
#' @return The path used.
#' @export
hwsd_download <- function(
  ws_path = file.path(tempdir(), "ws_db"),
  verbose = FALSE
) {
  if (Sys.getenv("WS_PATH") != "") {
    ws_path <- Sys.getenv("WS_PATH")
  }

  # ensure directory exists
  if (!dir.exists(ws_path)) {
    if (verbose) {
      cli::cli_inform("Creating directory {.path {ws_path}}")
    }
    dir.create(ws_path, recursive = TRUE, showWarnings = FALSE)
  }

  # if grid already exists, skip
  if (file.exists(file.path(ws_path, "HWSD2.bil"))) {
    if (verbose) {
      cli::cli_inform("Grid file exists, skipping download. Use {.path {ws_path}} in your calls.")
    }
    return(ws_path)
  }

  urls <- server()
  dest_zip <- file.path(ws_path, "hwsd2_raster.zip")

  resp <- httr::GET(
    urls$grid,
    httr::write_disk(dest_zip, overwrite = TRUE)
  )
  httr::stop_for_status(resp)

  utils::unzip(dest_zip, exdir = ws_path)
  file.remove(dest_zip)

  if (verbose) {
    cli::cli_inform("Downloaded HWSD v2.0 grid to {.path {ws_path}}")
  }

  ws_path
}
