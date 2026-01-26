test_that("point extract works with tiny mock grid", {
  skip_on_cran()
  tmp <- tempdir()
  # create a 1x1 raster with a valid SMU_ID from hwsd2
  id <- tidyhwsd::hwsd2$HWSD2_SMU_ID[1]
  r <- terra::rast(nrows = 1, ncols = 1, xmin = 0, xmax = 1, ymin = 0, ymax = 1, crs = "EPSG:4326")
  terra::values(r) <- id
  terra::writeRaster(r,
                     filename = file.path(tmp, "HWSD2.bil"),
                     filetype = "ENVI",
                     overwrite = TRUE)

  num_cols <- names(tidyhwsd::hwsd2)[vapply(tidyhwsd::hwsd2, is.numeric, logical(1))]
  res <- hwsd_extract(
    location = c(0.5, 0.5),
    param = num_cols[1],
    layer = tidyhwsd::hwsd2$LAYER[1],
    ws_path = tmp
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1)
})

test_that("bbox extract returns SpatRaster", {
  skip_on_cran()
  tmp <- tempdir()
  id <- tidyhwsd::hwsd2$HWSD2_SMU_ID[1]
  r <- terra::rast(nrows = 1, ncols = 1, xmin = 0, xmax = 1, ymin = 0, ymax = 1, crs = "EPSG:4326")
  terra::values(r) <- id
  terra::writeRaster(r,
                     filename = file.path(tmp, "HWSD2.bil"),
                     filetype = "ENVI",
                     overwrite = TRUE)

  num_cols <- names(tidyhwsd::hwsd2)[vapply(tidyhwsd::hwsd2, is.numeric, logical(1))]

  res <- hwsd_extract(
    location = c(0, 0, 1, 1),
    param = num_cols[1],
    layer = tidyhwsd::hwsd2$LAYER[1],
    ws_path = tmp
  )

  expect_s4_class(res, "SpatRaster")
})
