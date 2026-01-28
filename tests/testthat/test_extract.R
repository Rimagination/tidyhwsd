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
    overwrite = TRUE
  )

  num_cols <- names(tidyhwsd::hwsd2)[vapply(tidyhwsd::hwsd2, is.numeric, logical(1))]
  res <- hwsd_extract(
    coords = c(0.5, 0.5),
    param = num_cols[1],
    layer = tidyhwsd::hwsd2$LAYER[1],
    ws_path = tmp
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1)
  # Verify that we got an actual value in the parameter column
  val <- res[[num_cols[1]]]
  expect_false(is.na(val))
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
    overwrite = TRUE
  )

  num_cols <- names(tidyhwsd::hwsd2)[vapply(tidyhwsd::hwsd2, is.numeric, logical(1))]

  res <- hwsd_extract(
    bbox = c(0, 0, 1, 1),
    param = num_cols[1],
    layer = tidyhwsd::hwsd2$LAYER[1],
    ws_path = tmp
  )

  expect_s4_class(res, "SpatRaster")
})

test_that("data.frame multi-point input works", {
  skip_on_cran()
  tmp <- tempdir()
  id <- tidyhwsd::hwsd2$HWSD2_SMU_ID[1]
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:4326")
  terra::values(r) <- id
  terra::writeRaster(r,
    filename = file.path(tmp, "HWSD2.bil"),
    filetype = "ENVI",
    overwrite = TRUE
  )

  num_cols <- names(tidyhwsd::hwsd2)[vapply(tidyhwsd::hwsd2, is.numeric, logical(1))]

  # Test with data.frame input
  sites <- data.frame(lon = c(0.5, 1.5), lat = c(0.5, 1.5))
  res <- hwsd_extract(
    coords = sites,
    param = num_cols[1],
    layer = tidyhwsd::hwsd2$LAYER[1],
    ws_path = tmp
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)
  expect_equal(res$lon, c(0.5, 1.5))
  expect_equal(res$lat, c(0.5, 1.5))
})

test_that("ocean/invalid coordinates return NA", {
  skip_on_cran()
  tmp <- tempdir()
  # Create a raster with NA values
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:4326")
  terra::values(r) <- NA
  terra::writeRaster(r,
    filename = file.path(tmp, "HWSD2.bil"),
    filetype = "ENVI",
    overwrite = TRUE
  )

  num_cols <- names(tidyhwsd::hwsd2)[vapply(tidyhwsd::hwsd2, is.numeric, logical(1))]

  res <- hwsd_extract(
    coords = c(1, 1),
    param = num_cols[1],
    layer = tidyhwsd::hwsd2$LAYER[1],
    ws_path = tmp
  )

  expect_s3_class(res, "tbl_df")
  # In wide format, the parameter column should contain NA
  val <- res[[num_cols[1]]]
  expect_true(is.na(val[1]))
})

test_that("input validation errors work correctly", {
  skip_on_cran()

  # coords and bbox both provided

  expect_error(
    hwsd_extract(coords = c(0, 0), bbox = c(0, 0, 1, 1)),
    "either.*coords.*or.*bbox"
  )

  # Neither coords nor bbox provided
  expect_error(
    hwsd_extract(),
    "coords.*or.*bbox"
  )

  # Invalid coords length
  expect_error(
    hwsd_extract(coords = c(1, 2, 3)),
    "length 2"
  )
})
