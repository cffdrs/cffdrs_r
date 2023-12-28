test_that("The function Runs", {
  ## Make it run
  test_fbpRaster <- rast(
    system.file("extdata", "test_fbpRaster.tif", package = "cffdrs")
  )
  input <- test_fbpRaster
  # Stack doesn't hold the raster layer names, we have to assign
  # them:
  names(input) <- c(
    "FuelType", "LAT", "LONG", "ELV", "FFMC", "BUI", "WS", "WD", "GS",
    "Dj", "D0", "hr", "PC", "PDF", "GFL", "cc", "theta", "Accel", "Aspect",
    "BUIEff", "CBH", "CFL", "ISI"
  )

  expect_no_error(fbpRaster(input = input))

})

test_that("Output length is input length", {

  test_fbpRaster <- rast(
    system.file("extdata", "test_fbpRaster.tif", package = "cffdrs")
  )
  input <- test_fbpRaster
  # Stack doesn't hold the raster layer names, we have to assign
  # them:
  names(input) <- c(
    "FuelType", "LAT", "LONG", "ELV", "FFMC", "BUI", "WS", "WD", "GS",
    "Dj", "D0", "hr", "PC", "PDF", "GFL", "cc", "theta", "Accel", "Aspect",
    "BUIEff", "CBH", "CFL", "ISI"
  )

  ## Equal input / output length.
  expect_equal(object = nlyr(fbpRaster(input = input)), expected = 8)

})

test_that("The function produces the expected output.", {
  ## Equal Output
  test_fbpRaster <- rast(
    system.file("extdata", "test_fbpRaster.tif", package = "cffdrs")
  )
  input <- test_fbpRaster
  # Stack doesn't hold the raster layer names, we have to assign
  # them:
  names(input) <- c(
    "FuelType", "LAT", "LONG", "ELV", "FFMC", "BUI", "WS", "WD", "GS",
    "Dj", "D0", "hr", "PC", "PDF", "GFL", "cc", "theta", "Accel", "Aspect",
    "BUIEff", "CBH", "CFL", "ISI"
  )

  test_fbpRaster_out <- rast(system.file("./inst/extdata/test_fbpRaster_out.tif", package = "cffdrs"))
  expect_equal(object = round(fbpRaster(input = input)[[1]][],6),expected = round(test_fbpRaster_out[[1]][],6))

})
