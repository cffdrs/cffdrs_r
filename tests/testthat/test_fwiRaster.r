test_raster <- function(name, fct) {
  day01src <- rast(
    system.file("extdata", "test_rast_day01.tif", package = "cffdrs")
  )
  names(day01src) <- c("temp", "rh", "ws", "prec")
  day01 <- crop(day01src, c(250, 255, 47, 51))

  test_fwi <- read_raster(name)
  test_fwi <- crop(test_fwi, c(250, 255, 47, 51))

  day01_out <- fct(day01)
  out_cols <- setdiff(names(day01_out), toupper(names(day01)))
  # we don't actually know the names of the columns from the file, so assign from output
  # names(test_fwi) <- c("TEMP", "RH", "WS", "PREC", "FFMC", "DMC", "DC", "ISI", "BUI", "FWI", "DSR")
  names(test_fwi) <- names(day01_out)

  m <- minmax(day01_out[[out_cols]] - test_fwi[[out_cols]])
  expect_true(all(abs(m) < 1e-5))

  day01_dt <- data.frame(day01_out)
  fwi_dt <- data.frame(test_fwi)

  expect_true(all(abs(day01_dt - fwi_dt) < 1e-5))
}

test_that("fwiRaster_test1", {
  test_raster(
    "fwiRaster_test1",
    function(input) {
      fwiRaster(input = input)
    }
  )
})

test_that("fwiRaster_test2", {
  test_raster(
    "fwiRaster_test2",
    function(input) {
      fwiRaster(input = input, out = "all")
    }
  )
})

test_that("fwiRaster_test3", {
  test_raster(
    "fwiRaster_test3",
    function(input) {
      fwiRaster(input = input, out = "fwi")
    }
  )
})
