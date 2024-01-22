test_fwi_raster <- function(name, fct_fwi) {
  input <- rast(system.file("extdata", "test_rast_day01.tif", package = "cffdrs"))
  names(input) <- c("temp", "rh", "ws", "prec")
  test_raster(name,
              input,
              fct_fwi)
}

test_that("fwiRaster_test1", {
  # should produce same results as test 2
  test_fwi_raster(
    "fwiRaster_test1_and_2",
    function(input) {
      fwiRaster(input = input)
    }
  )
})

test_that("fwiRaster_test2", {
  # should produce same results as test 1
  test_fwi_raster(
    "fwiRaster_test1_and_2",
    function(input) {
      fwiRaster(input = input, out = "all")
    }
  )
})

test_that("fwiRaster_test3", {
  test_fwi_raster(
    "fwiRaster_test3",
    function(input) {
      fwiRaster(input = input, out = "fwi")
    }
  )
})
