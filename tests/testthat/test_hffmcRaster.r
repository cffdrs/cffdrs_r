test_that("hffmcRaster", {
  test_hffmcRaster <- rast(system.file("extdata", "test_rast_hour01.tif", package = "cffdrs"))
  names(test_hffmcRaster) <- c("temp", "rh", "ws", "prec")

  hour02 <- rast(system.file("extdata", "test_rast_hour02.tif", package = "cffdrs"))
  # Assign variable names to the layers:
  names(hour02) <- c("temp", "rh", "ws", "prec")

  # so we can reuse this as an input
  output1 <- hffmcRaster(test_hffmcRaster)

  test_that("hffmcRaster_test1", {
    test_raster(
      "hffmcRaster_test1",
      test_hffmcRaster,
      function(input) {
        output1
      }
    )
  })

  test_that("hffmcRaster_test2", {
    test_raster(
      "hffmcRaster_test2",
      hour02,
      function(input) {
        hffmcRaster(input, ffmc_old = output1)
      }
    )
  })

  hour02 <- c(hour02, setValues(hour02$temp, 50))
  # Re-assign variable names to the layers:
  names(hour02) <- c("temp", "rh", "ws", "prec", "bui")

  test_that("hffmcRaster_test3", {
    test_raster(
      "hffmcRaster_test3",
      hour02,
      function(input) {
        hffmcRaster(input, ffmc_old = output1, hourlyFWI = TRUE)
      }
    )
  })
  # HACK: for now just to get it to not complain that the test is empty
  expect_equal(TRUE, TRUE)
})
