test_that("The function Runs", {
  ## Make it run
  day01src <- rast(
    system.file("extdata", "test_rast_day01.tif", package = "cffdrs")
  )
  day01 <- crop(day01src, c(250, 255, 47, 51))
  # assign variable names:
  names(day01) <- c("temp", "rh", "ws", "prec")

  expect_no_error(fwiRaster(input = day01))

})

test_that("Output length is input length", {

  ## Equal input / output length.

  day01src <- rast(
    system.file("extdata", "test_rast_day01.tif", package = "cffdrs")
  )
  day01 <- crop(day01src, c(250, 255, 47, 51))
  # assign variable names:
  names(day01) <- c("temp", "rh", "ws", "prec")

  expect_equal(object = nlyr(fwiRaster(input = day01)), expected = 12)

})

# test_that("The function produces the expected output.", {
#   ## Equal Output
#   test_fwiRaster_out <- rast(system.file("./inst/extdata/test_fwiRaster_out.tif", package = "cffdrs"))
#   test_fwiRaster_out <- mask(test_fwiRaster_out,day01)
#   expect_equal(object = round(fwiRaster(input = day01)[[1]][],2),expected = round(test_fwiRaster_out[[1]][],2))
#
# })
