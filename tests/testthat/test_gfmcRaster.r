test_that("gfmcRaster", {
  set.seed(666)
  input <- rast(
    nrows = 25,
    ncols = 25,
    crs = "EPSG:3402",
    resolution = 100,
    ymin = 5652012,
    ymax = 5652012 + (25 * 100),
    xmin = 565550,
    xmax = 565550 + (25 * 100),
    names = "temp",
    vals = sample(x = 19:27, size = 25 * 25, replace = TRUE)
  )

  input <- c(
    input,
    setValues(input, sample(x = 0:3, size = 25 * 25, replace = TRUE)),
    setValues(input, sample(x = 10:20, size = 25 * 25, replace = TRUE)),
    setValues(input, sample(x = 30:70, size = 25 * 25, replace = TRUE)),
    setValues(input, sample(x = (5:950) / 1000, size = 25 * 25, replace = TRUE))
  )

  names(input) <- c("temp", "prec", "ws", "rh", "isol")

  expect_warning(
    {
      output1 <- gfmcRaster(input)
    },
    "Single time.step value for grid is applied to the whole grid")

  test_gfmc_raster <- function(name, fct_gfmc) {
      test_raster(
        name,
        input,
        function(input) {
          return(fct_gfmc(input))
        })
  }

  test_that("gfmcRaster_test1", {
    test_raster(
      "gfmcRaster_test1",
      input,
      function(input) {
        return(output1)
      }
    )
  })

  test_that("gfmcRaster_test2", {
    test_raster(
      "gfmcRaster_test2",
      input,
      function(input) {
        expect_warning(
          {return(gfmcRaster(input, GFMCold = output1[["GFMC"]]))},
          "Single time.step value for grid is applied to the whole grid")
      }
    )
  })
})
