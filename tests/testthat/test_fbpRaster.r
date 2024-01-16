test_fbp_raster <- function(name, fct_fbp) {
  input <- rast(system.file("extdata", "test_fbpRaster.tif", package = "cffdrs"))
  names(input) <- c("FuelType","LAT","LONG","ELV","FFMC","BUI", "WS","WD","GS","Dj","D0","hr","PC",
                    "PDF","GFL","cc","theta","Accel","Aspect","BUIEff","CBH","CFL","ISI")
  # ignore warning:
  #   "FD = 1,2,3 representing Surface (S), Intermittent (I), and Crown (C) fire"
  test_raster(name,
              input,
              function(input) {
                suppressMessages(
                  expect_warning(
                    expect_warning(
                      expect_warning(
                        { return(fct_fbp(input)) },
                        "FMC is a required input*"),
                      "SD is a required input*"),
                    "SH is a required input*")
                  )
              })
}

test_that("fbpRaster_test1", {
  test_fbp_raster(
    "fbpRaster_test1",
    function(input) {
      fbpRaster(input = input)
    }
  )
})

test_that("fbpRaster_test2", {
  test_fbp_raster(
    "fbpRaster_test2",
    function(input) {
      fbpRaster(input = input,select=c("HFI","TFC", "ROS"))
    }
  )
})

test_that("fbpRaster_test3", {
  test_fbp_raster(
    "fbpRaster_test3",
    function(input) {
      fbpRaster(input = input,output="S")
    }
  )
})

test_that("fbpRaster_test4", {
  test_fbp_raster(
    "fbpRaster_test4",
    function(input) {
      fbpRaster(input = input,output="A")
    }
  )
})

test_that("fbpRaster_test5", {
  test_fbp_raster(
    "fbpRaster_test5",
    function(input) {
      dat0 <- input[[c("FuelType","LAT","LONG","FFMC","BUI","WS","GS", "Dj","Aspect")]]
      expect_warning(
        expect_warning(
          expect_warning(
            expect_warning(
              expect_warning(
                expect_warning(
                  expect_warning(
                    expect_warning(
                      expect_warning(
                        expect_warning(
                          expect_warning(
                            expect_warning(
                              expect_warning(
                                expect_warning(
                                  { return(fbpRaster(input = dat0,output="A")) },
                                  "WD is a required input*"),
                                "ELV is a required input*"),
                              "D0 is a required input*"),
                            "HR is a required input*"),
                          "PC is a required input*"),
                        "PDF is a required input*"),
                      "GFL is a required input*"),
                    "CC is a required input*"),
                  "THETA is a required input*"),
                "BUIEFF is a required input*"),
              "CBH is a required input*"),
            "CFL is a required input*"),
          "ISI is a required input*"),
        "ACCEL is a required input*")
      return(output)
    }
  )
})
