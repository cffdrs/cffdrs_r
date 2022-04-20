test_that("BuildupEffect", {
  library(data.table)
  checkData("BuildupEffect",
            BuildupEffect,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(BUI=BUI)))
})
