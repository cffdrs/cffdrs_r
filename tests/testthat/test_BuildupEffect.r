test_that("BuildupEffect", {
  library(data.table)
  checkData("BuildupEffect",
            buildup_effect,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(BUI=BUI)))
})
