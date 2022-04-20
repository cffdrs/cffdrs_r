test_that("LengthToBreadthRatio", {
  checkData('LengthToBreadthRatio',
            LengthToBreadthRatio,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(WSV=WSV)))})
