test_that("LengthToBreadthRatio", {
  checkData('LengthToBreadthRatio',
            length_to_breadth,
            list(data.table(FUELTYPE=FUELTYPE),
                 data.table(WSV=WSV)))})
