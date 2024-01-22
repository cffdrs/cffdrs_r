test_that("SimardRateOfSpreadLine", {
  data("test_lros", package = "cffdrs", envir = environment())
  checkResults('SimardRateOfSpreadLine',
               lros(test_lros))
})
test_that("SimardRateOfSpreadPoint", {
  data("test_pros", package = "cffdrs", envir = environment())
  checkResults('SimardRateOfSpreadPoint',
               pros(test_pros))
})
