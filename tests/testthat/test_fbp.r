fct_test_fbp <- function(name, fct) {
  library(data.table)
  data("test_fbp", package = "cffdrs", envir = environment())
  actual <- fct(test_fbp)
  expected <- read_data(name)
  # HACK: for now change type here
  expected$ID <- as.integer(expected$ID)
  checkEqual(name, actual, expected)
}
test_that("fbp_01", {
  fct_test_fbp("fbp_01", function(test_fbp) { fbp(test_fbp) })
})
test_that("fbp_02", {
  fct_test_fbp("fbp_02", function(test_fbp) { fbp(test_fbp, output = "Primary") })
})
test_that("fbp_03", {
  fct_test_fbp("fbp_03", function(test_fbp) { fbp(test_fbp, "P") })
})
test_that("fbp_04", {
  fct_test_fbp("fbp_04", function(test_fbp) { fbp(test_fbp, "Secondary") })
})
test_that("fbp_05", {
  fct_test_fbp("fbp_05", function(test_fbp) { fbp(test_fbp, "S") })
})
test_that("fbp_06", {
  fct_test_fbp("fbp_06", function(test_fbp) { fbp(test_fbp, "All") })
})
test_that("fbp_07", {
  fct_test_fbp("fbp_07", function(test_fbp) { fbp(test_fbp, "A") })
})
test_that("fbp_08", {
  fct_test_fbp("fbp_08", function(test_fbp) { fbp(test_fbp[7, ]) })
})
test_that("fbp_09", {
  fct_test_fbp("fbp_09", function(test_fbp) { fbp(test_fbp[8:13, ]) })
})
test_that("fbp_10", {
  fct_test_fbp("fbp_10",
               function(test_fbp) {
                 actual <- fbp()
                 # HACK: for now change type here
                 actual$ID <- as.integer(actual$ID)
                 return(actual)
               })
})
test_that("fbp_11", {
  fct_test_fbp("fbp_11",
           function(test_fbp) {
             non_fuel <- copy(test_fbp)
             non_fuel$FuelType <- "NF"
             actual <- fbp(non_fuel, "All")
             # HACK: for now change type here
             actual$FD <- as.logical(actual$FD)
             return(actual)
           })
})
test_that("fbp_12", {
  fct_test_fbp("fbp_12",
           function(test_fbp) {
             water <- copy(test_fbp)
             water$FuelType <- "WA"
             actual <- fbp(water, "All")
             # HACK: for now change type here
             actual$FD <- as.logical(actual$FD)
             return(actual)
           })
})
