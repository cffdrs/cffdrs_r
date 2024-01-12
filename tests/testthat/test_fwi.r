fct_test_fwi <- function(name, fct) {
  library(data.table)
  data("test_fwi", package = "cffdrs", envir = environment())
  actual <- fct(test_fwi)
  expected <- read_data(name)
  checkEqual(name, actual, expected)
}
test_that("fwi_01", {
  fct_test_fwi("fwi_01", function(test_fwi) { fwi(test_fwi) })
})
test_that("fwi_02", {
  fct_test_fwi("fwi_02", function(test_fwi) { fwi(test_fwi, out = "all") })
})
test_that("fwi_03", {
  fct_test_fwi("fwi_03", function(test_fwi) { fwi(test_fwi, out = "fwi") })
})
test_that("fwi_04", {
  fct_test_fwi("fwi_04",
           function(test_fwi) {
             fwi(test_fwi, init=data.frame(ffmc=85, dmc=6, dc=15, lat=55))
           })
})
test_that("fwi_05", {
  fct_test_fwi("fwi_05",
           function(test_fwi) {
             fwi(test_fwi, init=data.frame(ffmc=0, dmc=0, dc=0, lat=55))
           })
})
test_that("fwi_06", {
  fct_test_fwi("fwi_06",
           function(test_fwi) {
             # HACK: we know there should be multiple warnings and 'all = TRUE' is deprecated
             expect_warning(
               expect_warning(
                 expect_warning(
                   expect_warning(
                     expect_warning(
                       expect_warning(
                         expect_warning(
                           expect_warning(
                             {actual <- fwi(test_fwi, init=data.frame(ffmc=200, dmc=1000, dc=10000, lat=55))},
                             "*NaNs produced*"),
                           "*NaNs produced*"),
                         "*NaNs produced*"),
                       "*NaNs produced*"),
                     "*NaNs produced*"),
                   "*NaNs produced*"),
                 "*NaNs produced*"),
               "*NaNs produced*")
             return(actual)
           })
})
test_that("fwi_07", {
  fct_test_fwi("fwi_07",
           function(test_fwi) {
             fwi(test_fwi, lat.adjust = FALSE)
           })
})
test_that("fwi_08", {
  fct_test_fwi("fwi_08",
           function(test_fwi) {
             fwi(test_fwi, uppercase = FALSE)
           })
})
test_that("fwi_09", {
  fct_test_fwi("fwi_09",
           function(test_fwi) {
             fwi(test_fwi[7, ])
           })
})
test_that("fwi_10", {
  fct_test_fwi("fwi_10",
           function(test_fwi) {
             fwi(test_fwi[8:13, ])
           })
})
test_that("fwi_11", {
  fct_test_fwi("fwi_11",
           function(test_fwi) {
             expect_warning({
               expect_warning(
                 {actual <- fwi(test_fwi, batch = FALSE)},
                 "*NaNs produced*")
               },
               "Same initial data were used for multiple weather stations")
           return(actual)
           })
})
