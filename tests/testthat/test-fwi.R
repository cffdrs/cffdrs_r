test_that("The function Runs", {
  ## Make it run
  data("test_fwi")
  expect_no_error(fwi(test_fwi))

})

test_that("Output length is input length", {
  ## Equal input / output length.
  expect_equal(object = nrow(fwi(test_fwi)), expected = nrow(test_fwi))

})

test_that("The function produces the expected output.", {
  ## Equal Output
  load(system.file("./inst/extdata/test_fwi_out.Rdata", package = "cffdrs"))
  expect_identical(object = fwi(test_fwi),expected = test_fwi_out)

})
