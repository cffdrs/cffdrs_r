test_that("The function Runs", {
  ## Make it run
  data("test_fbp")
  expect_no_error(fbp(test_fbp))

  })

test_that("Output length is input length", {
  ## Equal input / output length.
  expect_equal(object = nrow(fbp(test_fbp)), expected = nrow(test_fbp))

  })

test_that("The function produces the expected output.", {
  ## Equal Output
  load(system.file("./inst/extdata/test_fbp_out.Rdata", package = "cffdrs"))
  expect_identical(object = fbp(test_fbp),expected = test_fbp_out)

  })
