test_that("fbp", {
  library(cffdrs)
  library(data.table)
  data("test_fbp")
  test_columns <- function(actual, expected)
  {
    for (n in names(actual))
    {
      test_that(n, {
        expect_equal(as.character(unlist(actual[n])), as.character(unlist(expected[n])))
      })
    }
  }
  test_columns(fbp(test_fbp), read.csv("../data/fbp_01.csv"))
  test_columns(fbp(test_fbp,output="Primary"), read.csv("../data/fbp_02.csv"))
  test_columns(fbp(test_fbp,"P"), read.csv("../data/fbp_03.csv"))
  test_columns(fbp(test_fbp,"Secondary"), read.csv("../data/fbp_04.csv"))
  test_columns(fbp(test_fbp,"S"), read.csv("../data/fbp_05.csv"))
  test_columns(fbp(test_fbp,"All"), read.csv("../data/fbp_06.csv"))
  test_columns(fbp(test_fbp,"A"), read.csv("../data/fbp_07.csv"))
  test_columns(fbp(test_fbp[7,]), read.csv("../data/fbp_08.csv"))
  test_columns(fbp(test_fbp[8:13,]), read.csv("../data/fbp_09.csv"))
  test_columns(fbp(), read.csv("../data/fbp_10.csv"))
})
