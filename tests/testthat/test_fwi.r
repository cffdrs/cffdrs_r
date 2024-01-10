test_that("fwi_01", {
  library(data.table)
  test_fwi <- read.csv("../../data/test_fwi.csv", sep = ";")
  actual <- fwi(test_fwi)
  checkResults("fwi_01", actual)
})
test_that("fwi_02", {
  library(data.table)
  test_fwi <- read.csv("../../data/test_fwi.csv", sep = ";")
  actual <- fwi(test_fwi, out = "all")
  checkResults("fwi_02", actual)
})
test_that("fwi_03", {
  library(data.table)
  test_fwi <- read.csv("../../data/test_fwi.csv", sep = ";")
  actual <- fwi(test_fwi, out = "fwi")
  checkResults("fwi_03", actual)
})
test_that("fwi_04", {
  library(data.table)
  test_fwi <- read.csv("../../data/test_fwi.csv", sep = ";")
  actual <- fwi(test_fwi, init = data.frame(ffmc = 85, dmc = 6, dc = 15, lat = 55))
  checkResults("fwi_04", actual)
})
test_that("fwi_05", {
  library(data.table)
  test_fwi <- read.csv("../../data/test_fwi.csv", sep = ";")
  actual <- fwi(test_fwi, init = data.frame(ffmc = 0, dmc = 0, dc = 0, lat = 55))
  checkResults("fwi_05", actual)
})
test_that("fwi_06", {
  library(data.table)
  test_fwi <- read.csv("../../data/test_fwi.csv", sep = ";")\
  # warning about generating NaNs
  suppressWarnings({
    actual <- fwi(test_fwi, init = data.frame(ffmc = 200, dmc = 1000, dc = 10000, lat = 55))
  })
  checkResults("fwi_06", actual)
})
test_that("fwi_07", {
  library(data.table)
  test_fwi <- read.csv("../../data/test_fwi.csv", sep = ";")
  actual <- fwi(test_fwi, lat.adjust = FALSE)
  checkResults("fwi_07", actual)
})
test_that("fwi_08", {
  library(data.table)
  test_fwi <- read.csv("../../data/test_fwi.csv", sep = ";")
  actual <- fwi(test_fwi, uppercase = FALSE)
  checkResults("fwi_08", actual)
})
test_that("fwi_09", {
  library(data.table)
  test_fwi <- read.csv("../../data/test_fwi.csv", sep = ";")
  actual <- fwi(test_fwi[7, ])
  checkResults("fwi_09", actual)
})
test_that("fwi_10", {
  library(data.table)
  test_fwi <- read.csv("../../data/test_fwi.csv", sep = ";")
  expected <- read.csv("../data/fwi_10.csv")
  actual <- fwi(test_fwi[8:13, ])
  checkResults("fwi_10", actual)
})
# test_that("fwi_11", {
#   library(data.table)
#   test_fwi <- read.csv('../../data/test_fwi.csv', sep=';')
#   expected <- read.csv("../data/fwi_11.csv")
#   checkResults("fwi_11", actual)
# })
