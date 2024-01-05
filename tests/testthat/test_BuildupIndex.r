test_that("BuildupIndex", {
  expect_equal(BuildupIndex(0, 0), 0)
  expect_equal(BuildupIndex(0, 100), 0)
  expect_equal(BuildupIndex(-1, 0), 0)
  checkData('BuildupIndex',
            BuildupIndex,
            list(data.table(dmc=DMC),
                 data.table(dc=DC)))
})
