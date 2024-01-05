test_that("BuildupIndex", {
  expect_equal(buildup_index(0, 0), 0)
  expect_equal(buildup_index(0, 100), 0)
  expect_equal(buildup_index(-1, 0), 0)
  checkData('BuildupIndex',
            buildup_index,
            list(data.table(dmc=DMC),
                 data.table(dc=DC)))
})
