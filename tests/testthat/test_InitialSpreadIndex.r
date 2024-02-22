test_that("InitialSpreadIndex", {
  checkData('InitialSpreadIndex',
            initial_spread_index,
            list(data.table(ffmc=FFMC),
                 data.table(ws=WS),
                 data.table(fbpMod=BOOL)))
})
