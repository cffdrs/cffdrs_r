test_that("InitialSpreadIndex", {
  checkData('InitialSpreadIndex',
            InitialSpreadIndex,
            list(data.table(ffmc=FFMC),
                 data.table(ws=WS),
                 data.table(fbpMod=BOOL)))
})
