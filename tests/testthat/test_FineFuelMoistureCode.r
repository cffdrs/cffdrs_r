test_that("FineFuelMoistureCode", {
  checkData('FineFuelMoistureCode',
            FineFuelMoistureCode,
            list(data.table(ffmc_yda=FFMC),
                 data.table(temp=TEMP),
                 data.table(rh=RH),
                 data.table(ws=WS),
                 data.table(prec=PREC)))
})
