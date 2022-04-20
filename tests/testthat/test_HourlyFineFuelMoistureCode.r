test_that("hffmc", {
  checkData('hffmc',
            hffmc,
            list(data.table(hr=HOURS),
                 data.table(temp=TEMP),
                 data.table(prec=PREC),
                 data.table(rh=RH),
                 data.table(ws=WS)),
            split_args=FALSE)
})
test_that("HourlyFineFuelMoistureCode", {
  checkData('HourlyFineFuelMoistureCode',
            HourlyFineFuelMoistureCode,
            list(data.table(temp=TEMP),
                 data.table(rh=RH),
                 data.table(ws=WS),
                 data.table(prec=PREC),
                 data.table(ffmc_old=FFMC),
                 data.table(time.step=HOURS)))
})
