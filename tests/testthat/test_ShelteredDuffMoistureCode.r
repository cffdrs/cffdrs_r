test_that("sdmc", {
  checkData('sdmc',
            sdmc,
            list(data.table(dmc=DMC),
                 data.table(temp=TEMP),
                 data.table(prec=PREC),
                 data.table(rh=RH),
                 data.table(mon=MON),
                 data.table(day=DJ),
                 data.table(ws=WS)),
            split_args=FALSE)
})
