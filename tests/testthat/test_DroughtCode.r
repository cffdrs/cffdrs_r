test_that("DroughtCode", {
  checkData('DroughtCode',
            DroughtCode,
            list(data.table(dc_yda=DC),
                 data.table(temp=TEMP),
                 data.table(rh=RH),
                 data.table(prec=PREC),
                 data.table(lat=LAT),
                 data.table(mon=MON),
                 data.table(lat.adjust=BOOL)))
})
