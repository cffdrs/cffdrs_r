test_that("FineFuelMoistureCode", {
  checkData('FineFuelMoistureCode',
            fine_fuel_moisture_code,
            list(data.table(ffmc_yda=FFMC),
                 data.table(temp=TEMP),
                 data.table(rh=RH),
                 data.table(ws=WS),
                 data.table(prec=PREC)))
})
