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

fctHFFMC <- function(temp, rh, ws, prec, ffmc_old, time.step) {
  return(hourly_fine_fuel_moisture_code(temp, rh, ws, prec, ffmc_old, time.step))
}

test_that("HourlyFineFuelMoistureCode", {
  checkData('HourlyFineFuelMoistureCode',
            fctHFFMC,
            list(data.table(temp=TEMP),
                 data.table(rh=RH),
                 data.table(ws=WS),
                 data.table(prec=PREC),
                 data.table(ffmc_old=FFMC),
                 data.table(time.step=HOURS)))
})
