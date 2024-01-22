test_that("gfmcGFMC", {
  fctGFMC <- function(temp, rh, ws, prec, isol, mon)
  {
    mc <- grass_fuel_moisture(temp, rh, ws, prec, isol, mon,
                              GFMCold=85)
    return(grass_fuel_moisture_code(mc))
  }
  checkData('gfmcGFMC',
           fctGFMC,
           list(data.table(temp=TEMP),
                data.table(rh=RH[RH >= 0 & RH <= 100]),
                data.table(ws=WS),
                data.table(prec=PREC[PREC >= 0]),
                data.table(isol=seq(0, 10000)),
                data.table(mon=MON)))
})
test_that("gfmcMC", {
  fctMC <- function(temp, rh, ws, prec, isol, mon)
  {
    return(grass_fuel_moisture(temp, rh, ws, prec, isol, mon,
                              GFMCold=85))
  }
  checkData('gfmcMC',
           fctMC,
           list(data.table(temp=TEMP),
                data.table(rh=RH[RH >= 0 & RH <= 100]),
                data.table(ws=WS),
                data.table(prec=PREC[PREC >= 0]),
                data.table(isol=seq(0, 10000)),
                data.table(mon=MON)))
})
