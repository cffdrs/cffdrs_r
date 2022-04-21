test_that("gfmcGFMC", {
  fctGFMC <- function(input)
  {
    return(gfmc(input, GFMCold=rep(85, length(input$temp)), out="GFMC", batch=FALSE))
  }
  checkData('gfmcGFMC',
           fctGFMC,
           list(data.table(temp=TEMP),
                data.table(rh=RH[RH >= 0 & RH <= 100]),
                data.table(ws=WS),
                data.table(prec=PREC[PREC >= 0]),
                data.table(isol=seq(0, 10000)),
                data.table(mon=MON)),
           split_args=FALSE)
})
test_that("gfmcMC", {
  fctMC <- function(input)
  {
    return(gfmc(input, GFMCold=rep(85, length(input$temp)), out="MC", batch=FALSE))
  }
  checkData('gfmcMC',
           fctMC,
           list(data.table(temp=TEMP),
                data.table(rh=RH[RH >= 0 & RH <= 100]),
                data.table(ws=WS),
                data.table(prec=PREC[PREC >= 0]),
                data.table(isol=seq(0, 10000)),
                data.table(mon=MON)),
           split_args=FALSE)
})
