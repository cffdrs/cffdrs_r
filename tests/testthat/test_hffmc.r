test_that("hffmc", {
  fct <- function(input) {
    return(hffmc(input))
  }
  # this will warn about negative values
  suppressWarnings(
    checkData('hffmc',
            fct,
            list(data.table(hr=HOURS),
                 data.table(temp=TEMP),
                 data.table(prec=PREC),
                 data.table(rh=RH),
                 data.table(ws=WS)),
            split_args=FALSE))
})
