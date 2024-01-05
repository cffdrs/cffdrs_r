test_that("FireWeatherIndex", {
  checkData('FireWeatherIndex',
            FireWeatherIndex,
            list(data.table(isi=ISI),
                 data.table(bui=BUI)))
})
