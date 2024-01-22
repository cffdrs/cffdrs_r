test_that("FireWeatherIndex", {
  checkData('FireWeatherIndex',
            fire_weather_index,
            list(data.table(isi=ISI),
                 data.table(bui=BUI)))
})
