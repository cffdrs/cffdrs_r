test_that("FoliarMoistureContent", {
  checkData('FoliarMoistureContent',
            foliar_moisture_content,
            list(data.table(LAT=LAT),
                 data.table(LONG=LONG),
                 data.table(ELV=ELV),
                 data.table(DJ=DJ),
                 data.table(D0=D0)))})
