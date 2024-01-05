test_that("FoliarMoistureContent", {
  checkData('FoliarMoistureContent',
            FoliarMoistureContent,
            list(data.table(LAT=LAT),
                 data.table(LONG=LONG),
                 data.table(ELV=ELV),
                 data.table(DJ=DJ),
                 data.table(D0=D0)))})
