test_that("FoliarMoistureContent", {
  checkData('FoliarMoistureContent',
              suppressWarnings(
                expect_message({
                  expect_warning(
                    foliar_moisture_content,
                    "FMC Override provided, returning as FMC.")},
                  "Location outside of North America. Please define an FMC override in the FMCo variable."),
                ),
            FMC_ARGS)
  }
  )
