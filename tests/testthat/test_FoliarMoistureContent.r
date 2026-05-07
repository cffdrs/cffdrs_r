test_that("FoliarMoistureContent", {
  checkData('FoliarMoistureContent',
              suppressWarnings(
                  suppressMessages(
                    foliar_moisture_content,
                  )),
            FMC_ARGS)
  }
  )

# test_that("FoliarMoistureContent", {
#   checkData('FoliarMoistureContent',
#             suppressMessages(
#               expect_warning(
#                 foliar_moisture_content,
#                 "Location outside of North America. Please define an FMC override in the FMCo variable.")),
#             FMC_ARGS)
# }
# )
#
# test_that("FoliarMoistureContent", {
#   checkData('FoliarMoistureContent',
#             suppressWarnings(
#               expect_message(
#                 foliar_moisture_content,
#                 "FMC Override provided, returning as FMC.")
#               ),
#             FMC_ARGS)
# }
# )
