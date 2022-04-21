test_that("FireSeason_test_wDC_1", {
  test_wDC <- read.csv('../../data/test_wDC.csv', sep=';')
  input <- with(test_wDC, test_wDC[order(id,yr,mon,day),])
  result <- fireSeason(input[input$id==1,])
  # HACK: convert date to character so it matches how file is read
  result$date <- as.character(result$date)
  checkResults('FireSeason_test_wDC_1',
               result)
})
test_that("FireSeason_test_wDC_2", {
  test_wDC <- read.csv('../../data/test_wDC.csv', sep=';')
  input <- with(test_wDC, test_wDC[order(id,yr,mon,day),])
  result <- fireSeason(input[input$id==1,],fs.start=10, fs.end=3)
  # HACK: convert date to character so it matches how file is read
  result$date <- as.character(result$date)
  checkResults('FireSeason_test_wDC_2',
               result)
})
test_that("FireSeason_test_wDC_3", {
  test_wDC <- read.csv('../../data/test_wDC.csv', sep=';')
  input <- with(test_wDC, test_wDC[order(id,yr,mon,day),])
  result <- fireSeason(input[input$id==2,],method="WF93")
  # HACK: convert date to character so it matches how file is read
  result$date <- as.character(result$date)
  checkResults('FireSeason_test_wDC_3',
               result)
})
