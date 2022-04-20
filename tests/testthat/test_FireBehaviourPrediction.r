test_that("FireBehaviourPrediction_test_fbp", {
  test_fbp <- read.csv('../../data/test_fbp.csv', sep=';')
  test_fbp$FFMC <- as.numeric(test_fbp$FFMC)
  test_fbp$hr <- as.numeric(test_fbp$hr)
  test_fbp$WS <- as.numeric(test_fbp$WS)
  test_fbp$GFL <- as.numeric(test_fbp$GFL)
  test_fbp$CBH <- as.numeric(test_fbp$CBH)
  test_fbp$CFL <- as.numeric(test_fbp$CFL)
  checkResults('FireBehaviourPrediction_test_fbp',
               FireBehaviourPrediction(test_fbp, "A"))
})
test_that("FireBehaviourPrediction", {
  checkData('FireBehaviourPrediction',
            fctOnInput(FireBehaviourPrediction),
            FBP_ARGS)
})
