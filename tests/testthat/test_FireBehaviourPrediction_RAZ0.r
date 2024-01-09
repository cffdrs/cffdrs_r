fctRAZ0  <- function(input=NULL, output="ALL") {
  return(fire_behaviour_prediction(input, output="RAZ0"))
}
test_that("FireBehaviourPrediction_RAZ0", {
  checkData('FireBehaviourPrediction_RAZ0',
            fctOnInput(fctRAZ0),
            FBP_ARGS)
})
