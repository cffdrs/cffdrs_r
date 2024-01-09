fctWSV0  <- function(input=NULL, output="ALL") {
return(fire_behaviour_prediction(input, output="WSV0"))
}
test_that("FireBehaviourPrediction_WSV0", {
  checkData('FireBehaviourPrediction_WSV0',
            fctOnInput(fctWSV0),
            FBP_ARGS)
})
