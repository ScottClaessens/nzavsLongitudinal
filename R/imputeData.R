# data imputation with mice
imputeData <- function(d) {
  # remove some egame variables
  d <- 
    dplyr::select(
      d, 
      -starts_with("egame.Secs"),
      -starts_with("egame.Paid."),
      -Age.T10
      )
  # remove QNum, personality variables, and additional games
  # from predictor matrix
  init  <- mice(d, maxit = 0)
  predMat <- init$predictorMatrix
  predMat[,c("Questionnaire.Num","T10.AGREEABLENESS","T10.CONSCIENTIOUSNESS",
             "T10.EXTRAVERSION","T10.HONESTY_HUMILITY","T10.NARCISSISM",
             "T10.NEUROTICISM","T10.OPENNESS","egame.UG1.T10","egame.SPP1.T10",
             "egame.cmpUG.T10","egame.cmpSPP.T10","egame.UG1.T11",
             "egame.SPP1.T11","egame.cmpUG.T11","egame.cmpSPP.T11")] <- 0
  # impute the data
  out <- mice(d, m = 20, predictorMatrix = predMat, seed = 1)
  # dummy ethnicity columns
  out <- 
    complete(out, "long", include = TRUE) %>%
    dummy_columns("EthnicCats.T10") %>%
    as.mids()
  return(out)
}
