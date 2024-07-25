# fit sem
fitSEM2 <- function(dM, outcome, type) {
  if (type == "Full") {
    model <- 
      paste0(
        "# measurement model\n",
        "prosocial =~ egame.TG1.T10 + egame.TG2.T10 + egame.PGG.T10",
        " + egame.DG.T10 + egame.UG1.T10 + egame.SPP1.T10\n",
        "# covariance\n",
        "prosocial ~~ ", outcome
      )
    ordered <- 
      c("egame.TG1.T10","egame.SPP1.T10")
  } else if (type == "Reduced") {
    model <- 
      paste0(
        "# measurement model\n",
        "prosocial =~ egame.TG1.T10 + egame.TG2.T10 + egame.PGG.T10",
        " + egame.DG.T10\n",
        "# covariance\n",
        "prosocial ~~ ", outcome
      )
    ordered <- 
      "egame.TG1.T10"
  }
  # fit model with multiply imputed data
  sem.mi(
    model = model,
    data = dM,
    ordered = ordered
  )
}
