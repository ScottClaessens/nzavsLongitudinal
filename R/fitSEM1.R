# fit sem
fitSEM1 <- function(dM, type) {
  if (type == "Full") {
    model <- 
      paste0(
        "# measurement model\n",
        "prosocial =~ egame.TG1.T11 + egame.TG2.T11 + egame.PGG.T11",
        " + egame.DG.T11 + egame.UG1.T11 + egame.SPP1.T11\n",
        "# regression\n",
        "prosocial ~ T11.SDO"
      )
    ordered <- 
      c("egame.TG1.T11","egame.SPP1.T11")
  } else if (type == "Reduced") {
    model <- 
      paste0(
        "# measurement model\n",
        "prosocial =~ egame.TG1.T11 + egame.TG2.T11 + egame.PGG.T11",
        " + egame.DG.T11\n",
        "# regression\n",
        "prosocial ~ T11.SDO"
      )
    ordered <- 
      "egame.TG1.T11"
  }
  # fit model with multiply imputed data
  sem.mi(
    model = model,
    data = dM,
    ordered = ordered
  )
}
