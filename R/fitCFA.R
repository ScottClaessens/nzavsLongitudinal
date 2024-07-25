# fit cfa
fitCFA <- function(d, type) {
  # fit with full game set
  if (type == "Full") {
    model <-
      paste0(
        "prosocial =~ egame.TG1.T11 + egame.TG2.T11 + egame.PGG.T11",
        " + egame.DG.T11 + egame.UG1.T11 + egame.SPP1.T11"
      )
    ordered <- 
      c("egame.TG1.T11","egame.SPP1.T11")
  # fit with reduced game set
  } else if (type == "Reduced") {
    model <- 
      paste0(
        "prosocial =~ egame.TG1.T11 + egame.TG2.T11 + egame.PGG.T11",
        " + egame.DG.T11"
        )
    ordered <- 
      "egame.TG1.T11"
  }
  cfa(
    model = model,
    data = d,
    ordered = ordered
  )
}
