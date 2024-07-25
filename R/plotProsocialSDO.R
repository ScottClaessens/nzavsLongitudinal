# plot prosocial sdo
plotProsocialSDO <- function(dM, type) {
  # get "complete" data for plotting
  d <- complete(dM)
  # fit sem model
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
  semModel <- sem(
    model = model,
    data = d,
    ordered = ordered
  )
  # add prosocial latent variable scores to dataframe
  pred <- lavPredict(semModel)
  d$prosocial <- pred[,1]
  # plot
  out <-
    ggplot(
      data = d,
      aes(x = T11.SDO, y = prosocial)
      ) +
    geom_jitter(
      width = 0.1,
      height = 0.1,
      colour = "#0072B2",
      alpha = 0.5
      ) +
    stat_smooth(
      method = "lm",
      se = TRUE,
      colour = "black"
      ) +
    labs(
      x = "Social Dominance Orientation",
      y = "Prosocial phenotype"
      ) +
    theme_classic()
  return(out)
}
