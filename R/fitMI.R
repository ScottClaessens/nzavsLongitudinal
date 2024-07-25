# fit measurement invariance model
fitMI <- function(d, invariance, type) {
  # vector of games
  gamesT10 <- paste0("egame.", c("TG1","TG2","PGG","DG"), ".T10")
  gamesT11 <- paste0("egame.", c("TG1","TG2","PGG","DG"), ".T11")
  if (type == "Full") {
    gamesT10 <- c(gamesT10, paste0("egame.", c("UG1","SPP1"), ".T10"))
    gamesT11 <- c(gamesT11, paste0("egame.", c("UG1","SPP1"), ".T11"))
  }
  # create model specification
  # measurement model
  model <- "# measurement model\nprosocialT11"
  for (i in 1:length(gamesT11)) {
    model <- 
      paste0(
        model,
        ifelse(i == 1, " =~ ", " + "),
        ifelse(invariance != "config" & i != 1, paste0("l", i, "*"), ""),
        gamesT11[i]
        )
  }
  model <- paste0(model, "\nprosocialT10")
  for (i in 1:length(gamesT10)) {
    model <- 
      paste0(
        model,
        ifelse(i == 1, " =~ ", " + "),
        ifelse(invariance != "config" & i != 1, paste0("l", i, "*"), ""),
        gamesT10[i]
      )
  }
  # variances and covariances
  model <- paste0(
    model,
    "\n# variances and covariances\n",
    "prosocialT11 ~~ prosocialT10\n",
    "prosocialT11 ~~ prosocialT11\n",
    "prosocialT10 ~~ prosocialT10\n"
  )
  for (i in 1:length(gamesT11)) {
    if (!(str_starts(gamesT11[i], fixed("egame.TG1")) | 
          str_starts(gamesT11[i], fixed("egame.SPP1")))) {
      model <-
        paste0(
          model,
          gamesT11[i],
          " ~~ ",
          ifelse(invariance == "strict", paste0("v", i, "*"), ""),
          gamesT11[i],
          "\n",
          gamesT10[i],
          " ~~ ",
          ifelse(invariance == "strict", paste0("v", i, "*"), ""),
          gamesT10[i],
          "\n"
        )
    }
  }
  for (i in 1:length(gamesT11)) {
    model <-
      paste0(
        model,
        gamesT10[i],
        " ~~ ",
        gamesT11[i],
        "\n"
      )
  }
  # intercepts and thresholds
  model <- paste0(model, "# intercepts and thresholds\n")
  for (i in 1:length(gamesT11)) {
    model <-
      paste0(
        model,
        gamesT10[i],
        ifelse(
          str_starts(gamesT10[i], fixed("egame.TG1")) | 
            str_starts(gamesT10[i], fixed("egame.SPP1")),
          " | ",
          " ~ "
          ),
        ifelse(
          invariance %in% c("scalar","strict"),
          paste0("i", i, "*"),
          ""
        ),
        ifelse(
          str_starts(gamesT10[i], fixed("egame.TG1")) | 
            str_starts(gamesT10[i], fixed("egame.SPP1")),
          "t1",
          "1"
        ),
        "\n",
        gamesT11[i],
        ifelse(
          str_starts(gamesT11[i], fixed("egame.TG1")) | 
            str_starts(gamesT11[i], fixed("egame.SPP1")),
          " | ",
          " ~ "
        ),
        ifelse(
          invariance %in% c("scalar","strict"),
          paste0("i", i, "*"),
          ""
        ),
        ifelse(
          str_starts(gamesT11[i], fixed("egame.TG1")) | 
            str_starts(gamesT11[i], fixed("egame.SPP1")),
          "t1",
          "1"
        ),
        "\n"
      )
  }
  # ordered variables
  if (type == "Full") {
    ordered <- c("egame.TG1.T11", "egame.TG1.T10",
                 "egame.SPP1.T11", "egame.SPP1.T10")
  } else if (type == "Reduced") {
    ordered <- c("egame.TG1.T11", "egame.TG1.T10")
  }
  # fit model
  cfa(
    model = model,
    data = d,
    ordered = ordered
    )
}
