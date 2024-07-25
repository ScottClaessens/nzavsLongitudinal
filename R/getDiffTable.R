# get table of models testing for differences between waves
getDiffTable <- function(d, games) {
  # wrangle data
  d <-
    d %>%
    dplyr::select(Questionnaire.Num, egame.TG1.T10, egame.TG1.T11,
                  egame.TG2.T10, egame.TG2.T11, egame.DG.T10, egame.DG.T11,
                  egame.UG1.T10, egame.UG1.T11, egame.PGG.T10, egame.PGG.T11,
                  egame.SPP1.T10, egame.SPP1.T11) %>%
    pivot_longer(
      cols = !Questionnaire.Num,
      names_pattern = "egame.(.*)\\.(.*)",
      names_to = c(".value","Wave")
    )
  # model fitting function
  fitDiffModel <- function(d, game) {
    lmer(
      as.formula(paste0(game, " ~ Wave + (1 | Questionnaire.Num)")),
      data = d
    )
  }
  # create table
  tibble(Game = games) %>%
    mutate(
      # fit models
      model = map(Game, function(x) fitDiffModel(d, game = x)),
      # extract coefficient, SE, t, and p
      b  = map(model, function(x) summary(x)$coefficients[2,1]),
      SE = map(model, function(x) summary(x)$coefficients[2,2]),
      t  = map(model, function(x) summary(x)$coefficients[2,4]),
      p  = map(model, function(x) summary(x)$coefficients[2,5])
    ) %>%
    mutate(Game = c("Trust Game (Give)", "Trust Game (Return)",
                    "Public Goods Game", "Ultimatum Game (Offer)",
                    "Dictator Game", "Second Party Punishment Game (Give)")) %>%
    unnest(c(b, SE, t, p)) %>%
    dplyr::select(-model)
}
