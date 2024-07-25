# get table of models testing for selection bias
getBiasTable <- function(dFull, variables) {
  # model fitting function
  fitBiasModel <- function(dFull, variable) {
    dFull %>%
      # did participant drop out?
      mutate(
        dropout = ifelse(egame.Chk1.T11 == 1 & egame.Chk2.T11 == 1, 0, 1),
        dropout = ifelse(is.na(dropout), 1, dropout)
      ) %>%
      # fit model
      lm(as.formula(paste0(variable, " ~ dropout")), data = .)
  }
  # create table
  tibble(Variable = variables) %>%
    mutate(
      # fit models
      model = map(Variable, function(x) fitBiasModel(dFull, variable = x)),
      # extract coefficient, SE, t, and p
      b  = map(model, function(x) summary(x)$coefficients[2,1]),
      SE = map(model, function(x) summary(x)$coefficients[2,2]),
      t  = map(model, function(x) summary(x)$coefficients[2,3]),
      p  = map(model, function(x) summary(x)$coefficients[2,4])
    ) %>%
    unnest(c(b, SE, t, p)) %>%
    dplyr::select(-model)
}

