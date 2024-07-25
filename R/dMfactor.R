# make factors for multi-group analysis
dMfactor <- function(dM, var) {
  dM <- complete(dM, "long", include = TRUE)
  if (var == "EthnicCats.T10") {
    dM <- mutate(dM, EthnicCats.T10 = factor(ifelse(EthnicCats.T10 == "Pakeha", "Pakeha", "Non-Pakeha")))
  } else if (var == "Gender.T10") {
    dM <- mutate(dM, Gender.T10 = factor(ifelse(Gender.T10 == 0, "Female", "Male")))
  }
  # output
  as.mids(dM)
}
