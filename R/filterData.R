# filter data to participants who completed both waves
filterData <- function(dFull) {
  dFull %>%
    # remove timeouts and no pay (wave 2)
    filter(egame.Chk1.T11 == 1 & egame.Chk2.T11 == 1) %>%
    # "time on games"
    dplyr::select(
      -(egame.Secs49.T10:egame.Secs57.T10),
      -egame.SecsW.T10,
      -egame.SecsG.T10
      ) %>%
    dplyr::select(
      -(egame.Secs49.T11:egame.Secs57.T11),
      -egame.SecsW.T11,
      -egame.SecsG.T11
      ) %>%
    mutate(
      egame.SecsAll.T10 = rowSums(
        dplyr::select(., starts_with("egame.Secs") & ends_with(".T10")), 
        na.rm = TRUE
        ),
      egame.SecsAll.T11 = rowSums(
        dplyr::select(., starts_with("egame.Secs") & ends_with(".T11")),
        na.rm = TRUE)
      ) %>%
    mutate(
      # standardise variables
      egame.TG2.T10  = egame.TG2.T10 / 150,
      egame.PGG.T10  = egame.PGG.T10 / 100,
      egame.UG1.T10  = egame.UG1.T10 / 100,
      egame.DG.T10   = egame.DG.T10  / 100,
      egame.TG2.T11  = egame.TG2.T11 / 150,
      egame.PGG.T11  = egame.PGG.T11 / 100,
      egame.UG1.T11  = egame.UG1.T11 / 100,
      egame.DG.T11   = egame.DG.T11  / 100,
      # set types
      Age.T10.c      = as.numeric(scale(Age.T10)),
      NZDep.2013.T10 = as.numeric(scale(NZDep.2013.T10)),
      NZREG.T10      = as.numeric(scale(NZREG.T10)),
      NZSEI13.T10    = as.numeric(scale(NZSEI13.T10)),
      EthnicCats.T10 = factor(
        ifelse(EthnicCats.T10 == 1, "Pakeha",
               ifelse(EthnicCats.T10 == 2, "Maori",
                      ifelse(EthnicCats.T10 == 3, "Pacific",
                             ifelse(EthnicCats.T10 == 4, "Asian",
                                    EthnicCats.T10))))
        )
    )
}
