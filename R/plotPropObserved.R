# plot proportion of observed data
plotPropObserved <- function(d) {
  # plot
  out <-
    d %>%
    dplyr::select(
      Age.T10, Gender.T10, EthnicCats.T10, NZREG.T10, 
      NZSEI13.T10, NZDep.2013.T10, Religious.T10, T10.RWA, 
      T10.SDO, T11.SDO, Issue.IncomeRedistribution.T10, 
      Issue.IncomeRedistribution.T11, IncomeAttribution.T10, 
      IncomeAttribution.T11, Pol.SupNational.T10, Pol.SupNational.T11, 
      egame.TG1.T10, egame.TG1.T11, egame.TG2.T10, egame.TG2.T11, 
      egame.PGG.T10, egame.PGG.T11, egame.DG.T10, egame.DG.T11,
      egame.UG1.T10, egame.UG1.T11, egame.SPP1.T10, egame.SPP1.T11, 
      T10.EXTRAVERSION, T10.AGREEABLENESS, T10.CONSCIENTIOUSNESS, 
      T10.NEUROTICISM, T10.OPENNESS, T10.HONESTY_HUMILITY, T10.NARCISSISM
    ) %>%
    mutate_all(function(x) !is.na(x)) %>%
    pivot_longer(cols = everything()) %>%
    group_by(name) %>%
    summarise(prop = mean(value)) %>%
    ggplot(aes(x = name, y = prop)) +
    geom_col(fill = "skyblue") +
    labs(x = NULL, y = "Proportion of data observed") +
    scale_y_continuous(expand = c(0, 0)) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7)
    )
  # save plot
  ggsave(out, filename = "figures/observed.pdf", width = 6, height = 4)
  return(out)
}
