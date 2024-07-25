# timeline figure
plotTimeline <- function(d) {
  # plot timeline
  out <-
    d %>%
    dplyr::select("Questionnaire.Num", contains("TSCORE")) %>%
    rename(EconomicGames.T11 = egame.TSCORE.T11,
           EconomicGames.T10 = egame.TSCORE.T10,
           NZAVS.T11 = TSCORE.T11,
           NZAVS.T10 = TSCORE.T10) %>%
    pivot_longer(-Questionnaire.Num, names_to = "Data", values_to = "Year") %>%
    mutate(Year = as.Date("2009-06-30") + Year) %>%
    drop_na(Year) %>%
    ggplot(aes(y = Data, x = Year)) +
    geom_jitter(size = 0.85, alpha = 0.1) +
    labs(y = NULL) +
    theme_classic()
  # save plot
  ggsave(out, filename = "figures/timeline.pdf", height = 5, width = 7)
  return(out)
}
