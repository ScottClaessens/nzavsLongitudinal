# plot correlations between games at both waves
plotCorrelations <- function(d, type) {
  # extract games
  games1 <- 
    transmute(
      d,
      TG1  = egame.TG1.T10,
      TG2  = egame.TG2.T10, 
      DG   = egame.DG.T10,
      PGG  = egame.PGG.T10,
      UG1  = egame.UG1.T10,
      SPP1 = egame.SPP1.T10
      )
  games2 <- 
    transmute(
      d,
      TG1  = egame.TG1.T11,
      TG2  = egame.TG2.T11, 
      DG   = egame.DG.T11,
      PGG  = egame.PGG.T11,
      UG1  = egame.UG1.T11,
      SPP1 = egame.SPP1.T11
    )
  if (type == "Reduced") {
    games1 <- dplyr::select(games1, -UG1, -SPP1)
    games2 <- dplyr::select(games2, -UG1, -SPP1)
  }
  # plots
  p1 <- 
    ggcorrplot(
      cor(games1),
      type = "lower",
      lab = TRUE,
      legend.title = NULL
      )
  p2 <- 
    ggcorrplot(
      cor(games2),
      type = "lower",
      lab = TRUE,
      legend.title = NULL
      )
  # put together
  out <-
    plot_grid(
      p1 + theme(legend.position = "none") + ggtitle("First wave"),
      p2 + theme(legend.position = "none") + ggtitle("Second wave"),
      get_legend(p1),
      nrow = 1,
      rel_widths = c(1, 1, 0.4)
    )
  # save
  ggsave(
    out,
    filename = paste0("figures/correlations_", type, ".pdf"),
    width = 7,
    height = 4
    )
  return(out)
}
