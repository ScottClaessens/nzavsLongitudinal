# plot imputation model fit
plotImpModel <- function(dM) {
  vars <- 
    colnames(
      dM$data %>% 
        dplyr::select(
          -Questionnaire.Num,
          -starts_with("egame."),
          -contains("TSCORE"),
          -starts_with("EthnicCats")
          )
      )
  plots <- list()
  for (i in 1:length(vars)) {
    plots[[i]] <- 
      densityplot(
        dM,
        as.formula(paste("~", vars[i]))
        )
  }
  out <- 
    cowplot::plot_grid(
      plotlist = plots,
      ncol = 6,
      nrow = 4
      )
  ggsave(
    out,
    file = "figures/imputationModel.pdf",
    height = 13,
    width = 18
    )
  return(out)
}
