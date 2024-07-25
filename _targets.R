targets::tar_source()

# pipeline
list(
  # files
  tar_target(fileData, "data/NZAVS Longitudinal SPSS Base Dataset.sav", 
             format = "file"),
  # load data
  tar_target(dFull, loadData(fileData)),
  # filter data to participants who completed both waves
  tar_target(d, filterData(dFull)),
  # plot timeline
  tar_target(timeline, plotTimeline(d)),
  # make item table
  tar_target(itemTable, makeItemTable()),
  # plot proportion observed data
  tar_target(plotObs, plotPropObserved(d)),
  # test for selection bias
  tar_target(
    biasTable, 
    getBiasTable(
      dFull,
      variables = c(
        "egame.TG1.T10","egame.TG2.T10","egame.PGG.T10","egame.UG1.T10",
        "egame.DG.T10","egame.SPP1.T10","T10.SDO",
        "Issue.IncomeRedistribution.T10","IncomeAttribution.T10",
        "Pol.SupNational.T10"
        )
      )
    ),
  # test for differences between waves
  tar_target(diffTable, getDiffTable(d, games = c("TG1","TG2","PGG",
                                                  "UG1","DG","SPP1"))),
  # plot correlations between games
  tar_target(plotCors_Reduced, plotCorrelations(d, type = "Reduced")),
  tar_target(plotCors_Full, plotCorrelations(d, type = "Full")),
  # reliability
  tar_target(
    reliability1_Reduced, 
    omega(d[,c("egame.TG1.T10","egame.TG2.T10",
               "egame.DG.T10","egame.PGG.T10")])
    ),
  tar_target(
    reliability2_Reduced,
    omega(d[,c("egame.TG1.T11","egame.TG2.T11",
               "egame.DG.T11","egame.PGG.T11")])
    ),
  tar_target(
    reliability1_Full,
    omega(d[,c("egame.TG1.T10","egame.TG2.T10",
               "egame.DG.T10","egame.PGG.T10",
               "egame.UG1.T10","egame.SPP1.T10")])
    ),
  tar_target(
    reliability2_Full,
    omega(d[,c("egame.TG1.T11","egame.TG2.T11",
               "egame.DG.T11","egame.PGG.T11",
               "egame.UG1.T11","egame.SPP1.T11")])
    ),
  # impute data
  tar_target(dM, imputeData(d)),
  # plot imputation model
  tar_target(plotImputation, plotImpModel(dM)),
  # fit and plot models
  analysisTargets,
  # render manuscript
  tar_render(manuscript, "manuscript.Rmd"),
  # print session info for reproducibility
  tar_target(
    sessionInfo,
    writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
    )
)