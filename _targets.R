library(tidyverse)
library(targets)
library(tarchetypes)
source("R/functions.R")
tar_option_set(
  packages = c("car","cowplot","fastDummies","ggforce","ggraph",
               "haven","igraph","kableExtra","lavaan",
               "mice","papaja","semTools","tidyverse")
)
# pipeline
list(
  # files
  tar_target(fileData, "data/NZAVS Longitudinal SPSS Base Dataset.sav", format = "file"),
  # load data
  tar_target(d, loadData(fileData)),
  # plot timeline
  tar_target(timeline, plotTimeline(d)),
  # make item table
  tar_target(itemTable, makeItemTable()),
  # impute data
  tar_target(dM, imputeData(d)),
  # plot imputation model
  tar_target(plotImputation, plotImpModel(dM)),
  # fit cfa
  tar_target(cfa1, fitCFA1(d)),
  # fit sem
  tar_target(sem1, fitSEM1(dM)),
  tar_target(plotSem1, plotCoopSDO(dM)),
  # measurement invariance across waves
  tar_target(configMI, fitMI(d, type = "config")),
  tar_target(metricMI, fitMI(d, type = "metric")),
  tar_target(scalarMI, fitMI(d, type = "scalar")),
  tar_target(strictMI, fitMI(d, type = "strict")),
  tar_target(tableCompareMI, getComparisonTable(configMI, metricMI, scalarMI, strictMI)),
  # setup cross-lagged panel models
  tar_target(clpmControls, c(
    "Age.T10.c", "Gender.T10", "EthnicCats.T10_Asian + EthnicCats.T10_Maori + EthnicCats.T10_Pacific",
    "NZREG.T10", "NZSEI13.T10", "NZDep.2013.T10", "Religious.T10", "T10.RWA",
    "Age.T10.c + Gender.T10 + EthnicCats.T10_Asian + EthnicCats.T10_Maori + EthnicCats.T10_Pacific + NZREG.T10 + NZSEI13.T10 + NZDep.2013.T10 + Religious.T10 + T10.RWA"
    )),
  tar_target(clpmOrdered, list(
    c(paste0("egame.TG1.T1", 0:1)),
    c(paste0("egame.TG1.T1", 0:1), paste0("Issue.IncomeRedistribution.T1", 0:1)),
    c(paste0("egame.TG1.T1", 0:1), paste0("IncomeAttribution.T1", 0:1)),
    c(paste0("egame.TG1.T1", 0:1), paste0("Pol.SupNational.T1", 0:1))
  )),
  # run cross-lagged panel models
  # with mean sdo
  tar_target(clpm1.01, fitCLPM(dM, var1 = "T10.SDO", var2 = "T11.SDO", ordered = clpmOrdered[[1]])),
  tar_target(clpm1.02, fitCLPM(dM, var1 = "T10.SDO", var2 = "T11.SDO", ordered = clpmOrdered[[1]], controls = clpmControls[1])),
  tar_target(clpm1.03, fitCLPM(dM, var1 = "T10.SDO", var2 = "T11.SDO", ordered = clpmOrdered[[1]], controls = clpmControls[2])),
  tar_target(clpm1.04, fitCLPM(dM, var1 = "T10.SDO", var2 = "T11.SDO", ordered = clpmOrdered[[1]], controls = clpmControls[3])),
  tar_target(clpm1.05, fitCLPM(dM, var1 = "T10.SDO", var2 = "T11.SDO", ordered = clpmOrdered[[1]], controls = clpmControls[4])),
  tar_target(clpm1.06, fitCLPM(dM, var1 = "T10.SDO", var2 = "T11.SDO", ordered = clpmOrdered[[1]], controls = clpmControls[5])),
  tar_target(clpm1.07, fitCLPM(dM, var1 = "T10.SDO", var2 = "T11.SDO", ordered = clpmOrdered[[1]], controls = clpmControls[6])),
  tar_target(clpm1.08, fitCLPM(dM, var1 = "T10.SDO", var2 = "T11.SDO", ordered = clpmOrdered[[1]], controls = clpmControls[7])),
  tar_target(clpm1.09, fitCLPM(dM, var1 = "T10.SDO", var2 = "T11.SDO", ordered = clpmOrdered[[1]], controls = clpmControls[8])),
  tar_target(clpm1.10, fitCLPM(dM, var1 = "T10.SDO", var2 = "T11.SDO", ordered = clpmOrdered[[1]], controls = clpmControls[9])),
  # with income redistribution
  tar_target(clpm2.01, fitCLPM(dM, var1 = "Issue.IncomeRedistribution.T10", var2 = "Issue.IncomeRedistribution.T11", ordered = clpmOrdered[[2]])),
  tar_target(clpm2.02, fitCLPM(dM, var1 = "Issue.IncomeRedistribution.T10", var2 = "Issue.IncomeRedistribution.T11", ordered = clpmOrdered[[2]], controls = clpmControls[1])),
  tar_target(clpm2.03, fitCLPM(dM, var1 = "Issue.IncomeRedistribution.T10", var2 = "Issue.IncomeRedistribution.T11", ordered = clpmOrdered[[2]], controls = clpmControls[2])),
  tar_target(clpm2.04, fitCLPM(dM, var1 = "Issue.IncomeRedistribution.T10", var2 = "Issue.IncomeRedistribution.T11", ordered = clpmOrdered[[2]], controls = clpmControls[3])),
  tar_target(clpm2.05, fitCLPM(dM, var1 = "Issue.IncomeRedistribution.T10", var2 = "Issue.IncomeRedistribution.T11", ordered = clpmOrdered[[2]], controls = clpmControls[4])),
  tar_target(clpm2.06, fitCLPM(dM, var1 = "Issue.IncomeRedistribution.T10", var2 = "Issue.IncomeRedistribution.T11", ordered = clpmOrdered[[2]], controls = clpmControls[5])),
  tar_target(clpm2.07, fitCLPM(dM, var1 = "Issue.IncomeRedistribution.T10", var2 = "Issue.IncomeRedistribution.T11", ordered = clpmOrdered[[2]], controls = clpmControls[6])),
  tar_target(clpm2.08, fitCLPM(dM, var1 = "Issue.IncomeRedistribution.T10", var2 = "Issue.IncomeRedistribution.T11", ordered = clpmOrdered[[2]], controls = clpmControls[7])),
  tar_target(clpm2.09, fitCLPM(dM, var1 = "Issue.IncomeRedistribution.T10", var2 = "Issue.IncomeRedistribution.T11", ordered = clpmOrdered[[2]], controls = clpmControls[8])),
  tar_target(clpm2.10, fitCLPM(dM, var1 = "Issue.IncomeRedistribution.T10", var2 = "Issue.IncomeRedistribution.T11", ordered = clpmOrdered[[2]], controls = clpmControls[9])),
  # with income attribution
  tar_target(clpm3.01, fitCLPM(dM, var1 = "IncomeAttribution.T10", var2 = "IncomeAttribution.T11", ordered = clpmOrdered[[3]])),
  tar_target(clpm3.02, fitCLPM(dM, var1 = "IncomeAttribution.T10", var2 = "IncomeAttribution.T11", ordered = clpmOrdered[[3]], controls = clpmControls[1])),
  tar_target(clpm3.03, fitCLPM(dM, var1 = "IncomeAttribution.T10", var2 = "IncomeAttribution.T11", ordered = clpmOrdered[[3]], controls = clpmControls[2])),
  tar_target(clpm3.04, fitCLPM(dM, var1 = "IncomeAttribution.T10", var2 = "IncomeAttribution.T11", ordered = clpmOrdered[[3]], controls = clpmControls[3])),
  tar_target(clpm3.05, fitCLPM(dM, var1 = "IncomeAttribution.T10", var2 = "IncomeAttribution.T11", ordered = clpmOrdered[[3]], controls = clpmControls[4])),
  tar_target(clpm3.06, fitCLPM(dM, var1 = "IncomeAttribution.T10", var2 = "IncomeAttribution.T11", ordered = clpmOrdered[[3]], controls = clpmControls[5])),
  tar_target(clpm3.07, fitCLPM(dM, var1 = "IncomeAttribution.T10", var2 = "IncomeAttribution.T11", ordered = clpmOrdered[[3]], controls = clpmControls[6])),
  tar_target(clpm3.08, fitCLPM(dM, var1 = "IncomeAttribution.T10", var2 = "IncomeAttribution.T11", ordered = clpmOrdered[[3]], controls = clpmControls[7])),
  tar_target(clpm3.09, fitCLPM(dM, var1 = "IncomeAttribution.T10", var2 = "IncomeAttribution.T11", ordered = clpmOrdered[[3]], controls = clpmControls[8])),
  tar_target(clpm3.10, fitCLPM(dM, var1 = "IncomeAttribution.T10", var2 = "IncomeAttribution.T11", ordered = clpmOrdered[[3]], controls = clpmControls[9])),
  # with National party support
  tar_target(clpm4.01, fitCLPM(dM, var1 = "Pol.SupNational.T10", var2 = "Pol.SupNational.T11", ordered = clpmOrdered[[4]])),
  tar_target(clpm4.02, fitCLPM(dM, var1 = "Pol.SupNational.T10", var2 = "Pol.SupNational.T11", ordered = clpmOrdered[[4]], controls = clpmControls[1])),
  tar_target(clpm4.03, fitCLPM(dM, var1 = "Pol.SupNational.T10", var2 = "Pol.SupNational.T11", ordered = clpmOrdered[[4]], controls = clpmControls[2])),
  tar_target(clpm4.04, fitCLPM(dM, var1 = "Pol.SupNational.T10", var2 = "Pol.SupNational.T11", ordered = clpmOrdered[[4]], controls = clpmControls[3])),
  tar_target(clpm4.05, fitCLPM(dM, var1 = "Pol.SupNational.T10", var2 = "Pol.SupNational.T11", ordered = clpmOrdered[[4]], controls = clpmControls[4])),
  tar_target(clpm4.06, fitCLPM(dM, var1 = "Pol.SupNational.T10", var2 = "Pol.SupNational.T11", ordered = clpmOrdered[[4]], controls = clpmControls[5])),
  tar_target(clpm4.07, fitCLPM(dM, var1 = "Pol.SupNational.T10", var2 = "Pol.SupNational.T11", ordered = clpmOrdered[[4]], controls = clpmControls[6])),
  tar_target(clpm4.08, fitCLPM(dM, var1 = "Pol.SupNational.T10", var2 = "Pol.SupNational.T11", ordered = clpmOrdered[[4]], controls = clpmControls[7])),
  tar_target(clpm4.09, fitCLPM(dM, var1 = "Pol.SupNational.T10", var2 = "Pol.SupNational.T11", ordered = clpmOrdered[[4]], controls = clpmControls[8])),
  tar_target(clpm4.10, fitCLPM(dM, var1 = "Pol.SupNational.T10", var2 = "Pol.SupNational.T11", ordered = clpmOrdered[[4]], controls = clpmControls[9])),
  # multigroup analyses split by gender
  tar_target(dM_gender, dMfactor(dM, "Gender.T10")),
  tar_target(clpm1_gender, fitCLPMSplit(dM_gender, var1 = "T10.SDO", var2 = "T11.SDO", ordered = c(paste0("egame.TG1.T1", 0:1)), group = "Gender.T10")),
  tar_target(dM_ethnic, dMfactor(dM, "EthnicCats.T10")),
  tar_target(clpm1_ethnic, fitCLPMSplit(dM_ethnic, var1 = "T10.SDO", var2 = "T11.SDO", ordered = c(paste0("egame.TG1.T1", 0:1)), group = "EthnicCats.T10")),
  # figures
  tar_target(clpmPlot_SDO, drawCLPMFigure(
    list(clpm1.01, clpm1.02, clpm1.03, clpm1.04, clpm1.05,
         clpm1.06, clpm1.07, clpm1.08, clpm1.09, clpm1.10), 
    "T10.SDO", "T11.SDO", "SDO", -0.4, 0.2)),
  tar_target(clpmPlot_IncRed, drawCLPMFigure(
    list(clpm2.01, clpm2.02, clpm2.03, clpm2.04, clpm2.05,
         clpm2.06, clpm2.07, clpm2.08, clpm2.09, clpm2.10),
    "Issue.IncomeRedistribution.T10", "Issue.IncomeRedistribution.T11", "IncRed", -0.2, 0.4)),
  tar_target(clpmPlot_IncAtt, drawCLPMFigure(
    list(clpm3.01, clpm3.02, clpm3.03, clpm3.04, clpm3.05,
         clpm3.06, clpm3.07, clpm3.08, clpm3.09, clpm3.10),
    "IncomeAttribution.T10", "IncomeAttribution.T11", "IncAtt", -0.4, 0.3)),
  tar_target(clpmPlot_PolNat, drawCLPMFigure(
    list(clpm4.01, clpm4.02, clpm4.03, clpm4.04, clpm4.05,
         clpm4.06, clpm4.07, clpm4.08, clpm4.09, clpm4.10),
    "Pol.SupNational.T10", "Pol.SupNational.T11", "PolNat", -0.25, 0.5)),
  # render manuscript
  tar_render(manuscript, "manuscript.Rmd"),
  # print session info for reproducibility
  tar_target(sessionInfo, writeLines(capture.output(sessionInfo()), "sessionInfo.txt"))
)