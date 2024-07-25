library(tidyverse)
library(targets)
library(tarchetypes)
tar_option_set(
  packages = c("car","cowplot","dplyr","fastDummies","ggcorrplot","ggforce",
               "ggraph","ggplot2","haven","igraph","kableExtra","lavaan","lme4",
               "lmerTest","ltm","mice","papaja","psych","readr","semTools",
               "stringr")
)

#### set up targets for economic game analyses

# vector of controls
clpmControls <- 
  c(
    "", "Age.T10.c", "Gender.T10", 
    "EthnicCats.T10_Asian + EthnicCats.T10_Maori + EthnicCats.T10_Pacific",
    "NZREG.T10", "NZSEI13.T10", "NZDep.2013.T10", "Religious.T10", "T10.RWA",
    "T10.EXTRAVERSION", "T10.AGREEABLENESS", "T10.CONSCIENTIOUSNESS", 
    "T10.NEUROTICISM", "T10.OPENNESS", "T10.HONESTY_HUMILITY", 
    "T10.NARCISSISM",
    paste0(
      "Age.T10.c + Gender.T10 + EthnicCats.T10_Asian + EthnicCats.T10_Maori",
      " + EthnicCats.T10_Pacific + NZREG.T10 + NZSEI13.T10 + NZDep.2013.T10",
      " + Religious.T10 + T10.RWA + T10.EXTRAVERSION + T10.AGREEABLENESS",
      " + T10.CONSCIENTIOUSNESS + T10.NEUROTICISM + T10.OPENNESS",
      " + T10.HONESTY_HUMILITY + T10.NARCISSISM"
    )
  )

# vector of ordered items
clpmOrdered <- 
  list(
    c(paste0("egame.TG1.T1", 0:1)),
    c(paste0("egame.TG1.T1", 0:1), 
      paste0("Issue.IncomeRedistribution.T1", 0:1)),
    c(paste0("egame.TG1.T1", 0:1),
      paste0("IncomeAttribution.T1", 0:1)),
    c(paste0("egame.TG1.T1", 0:1),
      paste0("Pol.SupNational.T1", 0:1))
  )

# create targets for cross-lagged panel models
clpmTargets <-
  tar_map(
    values =
      expand_grid(
        # imputed data vs. listwise deletion
        data = rlang::syms(c("d", "dM")),
        df = tibble(
          # shorter name for target
          name = paste0(
            rep(1:4, each = 17), ".", 
            rep(sprintf("%02d", 1:17), times = 4)
          ),
          # variable 1 in cross-lagged panel model
          var1 = rep(
            c("T10.SDO","Issue.IncomeRedistribution.T10",
              "IncomeAttribution.T10", "Pol.SupNational.T10"),
            each = 17
          ),
          # variable 2 in cross-lagged panel model
          var2 = rep(
            c("T11.SDO","Issue.IncomeRedistribution.T11",
              "IncomeAttribution.T11", "Pol.SupNational.T11"),
            each = 17
          ),
          # which items are ordered in model
          ordered = rep(clpmOrdered, each = 17),
          # which controls to include in model
          controls = rep(clpmControls, times = 4)
        )
      ) %>%
      unnest(df),
    names = c("name","data","type"),
    tar_target(clpm, fitCLPM(data, var1, var2, ordered, controls, type))
  )

# create targets for all analyses
analysisTargets <-
  tar_map(
    values = tibble(
      # full vs. reduced set of games
      type = c("Full", "Reduced")
      ),
    # fit cfa
    tar_target(cfa, fitCFA(d, type)),
    # fit sem
    tar_target(sem1, fitSEM1(dM, type)),
    tar_target(sem2_SDO, fitSEM2(dM, outcome = "T11.SDO", type)),
    tar_target(sem2_PolNat, fitSEM2(dM, outcome = "Pol.SupNational.T11", type)),
    # plot sdo ~ prosocial latent variable
    tar_target(plotSem, plotProsocialSDO(dM, type)),
    # measurement invariance across waves
    tar_target(configMI, fitMI(d, invariance = "config", type)),
    tar_target(metricMI, fitMI(d, invariance = "metric", type)),
    tar_target(scalarMI, fitMI(d, invariance = "scalar", type)),
    tar_target(strictMI, fitMI(d, invariance = "strict", type)),
    tar_target(
      tableCompareMI,
      getComparisonTable(configMI, metricMI, scalarMI, strictMI)
      ),
    # cross-lagged panel models
    clpmTargets,
    tar_combine(clpm, clpmTargets[[1]], command = list(!!!.x)),
    # multi-group cross-lagged panel models
    tar_target(
      clpmSplitGender,
      fitCLPMSplit(dM, var1 = "T10.SDO", var2 = "T11.SDO", 
                   group = "Gender.T10", type = type)
      ),
    tar_target(
      clpmSplitEthnicity,
      fitCLPMSplit(dM, var1 = "T10.SDO", var2 = "T11.SDO",
                   group = "EthnicCats.T10", type = type)
    ),
    # figures
    # models with listwise-deleted data
    tar_target(
      clpmPlot_SDO_d,
      drawCLPMFigure(
        clpm[1:17], "T10.SDO", "T11.SDO", "SDO",
        "clpmPlot_SDO_listwise", type = type
      )
    ),
    tar_target(
      clpmPlot_IncRed_d,
      drawCLPMFigure(
        clpm[18:34], "Issue.IncomeRedistribution.T10", 
        "Issue.IncomeRedistribution.T11", "IncRed",
        "clpmPlot_IncRed_listwise", type = type
      )
    ),
    tar_target(
      clpmPlot_IncAtt_d,
      drawCLPMFigure(
        clpm[35:51], "IncomeAttribution.T10", 
        "IncomeAttribution.T11", "IncAtt",
        "clpmPlot_IncAtt_listwise", type = type
      )
    ),
    tar_target(
      clpmPlot_PolNat_d,
      drawCLPMFigure(
        clpm[52:68], "Pol.SupNational.T10", 
        "Pol.SupNational.T11", "PolNat",
        "clpmPlot_PolNat_listwise", type = type
      )
    ),
    # models with imputed data
    tar_target(
      clpmPlot_SDO_dM,
      drawCLPMFigure(
        clpm[69:85], "T10.SDO", "T11.SDO", "SDO",
        "clpmPlot_SDO_imputed", type = type
      )
    ),
    tar_target(
      clpmPlot_IncRed_dM,
      drawCLPMFigure(
        clpm[86:102], "Issue.IncomeRedistribution.T10", 
        "Issue.IncomeRedistribution.T11", "IncRed",
        "clpmPlot_IncRed_imputed", type = type
      )
    ),
    tar_target(
      clpmPlot_IncAtt_dM,
      drawCLPMFigure(
        clpm[103:119], "IncomeAttribution.T10", 
        "IncomeAttribution.T11", "IncAtt",
        "clpmPlot_IncAtt_imputed", type = type
      )
    ),
    tar_target(
      clpmPlot_PolNat_dM,
      drawCLPMFigure(
        clpm[120:136], "Pol.SupNational.T10", 
        "Pol.SupNational.T11", "PolNat",
        "clpmPlot_PolNat_imputed", type = type
      )
    )
  )
