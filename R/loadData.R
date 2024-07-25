# load NZAVS self-report and gameplay data
loadData <- function(fileData) {
  read_sav(
    fileData,
    col_select = c(
      "Questionnaire.Num",
      # time variables
      "egame.TSCORE.T11",
      "egame.TSCORE.T10",
      "TSCORE.T11",
      "TSCORE.T10",
      # demographic variables
      "Gender.T10",
      "Age.T10",
      "NZDep.2013.T10",
      "EthnicCats.T10", 
      "Religious.T10",
      "NZREG.T10",
      "NZSEI13.T10",
      # personality variables
      "T10.EXTRAVERSION",
      "T10.AGREEABLENESS",
      "T10.CONSCIENTIOUSNESS", 
      "T10.NEUROTICISM",
      "T10.OPENNESS",
      "T10.HONESTY_HUMILITY", 
      "T10.NARCISSISM",
      # economic games variables
      "egame.Chk1.T10",
      "egame.Chk2.T10",
      "egame.TG1.T10", 
      "egame.TG2.T10",
      "egame.PGG.T10",
      "egame.UG1.T10",
      "egame.DG.T10",
      "egame.SPP1.T10",
      "egame.cmpTG.T10",
      "egame.cmpPGG.T10",
      "egame.cmpUG.T10",
      "egame.cmpDG.T10",
      "egame.cmpSPP.T10",
      "egame.Chk1.T11",
      "egame.Chk2.T11",
      "egame.TG1.T11",
      "egame.TG2.T11",
      "egame.PGG.T11",
      "egame.UG1.T11",
      "egame.DG.T11",
      "egame.SPP1.T11",
      "egame.cmpTG.T11",
      "egame.cmpPGG.T11",
      "egame.cmpUG.T11",
      "egame.cmpDG.T11",
      "egame.cmpSPP.T11",
      starts_with("egame.Secs"),
      starts_with("egame.Paid."),
      # political variables
      "T10.SDO",
      "T11.SDO",
      "T10.RWA",
      "IncomeAttribution.T10",
      "IncomeAttribution.T11",
      "Issue.IncomeRedistribution.T10", 
      "Issue.IncomeRedistribution.T11",
      "Pol.SupNational.T10",
      "Pol.SupNational.T11"
      )
    ) %>%
    # remove labels
    zap_label() %>%
    zap_labels() %>%
    # remove timeouts and no pay (wave 1 only)
    filter(egame.Chk1.T10 == 1 & egame.Chk2.T10 == 1)
}
