# get comparison table
getComparisonTable <- function(configMI, metricMI, scalarMI, strictMI) {
  getMeasure <- function(measure) {
    c(fitMeasures(configMI)[measure],
      fitMeasures(metricMI)[measure],
      fitMeasures(scalarMI)[measure],
      fitMeasures(strictMI)[measure])
  }
  roundTable <- function(x, digits) {
    ifelse(
      is.na(x),
      "-",
      as.character(format(round(x, digits), trim = T, nsmall = digits))
      )
  }
  out <- 
    tibble(
      Model            = c("Configural", "Metric", "Scalar", "Strict"),
      `$\\chi^2$`      = getMeasure("chisq"),
      df               = getMeasure("df"),
      CFI              = getMeasure("cfi"),
      RMSEA            = getMeasure("rmsea"),
      SRMR             = getMeasure("srmr")
    ) %>%
    mutate(
      `$\\Delta$CFI`   = c(NA, .$CFI[-1]   - .$CFI[1:3]),
      `$\\Delta$RMSEA` = c(NA, .$RMSEA[-1] - .$RMSEA[1:3]),
      `$\\Delta$SRMR`  = c(NA, .$SRMR[-1]  - .$SRMR[1:3])
    ) %>%
    mutate_at(vars(`$\\chi^2$`),         roundTable, digits = 2) %>%
    mutate_at(vars(df),                  roundTable, digits = 0) %>%
    mutate_at(vars(CFI:`$\\Delta$SRMR`), roundTable, digits = 3)
  return(out)
}
