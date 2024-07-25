# fit standard cross-lagged panel model for multiply imputed data
fitCLPM <- function(d, var1, var2, ordered, controls = "", type) {
  # create model specification
  if (type == "Reduced") {
    model <- 
      paste0(
        "# economic game measurement model\n",
        "prosocialT11 =~ egame.TG1.T11 + l2*egame.TG2.T11 + l3*egame.PGG.T11",
        " + l4*egame.DG.T11\n",
        "prosocialT10 =~ egame.TG1.T10 + l2*egame.TG2.T10 + l3*egame.PGG.T10",
        " + l4*egame.DG.T10\n",
        "# variances\n",
        "prosocialT10 ~~ prosocialT10\n",
        "prosocialT11 ~~ prosocialT11\n",
        "egame.TG2.T11 ~~ v1*egame.TG2.T11\n",
        "egame.PGG.T11 ~~ v2*egame.PGG.T11\n",
        "egame.DG.T11  ~~ v3*egame.DG.T11\n",
        "egame.TG2.T10 ~~ v1*egame.TG2.T10\n",
        "egame.PGG.T10 ~~ v2*egame.PGG.T10\n",
        "egame.DG.T10  ~~ v3*egame.DG.T10\n",
        "egame.TG1.T10 ~~ egame.TG1.T11\n",
        "egame.TG2.T10 ~~ egame.TG2.T11\n",
        "egame.PGG.T10 ~~ egame.PGG.T11\n",
        "egame.DG.T10  ~~ egame.DG.T11\n",
        "# intercepts\n",
        "egame.TG1.T11 | i1*t1\n",
        "egame.TG2.T11 ~ i2*1\n",
        "egame.PGG.T11 ~ i3*1\n",
        "egame.DG.T11  ~ i4*1\n",
        "egame.TG1.T10 | i1*t1\n",
        "egame.TG2.T10 ~ i2*1\n",
        "egame.PGG.T10 ~ i3*1\n",
        "egame.DG.T10  ~ i4*1\n",
        "# covariances\n",
        "prosocialT11 ~~ ", var2, "\n",
        "prosocialT10 ~~ ", var1, "\n",
        "# regressions\n",
        "prosocialT11 ~ ar1*prosocialT10 + cl1*", var1, "\n",
        var2, " ~ cl2*prosocialT10 + ar2*", var1, "\n",
        "# difference between cross-lagged paths\n",
        "clDiff := cl1 - cl2"
      )
  } else if (type == "Full") {
    model <- 
      paste0(
        "# economic game measurement model\n",
        "prosocialT11 =~ egame.TG1.T11 + l2*egame.TG2.T11 + l3*egame.PGG.T11",
        " + l4*egame.DG.T11 + l5*egame.UG1.T11 + l6*egame.SPP1.T11\n",
        "prosocialT10 =~ egame.TG1.T10 + l2*egame.TG2.T10 + l3*egame.PGG.T10",
        " + l4*egame.DG.T10 + l5*egame.UG1.T10 + l6*egame.SPP1.T10\n",
        "# variances\n",
        "prosocialT10 ~~ prosocialT10\n",
        "prosocialT11 ~~ prosocialT11\n",
        "egame.TG2.T11 ~~ v1*egame.TG2.T11\n",
        "egame.PGG.T11 ~~ v2*egame.PGG.T11\n",
        "egame.DG.T11  ~~ v3*egame.DG.T11\n",
        "egame.UG1.T11 ~~ v4*egame.UG1.T11\n",
        "egame.TG2.T10 ~~ v1*egame.TG2.T10\n",
        "egame.PGG.T10 ~~ v2*egame.PGG.T10\n",
        "egame.DG.T10  ~~ v3*egame.DG.T10\n",
        "egame.UG1.T10 ~~ v4*egame.UG1.T10\n",
        "egame.TG1.T10 ~~ egame.TG1.T11\n",
        "egame.TG2.T10 ~~ egame.TG2.T11\n",
        "egame.PGG.T10 ~~ egame.PGG.T11\n",
        "egame.DG.T10  ~~ egame.DG.T11\n",
        "egame.UG1.T10 ~~ egame.UG1.T11\n",
        "egame.SPP1.T10 ~~ egame.SPP1.T11\n",
        "# intercepts\n",
        "egame.TG1.T11  | i1*t1\n",
        "egame.TG2.T11  ~ i2*1\n",
        "egame.PGG.T11  ~ i3*1\n",
        "egame.DG.T11   ~ i4*1\n",
        "egame.UG1.T11  ~ i5*1\n",
        "egame.SPP1.T11 | i6*t1\n",
        "egame.TG1.T10  | i1*t1\n",
        "egame.TG2.T10  ~ i2*1\n",
        "egame.PGG.T10  ~ i3*1\n",
        "egame.DG.T10   ~ i4*1\n",
        "egame.UG1.T10  ~ i5*1\n",
        "egame.SPP1.T10 | i6*t1\n",
        "# covariances\n",
        "prosocialT11 ~~ ", var2, "\n",
        "prosocialT10 ~~ ", var1, "\n",
        "# regressions\n",
        "prosocialT11 ~ ar1*prosocialT10 + cl1*", var1, "\n",
        var2, " ~ cl2*prosocialT10 + ar2*", var1, "\n",
        "# difference between cross-lagged paths\n",
        "clDiff := cl1 - cl2"
      )
  }
  # add controls
  if (controls != "") {
    model <- 
      paste0(
        model, "\n",
        "# control regressions\n",
        "prosocialT11 ~ ", controls, "\n",
        var2, " ~ ", controls, "\n",
        "# control covariances\n",
        "prosocialT10 ~~ ", controls, "\n",
        var1, " ~~ ", controls
      )
  }
  # get ordered items
  if (type == "Full") {
    ordered <-
      paste0(
        ordered,
        paste0("egame.SPP1.T1", 0:1)
      )
  }
  if (is.mids(d)) {
    # fit model with multiply imputed data
    out <- sem.mi(model, data = d, ordered = ordered)
  } else {
    # fit model with listwise-deleted data
    d <- dummy_columns(d, "EthnicCats.T10")
    out <- sem(model, data = d, ordered = ordered)
  }
  return(out)
}
