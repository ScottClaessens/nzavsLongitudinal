# fit cross-lagged panel model split by gender/ethnicity
fitCLPMSplit <- function(dM, var1, var2, group, type) {
  # factors in data
  dM <- dMfactor(dM, group)
  # model specification
  if (type == "Reduced") {
    model <- 
      paste0(
        "# economic game measurement model\n",
        "prosocialT11 =~ egame.TG1.T11 + c(l2a,l2b)*egame.TG2.T11 + ",
        "c(l3a,l3b)*egame.PGG.T11 + c(l4a,l4b)*egame.DG.T11\n",
        "prosocialT10 =~ egame.TG1.T10 + c(l2a,l2b)*egame.TG2.T10 + ",
        "c(l3a,l3b)*egame.PGG.T10 + c(l4a,l4b)*egame.DG.T10\n",
        "# variances\n",
        "prosocialT10 ~~ prosocialT10\n",
        "prosocialT11 ~~ prosocialT11\n",
        "egame.TG2.T11 ~~ c(v1a,v1b)*egame.TG2.T11\n",
        "egame.PGG.T11 ~~ c(v2a,v2b)*egame.PGG.T11\n",
        "egame.DG.T11  ~~ c(v3a,v3b)*egame.DG.T11\n",
        "egame.TG2.T10 ~~ c(v1a,v1b)*egame.TG2.T10\n",
        "egame.PGG.T10 ~~ c(v2a,v2b)*egame.PGG.T10\n",
        "egame.DG.T10  ~~ c(v3a,v3b)*egame.DG.T10\n",
        "egame.TG1.T10 ~~ egame.TG1.T11\n",
        "egame.TG2.T10 ~~ egame.TG2.T11\n",
        "egame.PGG.T10 ~~ egame.PGG.T11\n",
        "egame.DG.T10  ~~ egame.DG.T11\n",
        "# intercepts\n",
        "egame.TG1.T11 | c(i1a,i1b)*t1\n",
        "egame.TG2.T11 ~ c(i2a,i2b)*1\n",
        "egame.PGG.T11 ~ c(i3a,i3b)*1\n",
        "egame.DG.T11  ~ c(i4a,i4b)*1\n",
        "egame.TG1.T10 | c(i1a,i1b)*t1\n",
        "egame.TG2.T10 ~ c(i2a,i2b)*1\n",
        "egame.PGG.T10 ~ c(i3a,i3b)*1\n",
        "egame.DG.T10  ~ c(i4a,i4b)*1\n",
        "# covariances\n",
        "prosocialT11 ~~ ", var2, "\n",
        "prosocialT10 ~~ ", var1, "\n",
        "# regressions\n",
        "prosocialT11 ~ c(ar1a,ar1b)*prosocialT10 + c(cl1a,cl1b)*", var1, "\n",
        var2, " ~ c(cl2a,cl2b)*prosocialT10 + c(ar2a,ar2b)*", var1
      )
  } else if (type == "Full") {
    model <- 
      paste0(
        "# economic game measurement model\n",
        "prosocialT11 =~ egame.TG1.T11 + c(l2a,l2b)*egame.TG2.T11 + ",
        "c(l3a,l3b)*egame.PGG.T11 + c(l4a,l4b)*egame.DG.T11 + ",
        "c(l5a,l5b)*egame.UG1.T11 + c(l6a,l6b)*egame.SPP1.T11\n",
        "prosocialT10 =~ egame.TG1.T10 + c(l2a,l2b)*egame.TG2.T10 + ",
        "c(l3a,l3b)*egame.PGG.T10 + c(l4a,l4b)*egame.DG.T10 + ",
        "c(l5a,l5b)*egame.UG1.T10 + c(l6a,l6b)*egame.SPP1.T10\n",
        "# variances\n",
        "prosocialT10 ~~ prosocialT10\n",
        "prosocialT11 ~~ prosocialT11\n",
        "egame.TG2.T11 ~~ c(v1a,v1b)*egame.TG2.T11\n",
        "egame.PGG.T11 ~~ c(v2a,v2b)*egame.PGG.T11\n",
        "egame.DG.T11  ~~ c(v3a,v3b)*egame.DG.T11\n",
        "egame.UG1.T11 ~~ c(v4a,v4b)*egame.UG1.T11\n",
        "egame.TG2.T10 ~~ c(v1a,v1b)*egame.TG2.T10\n",
        "egame.PGG.T10 ~~ c(v2a,v2b)*egame.PGG.T10\n",
        "egame.DG.T10  ~~ c(v3a,v3b)*egame.DG.T10\n",
        "egame.UG1.T10 ~~ c(v4a,v4b)*egame.UG1.T10\n",
        "egame.TG1.T10 ~~ egame.TG1.T11\n",
        "egame.TG2.T10 ~~ egame.TG2.T11\n",
        "egame.PGG.T10 ~~ egame.PGG.T11\n",
        "egame.DG.T10  ~~ egame.DG.T11\n",
        "egame.UG1.T10 ~~ egame.UG1.T11\n",
        "egame.SPP1.T10 ~~ egame.SPP1.T11\n",
        "# intercepts\n",
        "egame.TG1.T11 | c(i1a,i1b)*t1\n",
        "egame.TG2.T11 ~ c(i2a,i2b)*1\n",
        "egame.PGG.T11 ~ c(i3a,i3b)*1\n",
        "egame.DG.T11  ~ c(i4a,i4b)*1\n",
        "egame.UG1.T11 ~ c(i5a,i5b)*1\n",
        "egame.SPP1.T11 | c(i6a,i6b)*t1\n",
        "egame.TG1.T10 | c(i1a,i1b)*t1\n",
        "egame.TG2.T10 ~ c(i2a,i2b)*1\n",
        "egame.PGG.T10 ~ c(i3a,i3b)*1\n",
        "egame.DG.T10  ~ c(i4a,i4b)*1\n",
        "egame.UG1.T10 ~ c(i5a,i5b)*1\n",
        "egame.SPP1.T10 | c(i6a,i6b)*t1\n",
        "# covariances\n",
        "prosocialT11 ~~ ", var2, "\n",
        "prosocialT10 ~~ ", var1, "\n",
        "# regressions\n",
        "prosocialT11 ~ c(ar1a,ar1b)*prosocialT10 + c(cl1a,cl1b)*", var1, "\n",
        var2, " ~ c(cl2a,cl2b)*prosocialT10 + c(ar2a,ar2b)*", var1
      )
  }
  # ordered variables
  if (type == "Full") {
    ordered <- c(paste0("egame.TG1.T1", 0:1), paste0("egame.SPP1.T1", 0:1))
  } else if (type == "Reduced") {
    ordered <- paste0("egame.TG1.T1", 0:1)
  }
  # fit model with multiply imputed data
  out <- 
    sem.mi(
      model = model,
      data = dM,
      ordered = ordered,
      group = group
      )
  return(out)
}
