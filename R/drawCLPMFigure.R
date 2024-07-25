# plot clpm model and parameters
drawCLPMFigure <- function(modelList, var1, var2, semPlotVar, 
                           filename, type) {
  # fig A
  # get labelling function
  getLabel <- function(model, LHS, OP, RHS) {
    if (class(model) == "lavaan.mi") {
      s <- filter(
        tibble(summary(model, standardized = TRUE)),
        lhs == LHS & op == OP & rhs == RHS
        )
      paste0(
        format(round(s$std.all, 2), nsmall = 2), 
        ifelse(s$pvalue < 0.001, "***", 
               ifelse(s$pvalue < 0.01, "**", 
                      ifelse(s$pvalue < 0.05, "*", "")))
        )
    } else {
      s <- filter(
        standardizedSolution(model),
        lhs == LHS & op == OP & rhs == RHS
        )
      paste0(
        format(round(s$est.std, 2), nsmall = 2),
        ifelse(s$pvalue < 0.001, "***", 
               ifelse(s$pvalue < 0.01, "**", 
                      ifelse(s$pvalue < 0.05, "*", "")))
        )
    }
  }
  # create graph
  graph <-
    data.frame(
      from = c(1, 1, 2, 2, 3, 4, 1, 2),
      to = c(3, 4, 3, 4, 4, 3, 2, 1), 
      coef = c(
        getLabel(modelList[[1]], "prosocialT11", "~",  "prosocialT10"),
        getLabel(modelList[[1]], var2,           "~",  "prosocialT10"),
        getLabel(modelList[[1]], "prosocialT11", "~",  var1          ),
        getLabel(modelList[[1]], var2,           "~",  var1          ),
        getLabel(modelList[[1]], "prosocialT11", "~~", var2          ), "",
        getLabel(modelList[[1]], "prosocialT10", "~~", var1          ), ""
        ),
      label_pos = c(0.5, 0.6, 0.6, 0.5, 0.5, 0.5, 0.5, 0.5)
      ) %>%
    graph_from_data_frame() %>%
    create_layout(layout = "stress")
  # modify graph
  graph$x <- c(0, 0, 2, 2)
  graph$y <- c(0.75, 0, 0.75, 0)
  graph$name <- c("Prosocial1", paste0(semPlotVar, 1), 
                  "Prosocial2", paste0(semPlotVar, 2))
  # plot
  pA <-
    ggraph(graph, layout = "stress") +
    geom_edge_link(
      aes(label = coef, label_pos = label_pos),
      start_cap = rectangle(width = 32, height = 10, "mm"),
      end_cap = rectangle(width = 32, height = 10, "mm"),
      angle_calc = 'along',
      label_dodge = unit(2.5, 'mm'),
      arrow = arrow(length = unit(4, 'mm'), type = "closed", ends = "last")
      ) +
    geom_rect(
      aes(xmin = 1.74, xmax = 2.26, ymin = -0.07, ymax = 0.07), 
      fill = "white", colour = "black"
      ) +
    geom_rect(
      aes(xmin = 0.26, xmax = -0.26, ymin = -0.07, ymax = 0.07), 
      fill = "white", colour = "black"
      ) +
    geom_ellipse(
      aes(x0 = 0, y0 = 0.75, a = 0.27, b = 0.07, angle = 0), 
      fill = "white", colour = "black"
      ) +
    geom_ellipse(
      aes(x0 = 2, y0 = 0.75, a = 0.27, b = 0.07, angle = 0), 
      fill = "white", colour = "black"
      ) +
    geom_node_text(aes(label = name), size = 5) +
    theme_graph()
  # plotting function for fig b and c
  getEstimatePlot <- function(modelList, iv, dv, title) {
    # extracting function
    extractFromModel <- function(model, LHS, OP, RHS, extract) {
      if (class(model) == "lavaan.mi") {
        filter(
          (summary(model)), lhs == LHS & op == OP & rhs == RHS
          )[,extract]
      } else {
        filter(
          parameterEstimates(model), lhs == LHS & op == OP & rhs == RHS
          )[,extract]
      }
    }
    # extract estimates and CIs
    est <- rep(NA, 17)
    for (i in 1:17) {
      est[i] <- extractFromModel(modelList[[i]], dv, "~", iv, "est")
    }
    se  <- rep(NA, 17)
    for (i in 1:17) {
      se[i]  <- extractFromModel(modelList[[i]], dv, "~", iv, "se")
    }
    models <- 
      c("No controls", "Age", "Gender", "Ethnicity", "Education", "SES", 
        "Deprivation", "Religious", "RWA", "Extraversion", "Agreeableness",
        "Conscientiousness", "Neuroticism", "Openness", "Honesty-Humility",
        "Narcissism", "Full model")
    # plot
    tibble(
      Model = factor(models, levels = models),
      Estimate = est, SE = se
      ) %>%
      mutate(Upper = Estimate + 1.96*SE,
             Lower = Estimate - 1.96*SE) %>%
      ggplot(
        aes(x = Estimate, y = fct_rev(Model), xmin = Lower, xmax = Upper)
        ) +
      geom_vline(linetype = "dashed", xintercept = 0) +
      geom_pointrange() +
      labs(y = NULL, x = "Unstandardised Estimate") +
      ggtitle(title) +
      theme_classic()
  }
  # fig B
  pB <- getEstimatePlot(
    modelList, var1, "prosocialT11", 
    expr(!!paste0(semPlotVar, "1 ") %->% " Prosocial2")
    )
  # fig C
  pC <- getEstimatePlot(
    modelList, "prosocialT10", var2, 
    expr("Prosocial1 " %->% !!paste0(" ", semPlotVar, "2"))
    )
  # put everything together
  top    <- plot_grid(pA, labels = letters[1])
  bottom <- plot_grid(pB, pC, nrow = 1, labels = letters[2:3])
  out <- plot_grid(top, bottom, nrow = 2)
  # save
  filename <- paste0("figures/", filename, "_", type, ".pdf")
  ggsave(out, filename = filename, height = 6, width = 7)
  return(out)
}
