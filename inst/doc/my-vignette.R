## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE, results="hide"-------------------------------------

library(phantSEM)
library(lavaan)
library(tidyr)
library(tidyverse)

## -----------------------------------------------------------------------------
memory_CrossSectional <- matrix(c(
  0.2509804, 0.9511983, 0.4257081,
  0.9511983, 8.8661765, 2.6609477,
  0.4257081, 2.6609477, 10.8592048
), nrow = 3, byrow = T)
colnames(memory_CrossSectional) <- c("X", "M2", "Y2")

## -----------------------------------------------------------------------------
Observed_Model <- "
M2 ~ X
Y2 ~ M2+X
"

fit_obs <- sem(model = Observed_Model, sample.cov = memory_CrossSectional, sample.nobs = 138)

## -----------------------------------------------------------------------------
Phantom_Model <- "
M2 ~ M1 + Y1 + a*X
Y2 ~ M1 + Y1 + b*M2 + cp*X
"

## -----------------------------------------------------------------------------
Step1 <- SA_step1(
  lavoutput = fit_obs,
  mod_obs = Observed_Model,
  mod_phant = Phantom_Model
)

## -----------------------------------------------------------------------------


phantom_assignment <- list(
  "CovM1X" = 0,
  "CovY1M1" = "CovY2M2",
  "CovY1X" = 0,
  "VarM1" = 1,
  "VarY1" = 1,
  "CovM1M2" = seq(0, .6, .1),
  "CovY1Y2" = "CovM1M2",
  "CovY1M2" = seq(-.6, .6, .1),
  "CovM1Y2" = "CovY1M2"
)



Step2 <- SA_step2(
  phantom_assignment = phantom_assignment,
  step1 = Step1
)

## -----------------------------------------------------------------------------
names(Step2)

## ----cache=TRUE---------------------------------------------------------------
Step3 <- SA_step3(
  step2 = Step2,
  n = 138
)

## -----------------------------------------------------------------------------
b_ests <- ghost_par_ests(
  step3 = Step3,
  parameter_label = "b",
  remove_NA = FALSE
)

a_ests <- ghost_par_ests(
  step3 = Step3,
  parameter_label = "a",
  remove_NA = FALSE
)

b_ests$aest <- a_ests$est
b_ests$abest <- b_ests$est * b_ests$aest
b_ests <- cbind(Step2[["combos"]], b_ests)
b_ests$`CovM1M2,CovY1Y2` <- round(b_ests$`CovM1M2,CovY1Y2`, 2)
b_ests$`CovY1M2,CovM1Y2` <- round(b_ests$`CovY1M2,CovM1Y2`, 2)

head(b_ests)

## ----fig.width=10,fig.height=5,eval=FALSE-------------------------------------
#  library(ggplot2)
#  ggplot(b_ests, aes(x = `CovM1M2,CovY1Y2`, y = est)) +
#    geom_point(size = 2) +
#    scale_shape_manual() + # scale_fill_manual(values = c("black", "lightgray")) +
#    scale_color_manual(values = c("grey", "black")) +
#    facet_grid(~ b_ests$`CovY1M2,CovM1Y2`) +
#    theme_bw() +
#    scale_fill_grey() +
#    theme(
#      axis.title.y = element_text(size = 18),
#      axis.text.x = element_text(size = 12),
#      axis.title.x = element_text(size = 20),
#      axis.text = element_text(size = 15),
#      strip.text.y = element_text(size = 15),
#      strip.text.x = element_text(size = 20),
#      legend.text = element_text(size = 15),
#      legend.title = element_text(size = 18),
#      legend.position = "bottom",
#      panel.spacing.x = unit(2, "mm")
#    )

## -----------------------------------------------------------------------------
cov2cor(memory_CrossSectional)
lookup_results <- SA_lookup(CorXM = .64, CorXY = .26, CorMY = .27)

## ----fig.width=9,fig.height=5,eval=FALSE--------------------------------------
#  lookup_results_plot <- lookup_results %>% filter(round(corr, 1) == .3 & (stabr > -.1 & stabr < .7))
#  
#  ggplot(lookup_results_plot, aes(x = stabr, y = est_ab)) +
#    geom_point(size = 2) +
#    scale_shape_manual() + # scale_fill_manual(values = c("black", "lightgray")) +
#    scale_color_manual(values = c("grey", "black")) +
#    facet_grid(~ lookup_results_plot$clr) +
#    theme_bw() +
#    scale_fill_grey() +
#    theme(
#      axis.title.y = element_text(size = 18),
#      axis.text.x = element_text(size = 12),
#      axis.title.x = element_text(size = 20),
#      axis.text = element_text(size = 15),
#      strip.text.y = element_text(size = 15),
#      strip.text.x = element_text(size = 20),
#      legend.text = element_text(size = 15),
#      legend.title = element_text(size = 18),
#      legend.position = "bottom",
#      panel.spacing.x = unit(2, "mm")
#    )

