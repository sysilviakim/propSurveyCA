# Packages =====================================================================
library(plyr)
library(tidyverse)
library(lubridate)
library(janitor)
library(assertthat)
library(here)
library(styler)
library(Kmisc)

# library(skimr)
# library(DescTools)
# library(prediction)
library(margins)
library(stargazer)
library(xtable)
# library(pixiedust)
library(survey)

# Subdirectories ===============================================================
if (!dir.exists(here("R", "eda"))) {
  dir.create(here("R", "eda"))
}

# Functions ====================================================================
reg_form <- function(x, vars, survey = FALSE) {
  if (survey == TRUE) {
    svyglm(
      as.formula(
        paste0(x, " ~ ", paste(var_list[vars] %>% unlist(), collapse = " + "))
      ),
      design = sv_design,
      family = "quasibinomial"
      ## Suppress warning about non-integer #n of successes so not
      ## family = binomial(link = "logit")
    )
  } else {
    glm(
      as.formula(
        paste0(x, " ~ ", paste(var_list[vars] %>% unlist(), collapse = " + "))
      ),
      data = cal_subset, 
      family = "binomial"
    )
  }
}

stargazer_custom <- function(x, type = "text", lab = 15) {
  stargazer(
    x,
    omit = "Constant",
    dep.var.labels = ifelse(
      lab == 15,
      "Support Proposition 15",
      "Support Proposition 16"
    ),
    # dep.var.labels.include = FALSE,
    model.numbers = FALSE,
    star.char = c("*", "**", "***"),
    star.cutoffs = c(.05, .01, .001),
    digits = 3,
    header = FALSE,
    type = type
  )
}

stargazer_custom_tex <- function(x, type = "latex", lab = 15) {
  stargazer(
    x,
    omit = "Constant",
    dep.var.labels = ifelse(
      lab == 15,
      "Support Proposition 15",
      "Support Proposition 16"
    ),
    # dep.var.labels.include = FALSE,
    model.numbers = FALSE,
    star.char = c("*", "**", "***"),
    star.cutoffs = c(.05, .01, .001),
    digits = 3,
    header = FALSE,
    type = type
  )
}

