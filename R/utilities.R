# Packages =====================================================================
library(plyr)
library(tidyverse)
library(lubridate)
library(janitor)
library(assertthat)
library(here)
library(styler)
# library(Kmisc)

library(margins)
library(stargazer)
library(xtable)
library(survey)
library(tableone)
library(GGally)
library(jtools)
library(ggstance)
library(broom)
library(broom.helpers)
library(scales)

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
### to make the latex tables - regressions
### functions have been modified for specific layout

stargazer_custom_tex <- function(x, type = "latex", lab = c(15, 16)) {
  stargazer(
    x,
    omit = "Constant",
    covariate.labels = c(
      "Gender: Male", "Age", "Race: Black", "Race: Hispanic",
      "Race: Asian", "Race: Other", "Education: HS",
      "Education: Some College", "Education: 2-yr",
      "Education: 4-yr", "Education: Post-grad",
      "Income: 50-100k", "Income: 100k+",
      "Income: Prefer not to say",
      "CA Region: Central Valley/Inland", "CA Region: Coastal",
      "CA Region: LA",
      "CA Region: Southern California (non-LA)",
      "Party: Other", "Party: Rep",
      "Election Integrity: Somewhat confident",
      "Election Integrity: Not too confident",
      "Electoral Integrity: Not at all confident",
      "Electoral Integrity: Don't know",
      "COVID Response: Less effective than others",
      "COVID Response: About as effective"
    ),
    dep.var.labels = ifelse(
      lab == 15,
      "Support Proposition 15",
      "Support Proposition 16"
    ),
    # dep.var.labels.include = FALSE,
    model.numbers = TRUE,
    star.char = c("*", "**", "***"),
    star.cutoffs = c(.05, .01, .001),
    digits = 3,
    header = FALSE,
    type = type,
    no.space = TRUE,
    font.size = "footnotesize",
    float.env = "table",
    title = paste(
      ifelse(
        lab == 15, "Proposition 15",
        ifelse(lab == 16, "Proposition 16", 0)
      ),
      "Models"
    ),
    out = ifelse(lab == 15, "reg_prop15_mod.tex", "reg_prop16_mod.tex")
  )
}


#### function to create odd-ratio latex tables

stargazer_odds_tex <- function(x, type = "latex", lab = c(15, 16)) {
  stargazer(
    x,
    omit = "Constant",
    covariate.labels = c(
      "Gender: Male", "Age", "Race: Black", "Race: Hispanic",
      "Race: Asian", "Race: Other", "Education: HS",
      "Education: Some College", "Education: 2-yr",
      "Education: 4-yr", "Education: Post-grad",
      "Income: 50-100k", "Income: 100k+",
      "Income: Prefer not to say",
      "CA Region: Central Valley/Inland", "CA Region: Coastal",
      "CA Region: LA",
      "CA Region: Southern California (non-LA)",
      "Party: Other", "Party: Rep",
      "Election Integrity: Somewhat confident",
      "Election Integrity: Not too confident",
      "Electoral Integrity: Not at all confident",
      "Electoral Integrity: Don't know",
      "COVID Response: Less effective than others",
      "COVID Response: About as effective"
    ),
    dep.var.labels = ifelse(
      lab == 15,
      "Support Proposition 15",
      "Support Proposition 16"
    ),
    apply.coef = exp,
    t.auto = F, 
    p.auto = F,
    report = "vct*",
    # dep.var.labels.include = FALSE,
    model.numbers = TRUE,
    star.char = c("*", "**", "***"),
    star.cutoffs = c(.05, .01, .001),
    digits = 3,
    header = FALSE,
    type = type,
    no.space = TRUE,
    font.size = "footnotesize",
    float.env = "table",
    title = paste(
      ifelse(
        lab == 15, "Proposition 15",
        ifelse(lab == 16, "Proposition 16", 0)
      ),
      "Odd-Ratios per Model"
    ),
    out = ifelse(
      lab == 15, "prop15_oddratios_mod.tex", "prop16_oddratios_mod.tex"
    )
  )
}

# to view the tables


stargazer_custom <- function(x, type = "text", lab = c(15, 16)) {
  stargazer(
    x,
    omit = "Constant",
    covariate.labels = c(
      "Gender: Male", "Age", "Race: Black", "Race: Hispanic",
      "Race: Asian", "Race: Other", "Education: HS",
      "Education: Some College", "Education: 2-yr",
      "Education: 4-yr", "Education: Post-grad",
      "Income: 50-100k", "Income: 100k+",
      "Income: Prefer not to say",
      "CA Region: Central Valley/Inland", "CA Region: Coastal",
      "CA Region: LA",
      "CA Region: Southern California (non-LA)",
      "Party: Other", "Party: Rep",
      "Election Integrity: Somewhat confident",
      "Election Integrity: Not too confident",
      "Electoral Integrity: Not at all confident",
      "Electoral Integrity: Don't know",
      "COVID Response: Less effective than others",
      "COVID Response: About as effective"
    ),
    dep.var.labels =
      ifelse(
        lab == 15,
        "Support Proposition 15", ifelse(lab == 16,
          "Support Proposition 16", 0
        )
      ),
    # dep.var.labels.include = TRUE,
    model.numbers = TRUE,
    star.char = c("*", "**", "***"),
    star.cutoffs = c(.05, .01, .001),
    digits = 3,
    header = FALSE,
    type = type,
    title = paste(
      ifelse(lab == 15, "Proposition 15", ifelse(lab == 16,
        "Proposition 16", 0
      )),
      "Models"
    )
  )
}

stargazer_custom_odds <- function(x, type = "text", lab = c(15, 16)) {
  stargazer(
    x,
    omit = "Constant",
    covariate.labels = c(
      "Gender: Male", "Age", "Race: Black", "Race: Hispanic",
      "Race: Asian", "Race: Other", "Education: HS",
      "Education: Some College", "Education: 2-yr",
      "Education: 4-yr", "Education: Post-grad",
      "Income: 50-100k", "Income: 100k+",
      "Income: Prefer not to say",
      "CA Region: Central Valley/Inland", "CA Region: Coastal",
      "CA Region: LA",
      "CA Region: Southern California (non-LA)",
      "Party: Other", "Party: Rep",
      "Election Integrity: Somewhat confident",
      "Election Integrity: Not too confident",
      "Electoral Integrity: Not at all confident",
      "Electoral Integrity: Don't know",
      "COVID Response: Less effective than others",
      "COVID Response: About as effective"
    ),
    dep.var.labels = ifelse(
      lab == 15,
      "Support Proposition 15",
      "Support Proposition 16"
    ),
    apply.coef = exp,
    t.auto = F, 
    p.auto = F,
    report = "vct*",
    # dep.var.labels.include = FALSE,
    model.numbers = TRUE,
    star.char = c("*", "**", "***"),
    star.cutoffs = c(.05, .01, .001),
    digits = 3,
    header = FALSE,
    type = type
  )
}
