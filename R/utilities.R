# Packages =====================================================================
library(MASS)
library(plyr)
library(tidyverse)
library(lubridate)
library(janitor)
library(assertthat)
library(here)
library(styler)
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
library(DescTools)
library(prediction)
library(gmodels)
library(huxtable) ## export_summs in jtools need this
library(flextable)
library(Kmisc) ## install_github("sysilviakim/Kmisc")
library(Hmisc) ## latexTranslate
library(pwr)

# Subdirectories ===============================================================
if (!dir.exists(here("R", "eda"))) {
  dir.create(here("R", "eda"))
}

# Helper vectors ===============================================================
# covariate labels as vectors
covars_names <- c(
  genderM = "Gender: Male", 
  age = "Age", 
  race5Black = "Race: Black", 
  race5Latino = "Race: Hispanic/Latino",
  race5Asian = "Race: Asian", 
  race5Other = "Race: Other", 
  educHS = "Education: HS",
  `educSome college` = "Education: Some College", 
  `educ2-yr` = "Education: 2-yr",
  `educ4-yr` = "Education: 4-yr", 
  `educPost-grad` = "Education: Post-grad",
  `income350-100k` = "Income: 50-100k",
  `income3100k+` = "Income: 100k+",
  `income3Prefer not to say` = "Income: Prefer not to say",
  `ca_regionCentral valley/inland` = "CA Region: Central Valley/Inland",
  ca_regionCoastal = "CA Region: Coastal",
  ca_regionLA = "CA Region: LA",
  `ca_regionSoCal (non-LA)` = "CA Region: Southern California (non-LA)",
  partyRep = "Party: Rep", 
  partyOther = "Party: Other",
  `elec_int_stateSomewhat confident` = "Election Integrity: Somewhat confident",
  `elec_int_stateNot too confident` = "Election Integrity: Not too confident",
  `elec_int_stateNot at all confident` = 
    "Electoral Integrity: Not at all confident",
  `elec_int_stateDon't know` = "Electoral Integrity: Don't know",
  `covid_responseLess effective than others` = 
    "COVID Response: Less effective than others",
  `covid_responseAbout as effective` = "COVID Response: About as effective",
  constant = "Constant"
)

short_covars_names <- c(
  "Race: Black", "Race: Hispanic/Latino",
  "Race: Asian", "Race: Other", "Party: Rep", "Party: Other",
  "Election Integrity: Somewhat confident",
  "Election Integrity: Not too confident",
  "Electoral Integrity: Not at all confident",
  "Electoral Integrity: Don't know",
  "COVID Response: Less effective than others",
  "COVID Response: About as effective"
)

# Functions ====================================================================
## glm function
reg_form <- function(x, vars, data, sv_design, survey = FALSE) {
  if (survey == TRUE) {
    svyglm(
      as.formula(
        paste0(x, " ~ ", paste(var_list[vars] %>% unlist(), collapse = " + "))
      ),
      design = sv_design,
      family = "quasibinomial", x = TRUE
      ## Suppress warning about non-integer #n of successes so not
      ## family = binomial(link = "logit")
    )
  } else {
    glm(
      as.formula(
        paste0(x, " ~ ", paste(var_list[vars] %>% unlist(), collapse = " + "))
      ),
      data = data,
      family = "binomial", x = TRUE
    )
  }
}

## lpm function
reg_form_lpm <- function(x, vars, data) {
  lm(paste(x, "~", paste(unlist(var_list_lpm[vars]),
    collapse = " + "
  )),
  data = cal_subset, weight = weight_ca
  )
}

## Custom Stargazer Functions
stargazer_custom_tex <- function(x, type = "latex", lab = c(15, 16),
                                 subgroup = "none",
                                 out15 = here("tab", "reg_prop15_long.tex"),
                                 out16 = here("tab", "reg_prop16_long.tex"),
                                 lab15 = "tab:reg_prop15_long",
                                 lab16 = "tab:reg_prop16_long", 
                                 dep15 = "Support Proposition 15",
                                 dep16 = "Support Proposition 16",
                                 ...) {
  if (subgroup == "party") {
    dep15 <- paste0(dep15, " by Party")
    dep16 <- paste0(dep16, " by Party")
  }
  stargazer(
    x,
    omit = "Constant",
    omit.stat = "f",
    covariate.labels = covars_names,
    label = ifelse(lab == 15, lab15, lab16),
    dep.var.labels = ifelse(lab == 15, dep15, dep16),
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
      paste("Proposition", lab),
      ifelse(subgroup == "party", "Models by Party", "Models (Full)")
    ),
    out = ifelse(lab == 15, out15, out16),
    ...
  )
}

## short version
stargazer_tex_omit <- function(x, type = "latex", lab = c(15, 16)) {
  stargazer(
    x,
    label = paste0("tab:reg_prop", lab, "_short"),
    omit = c(
      "Constant", "gender", "age", "educ",
      "income3", "ca_region"
    ),
    omit.yes.no = c("Yes", "No"),
    omit.labels = c(
      "Constant", "Gender", "Age", "Education",
      "Income", "CA Region"
    ),
    covariate.labels = short_covars_names,
    dep.var.labels = paste0("Support Proposition ", lab),
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
    title = paste("Proposition", lab, "Models"),
    out = here("tab", paste0("reg_prop", lab, "_short.tex"))
  )
}

## function to create odds-ratio latex tables
stargazer_odds_tex <- function(x, type = "latex", lab = c(15, 16)) {
  stargazer(
    x,
    omit = "Constant",
    covariate.labels = covars_names,
    dep.var.labels = paste0("Support Proposition ", lab),
    apply.coef = exp,
    t.auto = F,
    p.auto = F,
    # report = "vct*",
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
    title = paste("Proposition", lab, "Odds-Ratios"),
    out = here("tab", paste0("prop", lab, "_oddsratios_long.tex"))
  )
}

stargazer_custom <- function(x, type = "text", lab = c(15, 16)) {
  stargazer(
    x,
    omit = c(
      "Constant", "Gender: Male", "Age", "Race: Black", "Race: Hispanic/Latino",
      "Race: Asian", "Race: Other", "Education: HS",
      "Education: Some College", "Education: 2-yr",
      "Education: 4-yr", "Education: Post-grad",
      "Income: 50-100k", "Income: 100k+",
      "Income: Prefer not to say",
      "CA Region: Central Valley/Inland", "CA Region: Coastal",
      "CA Region: LA",
      "CA Region: Southern California (non-LA)"
    ),
    covariate.labels = covars_names,
    dep.var.labels = paste0("Support Proposition ", lab),
    model.numbers = TRUE,
    star.char = c("*", "**", "***"),
    star.cutoffs = c(.05, .01, .001),
    digits = 3,
    header = FALSE,
    type = type,
    title = paste("Proposition", lab, "Models")
  )
}

stargazer_custom_omit <- function(x, type = "text", lab = c(15, 16)) {
  stargazer(
    x,
    omit = c(
      "Constant", "genderM", "age", "educHS",
      "educSome college", "educ2-yr",
      "educ4-yr", "educPost-grad",
      "income350-100k", "income3100k+",
      "income3Prefer not to say", "ca_region"
    ),
    omit.yes.no = c("Yes", "No"),
    omit.labels = c(
      "Constant", "Gender: Male", "Age", "Education: HS",
      "Education: Some College", "Education: 2-yr",
      "Education: 4-yr", "Education: Post-grad",
      "Income: 50-100k", "Income: 100k+",
      "Income: Prefer not to say",
      "CA Region"
    ),
    covariate.labels = short_covars_names,
    dep.var.labels = paste0("Support Proposition ", lab),
    model.numbers = TRUE,
    star.char = c("*", "**", "***"),
    star.cutoffs = c(.05, .01, .001),
    digits = 3,
    header = FALSE,
    type = type,
    title = paste("Proposition", lab, "Models")
  )
}

stargazer_custom_odds <- function(x, type = "text", lab = c(15, 16)) {
  stargazer(
    x,
    omit = "Constant",
    covariate.labels =
      covars_names,
    dep.var.labels = paste0("Support Proposition ", lab),
    apply.coef = exp,
    t.auto = F,
    p.auto = F,
    # report = "vct*",
    # dep.var.labels.include = FALSE,
    model.numbers = TRUE,
    star.char = c("*", "**", "***"),
    star.cutoffs = c(.05, .01, .001),
    digits = 3,
    header = FALSE,
    type = type
  )
}

## misc.
desc_table <- function(x, y, variable) {
  prop_labels %>%
    map_dfr(
      ~ {
        tabyl(
          dat = x,
          !!as.name(paste0("prop_", .x)),
          .y,
          type = "f"
        ) %>%
          mutate(across(where(is.factor), as.character)) %>%
          adorn_percentages("col") %>%
          adorn_pct_formatting(digits = 2) %>%
          unite("front")
      }
    )
}

tidy_race <- function(x) {
  bind_cols(
    tidy(x),
    as.data.frame(confint(x))
  ) %>%
    rename("conf.low" = "2.5 %", "conf.high" = "97.5 %") %>%
    filter(grepl("race5", term)) %>%
    mutate(term = gsub("race5", "", term))
}

race_highlight <- function(x, y,
                           my_theme = TRUE,
                           limits = c(-.5, 2.0),
                           breaks = seq(-.5, 2.0, by = .5),
                           ylab = "Estimated Support (95% C.I.)") {
  p <- ggplot(x, aes(term, estimate, color = term)) +
    geom_point() +
    geom_pointrange(size = 1.2, aes(ymin = conf.low, ymax = conf.high)) +
    labs(
      x = "Race",
      y = ylab,
      color = "Race"
    ) +
    scale_colour_manual(
      values = c(
        "Black" = "gray24",
        "Latino" = "red1",
        "Asian" = "gray24",
        "Other" = "gray24"
      )
    ) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(
      limits = limits, breaks = breaks,
      labels =
        scales::number_format(accuracy = 0.01), oob = rescale_none
    ) +
    annotate("rect", fill = "lightgray", alpha = 0.4) +
    ggtitle(
      paste0(
        "Full Model, Prop. ", ifelse(grepl("15", y), 15, 16), " and Race"
      )
    )

  if (my_theme) {
    p +
      theme(
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14)
      )
  } else {
    p
  }
}
