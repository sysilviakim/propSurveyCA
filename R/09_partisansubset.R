### Subsetting by Partisanship 

dem_subset <- subset(cal_subset, subset = cal_subset$pid3 == "Dem")

rep_subset <- subset(cal_subset, subset = cal_subset$pid3 == "Rep")

other_subset <- subset(cal_subset, subset = cal_subset$pid3 == "Independent"
                       | cal_subset$pid3 == "Other" |
                         cal_subset$pid3 == "Not sure")

## Survey Weights

dem_design <- svydesign(id = ~1, weights = ~weight_ca, data = dem_subset)

rep_design <- svydesign(id = ~1, weights = ~weight_ca, data = rep_subset)

other_design <- svydesign(id = ~1, weights = ~weight_ca, data = other_subset)

#### Running regressions 

models_partisan <- list(
  dem = y %>%
    map(
      ~ reg_form(.x, model_nested[c(1,2, 4)], dem_subset, dem_design, 
                 survey = TRUE)
    ),
  rep = y %>% 
    map(
      ~ reg_form(.x, model_nested[c(1,2, 4)], rep_subset, rep_design, 
                 survey = TRUE)
    ),
  other = y %>% 
    map(
      ~ reg_form(.x, model_nested[c(1,2, 4)], other_subset, other_design, 
                 survey = TRUE)
    )
)

### Printing Tables 
covars_names_par <- c(
  "Gender: Male", "Age", "Race: Black", "Race: Hispanic/Latino",
  "Race: Asian", "Race: Other", "Education: HS",
  "Education: Some College", "Education: 2-yr",
  "Education: 4-yr", "Education: Post-grad",
  "Income: 50-100k", "Income: 100k+",
  "Income: Prefer not to say",
  "CA Region: Central Valley/Inland", "CA Region: Coastal",
  "CA Region: LA",
  "CA Region: Southern California (non-LA)",
  "Election Integrity: Somewhat confident",
  "Election Integrity: Not too confident",
  "Electoral Integrity: Not at all confident",
  "Electoral Integrity: Don't know",
  "COVID Response: Less effective than others",
  "COVID Response: About as effective",
  "Constant"
)
stargazer_par_tex <- function(x, type = "latex", lab = c(15, 16)) {
  stargazer(
    x,
    omit = "Constant",
    covariate.labels = covars_names_par,
    label = ifelse(lab == 15, "tab:reg_prop15_long", "tab:reg_prop16_long"),
    dep.var.labels = ifelse(
      lab == 15,
      "Support Proposition 15",
      "Support Proposition 16"
    ),
    column.labels = c("Dem", "Rep", "Other"),
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
    out = ifelse(
      lab == 15,
      here("tab", "partisan_prop15_long.tex"),
      here("tab", "partisan_prop16_long.tex")
    )
  )
}

stargazer_par_tex(models_partisan %>% map("prop_15"), lab = 15)
stargazer_par_tex(models_partisan %>% map("prop_16"), lab = 16)

summary(models_partisan$dem$prop_15)
