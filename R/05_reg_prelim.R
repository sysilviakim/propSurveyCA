source(here::here("R", "utilities.R"))
load(here("data", "tidy", "cal_survey_subset.Rda"))

# Setup ========================================================================
## Dependent variables
y <- c(prop_15 = "prop_15", prop_16 = "prop_16")

## Variable list
var_list <- list(
  null = "1",
  demo = c("gender", "age", "race5", "educ", "income3"),
  demo2 = c("gender", "age", "race5", "educ_cont", "income3"), ## continuous
  geo = "ca_region",
  pid3 = "pid3",
  party = "party",
  poli1 = "elec_int_state",
  poli2 = "covid_response",
  poli_all = c("elec_int_state", "covid_response")
)
model_nested <- c("demo", "geo", "party", "poli_all")

## Survey Weight Design
sv_design <- svydesign(id = ~1, weights = ~weight_ca, data = cal_subset)

# Model with weights (education is categorical) ================================
model_weight <- list(
  # null = y %>% map(~ reg_form(.x, "null", survey = TRUE)),
  demo_geo = y %>% map(~ reg_form(.x, model_nested[1:2], survey = TRUE)),
  demo_geo_party = y %>% map(~ reg_form(.x, model_nested[1:3], survey = TRUE)),
  all = y %>% map(~ reg_form(.x, model_nested, survey = TRUE))
)

## to see the table from paper
stargazer_custom_omit(model_weight %>% map("prop_15"), lab = 15)
stargazer_custom_omit(model_weight %>% map("prop_16"), lab = 16)

## to see full table
stargazer_custom(model_weight %>% map("prop_15"), lab = 15)
stargazer_custom(model_weight %>% map("prop_16"), lab = 16)

## to see odds ratios
stargazer_custom_odds(model_weight %>% map("prop_15"), lab = 15)
stargazer_custom_odds(model_weight %>% map("prop_16"), lab = 16)

## to make the latex tables with log odds
prop15_table <- stargazer_custom_tex(model_weight %>% map("prop_15"), lab = 15)
prop16_table <- stargazer_custom_tex(model_weight %>% map("prop_16"), lab = 16)

prop_15_short <- stargazer_tex_omit(model_weight %>% map("prop_15"), lab = 15)
prop_16_short <- stargazer_tex_omit(model_weight %>% map("prop_16"), lab = 16)

# to produce the tables with odd-ratios
prop15_odds <- stargazer_odds_tex(model_weight %>% map("prop_15"), lab = 15)
prop16_odds <- stargazer_odds_tex(model_weight %>% map("prop_16"), lab = 16)

