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

# Models with no weights (education is categorical) ============================

model_no_wgt <- list(
  ## Benchmark test: no covariates
  null = y %>% map(~ reg_form(.x, "null")),
  demo_geo = y %>% map(~ reg_form(.x, model_nested[1:2])),
  demo_geo_party = y %>% map(~ reg_form(.x, model_nested[1:3])),
  all = y %>% map(~ reg_form(.x, model_nested))
)

## Stargazer accepts lists: pick out only prop X ones using purrr::map
stargazer_custom_tex(model_no_wgt %>% map("prop_15"))
stargazer_custom_tex(model_no_wgt %>% map("prop_16"))

# Model with weights (education is categorical) ================================
model_weight <- list(
  null = y %>% map(~ reg_form(.x, "null", survey = TRUE)),
  demo_geo = y %>% map(~ reg_form(.x, model_nested[1:2], survey = TRUE)),
  demo_geo_party = y %>% map(~ reg_form(.x, model_nested[1:3], survey = TRUE)),
  all = y %>% map(~ reg_form(.x, model_nested, survey = TRUE))
)


# to see the table
stargazer_custom(model_weight %>% map("prop_15"), lab = 15)
stargazer_custom(model_weight %>% map("prop_16"), lab = 16)

stargazer_custom_odds(model_weight %>% map("prop_15"), lab = 15)
stargazer_custom_odds(model_weight %>% map("prop_16"), lab = 16)
# to make the latex tables with log odds
prop15_table <- stargazer_custom_tex(model_weight %>% map("prop_15"), lab = 15)

prop16_table <- stargazer_custom_tex(model_weight %>% map("prop_16"), lab = 16)


# to produce the tables with odd-ratios 

prop15_odds <- stargazer_odds_tex(model_weight %>% map("prop_15"), lab = 15)


prop16_odds <- stargazer_odds_tex(model_weight %>% map("prop_16"), lab = 16)
# Model with no weights (education is continuous) ==============================
model_no_wgt_contedu <- list(
  null = y %>% map(~ reg_form(.x, "null")),
  demo_only = y %>% map(~ reg_form(.x, "demo2")),
  demo_geo = y %>% map(~ reg_form(.x, c("demo2", "geo"))),
  demo_pid = y %>% map(~ reg_form(.x, c("demo2", "pid7"))),
  demo_geo_pid3 = y %>% map(~ reg_form(.x, c("demo2", "geo", "pid3"))),
  demo_geo_pid7 = y %>%
    map(~ reg_form(.x, c("demo", "geo", "pid7"))),
  add_elec_int = y %>%
    map(~ reg_form(.x, c("demo2", "geo", "pid7", "poli1"))),
  add_covid_resp = y %>%
    map(~ reg_form(.x, c("demo2", "geo", "pid7", "poli2"))),
  add_all_poli = y %>%
    map(~ reg_form(.x, c("demo2", "geo", "pid7", "poli_all")))
)

# Model with weights (education is continuous) =================================
model_weight_contedu <- list(
  null = y %>% map(~ reg_form(.x, "null", survey = TRUE)),
  demo_only = y %>% map(~ reg_form(.x, "demo2", survey = TRUE)),
  demo_geo = y %>% map(~ reg_form(.x, c("demo2", "geo"), survey = TRUE)),
  demo_pid = y %>% map(~ reg_form(.x, c("demo2", "pid7"), survey = TRUE)),
  demo_geo_pid = y %>%
    map(~ reg_form(.x, c("demo2", "geo", "pid7"), survey = TRUE)),
  add_elec_int = y %>%
    map(~ reg_form(.x, c("demo2", "geo", "pid7", "poli1"), survey = TRUE)),
  add_covid_resp = y %>%
    map(~ reg_form(.x, c("demo2", "geo", "pid7", "poli2"), survey = TRUE)),
  add_all_poli = y %>%
    map(~ reg_form(.x, c("demo2", "geo", "pid7", "poli_all"), survey = TRUE))
)

stargazer_custom(model_weight_contedu %>% map("prop_15"))
stargazer_custom(model_weight_contedu %>% map("prop_16"))
