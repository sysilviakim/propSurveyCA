source(here::here("R", "utilities.R"))
load(here("data", "tidy", "cal_survey_subset.Rda"))

# Setup ========================================================================
## Dependent variables
y <- c(prop_15 = "prop_15", prop_16 = "prop_16")

## Variable list
var_list <- list(
  demo = c("gender", "age", "race5", "educ", "income3"),
  geo = "ca_region",
  party = "party",
  poli_all = c("elec_int_state", "covid_response")
)
model_nested <- c("demo", "geo", "party", "poli_all")

## Survey Weight Design
sv_design <- svydesign(id = ~1, weights = ~weight_ca, data = cal_subset)

# Model with weights (education is categorical) ================================
model_weight <- list(
  demo_geo = y %>%
    map(
      ~ reg_form(.x, model_nested[1:2], cal_subset, sv_design, survey = TRUE)
    ),
  demo_geo_party = y %>%
    map(
      ~ reg_form(.x, model_nested[1:3], cal_subset, sv_design, survey = TRUE)
    ),
  all = y %>%
    map(
      ~ reg_form(.x, model_nested, cal_subset, sv_design, survey = TRUE)
    )
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

## to produce the tables with odd-ratios
prop15_odds <- stargazer_odds_tex(model_weight %>% map("prop_15"), lab = 15)
prop16_odds <- stargazer_odds_tex(model_weight %>% map("prop_16"), lab = 16)

# Subgroup differences? ========================================================
all_by_gen <- cal_subset %>%
  group_by(age_groups) %>%
  group_split() %>%
  `names<-`({
    .
  } %>% map(~ .x$age_groups[1]) %>% unlist()) %>%
  map(
    ~ y %>%
      map(
        function(x) {
          reg_form(
            x,
            vars = model_nested, data = .x,
            sv_design = svydesign(id = ~1, weights = ~weight_ca, data = .x),
            survey = TRUE
          )
        }
      )
  )

all_by_party <- cal_subset %>%
  group_by(party) %>%
  group_split() %>%
  `names<-`({
    .
  } %>% map(~ .x$party[1]) %>% unlist()) %>%
  map(
    ~ y %>%
      map(
        function(x) {
          reg_form(
            x,
            vars = model_nested[model_nested != "party"], data = .x,
            sv_design = svydesign(id = ~1, weights = ~weight_ca, data = .x),
            survey = TRUE
          )
        }
      )
  )
