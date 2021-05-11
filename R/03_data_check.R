source(here::here("R", "utilities.R"))
load(here("data", "tidy", "cal_survey_wrangled.Rda"))

# Checking for NA values =======================================================

## Dependent variables
prop(cal_survey, "prop_15")
prop(cal_survey, "prop_16")
prop(cal_survey, c("prop_15", "prop_16"))

## Covariates
prop(cal_survey, "race5")

# Comparing `race` and `hispanic` variable =====================================
race <- prop(cal_survey, race == "Hispanic")
race4 <- prop(cal_survey, race4 == "Hispanic")
race5 <- prop(cal_survey, race5 == "Hispanic")
hispanic <- prop(cal_survey, hispanic == "Yes")

hisp_var <- list(race, race4, race5, hispanic)
hispanic_comp <- as.data.frame(hisp_var)
print.xtable(xtable(hispanic_comp), file = "hispanic_comp.txt")

xtable(dfSummary(cal_subset[,1:18]))
xtable(dfSummary(cal_survey[,1:18]))

# Comparing `race` and `party` variable ========================================
table(cal_subset$party, cal_subset$race5) %>% xtable()
