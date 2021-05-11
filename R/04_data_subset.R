source(here::here("R", "utilities.R"))
load(here("data", "tidy", "cal_survey_wrangled.Rda"))

## Subset data so that no missing variables occur for both y-variables
cal_subset <- cal_survey %>%
  mutate(
    prop_15 = case_when(
      prop_15 == "Yes" ~ 1,
      prop_15 == "No" ~ 0,
    ),
    prop_16 = case_when(
      prop_16 == "Yes" ~ 1,
      prop_16 == "No" ~ 0,
    )
  ) %>%
  filter(!is.na(prop_15) & !is.na(prop_16)) %>%
  mutate(
    prop_15 = factor(prop_15, levels = c(0, 1), labels = c("No", "Yes")),
    prop_16 = factor(prop_16, levels = c(0, 1), labels = c("No", "Yes"))
  )

save(cal_subset, file = here("data", "tidy", "cal_survey_subset.Rda"))
