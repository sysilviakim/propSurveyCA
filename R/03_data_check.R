source(here::here("R", "utilities.R"))
load(here("data", "tidy", "cal_survey_wrangled.Rda"))

# Reused setups ================================================================
prop_labels <- c(prop_15 = 15, prop_16 = 16)
col_labels <- prop_labels %>%
  map(
    ~ c(
      paste0("Prop. ", .x, ": Yes"),
      paste0("Prop. ", .x, ": No")
    )
  )

# Checking for NA values =======================================================

## Dependent variables
prop(cal_survey, "prop_15")
prop(cal_survey, "prop_16")
prop(cal_survey, c("prop_15", "prop_16"))

## Rough conditional statements
pretty_condprob(cal_survey, "prop_15", "Yes", "race5", "Hispanic")
pretty_condprob(cal_survey, "prop_15", "Yes", "race5", "White")
pretty_condprob(cal_survey, "prop_16", "Yes", "race5", "Hispanic")
pretty_condprob(cal_survey, "prop_16", "Yes", "race5", "White")

## Partisan distribution
prop(cal_survey, "race5")
pretty_condprob(cal_survey, "pid3", "Dem", "race5", "Hispanic")
pretty_condprob(cal_survey, "pid3", "Dem", "race5", "White")
pretty_condprob(cal_survey, "pid3", "Dem", "race5", "Asian")
pretty_condprob(cal_survey, "pid3", "Dem", "race5", "Black")

pretty_condprob(cal_survey, "pid3", "Rep", "race5", "Hispanic")
pretty_condprob(cal_survey, "pid3", "Rep", "race5", "White")

## Covariates
prop(cal_survey, "race5")

# Weighted Table 1 =============================================================

## Prepping for weighting ======================================================
### Define Weight
svy_design_tab <- svydesign(~1, data = cal_survey, weights = ~weight_ca)

## Define named vector for purrr::map
xvar <- c(age = "age_groups", gen = "gender", race = "race5", party = "pid3")

## Age group variable
cal_survey$age_groups <- cut(
  cal_survey$age,
  breaks = c(0, 25, 41, 57, 76, 120),
  labels = c(
    "Gen Z (18-24)", "Milennial (25-40)",
    "Gen X (41-56)", "Boomer (57-75)", "Silent (75+)"
  )
)

## Table 1: Proportion of “Yes” Responses, Weighted
out_tab1 <- xvar %>%
  imap(
    ~ {
      form_15 <- as.formula(paste0("~ prop_15 + ", .x))
      freq_15 <- svytable(form_15, design = svy_design_tab)
      prop_15 <- round(prop.table(freq_15, margin = 2), digits = 3)
      form_16 <- as.formula(paste0("~ prop_16 + ", .x))
      freq_16 <- svytable(form_16, design = svy_design_tab)
      prop_16 <- round(prop.table(freq_15, margin = 2), digits = 3)
      return(
        list(
          freq_15 = freq_15, prop_15 = prop_15,
          freq_16 = freq_16, prop_16 = prop_16
        )
      )
    }
  )

## Table 2: Party ID and Race, Weighted Proportions
out_tab2 <- round(
  prop.table(
    svytable(~ race5 + party, design = svy_design_tab),
    margin = 1
  ) * 100,
  digits = 1
) %>%
  t() %>%
  as.data.frame.matrix() %>%
  rownames_to_column("Party") %>%
  mutate(
    Party = case_when(
      Party == "1" ~ "Dem",
      Party == "2" ~ "Rep",
      TRUE ~ Party
    )
  )

print.xtable(
  xtable(
    out_tab2,
    caption = "Party ID and Race, Weighted Proportions", digits = 1,
    label = "tab:party_race_comp"
  ),
  booktabs = TRUE, include.rownames = FALSE,
  file = here("tab", "pid_race_proportion.tex")
)
