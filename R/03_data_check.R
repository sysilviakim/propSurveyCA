source(here::here("R", "utilities.R"))
load(here("data", "tidy", "cal_survey_wrangled.Rda"))

# Reused setups ================================================================
prop_labels <- c(prop_15 = 15, prop_16 = 16)
col_labels <- prop_labels %>%
  map(
    ~ c(
      paste0("Prop. ", .x, ": Yes"),
      paste0("Prop. ", .x, ": No") # ,
      # paste0("Prop. ", .x, ": Didn't vote on it"),
      # paste0("Prop. ", .x, ": Skipped"),
      # paste0("Prop. ", .x, ": Not asked"),
      # paste0("Prop. ", .x, ": NA")
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

# Creating Descriptive Tables ==================================================

## Demographic Vars: Age =======================================================
temp <- prop_labels %>%
  map_dfr(
    ~ {
      tabyl(
        dat = cal_survey,
        !!as.name(paste0("prop_", .x)),
        age_groups,
        type = "f"
      ) %>%
        mutate(across(where(is.factor), as.character)) %>%
        adorn_percentages("col") %>%
        adorn_pct_formatting(digits = 2) %>%
        adorn_ns("front") %>%
        filter(grepl("Yes$|No$", !!as.name(paste0("prop_", .x)))) %>%
        rename(Vote = !!as.name(paste0("prop_", .x)))
    }
  ) %>%
  mutate(Vote = col_labels %>% unlist())

print.xtable(
  xtable(temp, caption = "By Age Groups", auto = TRUE),
  file = here("tab", "age_table.tex"),
  booktabs = TRUE,
  floating = FALSE,
  include.rownames = FALSE
)

## Demographic Vars: Gender ====================================================

gender <- prop_labels %>%
  map_dfr(
    ~ {
      tabyl(
        dat = cal_survey,
        !!as.name(paste0("prop_", .x)),
        gender,
        type = "f"
      ) %>%
        mutate(across(where(is.factor), as.character)) %>%
        adorn_percentages("col") %>%
        adorn_pct_formatting(digits = 2) %>%
        adorn_ns("front") %>%
        filter(grepl("Yes$|No$", !!as.name(paste0("prop_", .x)))) %>%
        rename(Vote = !!as.name(paste0("prop_", .x)), "Female" = "F",
               "Male" = "M")
    }
  ) %>%
  mutate(Vote = col_labels %>% unlist())

## Demographic Vars: Race ======================================================

race <- prop_labels %>%
  map_dfr(
    ~ {
      tabyl(
        dat = cal_survey,
        !!as.name(paste0("prop_", .x)),
        race5,
        type = "f"
      ) %>%
        mutate(across(where(is.factor), as.character)) %>%
        adorn_percentages("col") %>%
        adorn_pct_formatting(digits = 2) %>%
        adorn_ns("front") %>%
        filter(grepl("Yes$|No$", !!as.name(paste0("prop_", .x)))) %>%
        rename(Vote = !!as.name(paste0("prop_", .x)))
    }
  ) %>%
  mutate(Vote = col_labels %>% unlist())


### Combined Demographic Variables

combined_dem_vars <- merge(gender, race, by = "Vote")

print.xtable(
  xtable(
    combined_dem_vars,
    caption = "Votes on Prop. 15 and 16 by Gender and Race", auto = TRUE
  ),
  type = "latex", file = here("tab", "combined_dem_modified.tex"), booktabs = 
    getOption("xtable.booktabs", TRUE)
)

## Political Vars: Party ID ====================================================

party_tab <- prop_labels %>%
  map_dfr(
    ~ {
      tabyl(
        dat = cal_survey,
        !!as.name(paste0("prop_", .x)),
        pid3,
        type = "f"
      ) %>%
        mutate(across(where(is.factor), as.character)) %>%
        adorn_percentages("col") %>%
        adorn_pct_formatting(digits = 2) %>%
        adorn_ns("front") %>%
        filter(grepl("Yes$|No$", !!as.name(paste0("prop_", .x)))) %>%
        rename(Vote = !!as.name(paste0("prop_", .x)))
    }
  ) %>%
  mutate(Vote = col_labels %>% unlist())


print.xtable(
  xtable(
    party_tab,
    caption = "Votes on Prop. 15 and 16 by Party ID",
    auto = TRUE
  ),
  type = "latex", file = here("tab","party_table.tex"),
  booktabs = TRUE
)

