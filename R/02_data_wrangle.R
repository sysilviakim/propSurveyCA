source(here::here("R", "utilities.R"))
load(here("data", "tidy", "cal_survey_raw.Rda"))

# Restructure variables from int to factors ====================================
cal_survey <- cal_survey %>%
  mutate(
    gender = factor(
      gender,
      levels = c(1, 2),
      labels = c("F", "M")
    ),
    ## silent gen: 28-45, boomer: 46-64, gen x: 65-80, mil: 81-96, gen z: 97-'12
    birth_year = 2020 - age,
    age_groups = cut(
      age,
      breaks = c(0, 25, 41, 57, 76, 120),
      labels = c(
        "Gen Z (18-24)", "Milennial (25-40)",
        "Gen X (41-56)", "Boomer (57-75)", "Silent (75+)"
      )
    ),
    race = factor(
      race,
      levels = seq(8),
      labels = c(
        "White", "Black", "Hispanic", "Asian", "Native American",
        "Two or More", "Other", "Middle Eastern"
      )
    ),
    race4 = factor(
      race4,
      levels = seq(4),
      labels = c("White", "Black", "Hispanic", "Other")
    ),
    race5 = factor(
      race5,
      levels = seq(5),
      labels = c("White", "Black", "Hispanic", "Asian", "Other")
    ),
    # ordered?
    educ_cont = as.numeric(educ),
    educ = factor(
      educ,
      levels = seq(6),
      labels = c("No HS", "HS", "Some college", "2-yr", "4-yr", "Post-grad")
    ),
    ## Add more rough education categories
    ca_region = factor(
      ca_region,
      levels = seq(5),
      labels = c(
        "Bay area", "Central valley/inland", "Coastal", "LA", "SoCal (non-LA)"
      )
    ),
    social_media_use = factor(
      social_media_use,
      levels = c(1, 2, 3, 8),
      labels = c("Yes", "No", "Don't know", "Skipped")
    ),
    prop_15 = factor(
      prop_15,
      levels = c(1, 2, 3, 8, 9),
      labels = c("Yes", "No", "Didn't vote on it", "Skipped", "Not asked")
    ),
    prop_16 = factor(
      prop_16,
      levels = c(1, 2, 3, 8, 9),
      labels = c("Yes", "No", "Didn't vote on it", "Skipped", "Not asked")
    ),
    pid3 = factor(
      pid3,
      levels = seq(5),
      labels = c("Dem", "Rep", "Independent", "Other", "Not sure")
    ),
    ## Collapsing Party ID into Dem, Rep, or Other variable
    party = ifelse(
      pid3 != "Dem" & pid3 != "Rep", "Other", pid3
    ),
    pid7 = factor(
      pid7,
      levels = seq(8),
      labels = c(
        "Strong Dem", "Not strong Dem", "Lean Dem", "Independent",
        "Lean Rep", "Not strong Rep", "Strong Rep", "Not sure"
      )
    ),
    acs_own_rent = factor(
      acs_own_rent,
      levels = c(1, 2, 3, 8),
      labels = c("Own", "Rent", "Other", "Skipped")
    ),
    hispanic = factor(
      hispanic,
      levels = c(1, 2),
      labels = c("Yes", "No")
    ),
    income3 = factor(
      income3,
      levels = c(-1, 1, 2, 3, 4),
      labels = c(
        "No data", "Under 50k", "50-100k", "100k+", "Prefer not to say"
      )
    ),
    elec_int_state = factor(
      elec_int_state,
      levels = c(1, 2, 3, 4, 5, 8),
      labels = c(
        "Very confident",
        "Somewhat confident",
        "Not too confident",
        "Not at all confident",
        "Don't know",
        "Skipped"
      )
    ),
    covid_response = factor(
      covid_response,
      levels = c(1, 2, 3, 8),
      labels = c(
        "More effective than others",
        "Less effective than others",
        "About as effective",
        "Skipped"
      )
    )
  )

save(cal_survey, file = here("data", "tidy", "cal_survey_wrangled.Rda"))
