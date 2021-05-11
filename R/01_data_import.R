source(here::here("R", "utilities.R"))

# Data import, filter for CA voters, and rename variables ======================
cal_survey <- read.csv(
  here("data", "raw", "CALTECH_Post_Election_Survey_2020.csv")
) %>%
  filter(inputstate == 6) %>%
  select(
    # y-variables
    prop_15 = Q104a,
    prop_16 = Q104b,
    # Demographics
    age,
    gender,
    race, race4, race5, hispanic,
    educ,
    income3,
    # Geographics
    ca_region,
    ca_county = Q4,
    # Party ID
    pid3, pid7,
    # Political variables
    elec_int_state = Q32c,
    covid_response = Q103c,
    # Misc.
    social_media_use = Q36,
    acs_own_rent = acsownrent,
    everything()
  )

# Do not drop other variables
# Do not immediately use na.omit

save(cal_survey, file = here("data", "tidy", "cal_survey_raw.Rda"))
