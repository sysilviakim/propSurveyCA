source(here::here("R", "utilities.R"))
load(here("data", "tidy", "cal_survey_wrangled.Rda"))
library(flextable)
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

#### Weighted Table 1 & 2 ======================================================
# Weight
svy_design_tab <- svydesign(~1, data = cal_survey, 
                            weights = cal_survey$weight_ca)
# Age
cal_survey$age_groups = cut(
  cal_survey$age,
  breaks = c(0, 25, 41, 57, 76, 120),
  labels = c(
    "Gen Z (18-24)", "Milennial (25-40)",
    "Gen X (41-56)", "Boomer (57-75)", "Silent (75+)"
  )
)

age_freq_t <- svytable(~cal_survey$prop_15 + cal_survey$age_groups,
                       design = svy_design_tab)
age_prop <- round(prop.table(age_freq_t, margin = 2 ), digits = 3)


age_freq_t_16 <- svytable(~cal_survey$prop_16 + cal_survey$age_groups,
                          design = svy_design_tab)
age_p_16 <- round(prop.table(age_freq_t_16, margin = 2), digits = 3)


# Gender
gen_freq_t <- svytable(~cal_survey$prop_15 + cal_survey$gender,
                       design = svy_design_tab)
gen_prop <- round(prop.table(gen_freq_t, margin = 2), digits = 3)

gen_freq16 <- svytable(~cal_survey$prop_16 + cal_survey$gender,
                       design = svy_design_tab)
gen_prop16 <- round(prop.table(gen_freq16, margin = 2), digits = 3)
# Race

race_freq <- svytable(~cal_survey$prop_15 + cal_survey$race5,
                      design = svy_design_tab)
race_prop <- round(prop.table(race_freq, margin = 2), digits =3)

race_freq16 <- svytable(~cal_survey$prop_16 + cal_survey$race5,
                        design = svy_design_tab)
race_prop_16 <- round(prop.table(race_freq16, margin = 2), digits =3)

# Party 
party_freq <- svytable(~cal_survey$prop_15 + cal_survey$pid3,
                       design = svy_design_tab)
party_prop <- round(prop.table(party_freq, margin = 2), digits = 3)

party_freq16 <- svytable(~cal_survey$prop_16 + cal_survey$pid3,
                         design = svy_design_tab)
party_prop16 <- round(prop.table(party_freq16, margin = 2 ), digits = 3)

#### Table 2 ----- 
party_race_freq <- svytable(~ cal_survey$race5 + cal_survey$party, 
                            design = svy_design_tab)
party_race_prop <- prop.table(party_race_freq, margin = 1)


### Producing number by number for table 1 in Paper ============================

### Freq Age ------

# ### GENZ 
# gen_z_freq_yes <- age_freq[1]
# gen_z_freq_no <- age_freq[2]
# 
# ## MIL 
# mil_freq_yes  <- age_freq[6]
# mil_freq_no  <- age_freq[7]
# 
# ## GEN X 
# gen_x_freq_yes <- age_freq[11]
# gen_x_freq_no <- age_freq[12]
# 
# # BOOMER 
# boomer_freq_yes <-age_freq[16]
# boomer_freq_no <-age_freq[17]
# 
# ## SILENT
# silent_freq_yes <- age_freq[21]
# silent_freq_no <- age_freq[22]
# 
# stargazer(gen_z_freq_yes, out = "gen_z_freq_yes.tex")
# stargazer(gen_z_freq_no, out = "gen_z_freq_no.tex")
# stargazer(mil_freq_yes, out = "mil_freq_yes.tex")
# stargazer(mil_freq_no, out = "mil_freq_no.tex")
# stargazer(gen_x_freq_yes, out = "gen_x_freq_yes.tex")
# stargazer(gen_x_freq_no, out = "gen_x_freq_no.tex")
# stargazer(boomer_freq_yes, out = "boomer_freq_yes.tex")
# stargazer(boomer_freq_no, out = "boomer_freq_no.tex")
# stargazer(silent_freq_yes, out = "silent_freq_yes.tex")
# stargazer(silent_freq_no, out = "silent_freq_no.tex")

##### Proportion Age ----


### GENZ 
gen_z_freq_yes_p <- age_prop[1]
#gen_z_freq_no_p <- age_prop[2]

## MIL 
mil_freq_yes_p  <- age_prop[6]
#mil_freq_no_p  <- age_prop[7]

## GEN X 
gen_x_freq_yes_p <- age_prop[11]
#gen_x_freq_no_p <- age_prop[12]

# BOOMER 
boomer_freq_yes_p <-age_prop[16]
#boomer_freq_no_p <-age_prop[17]

## SILENT
silent_freq_yes_p <- age_prop[21]
#silent_freq_no_p <- age_prop[22]

View(age_prop)
stargazer(gen_z_freq_yes_p, out = "gen_z_yes_p.tex")
#stargazer(gen_z_freq_no_p, out = "gen_z_freq_no_p.tex")
stargazer(mil_freq_yes_p, out = "mil_yes_p.tex")
#stargazer(mil_freq_no_p, out = "mil_freq_no_p.tex")
stargazer(gen_x_freq_yes_p, out = "gen_x_yes_p.tex")
#stargazer(gen_x_freq_no_p, out = "gen_x_freq_no_p.tex")
stargazer(boomer_freq_yes_p, out = "boomer_yes_p.tex")
#stargazer(boomer_freq_no_p, out = "boomer_freq_no_p.tex")
stargazer(silent_freq_yes_p, out = "silent_yes_p.tex")
#stargazer(silent_freq_no_p, out = "silent_freq_no_p.tex")

## PROP 16 Age ------

#### Freq ------

# ### GENZ 
# gen_z_freq_yes16 <- age_f_16[1]
# gen_z_freq_no16 <- age_f_16[2]
# 
# ## MIL 
# mil_freq_yes16  <- age_f_16[6]
# mil_freq_no16  <- age_f_16[7]
# 
# ## GEN X 
# gen_x_freq_yes16 <- age_f_16[11]
# gen_x_freq_no16 <- age_f_16[12]
# 
# # BOOMER 
# boomer_freq_yes16 <-age_f_16[16]
# boomer_freq_no16 <-age_f_16[17]
# 
# ## SILENT
# silent_freq_yes16 <- age_f_16[21]
# silent_freq_no16 <- age_f_16[22]
# 
# stargazer(gen_z_freq_yes16, out = "gen_z_freq_yes16.tex")
# stargazer(gen_z_freq_no16, out = "gen_z_freq_no16.tex")
# stargazer(mil_freq_yes16, out = "mil_freq_yes16.tex")
# stargazer(mil_freq_no16, out = "mil_freq_no16.tex")
# stargazer(gen_x_freq_yes16, out = "gen_x_freq_yes16.tex")
# stargazer(gen_x_freq_no16, out = "gen_x_freq_no16.tex")
# stargazer(boomer_freq_yes16, out = "boomer_freq_yes16.tex")
# stargazer(boomer_freq_no16, out = "boomer_freq_no16.tex")
# stargazer(silent_freq_yes16, out = "silent_freq_yes16.tex")
# stargazer(silent_freq_no16, out = "silent_freq_no16.tex")

###### Proportion Age ------

### GENZ 
gen_z_freq_yes_p16 <- age_p_16[1]
#gen_z_freq_no_p16 <- age_p_16[2]

## MIL 
mil_freq_yes_p16  <- age_p_16[6]
#mil_freq_no_p16  <- age_p_16[7]

## GEN X 
gen_x_freq_yes_p16 <- age_p_16[11]
#gen_x_freq_no_p16 <- age_p_16[12]

# BOOMER 
boomer_freqyes_p16 <-age_p_16[16]
#boomer_freq_no_p16 <-age_p_16[17]

## SILENT
silent_freqyes_p16 <- age_p_16[21]
#silent_freq_no_p16 <- age_p_16[22]

View(age_prop)
stargazer(gen_z_freq_yes_p16, out = "gen_zyes_p16.tex")
#stargazer(gen_z_freq_no_p16, out = "gen_z_freq_no_p16.tex")
stargazer(mil_freq_yes_p16, out = "mil_yes_p16.tex")
#stargazer(mil_freq_no_p16, out = "mil_freq_no_p16.tex")
stargazer(gen_x_freq_yes_p16, out = "gen_x_yes_p16.tex")
#stargazer(gen_x_freq_no_p16, out = "gen_x_freq_no_p16.tex")
stargazer(boomer_freqyes_p16, out = "boomer_yes_p16.tex")
#stargazer(boomer_freq_no_p16, out = "boomer_freq_no_p16.tex")
stargazer(silent_freqyes_p16, out = "silent_yes_p16.tex")
#stargazer(silent_freq_no_p16, out = "silent_freq_no_p16.tex")

# RACE portion ------

### Prop 15 

### Freq Race -----
# ### White
# white_f_yes <- race_freq[1]
# white_f_no <- race_freq[2]
# 
# ## Black
# black_f_yes  <- race_freq[6]
# black_f_no  <- race_freq[7]
# 
# ## Hispanic 
# hisp_f_yes <- race_freq[11]
# hisp_f_no <- race_freq[12]
# 
# # Asian
# asian_f_yes <-race_freq[16]
# asian_f_no <-race_freq[17]
# 
# ## Other
# other_f_yes <- race_freq[21]
# other_f_no <- race_freq[22]
# 
# stargazer(white_f_yes, out = "white_f_yes.tex")
# stargazer(white_f_no, out = "white_f_no.tex")
# stargazer(black_f_yes, out = "black_f_yes.tex")
# stargazer(black_f_no, out = "black_f_no.tex")
# stargazer(hisp_f_yes, out = "hisp_f_yes.tex")
# stargazer(hisp_f_no, out = "hisp_f_no.tex")
# stargazer(asian_f_yes, out = "asian_f_yes.tex")
# stargazer(asian_f_no, out = "asian_f_no.tex")
# stargazer(other_f_yes, out = "other_f_yes.tex")
# stargazer(other_f_no, out = "other_f_no.tex")
#### Prop Race ----

### White
white_p_yes <- race_prop[1]
#white_p_no <- race_prop[2]

## Black
black_p_yes  <- race_prop[6]
#black_p_no  <- race_prop[7]

## Hispanic
hisp_p_yes <- race_prop[11]
#hisp_p_no <- race_prop[12]

# Asian 
asian_p_yes <-race_prop[16]
#asian_p_no <-race_prop[17]

## Other
other_p_yes <- race_prop[21]
#other_p_no <- race_prop[22]

stargazer(white_p_yes, out = "white_p_yes.tex")
#stargazer(white_p_no, out = "white_p_no.tex")
stargazer(black_p_yes, out = "black_p_yes.tex")
#stargazer(black_p_no, out = "black_p_no.tex")
stargazer(hisp_p_yes, out = "hisp_p_yes.tex")
#stargazer(hisp_p_no, out = "hisp_p_no.tex")
stargazer(asian_p_yes, out = "asian_p_yes.tex")
#stargazer(asian_p_no, out = "asian_p_no.tex")
stargazer(other_p_yes, out = "other_p_yes.tex")
#stargazer(other_p_no, out = "other_p_no.tex")


## PROP 16 Race ---- 

#### Freq Race 16 -----

# ### White
# white_f_yes16 <- race_freq16[1]
# white_f_no16 <- race_freq16[2]
# 
# ## Black
# black_f_yes16  <- race_freq16[6]
# black_f_no16  <- race_freq16[7]
# 
# ## Hispanic 
# hisp_f_yes16 <- race_freq16[11]
# hisp_f_no16 <- race_freq16[12]
# 
# # Asian
# asian_f_yes16 <-race_freq16[16]
# asian_f_no16 <-race_freq16[17]
# 
# ## Other
# other_f_yes16 <- race_freq16[21]
# other_f_no16 <- race_freq16[22]
# View(race_freq16)
# stargazer(white_f_yes16, out = "white_f_yes16.tex")
# stargazer(white_f_no16, out = "white_f_no16.tex")
# stargazer(black_f_yes16, out = "black_f_yes16.tex")
# stargazer(black_f_no16, out = "black_f_no16.tex")
# stargazer(hisp_f_yes16, out = "hisp_f_yes16.tex")
# stargazer(hisp_f_no16, out = "hisp_f_no16.tex")
# stargazer(asian_f_yes16, out = "asian_f_yes16.tex")
# stargazer(asian_f_no16, out = "asian_f_no16.tex")
# stargazer(other_f_yes16, out = "other_f_yes16.tex")
# stargazer(other_f_no16, out = "other_f_no16.tex")
##### Race prop 16 ------

### White
white_p_yes16 <- race_prop[1]
#white_p_no16 <- race_prop[2]

## Black
black_p_yes16  <- race_prop[6]
#black_p_no16  <- race_prop[7]

## Hispanic
hisp_p_yes16 <- race_prop[11]
#hisp_p_no16 <- race_prop[12]

# Asian 
asian_p_yes16 <-race_prop[16]
#asian_p_no16 <-race_prop[17]

## Other
other_p_yes16 <- race_prop[21]
#other_p_no16 <- race_prop[22]

stargazer(white_p_yes16, out = "white_p_yes16.tex")
#stargazer(white_p_no16, out = "white_p_no16.tex")
stargazer(black_p_yes16, out = "black_p_yes16.tex")
#stargazer(black_p_no16, out = "black_p_no16.tex")
stargazer(hisp_p_yes16, out = "hisp_p_yes16.tex")
#stargazer(hisp_p_no16, out = "hisp_p_no16.tex")
stargazer(asian_p_yes16, out = "asian_p_yes16.tex")
#stargazer(asian_p_no16, out = "asian_p_no16.tex")
stargazer(other_p_yes16, out = "other_p_yes16.tex")
#stargazer(other_p_no16, out = "other_p_no16.tex")


# GENDER portion ---- # Freq Gend ---- 
# female_freq_y <- gen_freq_t[1]
# female_freq_n <- gen_freq_t[2]
# male_freq_y <- gen_freq_t[6]
# male_freq_n <- gen_freq_t[7]
# 
# stargazer(female_freq_y, out = "female_freq_y.tex")
# stargazer(female_freq_n, out = "female_freq_n.tex")
# stargazer(male_freq_y, out = "male_freq_y.tex")
# stargazer(male_freq_n, out = "male_freq_n.tex")

### Gen Prop ------

female_prop_y <- gen_prop[1]
#female_prop_n <- gen_prop[2]
male_prop_y <- gen_prop[6]
#male_prop_n <- gen_prop[7]

stargazer(female_prop_y, out = "female_prop_y.tex")
#stargazer(female_prop_n, out = "female_prop_n.tex")
stargazer(male_prop_y, out = "male_prop_y.tex")
#stargazer(male_prop_n, out = "male_prop_n.tex")


### PROP 16  Gen ---- #### Freq Gen 16 ------
# female_freq_y16 <- gen_freq16[1]
# female_freq_n16 <- gen_freq16[2]
# male_freq_y16 <- gen_freq16[6]
# male_freq_n16 <- gen_freq16[7]
# 
# stargazer(female_freq_y16, out = "female_freq_y16.tex")
# stargazer(female_freq_n16, out = "female_freq_y16.tex")
# stargazer(male_freq_y16, out = "male_freq_y16.tex")
# stargazer(male_freq_n16, out = "male_freq_n16.tex")

### Gen Prop 16 -----

female_prop_y16 <- gen_prop16[1]
#female_prop_n16 <- gen_prop16[2]
male_prop_y16 <- gen_prop16[6]
#male_prop_n16 <- gen_prop16[7]

stargazer(female_prop_y16, out = "female_prop_y16.tex")
#stargazer(female_prop_n16, out = "female_prop_n16.tex")
stargazer(male_prop_y16, out = "male_prop_y16.tex")
#stargazer(male_prop_n16, out = "male_prop_n16.tex")

# PARTY portion------ ### Freq Party ----- 
# dem_y_freq <- party_freq[1]
# dem_n_freq <- party_freq[2]
# rep_y_freq <- party_freq[6]
# rep_n_freq <- party_freq[7]
# p_other_y_f <- party_freq[16]
# p_other_n_f <- party_freq[17]
# i_y <- party_freq[11]
# ns_y_15 <- party_freq[21]
# 
# stargazer(dem_y_freq, out = "dem_y_freq.tex")
# stargazer(dem_n_freq, out = "dem_n_freq.tex")
# stargazer(rep_y_freq, out = "rep_y_freq.tex")
# stargazer(rep_n_freq, out = "rep_n_freq.tex")
# stargazer(p_other_y_f, out = "p_other_y_f.tex")
# stargazer(p_other_n_f, out = "p_other_n_f.tex")
# stargazer(i_y, out = "i_y.tex")
# stargazer(ns_y_15, out = "ns_y_15.tex")


## Prop Party -----

dem_y_p <- party_prop[1]
#dem_n_p <- party_prop[2]
rep_y_p <- party_prop[6]
#rep_n_p <- party_prop[7]
p_ind_y_p <- party_prop[11]
p_other_y_p <- party_prop[16]
ns_y_p <- party_prop[21]
stargazer(dem_y_p, out = "dem_y_p.tex")
#stargazer(dem_n_p, out = "dem_n_p.tex")
stargazer(rep_y_p, out = "rep_y_p.tex")
#stargazer(rep_n_p, out = "rep_n_p.tex")
stargazer(p_ind_y_p, out = "p_ind_y_p.tex")
stargazer(p_other_y_p, out = "p_other_y_p.tex")
stargazer(ns_y_p, out = "ns_y_p.tex")
#summary(cal_survey$pid3)
### PROP 16  Party, Race ---- 

#### Party, Race Freq 16 -----
# dem_y_freq_16 <- party_freq16[1]
# dem_n_freq_16 <- party_freq16[2]
# rep_y_freq_16 <- party_freq16[6]
# rep_n_freq_16 <- party_freq16[7]
# i_other_y_f_16 <- party_freq16[11]
# i_other_n_f_16 <- party_freq16[12]
# p_other_y_f_16 <- party_freq16[16]
# p_other_n_f_16 <- party_freq16[17]
# ns_y <- party_freq16[21]
# 
# 
# stargazer(dem_y_freq_16, out = "dem_y_freq_16.tex")
# stargazer(dem_n_freq_16, out = "dem_n_freq_16.tex")
# stargazer(rep_y_freq_16, out = "rep_y_freq_16.tex")
# stargazer(rep_n_freq_16, out = "rep_n_freq_16.tex")
# stargazer(p_other_y_f_16, out = "p_other_y_f_16.tex")
# stargazer(p_other_n_f_16, out = "p_other_n_f_16.tex")
# stargazer(i_other_y_f_16, out = "i_other_y_f_16.tex")
# stargazer(ns_y, out = "ns_y_16.tex")

#### Prop 16 Party ------


dem_y_p_16 <- party_prop16[1]
#dem_n_p_16 <- party_prop16[2]
rep_y_p_16 <- party_prop16[6]
#rep_n_p_16 <- party_prop16[7]
p_ind_y_p_16 <- party_prop16[11]
p_other_y_p_16 <- party_prop16[16]
ns_y_16 <- party_prop16[21]
stargazer(dem_y_p_16, out = "dem_y_p_16.tex")
#stargazer(dem_n_p_16, out = "dem_n_p_16.tex")
stargazer(rep_y_p_16, out = "rep_y_p_16.tex")
#stargazer(rep_n_p_16, out = "rep_n_p_16.tex")
stargazer(p_ind_y_p_16, out = "p_ind_y_p_16.tex")
stargazer(p_other_y_p_16, out = "p_other_y_p_16.tex")
stargazer(ns_y_16, out = "ns_y_16.tex")
### Table 2 ---- 
# prop
white_dem_p <-party_race_prop[1]
black_dem_p <-party_race_prop[2]
hisp_dem_p <- party_race_prop[3]
asian_dem_p <- party_race_prop[4]
other_dem_p <- party_race_prop[5]

white_rep_p <-party_race_prop[6]
black_rep_p<-party_race_prop[7]
hisp_rep_p<-party_race_prop[8]
asian_rep_p<-party_race_prop[9]
other_rep_p<-party_race_prop[10]

white_other_p<-party_race_prop[11]
black_other_p<-party_race_prop[12]
hisp_other_p<-party_race_prop[13]
asian_other_p<-party_race_prop[14]
other_other_p<- party_race_prop[15]


stargazer(white_dem_p, out = "white_dem_p.tex")
stargazer(black_dem_p, out = "black_dem_p.tex")
stargazer(hisp_dem_p, out = "hisp_dem_p.tex")
stargazer(asian_dem_p, out = "asian_dem_p.tex")
stargazer(other_dem_p, out ="other_dem_p.tex")
stargazer(white_rep_p, out = "white_rep_p.tex")
stargazer(black_rep_p, out = "black_rep_p.tex")
stargazer(hisp_rep_p, out = "hisp_rep_p.tex")
stargazer(asian_rep_p, out = "asian_rep_p.tex")
stargazer(other_rep_p, out ="other_rep_p.tex")
stargazer(white_other_p, out = "white_other_p.tex")
stargazer(black_other_p, out = "black_other_p.tex")
stargazer(hisp_other_p, out = "hisp_other_p.tex")
stargazer(asian_other_p, out = "asian_other_p.tex")
stargazer(other_other_p, out ="other_other_p.tex")

party_race_freq <- svytable(~ cal_survey$party + cal_survey$prop_15, 
                            design = svy_design_tab)
View(party_race_freq)
