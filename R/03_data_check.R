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
gen_15 <- tabyl(cal_survey, prop_15, gender) %>% as.data.frame()
colnames(gen_15) <- list("Vote", "Females", "Males")
gen_15 <- gen_15 %>% t()
colnames(gen_15) <- col_labels$prop_15
gen_15 <- gen_15[-1, ] %>% t()

gen_16 <- tabyl(cal_survey, prop_16, gender) %>% as.data.frame()
colnames(gen_16) <- list("Vote", "Females", "Males")
gen_16 <- gen_16 %>% t()
colnames(gen_16) <- col_labels$prop_16
gen_16 <- gen_16[-1, ] %>% t()

gender_tab <- rbind(gen_15, gen_16)

## Demographic Vars: Race ======================================================
race_15 <- tabyl(cal_survey, prop_15, race5) %>% as.data.frame()
colnames(race_15) <- list("Vote", "White", "Black", "Hispanic", "Asian", "Other")
race_15 <- race_15 %>% t()
colnames(race_15) <- col_labels$prop_15
race_15 <- race_15[-1, ] %>% t()

race_16 <- tabyl(cal_survey, prop_16, race5) %>% as.data.frame()
colnames(race_16) <- list("Vote", "White", "Black", "Hispanic", "Asian", "Other")
race_16 <- race_16 %>% t()
colnames(race_16) <- col_labels$prop_16
race_16 <- race_16[-1, ] %>% t()

race_tab <- rbind(race_15, race_16)


combined_vars <- cbind(gender_tab, race_tab)
colnames(combined_vars) <- list(
  "Female", "Male", "White",
  "Black", "Hispanic", "Asian",
  "Other"
)
combined_vars <- combined_vars %>% t()
colnames(combined_vars) <- col_labels %>% unlist()
combined_vars <- combined_vars %>%
  t() %>%
  as.data.frame()

kbl(combined_vars,
  booktabs = TRUE,
  label = "Responses on Prop. 15 and 16 by Gender and Race", format = "latex"
) %>%
  save_kable(here("tab", "combined_dem_vars.tex"))

## Version with Prop 15/16 being just Yes and No
combined_dem <- combined_vars[-c(3:6, 9:12), ]

combined_list <- c("yes", "no", "yes", "no")
combined <- cbind(combined_list, combined_dem)

combined$Females <- as.numeric(combined$Females)
combined$Males <- as.numeric(combined$Males)
combined$White <- as.numeric(combined$White)
combined$Black <- as.numeric(combined$Black)
combined$Hispanic <- as.numeric(combined$Hispanic)
combined$Asian <- as.numeric(combined$Asian)
combined$Other <- as.numeric(combined$Other)

combined_dem_vars <- combined %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns(position = "front")

combined_dem_vars <- combined_dem_vars[, -1]

print.xtable(
  xtable(
    combined_dem_vars,
    caption = "Votes on Prop. 15 and 16 by Gender and Race", auto = TRUE
  ),
  type = "latex", file = here("tab", "combined_dem_mod.tex")
)

## Political Vars: Party ID ====================================================

party_id <- tabyl(cal_survey, prop_15, pid3) %>% as.data.frame()
colnames(party_id) <- list("Vote", "Dem", "Rep", "Ind", "Other", "Not Sure")
party_id <- party_id %>% t()
colnames(party_id) <- col_labels$prop_15
party_id <- party_id[-1, ] %>% t()
party_id_16 <- tabyl(cal_survey, prop_16, pid3) %>% as.data.frame()
colnames(party_id_16) <- list("Vote", "Dem", "Rep", "Ind", "Other", "Not Sure")
party_id_16 <- party_id_16 %>% t()
colnames(party_id_16) <- col_labels$prop_16
party_id_16 <- party_id_16[-1, ] %>% t()
party_id_both <- rbind(party_id, party_id_16) %>% as.data.frame()

### all responses, including don't know, didn't vote etc
kbl(
  party_id_both,
  booktabs = TRUE, label =
    "Responses on Prop. 15 and 16 by Party ID", format = "latex"
) %>%
  save_kable(here("tab", "partyid_table.tex"))


## Version with Prop 15/16 being just Yes and No + Percentages
party_table <- party_id_both[-c(3:6, 9:12), ]

# adding percentages
party_line <- c("yes", "no", "yes", "no")
party_table <- cbind(party_line, party_table)
party_table$Dem <- party_table$Dem %>% as.numeric()
party_table$Rep <- party_table$Rep %>% as.numeric()
party_table$Ind <- party_table$Ind %>% as.numeric()
party_table$`Not Sure` <- party_table$`Not Sure` %>% as.numeric()
party_table$Other <- party_table$Other %>% as.numeric()

party_tbl <- party_table %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns(position = "front")

party_tbl <- party_tbl[, -1]

print.xtable(
  xtable(
    party_tbl,
    caption = "Votes on Prop. 15 and 16 by Party ID",
    auto = TRUE
  ),
  type = "latex", file = here("tab", "party_table.tex"),
  booktabs = TRUE
)
