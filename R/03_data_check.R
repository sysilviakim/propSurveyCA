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
pretty_condprob(cal_survey, "prop_15", "Yes", "race5", "Latino")
pretty_condprob(cal_survey, "prop_15", "Yes", "race5", "White")
pretty_condprob(cal_survey, "prop_16", "Yes", "race5", "Latino")
pretty_condprob(cal_survey, "prop_16", "Yes", "race5", "White")

## Partisan distribution
prop(cal_survey, "race5")
pretty_condprob(cal_survey, "pid3", "Dem", "race5", "Latino")
pretty_condprob(cal_survey, "pid3", "Dem", "race5", "White")
pretty_condprob(cal_survey, "pid3", "Dem", "race5", "Asian")
pretty_condprob(cal_survey, "pid3", "Dem", "race5", "Black")

pretty_condprob(cal_survey, "pid3", "Rep", "race5", "Latino")
pretty_condprob(cal_survey, "pid3", "Rep", "race5", "White")

## Covariates
prop(cal_survey, "race5")

# Weighted Tables 1 and 2 ======================================================

## Prepping for weighting ======================================================
### Define Weight
svy_design_tab <- svydesign(~1, data = cal_survey, weights = ~weight_ca)

## Define named vector for purrr::map
xvar <- c(age = "age_groups", gen = "gender", race = "race5", party = "pid3")

## Table 1: Proportion of "Yes" Responses, Weighted ============================
out_list1 <- xvar %>%
  imap(
    ~ {
      form_15 <- as.formula(paste0("~ prop_15 + ", .x))
      freq_15 <- svytable(form_15, design = svy_design_tab)
      prop_15 <- round(prop.table(freq_15, margin = 2), digits = 3)
      form_16 <- as.formula(paste0("~ prop_16 + ", .x))
      freq_16 <- svytable(form_16, design = svy_design_tab)
      prop_16 <- round(prop.table(freq_16, margin = 2), digits = 3)
      return(
        list(
          prop_15 = list(freq = freq_15, prop = prop_15),
          prop_16 = list(freq = freq_16, prop = prop_16)
        )
      )
    }
  )

out_tab1 <- c(15, 16) %>%
  map(
    function(x) {
      out_list1 %>%
        map(paste0("prop_", x)) %>%
        map("prop") %>%
        imap_dfr(
          ~ .x %>%
            t() %>%
            as.data.frame.matrix() %>%
            select(!!as.name(paste0("Prop. ", x)) := Yes) %>%
            mutate(
              Var = simple_cap(.y),
              !!as.name(paste0("Prop. ", x)) := formatC(
                !!as.name(paste0("Prop. ", x)) * 100,
                digits = 1, format = "f"
              )
            ) %>%
            rownames_to_column("Variables") %>%
            select(Var, Variables, everything())
        )
    }
  ) %>%
  Reduce(left_join, .) %>%
  mutate(
    Variables = case_when(
      Variables == "F" ~ "Female",
      Variables == "M" ~ "Male",
      TRUE ~ Variables
    ),
    Variables = paste("---", Variables)
  )

addtorow <- list(
  pos = unname(xvar[-length(xvar)]) %>%
    map_dbl(~ length(unique(cal_survey[[.x]]))) %>%
    c(0, .) %>%
    cumsum() %>%
    as.list()
)
addtorow$command <- paste(
  "\\midrule \n \\textbf{", c("Age", "Gender", "Race", "Party"), "} & \\\\\n"
)

print.xtable(
  xtable(
    out_tab1 %>% select(-Var),
    caption = "Proportion of ``Yes'' Response, Weighted",
    label = "tab:desc", align = "llrr"
  ),
  booktabs = TRUE, include.rownames = FALSE, add.to.row = addtorow,
  file = here("tab", "desc_weighted.tex"), 
  hline.after = c(-1, nrow(out_tab1)),
  sanitize.colnames.function = 
    function(x) paste("{\\textbf{", x, "}}", sep = "")
)

## Table 2: Party ID and Race, Weighted Proportions ============================
out_tab2 <- formatC(
  prop.table(
    svytable(~ race5 + party, design = svy_design_tab),
    margin = 1
  ) * 100,
  digits = 1, format = "f"
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
    caption = "Party ID and Race, Weighted Proportions",
    label = "tab:party_race_comp", align = "llrrrrr"
  ),
  booktabs = TRUE, include.rownames = FALSE,
  file = here("tab", "pid_race_proportion.tex"),
  sanitize.colnames.function = 
    function(x) paste("{\\textbf{", x, "}}", sep = "")
)
