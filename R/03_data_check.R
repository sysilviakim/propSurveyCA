source(here::here("R", "utilities.R"))
load(here("data", "tidy", "cal_survey_wrangled.Rda"))

# Checking for NA values =======================================================

## Dependent variables
prop(cal_survey, "prop_15")
prop(cal_survey, "prop_16")
prop(cal_survey, c("prop_15", "prop_16"))

## Covariates
prop(cal_survey, "race5")


race <- length(cal_survey$race[cal_survey$race == "Hispanic"])/nrow(cal_survey)
race4 <- length(cal_survey$race4[cal_survey$race4 == "Hispanic"])/nrow(cal_survey)
race5 <- length(cal_survey$race5[cal_survey$race5 == "Hispanic"])/nrow(cal_survey)
hisp <- length(cal_survey$hispanic[cal_survey$hispanic == "Yes"])/nrow(cal_survey)

race1 <- list(race, race4, race5, hisp)
hisp_vars<- race1 %>% as.data.frame() %>% 
  rename("Race" = "X0.180489731437599","Race4" = "X0.180489731437599.1",
         "Race5" = "X0.180489731437599.2", "Hispanic" = "X0.225118483412322")
hisp_var_comp <- xtable(hisp_vars)
print.xtable(hisp_var_comp, file = "hisp_var_comp.tex")


# race <- prop(cal_survey, race == "Hispanic")
# race4 <- prop(cal_survey, race4 == "Hispanic")
# race5 <- prop(cal_survey, race5 == "Hispanic")
# hispanic <- prop(cal_survey, hispanic == "Yes")
# 
# hisp_var <- list(race, race4, race5, hispanic)
# hispanic_comp <- as.data.frame(hisp_var)
# print.xtable(xtable(hispanic_comp), file = "hispanic_comp.txt")

# xtable(dfSummary(cal_survey[,1:18]))
# xtable(dfSummary(cal_survey[,1:18]))


# Comparing `race` and `party` variable ========================================

party_race_comp <- xtable(table(cal_survey$party, cal_survey$race5))
print.xtable(party_race_comp, file = "party_race_comp.tex")

# Making the descriptive table in Appendix =====================================

# still tweaking this to make it look cleaner
dput(names(cal_survey))

myVars <- c("prop_15", "prop_16", "age", "gender", "race5", "educ", "income3", 
            "ca_region", "pid3", "elec_int_state", "covid_response")
catVars <- c("prop_15", "prop_16","gender", "race5", "educ", "income3", 
             "ca_region", "pid3", "elec_int_state", "covid_response")
desc_table <- CreateTableOne(vars = myVars, data = cal_survey, factorVars = catVars)
summ_table <- print(desc_table, printToggle = FALSE, noSpaces = TRUE)
desc_summ_table <- kbl(summ_table, booktabs = TRUE, format = "latex", longtable = TRUE)
save_kable(desc_summ_table, file = "desc_summ_table.tex")


######################## Age Table #############################################
#min (18), qrt1 (39), med (56), qt3 (65), max (90)
#gen cut offs by min, max age 
#silent gen: 1928-45, boomers: 1946-64, gen x: 1965-80, mil: 1981-96,genz: 97-'12

cal_survey$age_groups <- findInterval(cal_survey$age, c(18,25,41,57,76))

age_15<- tabyl(dat = cal_survey, prop_15, age_groups, type = "f") %>% 
  as.data.frame()
colnames(age_15) <- list("Vote", "Gen Z (18-24)", "Milennial (25-40)", 
                         "Gen X (41-56)", "Boomer (57-75)", "Silent (75+)")
age_15 <- age_15 %>% t()
colnames(age_15) <- list("Prop. 15 - Yes", "Prop. 15 - No", 
                         "Prop. 15 - Didn't Vote on it", 
                         "Prop. 15 - Not asked", "Prop. 15 - Skipped", 
                         "Prop. 15 - NA's") 
age_15 <- age_15[-1,] %>% t()

age_16<- tabyl(dat = cal_survey, prop_16, age_groups, type = "f") %>% 
  as.data.frame()
colnames(age_16) <- list("Vote", "Gen Z (18-24)", "Milennial (25-40)", 
                         "Gen X (41-56)", "Boomer (57-75)", "Silent (75+)")
age_16 <- age_16 %>% t()
colnames(age_16) <- list("Prop. 16 - Yes", "Prop. 16 - No", 
                         "Prop. 16 - Didn't Vote on it", 
                         "Prop. 16 - Not asked", "Prop. 16 - Skipped", 
                         "Prop. 16 - NA's") 
age_16 <- age_16[-1,] %>% t() 

table_age<- rbind(age_15, age_16) %>% as.data.frame()

age_table <- table_age[-c(3:6,9:12),]

age_list <- c("yes", "no", "yes", "no")
age_t <- cbind(age_list, age_table)

age_t$`Gen Z (18-24)` <- as.numeric(age_t$`Gen Z (18-24)`)
age_t$`Milennial (25-40)` <- as.numeric(age_t$`Milennial (25-40)`)
age_t$`Gen X (41-56)` <- as.numeric(age_t$`Gen X (41-56)`)
age_t$`Boomer (57-75)` <- as.numeric(age_t$`Boomer (57-75)`)
age_t$`Silent (75+)` <- as.numeric(age_t$`Silent (75+)`)

age_table_pct <- age_t %>% adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 2) %>% adorn_ns("front")

age_table_pct <- age_table_pct[,-1]

# 
# kbl(age_table, booktabs = TRUE, label = 
#       "Votes on Prop. 15 and 16 by Age Groups", 
#     format = "latex") %>% save_kable("age_table.tex")

print.xtable(xtable(age_table_pct, 
                    caption = "Vote on Prop. 15 and 16 by Age Groups", auto = TRUE),
             type = "latex", file = "age_table.tex", 
             getOption("xtable.booktabs", TRUE))
######################################### Demographic Vars #####################

### Gender and Race
gen_15<- tabyl(cal_survey, prop_15, gender) %>% as.data.frame()
colnames(gen_15)<- list("Vote", "Females", "Males")
gen_15 <- gen_15 %>% t()
colnames(gen_15) <- list("Prop. 15 - Yes", "Prop. 15 - No", 
                         "Prop. 15 - Didn't Vote on it", 
                         "Prop. 15 - Not asked", "Prop. 15 - Skipped", 
                         "Prop. 15 - NA's") 
gen_15 <- gen_15[-1,] %>% t()

gen_16<- tabyl(cal_survey, prop_16, gender) %>% as.data.frame()
colnames(gen_16)<- list("Vote", "Females", "Males")
gen_16<- gen_16 %>% t()
colnames(gen_16) <- list("Prop. 16 - Yes", "Prop. 16 - No", 
                         "Prop. 16 - Didn't Vote on it", 
                         "Prop. 16 - Not asked", "Prop. 16 - Skipped", 
                         "Prop. 16 - NA's") 
gen_16<- gen_16[-1,] %>% t()

gender_tab <- rbind(gen_15, gen_16)


race_15<- tabyl(cal_survey, prop_15, race5) %>% as.data.frame()
colnames(race_15)<- list("Vote", "White", "Black", "Hispanic", "Asian", "Other")
race_15 <- race_15 %>% t() 
colnames(race_15) <- list("Prop. 15 - Yes", "Prop. 15 - No", 
                          "Prop. 15 - Didn't Vote on it", 
                          "Prop. 15 - Not asked", "Prop. 15 - Skipped", 
                          "Prop. 15 - NA's") 
race_15 <- race_15[-1,] %>% t()

race_16<- tabyl(cal_survey, prop_16, race5) %>% as.data.frame()
colnames(race_16)<- list("Vote", "White", "Black", "Hispanic", "Asian", "Other")
race_16 <- race_16 %>% t()
colnames(race_16) <- list("Prop. 16 - Yes", "Prop. 16 - No", 
                          "Prop. 16 - Didn't Vote on it", 
                          "Prop. 16 - Not asked", "Prop. 16 - Skipped", 
                          "Prop. 16 - NA's") 
race_16<- race_16[-1,] %>% t()

race_tab<- rbind(race_15, race_16)


combined_vars<- cbind(gender_tab, race_tab)
colnames(combined_vars)<- list("Gender: Females", "Gender: Males", "Race: White", 
                               "Race: Black", "Race: Hispanic", "Race: Asian", 
                               "Race: Other")
combined_vars<- combined_vars %>% t()
colnames(combined_vars) <- list("Prop. 15 - Yes", "Prop. 15 - No", 
                                "Prop. 15 - Didn't Vote on it", 
                                "Prop. 15 - Not asked", "Prop. 15 - Skipped", 
                                "Prop. 15 - NA's", "Prop. 16 - Yes", "Prop. 16 - No", 
                                "Prop. 16 - Didn't Vote on it", 
                                "Prop. 16 - Not asked", "Prop. 16 - Skipped", 
                                "Prop. 16 - NA's") 
combined_vars <- combined_vars %>% t() %>% as.data.frame()

kbl(combined_vars, booktabs= TRUE, label = 
      "Responses on Prop. 15 and 16 by Gender and Race", format = "latex") %>%
  save_kable("combined_dem_vars.tex")

## Version with Prop 15/16 being just Yes and No 
combined_dem <- combined_vars[-c(3:6,9:12),]

combined_list <- c("yes", "no", "yes", "no")
combined <- cbind(combined_list, combined_dem)

combined$`Gender: Females` <- as.numeric(combined$`Gender: Females`)
combined$`Gender: Males`<- as.numeric(combined$`Gender: Males`)
combined$`Race: White` <- as.numeric(combined$`Race: White`)
combined$`Race: Black` <- as.numeric(combined$`Race: Black`)
combined$`Race: Hispanic` <- as.numeric(combined$`Race: Hispanic`)
combined$`Race: Asian` <- as.numeric(combined$`Race: Asian`)
combined$`Race: Other` <- as.numeric(combined$`Race: Other`)

combined_dem_vars <- combined %>% adorn_percentages("col") %>% 
  adorn_pct_formatting(digits = 2) %>% adorn_ns(position = "front")

combined_dem_vars <- combined_dem_vars[,-1]


# # kbl(combined_dem, booktabs= TRUE, label = 
#       "Votes on Prop. 15 and 16 by Gender and Race", format = "latex") %>%
#   save_kable("combined_dem.tex")

print.xtable(xtable(combined_dem_vars, 
          caption ="Votes on Prop. 15 and 16 by Gender and Race", auto = TRUE),
        type = "latex", file = "combined_dem.tex", getOption("xtable.booktabs",
                                                                  TRUE))
###################### Political Vars #########################################
# Party ID 
party_id<- tabyl(cal_survey, prop_15, pid3) %>% as.data.frame()
colnames(party_id)<- list("Vote", "Dem", "Rep", "Ind", "Other", "Not Sure")
party_id <- party_id %>% t() 
colnames(party_id) <- list("Prop. 15 - Yes", "Prop. 15 - No", 
                           "Prop. 15 - Didn't Vote on it", 
                           "Prop. 15 - Not asked", "Prop. 15 - Skipped", 
                           "Prop. 15 - NA's") 
party_id<- party_id[-1,] %>% t()
party_id_16 <- tabyl(cal_survey, prop_16, pid3) %>% as.data.frame()
colnames(party_id_16) <- list("Vote", "Dem", "Rep", "Ind", "Other", "Not Sure")
party_id_16 <- party_id_16 %>% t()
colnames(party_id_16)<- list("Prop. 16 - Yes", "Prop. 16 - No", 
                             "Prop. 16 - Didn't Vote on it", 
                             "Prop. 16 - Not asked", "Prop. 16 - Skipped", 
                             "Prop. 16 - NA's") 
party_id_16<- party_id_16[-1,] %>% t()
party_id_both <- rbind(party_id, party_id_16) %>% as.data.frame()

### all responses, including don't know, didn't vote etc
kbl(party_id_both, booktabs = TRUE, label = 
      "Responses on Prop. 15 and 16 by Party ID", format = "latex") %>% 
  save_kable("partyid_table.tex")


## Version with Prop 15/16 being just Yes and No + Percentages
party_table <- party_id_both[-c(3:6,9:12),]


# kbl(party_table, booktabs = TRUE, label = opts_chunk$get('label_party'), format = "html") %>% 
#   save_kable("party_table_coll.tex")



# adding percentages 
party_line <- c("yes", "no", "yes", "no")
party_table <- cbind(party_line, party_table)
party_table$Dem <- party_table$Dem %>% as.numeric()
party_table$Rep <- party_table$Rep %>% as.numeric()
party_table$Ind <- party_table$Ind %>% as.numeric()
party_table$`Not Sure` <- party_table$`Not Sure` %>% as.numeric()
party_table$Other <- party_table$Other %>% as.numeric()


party_tbl <- party_table %>% adorn_percentages("col") %>% 
  adorn_pct_formatting(digits = 2) %>% adorn_ns(position = "front")

party_tbl<- party_tbl[,-1]


print.xtable(xtable(party_tbl, caption = "Votes on Prop. 15 and 16 by Party ID", 
                    auto = TRUE), type = "latex", file = "party_table.tex",
             booktabs = getOption("xtable.booktabs", TRUE))

