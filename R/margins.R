source(here::here("R", "05_reg_prelim.R"))

### margins -- use purrr map and margins_summary instead -----------
margins_summary(model_weight$all$prop_16, data = cal_subset, design = sv_design)
model_weight$all %>% 
  map(~ margins_summary(.x, data = cal_subset, design = sv_design))

## doesn't recognize survey.design object
mapply(margins_summary, model = model_weight$all, data = cal_subset,
       design = sv_design)

# Clean data ===================================================================
## dropping unused levels so margins works
cal_subset <- cal_subset %>%
  droplevels()

cal_survey$party <- ifelse(
  cal_survey$pid3 != 1 & cal_survey$pid3 != 2, 3, cal_survey$pid3
)

# Regressions: ols and glm =====================================================
ols <-lm(as.numeric(prop_15) ~ gender + age + race5 + educ + income3 + ca_region + 
           party + elec_int_state + covid_response,
         data = cal_subset, weight = weight_ca)
glm <- glm(
  prop_15 ~ gender + age + race5 + educ + income3 + ca_region + party +
    elec_int_state + covid_response,
  data = cal_subset, weight = weight_ca,
  family = "quasibinomial"
)
ols_16 <- lm(as.numeric(prop_16) ~ gender + age + race5 + educ + income3 + ca_region + 
                  party + elec_int_state + covid_response,
                data = cal_subset, weight = weight_ca)
glm_16 <- glm(
  prop_16 ~ gender + age + race5 + educ + income3 + ca_region + party +
    elec_int_state + covid_response,
  data = cal_subset, weight = weight_ca, family = "quasibinomial"
)
summary(ols)

### dropping unused levels so margins works
cal_survey$income3 <- droplevels(cal_survey$income3)
cal_subset$elec_int_state <- droplevels(cal_subset$elec_int_state)
cal_subset$covid_response <- droplevels(cal_subset$covid_response)


cal_survey$party <- ifelse(
  cal_survey$pid3 != 1 & cal_survey$pid3 != 2, 3, cal_survey$pid3)




## regressions - ols and glm
ols <- lm(as.numeric(prop_15) ~ unlist(list_vars_num), 
               data = cal_survey, weights = weight_ca)
glm <- glm(prop_15 ~ gender + age + race5 + educ + income3 + ca_region + party +
             elec_int_state + covid_response, 
              data = cal_subset, weight = weight_ca, family = 
             "quasibinomial")
ols_16 <- lm(as.numeric(prop_16) ~ as.numeric(gender) + as.numeric(age) + 
               as.numeric(race5) + as.numeric(educ) + as.numeric(income3) + 
               as.numeric(ca_region) + as.numeric(party), 
          data = cal_survey, weights = weight_ca)
glm_16 <- glm(prop_16 ~ gender + age + race5 + educ + income3 + ca_region + party +
             elec_int_state + covid_response, 
           data = cal_subset, weight = weight_ca, family = 
             "quasibinomial")
summary(glm)
summary(ols)
### margins 
m_glm <- margins(glm)

# Margins calculations =========================================================
m_glm_15 <- m_glm %>% summary() %>% as.data.frame()
m_glm_16 <- margins(glm_16) %>% summary() %>% as.data.frame()

margins(glm)

# Compare and export ===========================================================
stargazer(ols, type = "latex", covariate.labels = c(
  "Gender: Male", "Age", "Race: Black", "Race: Hispanic",
  "Race: Asian", "Race: Other", "Education: HS",
  "Education: Some College", "Education: 2-yr",
  "Education: 4-yr", "Education: Post-grad",
  "Income: 50-100k", "Income: 100k+",
  "Income: Prefer not to say",
  "CA Region: Central Valley/Inland", "CA Region: Coastal",
  "CA Region: LA",
  "CA Region: Southern California (non-LA)",
  "Party: Rep", "Party: Other",
  "Election Integrity: Somewhat confident",
  "Election Integrity: Not too confident",
  "Electoral Integrity: Not at all confident",
  "Electoral Integrity: Don't know",
  "COVID Response: Less effective than others",
  "COVID Response: About as effective",
  "Constant"
), dep.var.labels = "Proposition 15", 
          out = "ols.tex")

stargazer(ols_16, type = "latex", covariate.labels = c("Gender: Male", "Age", 
                                                "Race: Black", "Race: Hispanic",
  "Race: Asian", "Race: Other", "Education: HS",
  "Education: Some College", "Education: 2-yr",
  "Education: 4-yr", "Education: Post-grad",
  "Income: 50-100k", "Income: 100k+",
  "Income: Prefer not to say",
  "CA Region: Central Valley/Inland", "CA Region: Coastal",
  "CA Region: LA",
  "CA Region: Southern California (non-LA)",
  "Party: Rep", "Party: Other",
  "Election Integrity: Somewhat confident",
  "Election Integrity: Not too confident",
  "Electoral Integrity: Not at all confident",
  "Electoral Integrity: Don't know",
  "COVID Response: Less effective than others",
  "COVID Response: About as effective",
  "Constant"), dep.var.labels = "Proposition 16", 
          out = "ols_16.tex")


stargazer(m_glm_15, summary = FALSE, type = "latex", out = here("tab",
"m_glm_15.tex"))

stargazer(m_glm_16, summary = FALSE, type = "latex", out = here("tab",
                                                                "m_glm_16.tex"))
## comparing


stargazer(ols, type = "text", out = "ols.tex")

ols_m <- margin(ols_16)
stargazer(ols_16, type = "text", out = "ols.tex")

export_summs(ols, model_weight$all$prop_15, type = "text")

export_summs(m_glm_16, type = "text")

## Plots to match the plot from Fisk article
glm_15_lat <- margins(glm, variables = "race5")

pdf(file = here("fig", "prop_15_ame.pdf"))




pdf(file = "prop_15_ame.pdf")

glm_15_lat$col[glm_15_lat$race5 == "Hispanic"] <- "red"
plot(glm_15_lat, xaxt = "n")
axis(1, at = seq(1, 4, 1), labels = c("Black", "Hispanic", "Asian", "Other"))
dev.off()

glm_16_lat <- margins(glm_16, variables = "race5")

pdf(file = here("fig", "prop_16_ame.pdf"))
plot(glm_16_lat, xaxt = "none")
axis(1, at = seq(1, 4, 1), labels = c("Black", "Hispanic", "Asian", "Other"))
dev.off()



