source(here::here("R", "05_reg_prelim.R"))

### margins -- use purrr map and margins_summary instead -----------
# Clean data ===================================================================
## dropping unused levels so margins works
cal_subset <- cal_subset %>%
  droplevels()

cal_survey$party <- ifelse(
  cal_survey$pid3 != 1 & cal_survey$pid3 != 2, 3, cal_survey$pid3
)

# Regressions: ols and glm =====================================================
ols <- lm(
  prop_15 ~ gender + age + race5 + educ + income3 + ca_region + party,
  data = cal_survey, weights = weight_ca
)
glm <- glm(
  prop_15 ~ gender + age + race5 + educ + income3 + ca_region + party +
    elec_int_state + covid_response,
  data = cal_subset, weight = weight_ca,
  family = "quasibinomial"
)
ols_16 <- lm(
  prop_16 ~ gender + age + race5 + educ + income3 + ca_region + party,
  data = cal_survey, weights = weight_ca
)
glm_16 <- glm(
  prop_16 ~ gender + age + race5 + educ + income3 + ca_region + party +
    elec_int_state + covid_response,
  data = cal_subset, weight = weight_ca, family = "quasibinomial"
)
summary(glm)

### dropping unused levels so margins works
cal_survey$income3 <- droplevels(cal_survey$income3)
cal_subset$elec_int_state <- droplevels(cal_subset$elec_int_state)
cal_subset$covid_response <- droplevels(cal_subset$covid_response)


cal_survey$party <- ifelse(
  cal_survey$pid3 != 1 & cal_survey$pid3 != 2, 3, cal_survey$pid3)

## vars 
list_vars <- list(cal_survey$gender, cal_survey$age,cal_survey$race5, 
                  cal_survey$educ, cal_survey$income3, cal_survey$ca_region, 
                  cal_survey$party)
lapply(list_vars, as.numeric)
reg <- function(x, vars) {
 lm(as.formula(
        paste0(x, " ~ ", paste(list_vars_num[vars] %>% unlist(),
        collapse = " + "))
      ),
      design = sv_design,
      data = cal_survey,
    )
  }


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
m_glm <- margins(glm)
m_glm_16 <- margins(glm_16)


# Compare and export ===========================================================
stargazer(ols, type = "text", out = here("tab", "ols.tex"))
stargazer(ols_16, type = "text", out = here("tab", "ols.tex"))

export_summs(m_glm, type = "text")

## comparing


stargazer(ols, type = "text", out = "ols.tex")

ols_m <- margin(ols_16)
stargazer(ols_16, type = "text", out = "ols.tex")

export_summs(ols, model_weight$all$prop_15, type = "text")

export_summs(m_glm_16, type = "text")

## Plots to match the plot from Fisk article
glm_15_lat <- margins(glm, variables = "race5")

pdf(file = here("fig", "prop_15_ame.pdf"))

glm$coefficients

pdf(file = "prop_15_ame.pdf")
plot(glm_15_lat, xaxt = "n")
axis(1, at = seq(1, 4, 1), labels = c("Black", "Hispanic", "Asian", "Other"))
dev.off()

glm_16_lat <- margins(glm_16, variables = "race5")

pdf(file = here("fig", "prop_16_ame.pdf"))
plot(glm_16_lat, xaxt = "none")
axis(1, at = seq(1, 4, 1), labels = c("Black", "Hispanic", "Asian", "Other"))
dev.off()
