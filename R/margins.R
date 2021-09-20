source(here::here("R", "utilities.R"))
load(here("data", "tidy", "cal_survey_subset.Rda"))
load(here("data", "tidy", "cal_survey_raw.Rda"))

### dropping unused levels so margins works
cal_subset$income3 <- droplevels(cal_subset$income3)
cal_subset$elec_int_state <- droplevels(cal_subset$elec_int_state)
cal_subset$covid_response <- droplevels(cal_subset$covid_response)


cal_survey$party <- ifelse(
  cal_survey$pid3 != 1 & cal_survey$pid3 != 2, 3, cal_survey$pid3)

## regressions - ols and glm
ols <- lm(prop_15 ~ gender + age + race5 + educ + income3 + ca_region + party, 
               data = cal_survey, weights = weight_ca)
glm <- glm(prop_15 ~ gender + age + race5 + educ + income3 + ca_region + party +
             elec_int_state + covid_response, 
              data = cal_subset, weight = weight_ca, family = 
             "quasibinomial")
ols_16 <- lm(prop_16 ~ gender + age + race5 + educ + income3 + ca_region + party, 
          data = cal_survey, weights = weight_ca)
glm_16 <- glm(prop_16 ~ gender + age + race5 + educ + income3 + ca_region + party +
             elec_int_state + covid_response, 
           data = cal_subset, weight = weight_ca, family = 
             "quasibinomial")
summary(glm)
### margins 
m_glm <- margins(glm)

m_glm_16 <- margins(glm_16)

## comparing


stargazer(ols, type = "text", out = "ols.tex")
stargazer(ols_16, type = "text", out = "ols.tex")

export_summs(m_glm, type = "text")

export_summs(m_glm_16, type = "text")

### Plots to match the plot from Fisk article 

glm_15_lat <- margins(glm, variables = "race5")


pdf(file = "prop_15_ame.pdf")
plot(glm_15_lat, xaxt = "n")
axis(1, at = seq(1, 4, 1),  labels = c("Black", "Hispanic", "Asian", "Other"))
dev.off()


glm_16_lat <- margins(glm_16, variables = "race5")


pdf(file="prop_16_ame.pdf")
plot(glm_16_lat, xaxt = "none")
axis(1, at = seq(1, 4, 1),  labels = c("Black", "Hispanic", "Asian", "Other"))
dev.off()