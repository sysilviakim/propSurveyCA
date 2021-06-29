
install.packages("huxtable")
library(huxtable)


### dropping unused levels so margins works
cal_subset$income3 <- droplevels(cal_subset$income3)
cal_subset$elec_int_state <- droplevels(cal_subset$elec_int_state)
cal_subset$covid_response <- droplevels(cal_subset$covid_response)

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

### margins 
m_glm <- margins(glm)

m_glm_16 <- margins(glm_16)

## comparing

xtable(export_summs(ols, m_glm, type = "latex"))

xtable(export_summs(ols_16, m_glm_16, type = "latex"))

