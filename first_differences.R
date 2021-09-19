### packages 
# install.packages("DescTools")

library(stats)
library(DescTools)
library(prediction)
library(margins)
library(gmodels)
library(MASS)

### data 
load(here("data", "tidy", "cal_survey_subset.Rda"))
load(here("data", "tidy", "cal_survey_raw.Rda"))
###
glm <- glm(prop_15 ~ gender + age + race5 + educ + income3 + ca_region + party +
             elec_int_state + covid_response, 
           data = cal_subset, weight = weight_ca, family = 
             "quasibinomial", x = TRUE)

glm_16 <- glm(prop_16 ~ gender + age + race5 + educ + income3 + ca_region + 
                party + elec_int_state + covid_response, 
              data = cal_subset, weight = weight_ca, family = 
                "quasibinomial", x = TRUE)

predicted15 <- prediction(glm, at=list(race5="Hispanic"))

predicted16 <- prediction(glm_16, at=list(race5="Hispanic"))
summary(predicted16)

summary(glm)

#### Proposition 16 
beta <- glm$coefficients
covmat.beta <- vcov(glm)
ndraws <- 1000
betadraw <- mvrnorm(ndraws, beta, covmat.beta)
baseline.x <- apply(glm$x, 2, min)


hypo.case.A0 <- baseline.x
simprob.A0 <- plogis(betadraw%*%hypo.case.A0)


hypo.case.B0 <- baseline.x
hypo.case.B0["race5Hispanic"] <- 1
simprob.B0 <- plogis(betadraw%*%hypo.case.B0)

simprob.diff0 <- simprob.B0 - simprob.A0
summary(hypo.case.B0)

### Proposition 16 
beta_16 <- glm_16$coefficients
covmat.beta_16 <- vcov(glm_16)
betadraw_16 <- mvrnorm(ndraws, beta_16, covmat.beta_16)
baseline.x_16 <- apply(glm_16$x, 2, min)


hypo.case.A1<- baseline.x_16
simprob.A1 <- plogis(betadraw_16%*%hypo.case.A1)


hypo.case.B1 <- baseline.x
hypo.case.B1["race5Hispanic"] <- 1
simprob.B1 <- plogis(betadraw%*%hypo.case.B1)

simprob.diff0 <- simprob.B1 - simprob.A1
summary(hypo.case.B1)
