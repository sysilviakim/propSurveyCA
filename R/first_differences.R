source(here::here("R", "05_reg_prelim.R"))


######## using the nested model --------
beta <- model_weight$all$prop_15$coefficients
covmat.beta <- vcov(model_weight$all$prop_15)
ndraws <- 1000
betadraw <- mvrnorm(ndraws, beta, covmat.beta)
baseline.x <- apply(model_weight$all$prop_15$x, 2, min)


hypo.case_15 <- baseline.x
simprob.A0 <- plogis(betadraw%*%hypo.case_15)


hypo.case_15["race5Hispanic"] <- 1
simprob.B0 <- plogis(betadraw%*%hypo.case_15)

simprob.diff0 <- simprob.B0 - simprob.A0
summary(hypo.case_15)

### Prop. 16 using nested model 
beta_16 <- model_weight$all$prop_16$coefficients
covmat.beta_16 <- vcov(model_weight$all$prop_16)
betadraw_16 <- mvrnorm(ndraws, beta_16, covmat.beta_16)
baseline.x_16 <- apply(model_weight$all$prop_16$x, 2, min)


hypo.case_16<- baseline.x_16
simprob.A1 <- plogis(betadraw_16%*%hypo.case_16)


hypo.case_16["race5Hispanic"] <- 1
simprob.B1 <- plogis(betadraw_16%*%hypo.case_16)

simprob.diff0 <- simprob.B1 - simprob.A1
summary(hypo.case_16)

###### Using lists 

prop_coefs <- list(model_weight$all$prop_15$coefficients, 
                   model_weight$all$prop_16$coefficients)
prop_xs <- list(model_weight$all$prop_15$x,
                model_weight$all$prop_16$x)




### Original code --------------------------------------------------------------
# glm <- glm(prop_15 ~ gender + age + race5 + educ + income3 + ca_region + party +
#              elec_int_state + covid_response, 
#            data = cal_subset, weight = weight_ca, family = 
#              "quasibinomial", x = TRUE)
# 
# glm_16 <- glm(prop_16 ~ gender + age + race5 + educ + income3 + ca_region + 
#                 party + elec_int_state + covid_response, 
#               data = cal_subset, weight = weight_ca, family = 
#                 "quasibinomial", x = TRUE)
# 
# predicted15 <- prediction(glm, at=list(race5="Hispanic"))
# 
# predicted16 <- prediction(glm_16, at=list(race5="Hispanic"))
# summary(predicted16)
# 
# summary(glm)

# #### Proposition 15 --- 
# beta <- glm$coefficients
# covmat.beta <- vcov(glm)
# ndraws <- 1000
# betadraw <- mvrnorm(ndraws, beta, covmat.beta)
# baseline.x <- apply(glm$x, 2, min)
# 
# 
# hypo.case.A0 <- baseline.x
# simprob.A0 <- plogis(betadraw%*%hypo.case.A0)
# 
# 
# hypo.case.B0 <- baseline.x
# hypo.case.B0["race5Hispanic"] <- 1
# simprob.B0 <- plogis(betadraw%*%hypo.case.B0)
# 
# simprob.diff0 <- simprob.B0 - simprob.A0
# summary(hypo.case.B0)

# ### Proposition 16 
# beta_16 <- glm_16$coefficients
# covmat.beta_16 <- vcov(glm_16)
# betadraw_16 <- mvrnorm(ndraws, beta_16, covmat.beta_16)
# baseline.x_16 <- apply(glm_16$x, 2, min)
# 
# 
# hypo.case.A1<- baseline.x_16
# simprob.A1 <- plogis(betadraw_16%*%hypo.case.A1)
# 
# 
# hypo.case.B1 <- baseline.x
# hypo.case.B1["race5Hispanic"] <- 1
# simprob.B1 <- plogis(betadraw%*%hypo.case.B1)
# 
# simprob.diff0 <- simprob.B1 - simprob.A1
# summary(hypo.case.B1)
