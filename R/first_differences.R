source(here::here("R", "05_reg_prelim.R"))


# Using the nested model =======================================================
beta <- model_weight$all$prop_15$coefficients
covmat.beta <- vcov(model_weight$all$prop_15)
ndraws <- 1000
betadraw <- mvrnorm(ndraws, beta, covmat.beta)
baseline.x <- apply(model_weight$all$prop_15$x, 2, min)


hypo.case_15 <- baseline.x
simprob.A0 <- plogis(betadraw %*% hypo.case_15)


hypo.case_15["race5Hispanic"] <- 1
simprob.B0 <- plogis(betadraw %*% hypo.case_15)

simprob.diff0 <- simprob.B0 - simprob.A0
summary(hypo.case_15)

### Prop. 16 using nested model
beta_16 <- model_weight$all$prop_16$coefficients
covmat.beta_16 <- vcov(model_weight$all$prop_16)
betadraw_16 <- mvrnorm(ndraws, beta_16, covmat.beta_16)
baseline.x_16 <- apply(model_weight$all$prop_16$x, 2, min)


hypo.case_16 <- baseline.x_16
simprob.A1 <- plogis(betadraw_16 %*% hypo.case_16)


hypo.case_16["race5Hispanic"] <- 1
simprob.B1 <- plogis(betadraw_16 %*% hypo.case_16)

simprob.diff1 <- simprob.B1 - simprob.A1
summary(hypo.case_16)

simprob_total <- simprob.diff1 - simprob.diff0

results.A0 <- append(quantile(simprob.A0, probs=c(0.5, 0.025, 0.975)), 
                     sd(simprob.A0))
results.B0 <- append(quantile(simprob.B0, probs=c(0.5, 0.025, 0.975)), 
                     sd(simprob.B0))
results.diff0 <- append(quantile(simprob.diff0, probs=c(0.5, 0.025, 0.975)), 
                        sd(simprob.diff0))

results.A1 <- append(quantile(simprob.A1, probs=c(0.5, 0.025, 0.975)), 
                     sd(simprob.A1))
results.B1 <- append(quantile(simprob.B1, probs=c(0.5, 0.025, 0.975)), 
                     sd(simprob.B1))
results.diff1 <- append(quantile(simprob.diff1, probs=c(0.5, 0.025, 0.975)), 
                        sd(simprob.diff1))

results.dd <- append(quantile(simprob_total, probs=c(0.5, 0.025, 0.975)), 
                     sd(simprob_total))

first.differences <- rbind(results.A0, results.B0, results.diff0, results.A1, 
                           results.B1, results.diff1, results.dd)
colnames(first.differences) <- c("Median", "2.5%", "97.5%", "(se)")
rownames(first.differences) <- c("Prop. 15: Non-Latino", "Prop. 15: Latino",
                                 "Prop. 15 Difference", "Prop. 16: Non-Latino",
                                 "Prop. 16: Latino", "Prop. 16 Difference",
                                 "Difference: Prop. 15 and Prop. 16")
print(round(first.differences, digits=3))
###### Using lists

prop_coefs <- list(
  model_weight$all$prop_15$coefficients,
  model_weight$all$prop_16$coefficients
)
prop_xs <- list(
  model_weight$all$prop_15$x,
  model_weight$all$prop_16$x
)




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
