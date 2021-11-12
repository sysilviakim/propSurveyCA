source(here::here("R", "05_reg_prelim.R"))

# Using the nested model =======================================================
### Prop. 15 using nested model
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

results.A0 <- append(
  quantile(simprob.A0, probs = c(0.5, 0.025, 0.975)),
  sd(simprob.A0)
)
results.B0 <- append(
  quantile(simprob.B0, probs = c(0.5, 0.025, 0.975)),
  sd(simprob.B0)
)
results.diff0 <- append(
  quantile(simprob.diff0, probs = c(0.5, 0.025, 0.975)),
  sd(simprob.diff0)
)

results.A1 <- append(
  quantile(simprob.A1, probs = c(0.5, 0.025, 0.975)),
  sd(simprob.A1)
)
results.B1 <- append(
  quantile(simprob.B1, probs = c(0.5, 0.025, 0.975)),
  sd(simprob.B1)
)
results.diff1 <- append(
  quantile(simprob.diff1, probs = c(0.5, 0.025, 0.975)),
  sd(simprob.diff1)
)

results.dd <- append(
  quantile(simprob_total, probs = c(0.5, 0.025, 0.975)),
  sd(simprob_total)
)

first.differences <- rbind(
  results.A0, results.B0, results.diff0, results.A1,
  results.B1, results.diff1, results.dd
)
colnames(first.differences) <- c("Median", "2.5%", "97.5%", "(se)")
rownames(first.differences) <- c(
  "Prop. 15: Non-Latino", "Prop. 15: Latino",
  "Prop. 15 Difference", "Prop. 16: Non-Latino",
  "Prop. 16: Latino", "Prop. 16 Difference",
  "Difference: Prop. 15 and Prop. 16"
)
xtable(
  round(first.differences, digits = 3), 
  file = "first_differences.tex"
)
###### Using lists

prop_coefs <- list(
  model_weight$all$prop_15$coefficients,
  model_weight$all$prop_16$coefficients
)
prop_xs <- list(
  model_weight$all$prop_15$x,
  model_weight$all$prop_16$x
)
