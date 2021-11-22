source(here::here("R", "05_reg_prelim.R"))

### margins -- use purrr map and margins_summary instead -----------
margins_summary(model_weight$all$prop_16, data = cal_subset, design = sv_design)
model_weight$all %>%
  map(~ margins_summary(.x, data = cal_subset, design = sv_design))

## doesn't recognize survey.design object
mapply(margins_summary,
  model = model_weight$all, data = cal_subset,
  design = sv_design
)

# Clean data ===================================================================
## dropping unused levels so margins works
cal_subset <- cal_subset %>%
  droplevels()

cal_survey$party <- ifelse(
  cal_survey$pid3 != 1 & cal_survey$pid3 != 2, 3, cal_survey$pid3
)

# Regressions: lpm and glm =====================================================

# making prop_15 and 16 numeric
cal_subset <- cal_subset %>% mutate(
  prop_15_num = as.numeric(prop_15),
  prop_16_num = as.numeric(prop_16)
)

# dv
y_num <- c(prop_15_num = "prop_15_num", prop_16_num = "prop_16_num")

# list of models
var_list_lpm <- list(
  demo_geo = c("gender", "age", "race5", "educ", "income3", "ca_region"),
  party = c("gender", "age", "race5", "educ", "income3", "ca_region", "party"),
  all = c(
    "gender", "age", "race5", "educ", "income3", "ca_region", "party",
    "elec_int_state", "covid_response"
  )
)

# LPM
lpm <- list(
  demo_geo = y_num %>% map(~ reg_form_lpm(.x, vars = 1)),
  demo_geo_party = y_num %>% map(~ reg_form_lpm(.x, vars = 2)),
  all = y_num %>% map(~ reg_form_lpm(.x, vars = 3))
)

## GLM manual for marginal effects
# mod 1 - 15
demo_glm <- glm(
  prop_15 ~ gender + age + race5 + educ + income3 + ca_region,
  data = cal_subset, weight = weight_ca,
  family = "quasibinomial"
)

# 16

demo_glm_16 <- glm(
  prop_16 ~ gender + age + race5 + educ + income3 + ca_region,
  data = cal_subset, weight = weight_ca,
  family = "quasibinomial"
)

# mod 2 - 15

par_glm <- glm(
  prop_15 ~ gender + age + race5 + educ + income3 + ca_region + party +
    elec_int_state + covid_response,
  data = cal_subset, weight = weight_ca,
  family = "quasibinomial"
)

#- 16 
par_glm_16 <- glm(
  prop_16 ~ gender + age + race5 + educ + income3 + ca_region + party +
    elec_int_state + covid_response,
  data = cal_subset, weight = weight_ca,
  family = "quasibinomial"
)

# mod 3 -16
full_glm <- glm(
  prop_15 ~ gender + age + race5 + educ + income3 + ca_region + party +
    elec_int_state + covid_response,
  data = cal_subset, weight = weight_ca,
  family = "quasibinomial"
)
# 16

full_glm_16 <- glm(
  prop_16 ~ gender + age + race5 + educ + income3 + ca_region + party +
    elec_int_state + covid_response,
  data = cal_subset, weight = weight_ca, family = "quasibinomial"
)

### dropping unused levels so margins works
cal_survey$income3 <- droplevels(cal_survey$income3)
cal_subset$elec_int_state <- droplevels(cal_subset$elec_int_state)
cal_subset$covid_response <- droplevels(cal_subset$covid_response)

cal_survey$party <- ifelse(
  cal_survey$pid3 != 1 & cal_survey$pid3 != 2, 3, cal_survey$pid3
)

### margins
m_glm <- margins(glm)

# Margins calculations =========================================================
m_glm_15 <- m_glm %>%
  summary() %>%
  as.data.frame()
m_glm_16 <- margins(glm_16) %>%
  summary() %>%
  as.data.frame()

margins(glm)

# Compare and export ===========================================================
# prop 15
stargazer(
  lpm %>% map("prop_15_num"),
  covariate.labels = covars_names, dep.var.labels = "Proposition 15",
  out = here("tab", "lpm_15.tex")
)

# prop 16
stargazer(
  lpm %>% map("prop_16_num"),
  covariate.labels = covars_names, dep.var.labels = "Proposition 16",
  out = here("tab", "lpm_16.tex")
)

stargazer(
  m_glm_15,
  summary = FALSE, type = "latex",
  out = here("tab", "m_glm_15.tex")
)

stargazer(
  m_glm_16,
  summary = FALSE, type = "latex",
  out = here("tab", "m_glm_16.tex")
)

## comparing
stargazer(ols, type = "text", out = "ols.tex")

ols_m <- margin(ols_16)
stargazer(ols_16, type = "text", out = "ols.tex")

export_summs(ols, model_weight$all$prop_15, type = "text")
export_summs(m_glm_16, type = "text")

## Plots to match the plot from Fisk article
glm_15_lat <- margins(glm, variables = "race5")

pdf(file = here("fig", "prop_15_ame.pdf"))
glm_15_lat$col[glm_15_lat$race5 == "Latino"] <- "red"
plot(glm_15_lat, xaxt = "n")
axis(1, at = seq(1, 4, 1), labels = c("Black", "Latino", "Asian", "Other"))
dev.off()

glm_16_lat <- margins(glm_16, variables = "race5")

pdf(file = here("fig", "prop_16_ame.pdf"))
plot(glm_16_lat, xaxt = "none")
axis(1, at = seq(1, 4, 1), labels = c("Black", "Latino", "Asian", "Other"))
dev.off()
