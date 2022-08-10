source(here::here("R", "05_reg_prelim.R"))
# Regressions: lpm and glm =====================================================

# making prop_15 and 16 numeric
cal_subset <- cal_subset %>%
  mutate(
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

# mod 3 -15
full_glm_15 <- glm(
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

cal_subset$elec_int_state <- droplevels(cal_subset$elec_int_state)
cal_subset$covid_response <- droplevels(cal_subset$covid_response)

# Margins calculations =========================================================
m_glm_15 <- margins(full_glm_15) %>%
  summary() %>%
  as.data.frame()
m_glm_16 <- margins(full_glm_16) %>%
  summary() %>%
  as.data.frame()
# Compare and export ===========================================================
# prop 15
stargazer(
  lpm %>% map("prop_15_num"),
  covariate.labels = covars_names, dep.var.labels = "Proposition 15",
  out = here("tab", "lpm_prop15.tex")
)

# prop 16
stargazer(
  lpm %>% map("prop_16_num"),
  covariate.labels = covars_names, dep.var.labels = "Proposition 16",
  out = here("tab", "lpm_prop16.tex")
)

stargazer(
  m_glm_15,
  summary = FALSE, type = "latex",
  out = here("tab", "mar_glm_15.tex")
)

stargazer(
  m_glm_16,
  summary = FALSE, type = "latex",
  out = here("tab", "mar_glm_16.tex")
)

## Plots to match the plot from Fisk article
race_mar_15 <- m_glm_15[grep("race", m_glm_15$factor),]

race_mar_15$factor <- ifelse(race_mar_15$factor == "race5Asian", "Asian",
                             ifelse(race_mar_15$factor == "race5Black",
                                    "Black", 
                                    ifelse(race_mar_15$factor == "race5Hispanic",
                                           "Hispanic",
                                           ifelse(race_mar_15$factor == "race5Other",
                                                  "Other", NA))))
race_mar_16 <- m_glm_16[grep("race", m_glm_16$factor),]
race_mar_16$factor <- ifelse(race_mar_16$factor == "race5Asian", "Asian",
                             ifelse(race_mar_16$factor == "race5Black",
                                    "Black", 
                                    ifelse(race_mar_16$factor == "race5Hispanic",
                                           "Hispanic",
                                           ifelse(race_mar_16$factor == "race5Other",
                                                  "Other", NA))))


mar_15_plot <- ggplot(race_mar_15, aes(x=factor, y=AME, color = factor)) +
  geom_point()+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .1) +
  labs(
    x = "Race",
    y = "Average Marginal Effects (95% C.I.)",
    color = "Race"
  ) +
  scale_colour_manual(
    values = c(
      "Black" = "gray24",
      "Hispanic" = "red1",
      "Asian" = "gray24",
      "Other" = "gray24"
    )
  ) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(
    labels =
      scales::number_format(accuracy = 0.01), oob = rescale_none
  ) +
  annotate("rect", fill = "lightgray", alpha = 0.4) +
  ggtitle(
    paste0(
      "Full Model, Prop. ", ifelse(grepl("15", y), 15, 16), " and Race"
    )
  )


pdf(file = here("fig", "prop_15_ame.pdf"))
glm_15_lat$col[glm_15_lat$race5 == "Latino"] <- "red"
plot(glm_15_lat, xaxt = "n")
axis(1, at = seq(1, 4, 1), labels = c("Black", "Latino", "Asian", "Other"))
dev.off()


mar_16_plot <- ggplot(race_mar_16, aes(x=factor, y=AME, color = factor)) +
  geom_point()+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .1) +
  labs(
    x = "Race",
    y = "Average Marginal Effects (95% C.I.)",
    color = "Race"
  ) +
  scale_colour_manual(
    values = c(
      "Black" = "gray24",
      "Hispanic" = "red1",
      "Asian" = "gray24",
      "Other" = "gray24"
    )
  ) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(
    labels =
      scales::number_format(accuracy = 0.01), oob = rescale_none
  ) +
  annotate("rect", fill = "lightgray", alpha = 0.4) +
  ggtitle(
    paste0(
      "Full Model, Prop. ", ifelse(grepl("15", y), 15, 16), " and Race"
    )
  )



pdf(file = here("fig", "mar_15_plot.pdf"))
print(mar_15_plot)
dev.off()


pdf(file = here("fig", "prop_16_ame.pdf"))
plot(glm_16_lat, xaxt = "none")
axis(1, at = seq(1, 4, 1), labels = c("Black", "Latino", "Asian", "Other"))

pdf(file = here("fig", "mar_16_plot.pdf"))
print(mar_16_plot)

dev.off()

### Power Analysis =============================================================
library(car)
anova(full_glm_15, type = "LRT")
eta_sq <- 13.71 /
  (13.71 + 2.07 + 84.30 + 45.85 + 13.14 + 32.7 + 374.25 + 116.75 + 29.56)
f_2 <- eta_sq / (1 - eta_sq)
pwr.f2.test(u = 4, v = 2201, f2 = f_2, sig.level = .05)