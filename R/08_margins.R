source(here::here("R", "05_reg_prelim.R"))

# Prep for regressions =========================================================

# Making prop_15 and 16 numeric
cal_subset <- cal_subset %>%
  mutate(
    prop_15_num = as.numeric(prop_15),
    prop_16_num = as.numeric(prop_16)
  )

# dv
y_num <- c(prop_15_num = "prop_15_num", prop_16_num = "prop_16_num")

# List of models
var_list_lpm <- list(
  demo_geo = c("gender", "age", "race5", "educ", "income3", "ca_region"),
  party = c("gender", "age", "race5", "educ", "income3", "ca_region", "party"),
  all = c(
    "gender", "age", "race5", "educ", "income3", "ca_region", "party",
    "elec_int_state", "covid_response"
  )
)

# LPM ==========================================================================
lpm <- list(
  demo_geo = y_num %>% map(~ reg_form_lpm(.x, vars = 1)),
  demo_geo_party = y_num %>% map(~ reg_form_lpm(.x, vars = 2)),
  all = y_num %>% map(~ reg_form_lpm(.x, vars = 3))
)

## GLM manual for marginal effects =============================================
## (class "svyglm" does not work with margins)

## Model 1
demo_glm <- glm(
  prop_15 ~ gender + age + race5 + educ + income3 + ca_region,
  data = cal_subset, weight = weight_ca,
  family = "quasibinomial"
)
demo_glm_16 <- glm(
  prop_16 ~ gender + age + race5 + educ + income3 + ca_region,
  data = cal_subset, weight = weight_ca,
  family = "quasibinomial"
)

## Model 2
par_glm <- glm(
  prop_15 ~ gender + age + race5 + educ + income3 + ca_region + party +
    elec_int_state + covid_response,
  data = cal_subset, weight = weight_ca,
  family = "quasibinomial"
)
par_glm_16 <- glm(
  prop_16 ~ gender + age + race5 + educ + income3 + ca_region + party +
    elec_int_state + covid_response,
  data = cal_subset, weight = weight_ca,
  family = "quasibinomial"
)

## Model 3
full_glm_15 <- glm(
  prop_15 ~ gender + age + race5 + educ + income3 + ca_region + party +
    elec_int_state + covid_response,
  data = cal_subset, weight = weight_ca,
  family = "quasibinomial"
)
full_glm_16 <- glm(
  prop_16 ~ gender + age + race5 + educ + income3 + ca_region + party +
    elec_int_state + covid_response,
  data = cal_subset, weight = weight_ca,
  family = "quasibinomial"
)

## Dropping unused levels so margins works
cal_subset$elec_int_state <- droplevels(cal_subset$elec_int_state)
cal_subset$covid_response <- droplevels(cal_subset$covid_response)

# Margins calculations =========================================================
m_glm_15 <- margins(full_glm_15) %>%
  summary() %>%
  as.data.frame()
m_glm_16 <- margins(full_glm_16) %>%
  summary() %>%
  as.data.frame()

# Export LPM results ===========================================================
lpm_export <- function(x, y = 15) {
  stargazer(
    x,
    covariate.labels = covars_names,
    dep.var.labels = paste0("Proposition ", y),
    omit = "Constant",
    omit.stat = c("f", "ser"),
    model.numbers = TRUE,
    star.char = c("*", "**", "***"),
    star.cutoffs = c(.05, .01, .001),
    digits = 3,
    header = FALSE,
    type = "latex",
    no.space = TRUE,
    font.size = "footnotesize",
    float.env = "table",
    title = paste0(
      "Proposition ", y, " Linear Probability Models"
    ),
    out = here("tab", paste0("lpm_prop", y, ".tex"))
  )
}

lpm_export(lpm %>% map("prop_15_num"), y = 15)
lpm_export(lpm %>% map("prop_16_num"), y = 16)

# Export margins results =======================================================
mar_export <- function(x, y = 15) {
  print(
    left_join(x, as_tibble(covars_names, rownames = "factor")) %>%
      select(-factor) %>%
      rename(Variable = value, `s.e.` = SE, `Z-score` = z, `p-value` = p) %>%
      rowwise() %>%
      mutate(
        `95% CI` = paste0(
          "[", formatC(lower, digits = 3, format = "f"),
          ", ", formatC(upper, digits = 3, format = "f"), "]"
        )
      ) %>%
      ungroup() %>%
      select(-lower, -upper) %>%
      select(Variable, everything()) %>%
      xtable(digits = 3),
    file = here("tab", paste0("mar_glm_", y, ".tex")),
    include.rownames = FALSE,
    comment = FALSE,
    floating = FALSE,
    booktabs = TRUE
  )
}

mar_export(m_glm_15, y = 15)
mar_export(m_glm_16, y = 16)

# Plots to match the plot from Fisk article ====================================
mar_temp <- function(x, y = 15) {
  x %>%
    filter(grepl("race", factor)) %>%
    mutate(factor = gsub("race5", "", factor)) %>%
    rename(conf.low = lower, conf.high = upper, term = factor, estimate = AME) %>%
    race_highlight(
      y = y,
      limits = c(-0.2, 0.3),
      breaks = seq(-0.2, 0.3, 0.1),
      my_theme = FALSE,
      ylab = "Average Marginal Effects (95% C.I.)"
    )
}
mar_15_plot <- mar_temp(m_glm_15, y = 15)
mar_16_plot <- mar_temp(m_glm_16, y = 16)

pdf(file = here("fig", "mar_15_ame.pdf"), width = 3.5, height = 3)
print(
  pdf_default(mar_15_plot) +
    labs(title = NULL) +
    theme(axis.title = element_blank(), legend.position = "none")
)
dev.off()

pdf(file = here("fig", "mar_16_ame.pdf"), width = 4, height = 3)
print(
  pdf_default(mar_16_plot) +
    labs(title = NULL) +
    theme(axis.title = element_blank(), legend.position = "none")
)
dev.off()
