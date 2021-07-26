source(here::here("R", "05_reg_prelim.R"))

#### Breaking down model list into separate ones by proposition

demo_geo_15 <- tidy(model_weight$demo_geo$prop_15)
demo_geo_16 <- tidy(model_weight$demo_geo$prop_16)
dm_g_p_15 <- tidy(model_weight$demo_geo_party$prop_15)
dm_g_p_16 <- tidy(model_weight$demo_geo_party$prop_16)
all_15 <- tidy(model_weight$all$prop_15)
all_16 <- tidy(model_weight$all$prop_16)

demo_geo_15_con <- confint(model_weight$demo_geo$prop_15, level = .95) %>%
  as.data.frame()
demo_geo_16_con <- confint(model_weight$demo_geo$prop_16, level = .95) %>%
  as.data.frame()
dm_g_p_15_con <- confint(model_weight$demo_geo_party$prop_15, level = .95) %>%
  as.data.frame()
dm_g_p_16_con <- confint(model_weight$demo_geo_party$prop_16, level = .95) %>%
  as.data.frame()
all_15_con <- confint(model_weight$all$prop_15, level = .95) %>%
  as.data.frame()
all_16_con <- confint(model_weight$all$prop_16, level = .95) %>%
  as.data.frame()
# Getting rid of other variables -- specifying in the ggplot args doesn't work
all_15_total <- cbind(all_15, all_15_con)
all_16_total <- cbind(all_16, all_16_con)

all_15_reg <- all_15_total[c(4:7), ]

all_16_reg <- all_16_total[c(4:7), ]

all_15_reg <- all_15_reg %>%
  rename("conf.low" = "2.5 %", "conf.high" = "97.5 %")
all_16_reg <- all_16_reg %>%
  rename("conf.low" = "2.5 %", "conf.high" = "97.5 %")

all_15_reg$term <- c("Black", "Hispanic", "Asian", "Other")
all_16_reg$term <- c("Black", "Hispanic", "Asian", "Other")

# working on shortening the code

models <- list(
  tidy(model_weight$demo_geo$prop_15),
  tidy(model_weight$demo_geo$prop_16),
  tidy(model_weight$demo_geo_party$prop_15),
  tidy(model_weight$demo_geo_party$prop_16),
  tidy(model_weight$all$prop_15),
  tidy(model_weight$all$prop_16)
)

### Making the plots  --- ------------------------------------------------------
My_Theme <- theme(
  axis.title.x = element_text(size = 14),
  axis.text.x = element_text(size = 12),
  axis.title.y = element_text(size = 14)
)
plot_15 <- ggplot(all_15_reg, aes(term, estimate, color = term)) +
  geom_point() +
  geom_pointrange(size = 1.2, aes(ymin = conf.low, ymax = conf.high)) +
  labs(x = "Race", y = "Proposition 15 (95% C.I.)", color = "Race") +
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
    limits = c(-1, 1), breaks = seq(1, -1, by = -.25),
    labels =
      scales::number_format(accuracy = 0.01), oob = rescale_none
  ) +
  annotate("rect", fill = "lightgray", alpha = 0.4) +
  ggtitle("Model 3, Prop. 15 and Race")

plot_15 <- plot_15 + My_Theme
print(plot_15)


plot_16 <- ggplot(all_16_reg, aes(term, estimate, color = term)) +
  geom_point() +
  geom_pointrange(size = 1.2, aes(ymin = conf.low, ymax = conf.high)) +
  labs(x = "Race", y = "Proposition 16 (95% C.I.)", color = "Race") +
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
    limits = c(-.75, 1.8), breaks = seq(1.8, -.75, by = -.25),
    labels =
      scales::number_format(accuracy = 0.01), oob = rescale_none
  ) +
  annotate("rect", fill = "lightgray", alpha = 0.4) +
  ggtitle("Model 3, Prop. 16 and Race")

plot_16 <- plot_16 + My_Theme
print(plot_16)

# Export =======================================================================
# ggsave("plot16_upd.pdf", plot_16)
# ggsave("plot15_upd.pdf", plot_15)

pdf(file = here("fig", "plot16_upd.pdf"), width = 4, height = 3)
print(plot_nolegend(pdf_default(plot_16)))
dev.off()

pdf(file = here("fig", "plot15_upd.pdf"), width = 4, height = 3)
print(plot_nolegend(pdf_default(plot_15)))
dev.off()

