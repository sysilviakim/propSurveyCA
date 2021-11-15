source(here::here("R", "05_reg_prelim.R"))

# Breaking down model list into separate ones by proposition ===================
model_tidy <- model_weight$all %>%
  map(
    ~ bind_cols(
      tidy(.x),
      as.data.frame(confint(.x))
    ) %>%
      rename("conf.low" = "2.5 %", "conf.high" = "97.5 %") %>%
      filter(grepl("race5", term)) %>%
      mutate(term = gsub("race5", "", term))
  )

# Creating plots ===============================================================
My_Theme <- theme(
  axis.title.x = element_text(size = 14),
  axis.text.x = element_text(size = 12),
  axis.title.y = element_text(size = 14)
)

p_list <- model_tidy %>%
  imap(
    ~ ggplot(.x, aes(term, estimate, color = term)) +
      geom_point() +
      geom_pointrange(size = 1.2, aes(ymin = conf.low, ymax = conf.high)) +
      labs(
        x = "Race",
        y = paste0(
          "Proposition ", ifelse(grepl("15", .y), 15, 16), " (95% C.I.)"
        ),
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
        limits = c(-.75, 1.8), breaks = seq(1.8, -.75, by = -.25),
        labels =
          scales::number_format(accuracy = 0.01), oob = rescale_none
      ) +
      annotate("rect", fill = "lightgray", alpha = 0.4) +
      ggtitle("Model 3, Prop. 15 and Race") +
      My_Theme
  )

# Export =======================================================================
pdf(file = here("fig", "plot16_upd.pdf"), width = 4, height = 3)
print(plot_nolegend(pdf_default(p_list$prop_16)))
dev.off()

pdf(file = here("fig", "plot15_upd.pdf"), width = 4, height = 3)
print(plot_nolegend(pdf_default(p_list$prop_15)))
dev.off()
