source(here::here("R", "05_reg_prelim.R"))

# Creating plots ===============================================================
p_list <- model_weight$all %>%
  map(tidy_race) %>%
  imap(race_highlight)

p_by_gen <- all_by_gen %>%
  map(
    function(x) {
      x %>%
        map(tidy_race) %>%
        imap(
          ~ race_highlight(
            .x, .y,
            my_theme = FALSE,
            limits = c(-4, 6), breaks = seq(-4, 6, by = 1)
          )
        )
    }
  )

p_by_party <- all_by_party %>%
  map(
    function(x) {
      x %>%
        map(tidy_race) %>%
        imap(
          ~ race_highlight(
            .x, .y,
            my_theme = FALSE,
            limits = c(-4, 6), breaks = seq(-4, 6, by = 1)
          )
        )
    }
  )

# Export regression tables =====================================================
covars_names <- setdiff(covars_names, c("Party: Rep", "Party: Other"))
stargazer_custom_tex(
  all_by_party %>% map("prop_15"),
  lab = 15,
  out15 = here("tab", "partisan_prop15_long.tex"),
  out16 = here("tab", "partisan_prop16_long.tex"),
  lab15 = "tab:partisan_prop15_long",
  lab16 = "tab:partisan_prop16_long",
  column.labels = c("Dem", "Rep", "Other")
)
stargazer_custom_tex(
  all_by_party %>% map("prop_16"),
  lab = 16,
  out15 = here("tab", "partisan_prop15_long.tex"),
  out16 = here("tab", "partisan_prop16_long.tex"),
  lab15 = "tab:partisan_prop15_long",
  lab16 = "tab:partisan_prop16_long",
  column.labels = c("Dem", "Rep", "Other")
)

# Export plots =================================================================
pdf(file = here("fig", "plot16_upd.pdf"), width = 3.5, height = 3)
print(plot_nolegend(pdf_default(p_list$prop_16)))
dev.off()

pdf(file = here("fig", "plot15_upd.pdf"), width = 3.5, height = 3)
print(plot_nolegend(pdf_default(p_list$prop_15)))
dev.off()

pdf(file = here("fig", "plot15_bygen.pdf"), width = 5, height = 4)
within(p_by_gen, rm("Silent (75+)")) %>%
  map("prop_15") %>%
  map(pdf_default) %>%
  imap(~ .x + ggtitle(.y)) %>%
  map(~ .x + theme(axis.title = element_blank(), legend.position = "none")) %>%
  Kmisc::grid_arrange_shared_legend(list = ., ncol = 2, nrow = 2)
dev.off()

pdf(file = here("fig", "plot16_bygen.pdf"), width = 5, height = 4)
within(p_by_gen, rm("Silent (75+)")) %>%
  map("prop_16") %>%
  map(pdf_default) %>%
  imap(~ .x + ggtitle(.y)) %>%
  map(~ .x + theme(axis.title = element_blank(), legend.position = "none")) %>%
  Kmisc::grid_arrange_shared_legend(list = ., ncol = 2, nrow = 2)
dev.off()

pdf(file = here("fig", "plot15_byparty.pdf"), width = 7, height = 3)
p_by_party %>%
  map("prop_15") %>%
  map(pdf_default) %>%
  imap(~ .x + ggtitle(.y)) %>%
  map(~ .x + theme(axis.title = element_blank(), legend.position = "none")) %>%
  Kmisc::grid_arrange_shared_legend(list = .)
dev.off()

pdf(file = here("fig", "plot16_byparty.pdf"), width = 7, height = 3)
p_by_party %>%
  map("prop_16") %>%
  map(pdf_default) %>%
  imap(~ .x + ggtitle(.y)) %>%
  map(~ .x + theme(axis.title = element_blank(), legend.position = "none")) %>%
  Kmisc::grid_arrange_shared_legend(list = .)
dev.off()
