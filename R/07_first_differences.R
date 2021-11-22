source(here::here("R", "05_reg_prelim.R"))

# Basic setup ==================================================================
ndraws <- 1000
pvec <- c(0.5, 0.025, 0.975)
set.seed(123)
base_vec <- c(prop_15 = "prop_15", prop_16 = "prop_16")

# Using the nested model =======================================================
beta <- model_weight$all %>%
  map("coefficients")

covmat.beta <- model_weight$all %>%
  map(vcov)

betadraw <- base_vec %>%
  imap(~ mvrnorm(ndraws, beta[[.x]], covmat.beta[[.x]]))

baseline.x <- model_weight$all %>%
  map("x") %>%
  map(~ apply(.x, 2, min))

out <- base_vec %>%
  imap_dfr(
    ~ {
      ## Hypothetical cases
      hypo.case <- baseline.x[[.x]]
      assert_that(hypo.case["race5Latino"] == 0)
      simprob.0 <- plogis(betadraw[[.x]] %*% hypo.case)
      hypo.case["race5Latino"] <- 1
      simprob.1 <- plogis(betadraw[[.x]] %*% hypo.case)
      
      ## difference
      diff <- simprob.1 - simprob.0
      
      ## three sets of quantiles and s.d.
      qt.diff <- quantile(diff, probs = pvec)
      sd.diff <- sd(diff)
      qt.0 <- quantile(simprob.0, probs = pvec)
      sd.0 <- sd(simprob.0)
      qt.1 <- quantile(simprob.1, probs = pvec)
      sd.1 <- sd(simprob.1)
      
      return(
        tibble(
          ## Suppress label
          ` ` = paste0(
            ## paste0("Prop. ", parse_number(.y), ": "), 
            "--- ", c("Non-Latino", "Latino", "Difference")
          ),
          Median = list(qt.0, qt.1, qt.diff) %>% map_dbl("50%"),
          `2.5%` = list(qt.0, qt.1, qt.diff) %>% map_dbl("2.5%"),
          `97.5%` = list(qt.0, qt.1, qt.diff) %>% map_dbl("97.5%"),
          `s.e.` = c(sd.0, sd.1, sd.diff)
        )
      )
    }
  )

# Export to xtable =============================================================
addtorow <- list(pos = list(0, 3))
addtorow$command <- paste(
  "\\midrule \n \\textbf{", c("Proposition 15", "Proposition 16"), "} & \\\\\n"
)

print.xtable(
  xtable(
    out,
    label = "tab:first_diff", align = "llrrrr",
    caption = paste0(
      "First Differences in the Expected Probabilities ",
      "for Latinos and Non-Latinos"
    )
  ),
  file = here("tab", "first_diff.tex"),
  booktabs = TRUE, include.rownames = FALSE, add.to.row = addtorow,
  hline.after = c(-1, nrow(out))
)
