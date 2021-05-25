
#### Breaking down model list into separate ones by proposition

demo_geo_15<- tidy(model_weight$demo_geo$prop_15)
demo_geo_16 <- tidy(model_weight$demo_geo$prop_16)
dm_g_p_15<- tidy(model_weight$demo_geo_party$prop_15)
dm_g_p_16 <- tidy(model_weight$demo_geo_party$prop_16)
all_15 <- tidy(model_weight$all$prop_15)
all_16 <- tidy(model_weight$all$prop_16)


# Getting rid of other variables -- specifying in the ggplot args doesn't work
all_15_reg<- all_15[c(4:7),]

all_16_reg <- all_16[c(4:7),]

# Doesn't work yet --- 
# all_16_reg <- all_16_reg %>% rename("race5Asian" = "Asian",
                                    # "race5Black" = "Black",
                                    # "race5Hispanic" = "Hispanic",
                                    # "race5Other" = "Other")


### Making the plots, red for Hispanic won't show up yet, working on it --- 

plot_15 <- ggplot(all_15_reg, aes(term, estimate)) + geom_point() +
  geom_pointrange(size = 1.2, aes(ymin = -1, ymax= 1)) +
  xlab("Race") + ylab("Proposition 16") +
  scale_colour_manual(values = c("race5Hispanic" = "red", "N" = "gray10")) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(
    limits = c(-1, 1), breaks = seq(-1, 1, by = .25),
    labels =
      scales::number_format(accuracy = 0.01)
  ) +
  annotate("rect", fill = "lightgray", alpha = 0.4) + 
  ggtitle("Model 3, Prop. 15 and Race")

print(plot_15)




plot_16 <- ggplot(all_16_reg, aes(term, estimate)) + geom_point() +
  geom_pointrange(size = 1.2, aes(ymin = -1.5, ymax= 1.5)) +
  xlab("Race") + ylab("Proposition 16") +
  scale_colour_manual(values = c("race5Hispanic" = "red", "N" = "gray10")) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(
    limits = c(-1.5, 1.5), breaks = seq(-1.5, 1.5, by = .5),
    labels =
      scales::number_format(accuracy = 0.01)
  ) +
  annotate("rect", fill = "lightgray", alpha = 0.4) + 
  ggtitle("Model 3, Prop. 16 and Race")


print(plot_16)


ggsave("plot_all_16.png", plot_16)
ggsave("plot_all_16.png", plot_15)

