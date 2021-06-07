renv::init()
install.packages("devtools")
install.packages("remotes")
install.packages("colorspace")
library(remotes)
install_github(
  "sysilviakim/Kmisc", INSTALL_opts = c("--no-multiarch"), dependencies = TRUE
)
Kmisc::proj_skeleton()

# install.packages installation
install.packages("plyr")
install.packages("tidyverse")
install.packages("lubridate")

install.packages("here")
install.packages("assertthat")
install.packages("styler")
install.packages("janitor")
install.packages("here")

install.packages("margins")
install.packages("stargazer")
install.packages("xtable")
install.packages("survey")
install.packages("knitr")
install.packages("kableExtra")
install.packages("tableone")
install.packages("GGally")
install.packages("jtools")
install.packages("ggstance")
install.packages("broom")
install.packages("broom.helpers")
install.packages("scales")

renv::snapshot()

dir.create(here::here("tab"))