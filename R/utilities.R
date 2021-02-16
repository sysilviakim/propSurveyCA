# Packages =====================================================================
library(plyr)
library(tidyverse)
library(lubridate)
library(janitor)
library(assertthat)
library(here)
library(styler)
library(Kmisc)

# Subdirectories ===============================================================
if (!dir.exists(here("R", "eda"))) {
  dir.create(here("R", "eda"))
}
