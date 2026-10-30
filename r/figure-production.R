# Load Packages
library (tidyverse)
library (janitor)

# read data from lab output spreadsheets
pah_original <- readxl::read_xlsx("data/P-091-spatial-set1.xlsx", sheet = 2, skip = 6)
apah_original <- readxl::read_xlsx("data/P-091-spatial-set1.xlsx", sheet = 3, skip = 6) 
hpac_original <- readxl::read_xlsx("data/P-091-spatial-set1.xlsx", sheet = 4, skip = 6) 

#clean up the data including column names (sample names) and excess empty sample column and zero negative values
pah <- pah_original %>%
  clean_names() %>%
  remove_empty(which = c("cols"),cutoff = 1) %>%
  remove_constant()