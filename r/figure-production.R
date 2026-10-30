# Load Packages
library (tidyverse)
library (janitor)

# read data from lab output spreadsheets
pah_original <- readxl::read_xlsx("data/P-091-spatial-set1.xlsx", sheet = 2, skip = 6)
apah_original <- readxl::read_xlsx("data/P-091-spatial-set1.xlsx", sheet = 3, skip = 6) 
hpac_original <- readxl::read_xlsx("data/P-091-spatial-set1.xlsx", sheet = 4, skip = 6) 

#clean up the data including column names (sample names) and excess empty sample column and zero negative values
#for pah
pah <- pah_original %>%
  clean_names() %>%
  remove_empty(which = c("cols"),cutoff = 1) %>%
  remove_constant() %>%
  mutate(across(-1, ~ pmax(.x, 0)))
#for apah
apah <- apah_original %>%
  clean_names() %>%
  remove_empty(which = c("cols"),cutoff = 1) %>%
  remove_constant() %>%
  mutate(across(-1, ~ pmax(.x, 0)))
#for hpac
hpac <- hpac_original %>%
  clean_names() %>%
  remove_empty(which = c("cols"),cutoff = 1) %>%
  remove_constant() %>%
  mutate(across(-1, ~ pmax(.x, 0)))

#generate preliminary heatmaps for each compound, with a pivot
pah_long <- pah %>%
  rename(compound = x1) %>%
  pivot_longer(-compound, names_to = "sample", values_to = "conc") %>%
  mutate(
    compound = factor(compound, levels = rev(unique(compound))),  # keep original order, top to bottom
    sample   = factor(sample, levels = unique(sample)),           # keep column order
    conc = na_if(conc, 0)
  )

ggplot(pah_long, aes(x = sample, y = compound, fill = conc)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Conc.") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x  = element_blank(),   
    axis.ticks.x = element_blank(),
    panel.grid   = element_blank()
  ) #remove sample names as will group together by location later
#second heat map
apah_long <- apah %>%
  rename(compound = x1) %>%
  pivot_longer(-compound, names_to = "sample", values_to = "conc") %>%
  mutate(
    compound = factor(compound, levels = rev(unique(compound))),  # keep original order, top to bottom
    sample   = factor(sample, levels = unique(sample)),           # keep column order
    conc = na_if(conc, 0)
    )

ggplot(apah_long, aes(x = sample, y = compound, fill = conc)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x  = element_blank(),   
    axis.ticks.x = element_blank(),
    panel.grid   = element_blank()
  )
#combine all compounds and two datasets into one tibble then generate big heat map