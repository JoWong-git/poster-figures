# Load Packages
library (tidyverse)
library (janitor)

#combine all compounds and two datasets into one tibble then generate big heat map
#file is lab output data
file <- "data/P-091-spatial-set1.xlsx"
#read full excel sheet
clean_sheet <- function(sheet) {
  readxl::read_xlsx(file, sheet = sheet, skip = 6) %>%
    clean_names() %>%
    remove_empty(which = "cols", cutoff = 1) %>%
    remove_constant() %>%
    mutate(across(-1, ~ pmax(.x, 0)))
}
#sheet is split into compound class
pah  <- clean_sheet(2)
apah <- clean_sheet(3)
hpac <- clean_sheet(4)

# --- long format + combine ---
make_long <- function(df) {
  df %>%
    rename(compound = 1) %>%                      # first column = compound names
    pivot_longer(-compound, names_to = "sample", values_to = "conc")
}

# keep samples in original column order, across all three tables
sample_levels <- unique(c(names(pah)[-1], names(apah)[-1], names(hpac)[-1]))

all_long <- bind_rows(
  PAH  = make_long(pah),
  APAH = make_long(apah),
  HPAC = make_long(hpac),
  .id = "class"
) %>%
  mutate(
    class    = factor(class, levels = c("PAH", "APAH", "HPAC")),
    compound = factor(compound, levels = rev(unique(compound))),
    sample   = factor(sample, levels = sample_levels),
    conc     = na_if(conc, 0)                     # zeros -> NA (non-detect)
  )

# --- plot ---
all_plot <- all_long %>%
ggplot(aes(x = sample, y = compound, fill = conc)) +
  geom_tile(colour = "white", linewidth = 0.2) +
  facet_grid(class ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_viridis_c(
    option = "magma", direction = -1, name = "Conc.",
    na.value = "grey93"                           # non-detects: soft light grey
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x       = element_blank(),
    axis.ticks.x      = element_blank(),
    panel.grid        = element_blank(),
    strip.placement   = "outside",
    strip.text.y.left = element_text(angle = 0, face = "bold"),
    panel.spacing.y   = unit(6, "pt")
  )
ggsave("figures/plot_1.pdf", all_plot,
        units = "mm")  # as a pdf
ggsave("figures/plot_1.png", all_plot,
       width = 100, height = 200, units = "mm")  # as a png
all_plot