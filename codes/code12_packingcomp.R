# Green Gold Script: packing companies
# Author: Vidal Mendoza-Tinoco
# Last date modified: 2025-05-02

# Load libraries -----
library(dplyr)
library(ggplot2)
library(ggthemes)
library(readxl)
library(here)

# Load and clean data -----
df <- read_xlsx(here("data", "empacadoras_23.xlsx"))

## Create treatment period variable
df <- df %>% 
  mutate(treatment_period = ifelse(year_const >= 2011, "Post-2011 (Export Access)", "Pre-2011"))

# Final plot -----
plot_exporters <- ggplot(df, aes(x = year_const, fill = treatment_period)) +
  geom_histogram(binwidth = 1, boundary = 2010.5, color = "black") +
  scale_fill_manual(values = c("darkolivegreen3", "darkgreen")) +
  guides(fill = guide_legend(title = "Market Access Period")) +
  labs(
    x = "Year of Establishment",
    y = "Number of Avocado Packing Companies",
    title = "Yearly Entry of Avocado Exporters",
   ) +
  theme_clean()

plot_exporters

ggsave("exporters_by_year.png", plot_exporters, path = "plots", dpi = "retina", width = 7, height = 3)
