# Green Gold Script: Poverty stats
# Author: Vidal Mendoza-Tinoco
# Last date modified: 2025-05-01

# Load libraries -----
library(dplyr)
library(ggplot2)
library(readxl)
library(ggthemes)
library(tidyr)
library(here)
library(patchwork)

# Load data and clean data -----
coneval <- read_xlsx(here("data", "concentrado_ind_de_pob_2020_clean.xlsx"))

## Filter for Michoacán (entity code 16)
df_mich <- coneval %>%
  filter(Clave_entidad == "16") %>%
  select(
    Clave_entidad,
    Entidad_federativa,
    ExtremePov_2010 = PobExt_Porcentaje2010,
    ExtremePov_2015 = PobExt_Porcentaje2015,
    ExtremePov_2020 = PobExt_Porcentaje2020,
    Poverty_2010 = Pobreza_Porcentaje2010,
    Poverty_2015 = Pobreza_Porcentaje2015,
    Poverty_2020 = Pobreza_Porcentaje2020,
    ModeratePov_2010 = PobrMod_Porcentaje2010,
    ModeratePov_2015 = PobMod_Porcentaje2015,
    ModeratePov_2020 = PobMod_Porcentaje2020
  )

## Pivot to long format
df_long <- df_mich %>%
  pivot_longer(
    cols = -c(Clave_entidad, Entidad_federativa),
    names_to = "Indicator",
    values_to = "Percentage"
  ) %>%
  mutate(
    Year = sub(".*_", "", Indicator),
    Type = case_when(
      grepl("Extreme", Indicator) ~ "Extreme Poverty",
      grepl("Moderate", Indicator) ~ "Moderate Poverty",
      grepl("Poverty", Indicator) ~ "Poverty"
    )
  )

# Plot -----
plot_poverty <- function(df, poverty_type, color) {
  df_plot <- df %>% filter(Type == poverty_type)
  
  ggplot(df_plot, aes(x = Year, y = Percentage, group = 1)) +
    geom_point(color = color, shape = 17, size = 2.5) +
    geom_line(color = color, linewidth = 1) +
    geom_text(aes(label = paste0(Percentage, "%")), vjust = -1, color = color, size = 3.5) +
    labs(
      title = paste("Percentage of the Population in", poverty_type),
      subtitle = "Michoacán",
      x = "Year", y = "Percentage",
      caption = "Source: CONEVAL"
    ) +
    ylim(0, max(df$Percentage) * 1.15) +
    theme_clean()
}

## Generate individual plots
plot_extreme <- plot_poverty(df_long, "Extreme Poverty", "seagreen")
plot_moderate <- plot_poverty(df_long, "Moderate Poverty", "goldenrod")
plot_total <- plot_poverty(df_long, "Poverty", "tomato")

ggsave("pov_extreme_mich.png", plot_extreme, path = "plots", dpi = "retina", width = 7, height = 4)
ggsave("pov_moderate_mich.png", plot_moderate, path = "plots", dpi = "retina", width = 7, height = 4)
ggsave("pov_total_mich.png", plot_total, path = "plots", dpi = "retina", width = 7, height = 4)

combined_plot <- plot_extreme / plot_moderate / plot_total
combined_plot

ggsave("poverty_mich_combined.png", combined_plot, path = "plots", dpi = "retina", width = 8, height = 8.5)
