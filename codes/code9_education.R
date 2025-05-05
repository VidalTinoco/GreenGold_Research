# Green Gold Script: Education and inequality
# Author: Vidal Mendoza-Tinoco
# Last date modified: 2025-05-01

# Libraries -----
library(dplyr)
library(ggplot2)
library(readxl)
library(readr)
library(scales)
library(ggthemes)
library(patchwork)
library(here)

# Load datasets and cleaning -----
edu <- read_csv(here("data", "df_ed_2005_mun.csv"))
df <- read_csv(here("data", "df_final_general_year.csv"))

## clean names
edu_clean <- edu %>%
  mutate(nom_mun = recode(nom_mun,
                          "Álvaro Obregón" = "Alvaro Obregon",
                          "Apatzingán" = "Apatzingan",
                          "Briseñas" = "Brisenas",
                          "Carácuaro" = "Caracuaro",
                          "Coalcomán de Vázquez Pallares" = "Coalcoman",
                          "Copándaro" = "Copandaro",
                          "Cherán" = "Cheran",
                          "Chucándiro" = "Chucandiro",
                          "Erongarícuaro" = "Erongaricuaro",
                          "Ixtlán" = "Ixtlan",
                          "Jiménez" = "Jimenez",
                          "Juárez" = "Juarez",
                          "Maravatío" = "Maravatio",
                          "Lázaro Cárdenas" = "Lazaro Cardenas",
                          "Múgica" = "Mugica",
                          "Nocupétaro" = "Nocupetaro",
                          "Numarán" = "Numaran",
                          "Pajacuarán" = "Pajacuaran",
                          "Panindícuaro" = "Panindicuaro",
                          "Parácuaro" = "Paracuaro",
                          "Pátzcuaro" = "Patzcuaro",
                          "Peribán" = "Periban",
                          "Purépero" = "Purepero",
                          "Puruándiro" = "Puruandiro",
                          "Queréndaro" = "Querendaro",
                          "Cojumatlán de Régules" = "Cojumatlan de Regules",
                          "Tacámbaro" = "Tacambaro",
                          "Tancítaro" = "Tancitaro",
                          "Tangancícuaro" = "Tangancicuaro",
                          "Tarímbaro" = "Tarimbaro",
                          "Tingüindín" = "Tinguindin",
                          "Tiquicheo de Nicolás Romero" = "Tiquicheo",
                          "Tumbiscatío" = "Tumbiscatio",
                          "Yurécuaro" = "Yurecuaro",
                          "Zináparo" = "Zinaparo",
                          "Zinapécuaro" = "Zinapecuaro",
                          "Zitácuaro" = "Zitacuaro",
                          "José Sixto Verduzco" = "Jose Sixto Verduzco"
  ))

## Merge with main panel
df_merged <- left_join(df, edu_clean, by = c("mun" = "nom_mun")) %>%
  mutate(mun = factor(mun))

# Plots -----

## Create plotting function
make_dd_data <- function(df_group) {
  df_group %>%
    mutate(
      Education = factor(ifelse(upper == 1, "Above Average", "Below Average")),
      Period = factor(ifelse(pto == 1, "(b) Post 2011", "(a) Pre 2011"))
    ) %>%
    group_by(Education, Period) %>%
    summarise(
      mean_gini = mean(gini_mean),
      se_gini = sd(gini_mean, na.rm = TRUE) / sqrt(n()),
      upper = mean_gini + 1.96 * se_gini,
      lower = mean_gini - 1.96 * se_gini,
      .groups = "drop"
    )
}

make_dd_plot <- function(data, subtitle = NULL) {
  ggplot(data, aes(x = Period, y = mean_gini, color = Education)) +
    geom_point(size = 2.5) +
    geom_line(aes(group = Education), linewidth = 0.8) +
    scale_color_manual(values = c("brown1", "cornflowerblue")) +
    labs(
      subtitle = subtitle,
      x = "Period",
      y = "Average Gini Coefficient",
      color = "Education Level"
    ) +
    theme_clean()
}

## Generate plots
dd_all <- make_dd_data(df_merged)
dd_trat <- make_dd_data(df_merged %>% filter(trat_2 == 1))
dd_ctrl <- make_dd_data(df_merged %>% filter(trat_2 == 0))

plot_all <- make_dd_plot(dd_all)
plot_trat <- make_dd_plot(dd_trat, subtitle = "Treated Group")
plot_ctrl <- make_dd_plot(dd_ctrl, subtitle = "Control Group")

ggsave("ddplot_ed.png", plot_all, path = "plots", dpi = "retina", width = 7, height = 3.5)
ggsave("ddplot_ed_trat.png", plot_trat, path = "plots", dpi = "retina", width = 7, height = 3.5)
ggsave("ddplot_ed_control.png", plot_ctrl, path = "plots", dpi = "retina", width = 7, height = 3.5)

combined_plot <- plot_all / plot_trat / plot_ctrl
combined_plot

ggsave("edu_dd_plots_atc.png", combined_plot, path = "plots", dpi = "retina", width = 7, height = 7)
