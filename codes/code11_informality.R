# Green Gold Script: Informality and Exporters Analysis
# Author: Vidal Mendoza-Tinoco
# Last date modified: 2025-05-02

# Libraries -----
library(dplyr)
library(ggplot2)
library(readxl)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggthemes)
library(scales)
library(here)

# Load and clean datasets -----
df <- read_xlsx(here("data", "informalidad_trim_mich.xlsx"))
inf_ag <- read_xlsx(here("data", "inegi_enoe_clean_mich.xlsx"))
var_trat <- read_csv(here("data", "df_final_general_year.csv"))

## Clean main date column
df <- df %>% mutate(quarter_date = as.Date(trimestre))

## Clean character encoding in categories and municipality names
recode_utf8 <- function(df, vars) {
  for (var in vars) {
    df[[var]] <- str_replace_all(df[[var]], "\\u00c1", "A")
    df[[var]] <- str_replace_all(df[[var]], "\\u00e9", "e")
    df[[var]] <- str_replace_all(df[[var]], "\\u00ed", "i")
    df[[var]] <- str_replace_all(df[[var]], "\\u00f3", "o")
    df[[var]] <- str_replace_all(df[[var]], "\\u00fa", "u")
    df[[var]] <- str_replace_all(df[[var]], "\\u00f1", "n")
  }
  df
}

inf_ag <- recode_utf8(inf_ag, c("Group", "Subgroup", "Municipality"))

## Convert to factors
inf_ag <- inf_ag %>% mutate(across(c(State, Municipality, Group, Subgroup,
                                     `Classification of Formal and Informal Jobs of the First Activity`), factor))

## Fix treatment data frame
var_trat <- var_trat %>%
  distinct(mun, trat_2) %>%
  mutate(mun = if_else(mun == "Cojumatlan de Regules", "Cojumatlan", mun))

## Join treatment to ENOE
inf_ag <- inf_ag %>%
  rename(Formal_Informal = `Classification of Formal and Informal Jobs of the First Activity`)
inf_full <- left_join(inf_ag, var_trat, by = c("Municipality" = "mun"))

## Create year, quarter and date
inf_full <- inf_full %>%
  mutate(
    year = as.numeric(substr(Quarter, 1, 4)),
    q = as.numeric(substr(Quarter, 7, 7)),
    date = ymd(paste(year, q + 1, "01")) - 1
  ) %>%
  filter(year <= 2020)

## Compute workforce and wages by quarter/type
inf_full <- inf_full %>%
  group_by(Quarter, Formal_Informal) %>%
  mutate(
    WF_FI_Qua = sum(Workforce, na.rm = TRUE),
    Wage_FI_Qua = mean(`Monthly Wage`, na.rm = TRUE)
  ) %>%
  ungroup()

# Plot: Workforce (overall) -----
plot_workforce <- ggplot(inf_full, aes(x = date, y = WF_FI_Qua, color = Formal_Informal)) +
  geom_smooth() +
  geom_point() +
  geom_vline(xintercept = ymd("2011-01-01"), linetype = 2) +
  geom_text(aes(x = ymd("2011-08-01"), y = 25000, label = "2011"), color = "seagreen") +
  scale_color_manual(values = c("deepskyblue3", "firebrick1"),
                     labels = c("Formal Employment", "Informal Employment")) +
  labs(title = "Workforce by Employment Type",
       x = "Quarter", y = "Workers", color = "Employment Type",
       caption = "Source: ENOE") +
  theme_clean()

plot_workforce

ggsave("workforce_by_employment_type.png", plot_workforce, path = "plots", dpi = 300, width = 8, height = 3.5)

# Plot: Wages (overall) -----
plot_wages <- ggplot(inf_full, aes(x = date, y = Wage_FI_Qua, color = Formal_Informal)) +
  geom_smooth() +
  geom_point() +
  geom_vline(xintercept = ymd("2011-01-01"), linetype = 2) +
  geom_text(aes(x = ymd("2011-08-01"), y = 2500, label = "2011"), color = "seagreen") +
  scale_color_manual(values = c("deepskyblue3", "firebrick1"),
                     labels = c("Formal Employment", "Informal Employment")) +
  labs(title = "Average Monthly Wage by Employment Type",
       x = "Quarter", y = "Monthly Wage (MXN)", color = "Employment Type",
       caption = "Source: ENOE") +
  theme_clean()

plot_wages

ggsave("monthly_wage_by_employment_type.png", plot_wages, path = "plots", dpi = 300, width = 8, height = 3.5)

# Plot: General Informality Rate -----
inf_rate_plot <- ggplot(df, aes(x = quarter_date, y = til_1)) +
  geom_line(color = "steelblue") +
  geom_vline(xintercept = ymd("2011-01-01"), linetype = 2, color = "red") +
  geom_text(aes(x = ymd("2011-12-01"), y = 68, label = "2011")) +
  labs(title = "Labor Informality Rate in Michoacán",
       x = "Quarter", y = "Informality Rate (%)") +
  theme_clean()

inf_rate_plot

ggsave("informality_rate_michoacan.png", inf_rate_plot, path = "plots", dpi = 300, width = 8, height = 3.5)
