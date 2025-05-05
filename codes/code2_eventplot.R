# Green Gold Script: Event Study Plot (TWFE)
# Author: Vidal Mendoza-Tinoco
# Last date modified: 2025-04-30

# Libraries -----
library(dplyr)
library(ggplot2)
library(fixest)
library(broom)
library(readr)
library(lubridate)
library(here)
library(ggthemes)

# Load and prepare dataset -----
df <- read_csv(here("data", "df_final_general_ginimonth.csv"))

df <- df %>%
  mutate(
    across(c(cve, mun, mun_name, mes, pto), as.factor),
    time_to_treat = ifelse(trat_2 == 1, year - 2011, 0)
  ) %>%
  select(mun_name, gini, mes, year, pto, trat_2, date, time_to_treat)

# Event Study Regression (TWFE) -----
mod_twfe <- feols(
  gini ~ i(time_to_treat, trat_2, ref = -1) | mun_name + date,
  cluster = ~mun_name,
  data = df
)

summary(mod_twfe)

# Extract event time coefficients -----
coefs <- broom::tidy(mod_twfe)

event_df <- coefs %>%
  filter(grepl("time_to_treat::", term)) %>%
  mutate(
    time_to_treat = as.integer(gsub("time_to_treat::(-?\\d+):trat_2", "\\1", term))
  ) %>%
  filter(!is.na(time_to_treat))

# Compute average post treatment effect (ATT) -----
att_value <- event_df %>%
  filter(time_to_treat >= 0) %>%
  summarise(att = mean(estimate, na.rm = TRUE)) %>%
  pull(att) # 0.004009781

# Manual Event Study Plot with ATT line -----
event_plot <- ggplot(event_df, aes(x = time_to_treat, y = estimate)) +
  geom_point(size = 2, color = "seagreen") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.2, color = "seagreen") +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey40") +
  geom_hline(yintercept = att_value, linetype = "dashed", color = "red") +
  annotate("text",
           x = max(event_df$time_to_treat),
           y = att_value - 0.04,  # etiqueta debajo de la línea
           label = "--- Avg. Post-Treatment Effect (0.004)",
           hjust = 1,
           color = "red",
           size = 3.5) +
  labs(
    title = "Event Study Plot (TWFE)",
    x = "Time to Treatment",
    y = "Effect on Gini (95% CI)"
  ) +
  theme_clean()

event_plot

ggsave("ev_plot_manual_with_att.png", plot = event_plot, path = "plots",
       dpi = "retina", width = 9, height = 3.5)
