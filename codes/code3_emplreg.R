# Green Gold Script: Employment Rate Regression (TWFE)
# Author: Vidal Mendoza-Tinoco
# Last date modified: 2025-04-30

# Libraries -----
library(dplyr)
library(ggplot2)
library(fixest)
library(modelsummary)
library(readr)
library(here)
library(ggthemes)

# Load dataset -----
df <- read_csv(here("data", "year_mun_workers_pob.csv"))

df <- df %>%
  mutate(
    mun = factor(mun),
    pto = factor(pto),
    trat_2 = factor(trat_2)
  )

# Run TWFE regression on log employment rate -----
mod <- feols(
  log_tasa ~ trat_2 * pto | mun + year,
  cluster = ~mun,
  data = df
)

summary(mod)

# Export regression table (HTML) -----
modelsummary(
  list("TWFE Employment Model" = mod),
  output = "tables/table2_regresion_emp_year.html",
  coef_map = c(
    "trat_21" = "Treated",
    "pto1" = "Post",
    "trat_21:pto1" = "Treated * Post"
  ),
  statistic = "std.error",
  stars = TRUE,
  gof_map = tibble::tribble(
    ~raw,            ~clean,        ~fmt,
    "nobs",          "Num. Obs.",   0,
    "r.squared",     "R²",          3,
    "adj.r.squared", "Adj. R²",     3
  ),
  notes = list(
    "Robust standard errors clustered by municipality in parentheses.",
    "Includes municipality and year fixed effects.")
)

# Plot average log employment rate by group -----
df_plot <- df %>% filter(!is.na(trat_2))

employment_plot <- df_plot %>%
  group_by(trat_2, year) %>%
  summarise(log_tasa = mean(log_tasa, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = log_tasa, color = trat_2)) +
  geom_line() +
  geom_vline(xintercept = 2011, linetype = 2) +
  geom_text(aes(x = 2011.9, y = 1.75, label = "2011"), color = "red") +
  scale_color_manual(labels = c("Control", "Treated"), values = c("tan3", "seagreen")) +
  labs(
    x = "Year",
    y = "Log Employment Rate per 1000 inhabitants",
    color = "Group"
  ) +
  theme_clean()

employment_plot

ggsave("plot_empleo_trat_year.png", plot = employment_plot, path = "plots", dpi = "retina", width = 9, height = 3.5)

