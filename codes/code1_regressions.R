# Green Gold Script: Gini Regressions and Descriptive Statistics
# Author: Vidal Mendoza-Tinoco
# Last date modified: 2025-04-30

# Load libraries -----
library(dplyr)
library(ggplot2)
library(lfe)
library(readr)
library(lubridate)
library(here)
library(stargazer)
library(scales)
library(ggthemes)
library(stringr)

# Load and clean the dataset -----
df <- read_csv(here("data", "df_final_general_ginimonth.csv"))

df <- df %>%
  mutate(across(c(cve, mun, mun_name, mes, trat_2, pto), as.factor)) %>%
  select(mun_name, gini, mes, year, pto, trat_2, date)

# Plot average Gini over time by group -----
gini_time_plot <- df %>%
  group_by(trat_2, date) %>%
  summarise(mean_gini = mean(gini, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = date, y = mean_gini, color = trat_2)) +
  geom_line() +
  geom_vline(xintercept = ymd("2011-01-01"), linetype = 2) +
  geom_text(aes(x = ymd("2011-12-01"), y = 0.325, label = "2011"), color = "red") +
  scale_color_manual(labels = c("Control", "Treated"), values = c("tan3", "seagreen")) +
  labs(x = "Date", y = "Gini Coefficient", color = "Group") +
  scale_x_date(labels = date_format("%Y-%m-%d")) +
  theme_clean()

ggsave(filename = "plot_gini_month_trat.png", plot = gini_time_plot, path = "plots", dpi = "retina", width = 9, height = 3.5)



# Difference-in-Differences Estimate -----
means <- df %>%
  group_by(trat_2, pto) %>%
  summarise(gini_avg = mean(gini, na.rm = TRUE), .groups = "drop")

get_mean <- function(treat, period) {
  means %>% filter(trat_2 == treat, pto == period) %>% pull(gini_avg)
}

dd_estimate <- (get_mean(1, 1) - get_mean(0, 1)) - (get_mean(1, 0) - get_mean(0, 0))
print(dd_estimate)  # 0.003418386

# Difference-in-Differences Regressions ------
mod1 <- felm(gini ~ trat_2 + pto + trat_2:pto | 0 | 0 | mun_name, data = df)

mod2 <- felm(gini ~ trat_2:pto | mun_name + date | 0 | mun_name, data = df)

## Export regression results
stargazer(
  mod1, mod2,
  omit.stat = c("f", "ser", "aic", "bic", "ll"),
  covariate.labels = c("Treated", "Post", "Treated x Post"),
  dep.var.labels = "Gini Coefficient",
  notes = c(
    "Robust standard errors clustered by municipality.",
    "Column 2 includes municipality and time fixed effects.",
    "Significance levels: * p < 0.10, ** p < 0.05, *** p < 0.01"
  ),
  type = "html",
  out = "tables/table1_regressions_month_gini_mun_ptotrat.html"
)


# Density Plot of Gini by Group -----
density_plot <- ggplot(df, aes(gini, ..scaled.., color = trat_2)) +
  geom_density(alpha = 0.2, na.rm = TRUE) +
  scale_color_manual(labels = c("Control", "Treated"), values = c("tan3", "seagreen")) +
  labs(x = "Gini Coefficient", y = "Density", color = "Group") +
  theme_clean()

ggsave("dens_03_20_01.png", density_plot, path = "plots", dpi = "retina", width = 7, height = 3.5)


# Difference-in-Differences Mean Plot -----
plot_data <- df %>%
  mutate(
    group_label = factor(ifelse(trat_2 == "1", "Treated", "Control")),
    period_label = factor(ifelse(pto == "1", "b) Post (2011)", "a) Pre (Before 2011)"))
  ) %>%
  group_by(group_label, period_label) %>%
  summarise(
    mean_gini = mean(gini),
    se = sd(gini, na.rm = TRUE) / sqrt(n()),
    lower = mean_gini - 1.96 * se,
    upper = mean_gini + 1.96 * se,
    .groups = "drop"
  )

dd_plot <- ggplot(plot_data, aes(x = period_label, y = mean_gini, color = group_label)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
  geom_line(aes(group = group_label)) +
  labs(x = "Period", y = "Mean Gini Coefficient", color = "Group") +
  scale_color_manual(values = c("tan3", "seagreen")) +
  theme_clean()

ggsave("ddplot_month.png", dd_plot, path = "plots", dpi = "retina", width = 6, height = 4)

# Difference-in-Differences Mean Plot dash line -----

## Data Wrangling -----
treated_pre  <- plot_data %>% filter(group_label == "Treated", str_detect(period_label, "Pre")) %>% pull(mean_gini)
treated_post <- plot_data %>% filter(group_label == "Treated", str_detect(period_label, "Post")) %>% pull(mean_gini)
control_pre  <- plot_data %>% filter(group_label == "Control", str_detect(period_label, "Pre")) %>% pull(mean_gini)
control_post <- plot_data %>% filter(group_label == "Control", str_detect(period_label, "Post")) %>% pull(mean_gini)

expected_treated_post <- treated_pre + (control_post - control_pre)
dd_effect <- treated_post - expected_treated_post

plot_data <- plot_data %>%
  mutate(x = ifelse(str_detect(period_label, "Pre"), 1, 2),
         group = group_label)

## Plot -----
dd_plot_2 <- ggplot(plot_data, aes(x = x, y = mean_gini, color = group)) +
  geom_point(size = 2.5) +
  geom_line(aes(group = group), linewidth = 0.9) +
  
  geom_segment(aes(x = 1, xend = 2,
                   y = treated_pre,
                   yend = expected_treated_post),
               linetype = "dashed", color = "grey50", linewidth = 1) +
  
  geom_segment(aes(x = 2, xend = 2,
                   y = expected_treated_post,
                   yend = treated_post),
               linetype = "dotted", color = "blue", linewidth = 1) +
  
  annotate("label", 
           x = 2.2,
           y = (treated_post + expected_treated_post) / 2,
           label = "Treatment Effect", 
           size = 3.5,
           fill = "white") +
  
  labs(
    x = "Period",
    y = "Average Gini",
    color = "Group"
  ) +
  scale_color_manual(values = c("Control" = "tan3", "Treated" = "seagreen")) +
  scale_x_continuous(breaks = c(1, 2),
                     labels = c("Before 2011", "After 2011"),
                     limits = c(0.9, 2.3)) +
  theme_clean()

dd_plot_2

ggsave("ddplot_estimate_month.png", dd_plot_2, path = "plots", dpi = "retina",
       width = 8, height = 4)
