# Green Gold Script: IMSS Descriptive Analysis
# Author: Vidal Mendoza-Tinoco
# Last date modified: 2025-05-01

# Libraries -----
library(dplyr)
library(ggplot2)
library(readr)
library(ggthemes)
library(patchwork)
library(tidyr)
library(scales)
library(here)

# Economic Sector -----

## Load and prepare data -----
imss <- read_csv(here("data", "imss_final_estdesc_year.csv"))

imss <- imss %>%
  mutate(across(c(mun, trat, pto), as.factor))

### Select and reshape sector employment variables
imss_se <- imss %>%
  select(year, mun, trat, pto, se1_0, se1_4, se1_6, se1_7) %>%
  pivot_longer(cols = starts_with("se1_"),
               names_to = "sector",
               values_to = "jobs") %>%
  mutate(sector = factor(sector,
                         levels = c("se1_0", "se1_4", "se1_6", "se1_7"),
                         labels = c("Agriculture", "Construction", "Commerce", "Transport")))

## Plot: All municipalities -----
df_all <- imss_se %>%
  group_by(year, sector) %>%
  summarise(mean_jobs = mean(jobs, na.rm = TRUE), .groups = "drop")

labels_all <- df_all %>%
  group_by(sector) %>%
  filter(year == max(year))

plot_all <- ggplot(df_all, aes(x = year, y = mean_jobs, color = sector)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2011, linetype = 2) +
  geom_text(aes(x = 2012, y = 3500, label = "2011"), color = "red") +
  geom_vline(xintercept = 2016, linetype = 2) +
  geom_text(aes(x = 2015, y = 4000, label = "2016"), color = "green") +
  geom_text(data = labels_all,
            aes(label = sector),
            hjust = -0.1, size = 3.5, show.legend = FALSE) +
  expand_limits(x = max(imss_se$year) + 2) +
  labs(
    x = "Year",
    y = "Average Employment",
    color = "Economic Sector"
  ) +
  scale_color_manual(values = c("seagreen", "saddlebrown", "purple", "navy")) +
  theme_clean()

plot_all

ggsave("imss_se_0320_all_labeled.png", plot_all, path = "graficas", dpi = "retina", width = 11, height = 5)

## Plot: Treatment group only -----
df_treated <- imss_se %>%
  filter(trat == "1") %>%
  group_by(year, sector) %>%
  summarise(mean_jobs = mean(jobs, na.rm = TRUE), .groups = "drop")

labels_treated <- df_treated %>%
  group_by(sector) %>%
  filter(year == max(year))

plot_treated <- ggplot(df_treated, aes(x = year, y = mean_jobs, color = sector)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2011, linetype = 2) +
  geom_text(aes(x = 2010, y = 4000, label = "2011"), color = "red") +
  geom_vline(xintercept = 2016, linetype = 2) +
  geom_text(aes(x = 2015, y = 4000, label = "2016"), color = "green") +
  geom_text(data = labels_treated,
            aes(label = sector),
            hjust = -0.1, size = 3.5, show.legend = FALSE) +
  expand_limits(x = max(imss_se$year) + 2) +
  labs(
    subtitle = "Treatment Group",
    x = "Year",
    y = "Average Employment",
    color = "Economic Sector"
  ) +
  scale_color_manual(values = c("seagreen", "saddlebrown", "purple", "navy")) +
  theme_clean()

plot_treated

ggsave("imss_se_0320_treated_labeled.png", plot_treated, path = "graficas", dpi = "retina", width = 11, height = 5)

## Plot: Control group only -----
df_control <- imss_se %>%
  filter(trat == "0") %>%
  group_by(year, sector) %>%
  summarise(mean_jobs = mean(jobs, na.rm = TRUE), .groups = "drop")

labels_control <- df_control %>%
  group_by(sector) %>%
  filter(year == max(year))

plot_control <- ggplot(df_control, aes(x = year, y = mean_jobs, color = sector)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2011, linetype = 2) +
  geom_text(aes(x = 2010, y = 4000, label = "2011"), color = "red") +
  geom_vline(xintercept = 2016, linetype = 2) +
  geom_text(aes(x = 2015, y = 4000, label = "2016"), color = "green") +
  geom_text(data = labels_control,
            aes(label = sector),
            hjust = -0.1, size = 3.5, show.legend = FALSE) +
  expand_limits(x = max(imss_se$year) + 2) +
  labs(
    subtitle = "Control Group",
    x = "Year",
    y = "Average Employment",
    color = "Economic Sector"
  ) +
  scale_color_manual(values = c("seagreen", "saddlebrown", "purple", "navy")) +
  theme_clean()

plot_control


ggsave("imss_se_0320_control_labeled.png", plot_control, path = "graficas", dpi = "retina", width = 11, height = 5)

## Combine labeled plots -----
combined_se_plot <- plot_all / plot_treated / plot_control

combined_se_plot

ggsave("context_imss_se_0320_labeled.png", combined_se_plot, path = "plots", dpi = "retina", width = 10, height = 7)



# Wage ranges -----

## Load and prepare data -----
imss <- read_csv(here("data", "imss_final_estdesc_year.csv"))

imss <- imss %>%
  mutate(across(c(mun, trat, pto), as.factor))

### Select and reshape wage range variables
wage_cols <- paste0("rs_w", 1:25)

imss_rs <- imss %>%
  select(year, mun, trat, pto, all_of(wage_cols)) %>%
  pivot_longer(cols = starts_with("rs_w"),
               names_to = "wage_bracket",
               values_to = "jobs") %>%
  mutate(wage_class = case_when(
    wage_bracket %in% paste0("rs_w", 1:5)   ~ "1-5",
    wage_bracket %in% paste0("rs_w", 6:10)  ~ "6-10",
    wage_bracket %in% paste0("rs_w", 11:15) ~ "11-15",
    wage_bracket %in% paste0("rs_w", 16:20) ~ "16-20",
    wage_bracket %in% paste0("rs_w", 21:25) ~ "21-25"
  ),
  wage_class = factor(wage_class, levels = c("1-5", "6-10", "11-15", "16-20", "21-25")))

## Plot: All municipalities -----
df_all <- imss_rs %>%
  group_by(year, wage_class) %>%
  summarise(mean_jobs = mean(jobs, na.rm = TRUE), .groups = "drop")

labels_all <- df_all %>%
  group_by(wage_class) %>%
  filter(year == max(year))

plot_rs_all <- ggplot(df_all, aes(x = year, y = mean_jobs, color = wage_class)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2011, linetype = 2) +
  geom_text(aes(x = 2012, y = 1000, label = "2011"), color = "red") +
  geom_vline(xintercept = 2016, linetype = 2) +
  geom_text(aes(x = 2017, y = 1000, label = "2016"), color = "green") +
  geom_text(data = labels_all,
            aes(label = wage_class),
            hjust = -0.1, size = 3.5, show.legend = FALSE) +
  expand_limits(x = max(imss_rs$year) + 2) +
  labs(
    x = "Year",
    y = "Average Jobs",
    color = "Wage Bracket"
  ) +
  theme_clean()

plot_rs_all

ggsave("imss_rs_0320_all_labeled.png", plot_rs_all, path = "plots", dpi = "retina", width = 10, height = 4)

## Plot: Treatment group only -----
df_treated <- imss_rs %>%
  filter(trat == "1") %>%
  group_by(year, wage_class) %>%
  summarise(mean_jobs = mean(jobs, na.rm = TRUE), .groups = "drop")

labels_treated <- df_treated %>%
  group_by(wage_class) %>%
  filter(year == max(year))

plot_rs_treated <- ggplot(df_treated, aes(x = year, y = mean_jobs, color = wage_class)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2011, linetype = 2) +
  geom_text(aes(x = 2012, y = 1000, label = "2011"), color = "red") +
  geom_vline(xintercept = 2016, linetype = 2) +
  geom_text(aes(x = 2017, y = 1000, label = "2016"), color = "green") +
  geom_text(data = labels_treated,
            aes(label = wage_class),
            hjust = -0.1, size = 3.5, show.legend = FALSE) +
  expand_limits(x = max(imss_rs$year) + 2) +
  labs(
    subtitle = "Treatment Group",
    x = "Year",
    y = "Average Jobs",
    color = "Wage Bracket"
  ) +
  theme_clean()

plot_rs_treated

ggsave("imss_rs_0320_treated_labeled.png", plot_rs_treated, path = "plots", dpi = "retina", width = 10, height = 4)

## Plot: Control group only -----
df_control <- imss_rs %>%
  filter(trat == "0") %>%
  group_by(year, wage_class) %>%
  summarise(mean_jobs = mean(jobs, na.rm = TRUE), .groups = "drop")

labels_control <- df_control %>%
  group_by(wage_class) %>%
  filter(year == max(year))

plot_rs_control <- ggplot(df_control, aes(x = year, y = mean_jobs, color = wage_class)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2011, linetype = 2) +
  geom_text(aes(x = 2012, y = 1000, label = "2011"), color = "red") +
  geom_vline(xintercept = 2016, linetype = 2) +
  geom_text(aes(x = 2017, y = 1000, label = "2016"), color = "green") +
  geom_text(data = labels_control,
            aes(label = wage_class),
            hjust = -0.1, size = 3.5, show.legend = FALSE) +
  expand_limits(x = max(imss_rs$year) + 2) +
  labs(
    subtitle = "Control Group",
    x = "Year",
    y = "Average Jobs",
    color = "Wage Bracket"
  ) +
  theme_clean()

plot_rs_control

ggsave("imss_rs_0320_control_labeled.png", plot_rs_control, path = "plots", dpi = "retina", width = 10, height = 4)

### Combine all plots
combined_rs_plot <- plot_rs_all / plot_rs_treated / plot_rs_control

combined_rs_plot

ggsave("context_imss_rs_0320_labeled.png", combined_rs_plot, path = "plots", dpi = "retina", width = 7, height = 7)



# Employer Size -----

## Load and prepare data ----
imss <- read_csv(here("data", "imss_final_estdesc_year.csv"))

imss <- imss %>%
  mutate(across(c(mun, trat, pto), as.factor))

### Select and reshape employer size variables
size_cols <- paste0("sp_", 1:7)

imss_sp <- imss %>%
  select(year, mun, trat, pto, all_of(size_cols)) %>%
  pivot_longer(cols = starts_with("sp_"),
               names_to = "size_group",
               values_to = "jobs") %>%
  mutate(size_group = factor(size_group,
                             levels = paste0("sp_", 1:7),
                             labels = c("1 job", "2-5 jobs", "6-50 jobs", "51-250 jobs",
                                        "251-500 jobs", "501-1000 jobs", "More than 1000 jobs")))

## Plot: All municipalities -----
df_all <- imss_sp %>%
  group_by(year, size_group) %>%
  summarise(mean_jobs = mean(jobs, na.rm = TRUE), .groups = "drop")

labels_all <- df_all %>%
  group_by(size_group) %>%
  filter(year == max(year))

plot_sp_all <- ggplot(df_all, aes(x = year, y = mean_jobs, color = size_group)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2011, linetype = 2) +
  geom_text(aes(x = 2010, y = 4500, label = "2011"), color = "red") +
  geom_vline(xintercept = 2016, linetype = 2) +
  geom_text(aes(x = 2015, y = 4500, label = "2016"), color = "green") +
  geom_text(data = labels_all,
            aes(label = size_group),
            hjust = -0.1, size = 3.5, show.legend = FALSE) +
  expand_limits(x = max(imss_sp$year) + 2) +
  labs(
    x = "Year",
    y = "Average Employer Size",
    color = "Employer Size"
  ) +
  scale_color_manual(values = c("seagreen", "saddlebrown", "red", "black", "gold1", "navy", "purple")) +
  theme_clean()

plot_sp_all

ggsave("imss_sp_0320_all_labeled.png", plot_sp_all, path = "plots", dpi = "retina", width = 13, height = 5.5)

# Plot: Treatment group only -----
df_treated <- imss_sp %>%
  filter(trat == "1") %>%
  group_by(year, size_group) %>%
  summarise(mean_jobs = mean(jobs, na.rm = TRUE), .groups = "drop")

labels_treated <- df_treated %>%
  group_by(size_group) %>%
  filter(year == max(year))

plot_sp_treated <- ggplot(df_treated, aes(x = year, y = mean_jobs, color = size_group)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2011, linetype = 2) +
  geom_text(aes(x = 2010, y = 4500, label = "2011"), color = "red") +
  geom_vline(xintercept = 2016, linetype = 2) +
  geom_text(aes(x = 2015, y = 4800, label = "2016"), color = "green") +
  geom_text(data = labels_treated,
            aes(label = size_group),
            hjust = -0.1, size = 3.5, show.legend = FALSE) +
  expand_limits(x = max(imss_sp$year) + 2) +
  labs(
    subtitle = "Treatment Group",
    x = "Year",
    y = "Average Employer Size",
    color = "Employer Size"
  ) +
  scale_color_manual(values = c("seagreen", "saddlebrown", "red", "black", "gold1", "navy", "purple")) +
  theme_clean()

plot_sp_treated

ggsave("imss_sp_0320_treated_labeled.png", plot_sp_treated, path = "plots", dpi = "retina", width = 13, height = 5.5)

# Plot: Control group only -----
df_control <- imss_sp %>%
  filter(trat == "0") %>%
  group_by(year, size_group) %>%
  summarise(mean_jobs = mean(jobs, na.rm = TRUE), .groups = "drop")

labels_control <- df_control %>%
  group_by(size_group) %>%
  filter(year == max(year))

plot_sp_control <- ggplot(df_control, aes(x = year, y = mean_jobs, color = size_group)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2011, linetype = 2) +
  geom_text(aes(x = 2010, y = 4500, label = "2011"), color = "red") +
  geom_vline(xintercept = 2016, linetype = 2) +
  geom_text(aes(x = 2015, y = 4800, label = "2016"), color = "green") +
  geom_text(data = labels_control,
            aes(label = size_group),
            hjust = -0.1, size = 3.5, show.legend = FALSE) +
  expand_limits(x = max(imss_sp$year) + 2) +
  labs(
    subtitle = "Control Group",
    x = "Year",
    y = "Average Employer Size",
    color = "Employer Size"
  ) +
  scale_color_manual(values = c("seagreen", "saddlebrown", "red", "black", "gold1", "navy", "purple")) +
  theme_clean()

plot_sp_control

ggsave("imss_sp_0320_control_labeled.png", plot_sp_control, path = "plots", dpi = "retina", width = 13, height = 5.5)

#### Combine all plots
combined_sp_plot <- plot_sp_all / plot_sp_treated / plot_sp_control

combined_sp_plot

ggsave("context_imss_sp_0320_labeled.png", combined_sp_plot, path = "plots", dpi = "retina", width = 13, height = 8.5)

# Plot: Average employment by group -----
imss_agro <- imss_se %>% 
  filter(sector == "Agriculture")

(ag_tg <- ggplot(data = imss_agro %>% 
                   group_by(year, trat) %>% 
                   summarise(seval_mean = mean(jobs, na.rm = TRUE)),
                 aes(year, seval_mean, color = trat)) +
    geom_line() +
    geom_vline(xintercept= 2011, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2011.5, label="2011", y=1100), angle=0, color = "red") +
    geom_vline(xintercept= 2016, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2015.5, label="2016", y=1100), angle=0, color = "green") +
    labs(x = "Year", y = "Average Employment", color = "Group") +
    scale_color_manual(labels = c("Non-Avocado Municipality",
                                  "Avocado Municipality"),
                       values = c("tan3",
                                  "seagreen")) +
    theme_clean()
)

ggsave("average_employment_group.png", ag_tg, path = "plots", dpi = "retina", width = 9, height = 4)
