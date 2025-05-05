# Green Gold Script: Exports stats
# Author: Vidal Mendoza-Tinoco
# Last date modified: 2025-05-01

# Libraries -----
library(dplyr)
library(ggplot2)
library(readxl)
library(ggthemes)
library(scales)
library(lubridate)
library(here)

# Load data and clean -----
exports <- read_xlsx(here("data", "Exp_Ag_clean.xlsx"))

exports <- exports %>%
  mutate(Date = as_date(fecha))

latest_label <- exports %>%
  filter(Date == max(Date))

# Plot value of avocado exports -----
plot_exports <- ggplot(exports, aes(x = Date, y = miles_dolares)) +
  geom_line(color = "seagreen", linewidth = 1) +
  geom_text(aes(x = ymd("2012-05-02"), y = 300000, label = "2011"),
            color = "red") +
  geom_vline(xintercept = ymd("2011-01-01"), linetype = 2) +
  geom_text(data = latest_label,
            aes(label = scales::comma(miles_dolares)), 
            hjust = -0.1, color = "seagreen", size = 3.5) +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "2 years") +
  expand_limits(x = max(exports$Date) + 150) +
  labs(
    x = "Date",
    y = "Thousands of USD",
    title = "Monthly Avocado Export Value from Mexico"
  ) +
  theme_clean()

plot_exports

ggsave("exp_avo_mex.png", plot_exports, path = "plots", dpi = "retina",
       width = 9.5, height = 3.5)
