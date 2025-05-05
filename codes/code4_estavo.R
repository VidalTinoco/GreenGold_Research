# Green Gold Script: Crop Descriptive Statistics
# Author: Vidal Mendoza-Tinoco
# Last date modified: 2025-04-30

# Libraries -----
library(dplyr)
library(ggplot2)
library(readxl)
library(ggthemes)
library(here)
library(patchwork)

# Load agricultural dataset -----
ag <- read_xlsx(here("data", "datos_limpios_mich_ag.xlsx"))

ag <- ag %>%
  select(Year = Anio,
         State = Nomestado,
         Municipality = munsinac,
         Crop = Nomcultivo,
         Unit = Nomunidad,
         Volume = Volumenproduccion,
         Price = Precio,
         Value = Valorproduccion) %>%
  mutate(across(c(State, Municipality, Crop, Unit), as.factor))

# Top 10 crops by total production value (2003–2020) -----
ag_sum <- ag %>%
  group_by(Crop) %>%
  mutate(total_value = sum(Value, na.rm = TRUE)) %>%
  ungroup()

top10_crops <- ag_sum %>%
  group_by(Crop) %>%
  slice_max(total_value, n = 1, with_ties = FALSE) %>%
  arrange(desc(total_value)) %>%
  head(10)

top10_table <- top10_crops %>%
  select(Crop, Unit, total_value)

print(top10_table)

# Plot: Value of top 3 crops over time -----
top3_crops <- c("Aguacate", "Maíz grano", "Zarzamora")

plot_top3 <- ag %>%
  filter(Crop %in% top3_crops) %>%
  group_by(Year, Crop) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Crop = recode(Crop,
                       "Aguacate" = "Avocado",
                       "Maíz grano" = "Corn (grain)",
                       "Zarzamora" = "Blackberry")) %>%
  ggplot(aes(x = Year, y = Value, color = Crop)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 2011, linetype = 2) +
  geom_text(aes(x = 2012, y = 28000000000, label = "2011"), color = "red") +
  labs(
    x = "Year", y = "Production Value (MXN)",
    color = "Crop"
  ) +
  scale_color_manual(
    values = c("Avocado" = "seagreen", "Corn (grain)" = "gold2", "Blackberry" = "darkorchid1")
  ) +
  theme_clean()

plot_top3

ggsave("valorprod_top3crops.png", plot_top3, path = "plots", dpi = "retina", width = 9, height = 3.5)

# Focus on avocado -----
avocado <- ag %>% filter(Crop == "Aguacate")

## Plot 1: Production value
plot_value <- avocado %>%
  group_by(Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Year, y = Value)) +
  geom_point(color = "seagreen") +
  geom_line(color = "seagreen") +
  geom_vline(xintercept = 2011, linetype = 2) +
  geom_text(aes(x = 2012, y = 28000000000, label = "2011"), color = "red") +
  labs(x = "Year", y = "Avocado Production Value (MXN)") +
  theme_clean()

plot_value

## Plot 2: Production volume
plot_volume <- avocado %>%
  group_by(Year) %>%
  summarise(Volume = sum(Volume, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Year, y = Volume)) +
  geom_point(color = "seagreen") +
  geom_line(color = "seagreen") +
  geom_vline(xintercept = 2011, linetype = 2) +
  geom_text(aes(x = 2012, y = 1500000, label = "2011"), color = "red") +
  labs(x = "Year", y = "Avocado Production Volume (tons)") +
  theme_clean()

plot_volume

## Plot 3: Price
plot_price <- avocado %>%
  group_by(Year) %>%
  summarise(Price = mean(Price, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Year, y = Price)) +
  geom_point(color = "seagreen") +
  geom_line(color = "seagreen") +
  geom_vline(xintercept = 2011, linetype = 2) +
  geom_text(aes(x = 2012, y = 17000, label = "2011"), color = "red") +
  labs(x = "Year", y = "Average Price (MXN/ton)") +
  theme_clean()

plot_price

## Combine avocado plots
plot_avo <- plot_value / plot_volume / plot_price

plot_avo

ggsave("context_avo_0320.png", plot_avo, path = "plots", dpi = "retina", width = 7, height = 7)
