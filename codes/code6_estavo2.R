# Green Gold Script: Avocado harvested
# Author: Vidal Mendoza-Tinoco
# Last date modified: 2025-05-01

# Libraries ------
library(dplyr)
library(ggplot2)
library(readxl)
library(ggthemes)
library(scales)
library(here)

# Load and prepare data -----
ag <- read_xlsx(here("data", "datos_limpios_mich_ag.xlsx"))

avocado <- ag %>%
  select(Year = Anio,
         State = Nomestado,
         Municipality = munsinac,
         Crop = Nomcultivo,
         Unit = Nomunidad,
         Harvested = Cosechada) %>%
  filter(Crop == "Aguacate") %>%
  mutate(across(c(State, Municipality, Crop, Unit), as.factor))

# Summarize total harvested area -----
df_avo <- avocado %>%
  group_by(Year) %>%
  summarise(total_harvested = sum(Harvested, na.rm = TRUE), .groups = "drop")

label_last <- df_avo %>%
  filter(Year == max(Year))

# Plot harvested hectares of avocado -----
plot_avo <- ggplot(df_avo, aes(x = Year, y = total_harvested)) +
  geom_line(color = "seagreen", size = 1) +
  geom_point(color = "seagreen") +
  geom_text(data = label_last, aes(label = scales::comma(total_harvested)),
            hjust = -0.1, color = "seagreen", size = 3.5) +
  geom_vline(xintercept = 2011, linetype = 2) +
  geom_text(aes(x = 2012, y = 150000, label = "2011"), color = "red") +
  expand_limits(x = max(df_avo$Year) + 2) +
  labs(
    x = "Year",
    y = "Harvested Area (hectares)",
    title = "Avocado Harvested Area in Michoacán (2003–2020)"
  ) +
  theme_clean()


plot_avo

ggsave("avo_harvest_0320_labeled.png", plot_avo, path = "plots", dpi = "retina",
       width = 9, height = 3.5)
