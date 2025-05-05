# Green Gold Script: Regression with Education Heterogeneity
# Author: Vidal Mendoza-Tinoco
# Last date modified: 2025-05-01

# Libraries -----
library(dplyr)
library(readr)
library(readxl)
library(lfe)
library(modelsummary)
library(here)

# Load and data cleaning -----
ed <- read_csv(here("data", "df_ed_2005_mun.csv"))
df <- read_csv(here("data", "df_final_general_year.csv"))

## change municipality names
ed_sa <- ed %>%
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

## join
df_ul <- left_join(df, ed_sa, by = c("mun" = "nom_mun"))

## variables to factor
df_ul <- df_ul %>%
  mutate(
    mun     = factor(mun),
    pto     = factor(pto),
    trat_2  = factor(trat_2),
    upper   = factor(upper),
    lower   = factor(lower)
  )


# Modeling -----

## Heterogeneous effects model
mod_ef_al <- felm(
  gini_mean ~ trat_2 * pto * upper | 0 | 0 | mun,
  data = df_ul
)

mod_ef_al


## export Table
modelsummary(
  mod_ef_al,
  coef_map = c(
    "(Intercept)"           = "Intercept",
    "trat_21"               = "Treated",
    "pto1"                  = "Post",
    "upper1"                = "High education",
    "trat_21:pto1"          = "Treated x Post",
    "trat_21:upper1"        = "Treated x High education",
    "pto1:upper1"           = "Post x High education",
    "trat_21:pto1:upper1"   = "Treated x Post x High education"
  ),
  gof_omit = "IC|Log|F|Std",
  output = here("tables", "table5_reg_heteff_rename.html"),
  notes = list(
    "Robust standard errors clustered by municipality.",
    "Significance levels: * p < 0.10, ** p < 0.05, *** p < 0.01."
  )
)
