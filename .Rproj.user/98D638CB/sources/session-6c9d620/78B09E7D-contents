#### Tesis #####
##### Codigo 5 (est desc imss)#####

#rm (list=ls ()) # para vaciar el workspace donde vas a trabajar

### llamando a las librerias 
library (dplyr)
library (ggplot2)
library (PanelMatch)
library (readstata13)
library (readxl)
library (Synth)
library (tidyverse)
library (tidyr)
library (viridis)
library (ggthemes)
library (lubridate)
library (scales)
library (stargazer)
library (patchwork)
library (treemap)
library (here)



###Cargando base de datos imss
imss <- read_csv(here("data", "imss_final_estdesc_year.csv"))

### Explorando
#summary(imss)

###cambiando variables a factores
imss <- imss %>% 
  mutate(mun = factor(mun),
         trat = factor(trat),
         pto = factor(pto))
#summary(imss)

### seleccionamos imss por sector economico
imss_se <- imss %>% 
  select(year,
         se1_0,
         se1_4,
         se1_6,
         se1_7,
         mun,
         trat,
         pto)
#summary(imss_se)

### cambiamos formato de base de datos
imss_se1 <- imss_se %>% 
  gather(key = "se",
         value = "value",
         se1_0,
         se1_4,
         se1_6,
         se1_7) %>% 
  mutate(se = factor(se))

#summary(imss_se1) ### quedo chula

### Ora si hacemos ggplot
(se <- ggplot(data = imss_se1 %>% 
                group_by(year, se) %>% 
                summarise(seval_mean = mean(value, na.rm = TRUE)),
              aes(year, seval_mean, color = se)) +
    geom_line()
  )

## la ponemos wapa
(se <- ggplot(data = imss_se1 %>% 
                group_by(year, se) %>% 
                summarise(seval_mean = mean(value, na.rm = TRUE)),
              aes(year, seval_mean, color = se)) +
    geom_line() +
    geom_vline(xintercept= 2011, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2012, label="2011", y=3500), angle=0, color = "red") +
    geom_vline(xintercept= 2016, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2015, label="2016", y=4000), angle=0, color = "green") +
    labs(#title = "Jobs by economic sector",
         x = "Año", y = "Promedio de Empleos", color = "Sector Económico") +
    scale_color_manual(labels = c("Agricultura, ganadería, silvicultura, pesca y caza",
                                  "Industria de la Construcción",
                                  "Comercio",
                                  "Transporte y Compunicaciones"),
                       values = c("seagreen",
                                  "saddlebrown",
                                  "purple",
                                  "navy")) +
    theme_clean()
)

ggsave("imss_se_0320_tyc_filter.png", se, path = "graficas", dpi = "retina", width = 11, height = 5)


### misma grafica pero solo tratados
(se_trat <- ggplot(data = imss_se1 %>% 
                     filter(trat == "1") %>% 
                group_by(year, se) %>% 
                summarise(seval_mean = mean(value, na.rm = TRUE)),
              aes(year, seval_mean, color = se)) +
    geom_line() +
    geom_vline(xintercept= 2011, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2010, label="2011", y=4000), angle=0, color = "red") +
    geom_vline(xintercept= 2016, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2015, label="2016", y=4000), angle=0, color = "green") +
    labs(#title = "Jobs by economic sector",
         subtitle = "Grupo de Tratamiento",
         x = "Año", y = "Promedio de Empleos", color = "Sector Económico") +
    scale_color_manual(labels = c("Agricultura, ganadería, silvicultura, pesca y caza",
                                  "Industria de la Construcción",
                                  "Comercio",
                                  "Transporte y Comunicaciones"),
                       values = c("seagreen",
                                  "saddlebrown",
                                  "purple",
                                  "navy")) +
    theme_clean()
)

ggsave("imss_se_0320_trat_filter.png", se_trat, path = "graficas", dpi = "retina", width = 11, height = 5)


### misma grafica pero solo grupo de control
(se_control <- ggplot(data = imss_se1 %>% 
                     filter(trat == "0") %>% 
                     group_by(year, se) %>% 
                     summarise(seval_mean = mean(value, na.rm = TRUE)),
                   aes(year, seval_mean, color = se)) +
    geom_line() +
    geom_vline(xintercept= 2011, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2010, label="2011", y=4000), angle=0, color = "red") +
    geom_vline(xintercept= 2016, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2015, label="2016", y=4000), angle=0, color = "green") +
    labs(#title = "Jobs by economic sector",
         subtitle = "Grupo de Control",
         x = "Año", y = "Promedio de Empleos", color = "Sector Económico") +
    scale_color_manual(labels = c("Agricultura, ganadería, silvicultura, pesca y caza",
                                  "Industria de la Construcción",
                                  "Comercio",
                                  "Transporte y Comunicaciones"),
                       values = c("seagreen",
                                  "saddlebrown",
                                  "purple",
                                  "navy")) +
    theme_clean()
)

ggsave("imss_se_0320_control_filter.png", se_control, path = "graficas", dpi = "retina", width = 11, height = 5)

###juntando plots
imss_se_plot <- (se / se_trat / se_control)

ggsave("context_imss_se_0320.png", imss_se_plot, path = "graficas", dpi = "retina", width = 10, height = 7)


### filtramos solo sector agropecuario y agrupamos por tratamiento
imss_agro <- imss_se1 %>% 
  filter(se == "se1_0")


## ploteamos
(ag_t <- ggplot(data = imss_agro %>% 
                group_by(year, trat) %>% 
                summarise(seval_mean = mean(value, na.rm = TRUE)),
              aes(year, seval_mean, color = trat)) +
    geom_line()
)


## la ponemos wapa
(ag_tg <- ggplot(data = imss_agro %>% 
                group_by(year, trat) %>% 
                summarise(seval_mean = mean(value, na.rm = TRUE)),
              aes(year, seval_mean, color = trat)) +
    geom_line() +
    geom_vline(xintercept= 2011, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2011.5, label="2011", y=1100), angle=0, color = "red") +
    geom_vline(xintercept= 2016, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2015.5, label="2016", y=1100), angle=0, color = "green") +
    labs(#title = "Agriculture, ranching, forestry, fishing and hunting jobs by treatment",
         x = "Año", y = "Promedio de Emplos", color = "Grupo") +
    scale_color_manual(labels = c("No Aguacatero",
                                  "Aguacatero"),
                       values = c("tan3",
                                            "seagreen")) +
                                              theme_clean()
)

ggsave("imss_agjobs_0320_tyc.png", ag_tg, path = "graficas", dpi = "retina", width = 9, height = 4)


### pasamos a rango salarial
imss_rs <- imss %>% 
  select(year,
         mun,
         trat,
         pto,
         rs_w1,
         rs_w2,
         rs_w3,
         rs_w4,
         rs_w5,
         rs_w6,
         rs_w7,
         rs_w8,
         rs_w9,
         rs_w10,
         rs_w11,
         rs_w12,
         rs_w13,
         rs_w14,
         rs_w15,
         rs_w16,
         rs_w17,
         rs_w18,
         rs_w19,
         rs_w20,
         rs_w21,
         rs_w22,
         rs_w23,
         rs_w24,
         rs_w25)

## cambiamos formato del df
imss_rs1 <- imss_rs %>% 
  gather(key = "rs",
         value = "value",
         rs_w1,
         rs_w2,
         rs_w3,
         rs_w4,
         rs_w5,
         rs_w6,
         rs_w7,
         rs_w8,
         rs_w9,
         rs_w10,
         rs_w11,
         rs_w12,
         rs_w13,
         rs_w14,
         rs_w15,
         rs_w16,
         rs_w17,
         rs_w18,
         rs_w19,
         rs_w20,
         rs_w21,
         rs_w22,
         rs_w23,
         rs_w24,
         rs_w25) %>% 
  mutate(rs = factor(rs))

#summary(imss_rs1) ### quedo chula

## creamos etiquetas
imss_rs1 <- imss_rs1 %>% 
  mutate(class_rs = case_when(rs == "rs_w1" ~ "1-5",
                              rs == "rs_w2" ~ "1-5",
                              rs == "rs_w3" ~ "1-5",
                              rs == "rs_w4" ~ "1-5",
                              rs == "rs_w5" ~ "1-5",
                              rs == "rs_w6" ~ "6-10",
                              rs == "rs_w7" ~ "6-10",
                              rs == "rs_w8" ~ "6-10",
                              rs == "rs_w9" ~ "6-10",
                              rs == "rs_w10" ~ "6-10",
                              rs == "rs_w11" ~ "11-15",
                              rs == "rs_w12" ~ "11-15",
                              rs == "rs_w13" ~ "11-15",
                              rs == "rs_w14" ~ "11-15",
                              rs == "rs_w15" ~ "11-15",
                              rs == "rs_w16" ~ "16-20",
                              rs == "rs_w17" ~ "16-20",
                              rs == "rs_w18" ~ "16-20",
                              rs == "rs_w19" ~ "16-20",
                              rs == "rs_w20" ~ "16-20",
                              rs == "rs_w21" ~ "21-25",
                              rs == "rs_w22" ~ "21-25",
                              rs == "rs_w23" ~ "21-25",
                              rs == "rs_w24" ~ "21-25",
                              rs == "rs_w25" ~ "21-25"),
         class_rs = factor(class_rs))
#summary(imss_rs1)


### Ora si hacemos ggplot
(rs <- ggplot(data = imss_rs1 %>% 
                group_by(year, class_rs) %>% 
                summarise(rsval_mean = mean(value, na.rm = TRUE)),
              aes(year, rsval_mean, color = class_rs)) +
    geom_line()
)

## la ponemos wapa
(rs <- ggplot(data = imss_rs1 %>% 
                group_by(year, class_rs) %>% 
                summarise(rsval_mean = mean(value, na.rm = TRUE)),
              aes(year, rsval_mean, color = class_rs)) +
    geom_line() +
    geom_vline(xintercept= 2011, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2012, label="2011", y=1000), angle=0, color = "red") +
    geom_vline(xintercept= 2016, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2017, label="2016", y=1000), angle=0, color = "green") +
    labs(#title = "Wages grouped by classes",
         x = "Año", y = "Salarios Promedio", color = "Salarios Mínimos") +
    theme_clean()
)

ggsave("imss_rs_0320_tyc.png", rs, path = "graficas", dpi = "retina",
       width = 10, height = 4)

###ahora solo tratados
summary(imss_rs1)

(rs_trat <- ggplot(data = imss_rs1 %>% 
                filter(trat == "1") %>% 
                group_by(year, class_rs) %>% 
                summarise(rsval_mean = mean(value, na.rm = TRUE)),
              aes(year, rsval_mean, color = class_rs)) +
    geom_line() +
    geom_vline(xintercept= 2011, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2012, label="2011", y=1000), angle=0, color = "red") +
    geom_vline(xintercept= 2016, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2017, label="2016", y=1000), angle=0, color = "green") +
    labs(#title = "Wages grouped by classes",
           subtitle = "Grupo de Tratamiento",
         x = "Año", y = "Salarios Promedio", color = "Salarios Mínimos") +
    theme_clean()
)

ggsave("imss_rs_0320_trat.png", rs_trat, path = "graficas", dpi = "retina",
       width = 10, height = 4)


###ahora solo control
summary(imss_rs1)

(rs_control <- ggplot(data = imss_rs1 %>% 
                     filter(trat == "0") %>% 
                     group_by(year, class_rs) %>% 
                     summarise(rsval_mean = mean(value, na.rm = TRUE)),
                   aes(year, rsval_mean, color = class_rs)) +
    geom_line() +
    geom_vline(xintercept= 2011, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2012, label="2011", y=1000), angle=0, color = "red") +
    geom_vline(xintercept= 2016, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2017, label="2016", y=1000), angle=0, color = "green") +
    labs(#title = "Wages grouped by classes",
         subtitle = "Grupo de Control",
         x = "Año", y = "Salarios Promedio", color = "Salarios Mínimos") +
    theme_clean()
)

ggsave("imss_rs_0320_control.png", rs_control, path = "graficas", dpi = "retina",
       width = 10, height = 4)

###juntando plots
imss_rs_plot <- (rs / rs_trat / rs_control)

ggsave("context_imss_rs_0320.png", imss_rs_plot, path = "graficas", dpi = "retina", width = 7, height = 7)


### ahora lo hacemos para size patron
summary(imss)

imss_sp <- imss %>% 
  select(year,
         mun,
         trat,
         pto,
         sp_1,
         sp_2,
         sp_3,
         sp_4,
         sp_5,
         sp_6,
         sp_7)

### cambiamos formato de la base de datos
imss_sp1 <- imss_sp %>% 
  gather(key = "sp",
         value = "value",
         sp_1,
         sp_2,
         sp_3,
         sp_4,
         sp_5,
         sp_6,
         sp_7) %>% 
  mutate(sp = factor(sp))
summary(imss_sp1)


### Ora si hacemos ggplot
(sp <- ggplot(data = imss_sp1 %>% 
                group_by(year, sp) %>% 
                summarise(spval_mean = mean(value, na.rm = TRUE)),
              aes(year, spval_mean, color = sp)) +
    geom_line()
)

## la ponemos wapa
(sp <- ggplot(data = imss_sp1 %>% 
                group_by(year, sp) %>% 
                summarise(spval_mean = mean(value, na.rm = TRUE)),
              aes(year, spval_mean, color = sp)) +
    geom_line() +
    geom_vline(xintercept= 2011, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2010, label="2011", y=4500), angle=0, color = "red") +
    geom_vline(xintercept= 2016, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2015, label="2016", y=4500), angle=0, color = "green") +
    labs(#title = "Employer registry size",
         x = "Año", y = "Promedio Tamaño Patronal", color = "Tamaño Patronal") +
    scale_color_manual(labels = c("1 empleo", 
                                  "2-5 empleos",
                                  "6-50 empleos",
                                  "51-250 empleos",
                                  "251-500 empleos",
                                  "501-1000 empleos",
                                  "Más de 1000 empleos"),
                       values = c("seagreen",
                                  "saddlebrown",
                                  "red",
                                  "black",
                                  "gold1",
                                  "navy",
                                  "purple")) +
    theme_clean()
)

ggsave("imss_sp_0320_tyc.png", sp, path = "graficas", dpi = "retina",
       width = 10, height = 5.5)

## solo tratados
(sp_trat <- ggplot(data = imss_sp1 %>% 
                filter(trat == "1") %>% 
                group_by(year, sp) %>% 
                summarise(spval_mean = mean(value, na.rm = TRUE)),
              aes(year, spval_mean, color = sp)) +
    geom_line() +
    geom_vline(xintercept= 2011, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2010, label="2011", y=4500), angle=0, color = "red") +
    geom_vline(xintercept= 2016, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2015, label="2016", y=4800), angle=0, color = "green") +
    labs(#title = "Employer registry size",
         subtitle = "Grupo de Tratamiento",
         x = "Año", y = "Promedio Tamaño Patronal", color = "Tamaño Patronal") +
    scale_color_manual(labels = c("1 empleo", 
                                  "2-5 empleos",
                                  "6-50 empleos",
                                  "51-250 empleos",
                                  "251-500 empleos",
                                  "501-1000 empleos",
                                  "Más de 1000 empleos"),
                       values = c("seagreen",
                                  "saddlebrown",
                                  "red",
                                  "black",
                                  "gold1",
                                  "navy",
                                  "purple")) +
    theme_clean()
)

ggsave("imss_sp_0320_trat.png", sp_trat, path = "graficas", dpi = "retina",
       width = 10, height = 5.5)

## solo control
(sp_control <- ggplot(data = imss_sp1 %>% 
                     filter(trat == "0") %>% 
                     group_by(year, sp) %>% 
                     summarise(spval_mean = mean(value, na.rm = TRUE)),
                   aes(year, spval_mean, color = sp)) +
    geom_line() +
    geom_vline(xintercept= 2011, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2010, label="2011", y=4500), angle=0, color = "red") +
    geom_vline(xintercept= 2016, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2015, label="2016", y=4800), angle=0, color = "green") +
    labs(#title = "Employer registry size",
         subtitle = "Grupo de Control",
         x = "Año", y = "Promedio Tamaño Patronal", color = "Tamaño Patronal") +
    scale_color_manual(labels = c("1 empleo", 
                                  "2-5 empleos",
                                  "6-50 empleos",
                                  "51-250 empleos",
                                  "251-500 empleos",
                                  "501-1000 empleos",
                                  "Más de 1000 empleos"),
                       values = c("seagreen",
                                  "saddlebrown",
                                  "red",
                                  "black",
                                  "gold1",
                                  "navy",
                                  "purple")) +
    theme_clean()
)

ggsave("imss_sp_0320_control.png", sp_control, path = "graficas", dpi = "retina",
       width = 10, height = 5.5)


###juntando los plots
imss_sp_plot <- (sp / sp_trat / sp_control)

ggsave("context_imss_sp_0320.png", imss_sp_plot, path = "graficas", dpi = "retina", width = 7, height = 8.5)

### explorando df con formato cambiado
imss_rs1
imss_se1
imss_sp1

### renombrando el value de cada df
imss_rs1 <- imss_rs1 %>% 
  rename(value_rs = value)

imss_se1 <- imss_se1 %>% 
  rename(value_se = value)

imss_sp1 <- imss_sp1 %>% 
  rename(value_sp = value)

imss_rs1
imss_se1
imss_sp1

### juntando df
imss_master <- left_join(imss_rs1, imss_se1, by = c("year", "mun", "trat", "pto"))
summary(imss_master)

imss_master <- left_join(imss_master, imss_sp1, by = c("year", "mun", "trat", "pto"))
summary(imss_master)

# ### bubble plot
# 
# ##prueba
# (bb <- ggplot(imss_master,
#               aes(x = value_sp, y = value_rs, size = value_se)) +
#     geom_point(alpha = 0.7)
#     )
# 
# 
# 
# ggplot(data, aes(x=gdpPercap, y=lifeExp, size = pop)) +
#   geom_point(alpha=0.7)















