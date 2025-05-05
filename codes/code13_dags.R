# Green Gold Script: DAGs
# Author: Vidal Mendoza-Tinoco
# Last date modified: 2025-05-02

# Load libraries -----
library(tidyverse)
library(ggdag)
library(ggthemes)

# Available layouts: 'star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 'randomly', 'fr', 'kk', 'drl', 'lgl'

# DAG: Initial Hypothesis -----
hip_0 <- dagify(ineq ~ agric,
                labels = c("ineq" = "Inequality",
                           "agric" = "Agricultural\n Commerce"),
                outcome = "ineq",
                exposure = "agric",
                coords = list(x = c("agric" = 1, "ineq" = 3),
                              y = c("agric" = 2, "ineq" = 2)))

d_hip_0 <- ggdag_status(hip_0, text = FALSE, layout = "fr") +
  theme_dag_blank(legend.position = "none") +
  scale_color_manual(values = c("seagreen", "dodgerblue2")) +
  geom_dag_label_repel(aes(label = label), colour = "black", show.legend = FALSE) +
  geom_dag_edges_arc(edge_color = "orange", curvature = 0)

d_hip_0

ggsave("dag_hip0.png", d_hip_0, path = "plots", dpi = "retina", width = 7, height = 3.5)

## DAG: Literature Hypothesis 1
lit_1 <- dagify(ineq ~ val,
                val ~ prod + trade,
                prod ~ demand,
                trade ~ demand,
                demand ~ market,
                labels = c("ineq" = "Inequality",
                           "market" = "Market\nSize",
                           "demand" = "Demand",
                           "val" = "Crop\nValue",
                           "prod" = "Production",
                           "trade" = "Trade"),
                outcome = "ineq",
                exposure = "val",
                latent = "market",
                coords = list(x = c("market" = 1, "demand" = 2, "prod" = 3, "trade" = 3, "val" = 4, "ineq" = 5),
                              y = c("market" = 2, "demand" = 2, "prod" = 3, "trade" = 1, "val" = 2, "ineq" = 2)))

dag_l1 <- ggdag_status(lit_1, text = FALSE, check_overlap = TRUE, layout = "fr") +
  theme_dag_blank(legend.position = "none") +
  scale_color_manual(values = c("seagreen", "orange", "dodgerblue2")) +
  geom_dag_label_repel(aes(label = label), colour = "black", show.legend = FALSE)

dag_l1

ggsave("dag_lit1.png", dag_l1, path = "plots", dpi = "retina", width = 7, height = 3.5)

## DAG: Literature Summary
res_lit <- dagify(ineq ~ wages,
                  wages ~ trade,
                  trade ~ prod + access,
                  prod ~ modern,
                  labels = c("ineq" = "Inequality",
                             "wages" = "Wages",
                             "trade" = "Agricultural\n Commerce",
                             "prod" = "Production\nCapacity",
                             "access" = "Market\nAccess",
                             "modern" = "Modernization"),
                  outcome = "ineq",
                  exposure = "trade",
                  latent = "wages",
                  coords = list(x = c("modern" = 1, "prod" = 2, "access" = 2, "trade" = 3, "wages" = 4, "ineq" = 5),
                                y = c("modern" = 3, "prod" = 3, "access" = 1, "trade" = 2, "wages" = 2, "ineq" = 2)))

dag_rs <- ggdag_status(res_lit, text = FALSE, check_overlap = TRUE, layout = "fr") +
  theme_dag_blank(legend.position = "none") +
  scale_color_manual(values = c("seagreen", "orange", "dodgerblue2")) +
  geom_dag_label_repel(aes(label = label), colour = "black", show.legend = FALSE)

dag_rs

ggsave("dag_rs.png", dag_rs, path = "plots", dpi = "retina", width = 8, height = 3.5)
