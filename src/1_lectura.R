### Lectura ###

# Librerías ----
library(tidyverse)

# Lectura ----

# Backdoor hacia los archivos de LAPOP http://datasets.americasbarometer.org/database/files/

ab_raw <- haven::read_dta("./data/raw/Merged_LAPOP_AmericasBarometer_2021_v1.2_w.dta")

# Parece ser que las preguntas que inician con q corresponden a migración
ab_clean <- arg_raw %>%
  select(uniq_id, pais, year, nationality, estratopri, strata, upm, ur1new, q1tb, q2, wt, weight1500, a4, starts_with("q"))

# Mini EDA
ab_clean %>%
  group_by(pais) %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(cols = -pais, names_to = "variable", values_to = "NA_count") %>%
  ggplot(aes(NA_count, variable)) +
  geom_bar(stat = "identity")

# De acuerdo con el cuestionario, las preguntas de migración solo fueron obligatorias para México (1), Nicaragua (5), El Salvador (3), Honduras (4) y Guatemala (2)
# Acotando

ab_clean %>%
  filter(pais %in% c(1, 2, 3, 4, 5)) %>%
  group_by(pais) %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(cols = -pais, names_to = "variable", values_to = "NA_count") %>%
  ggplot(aes(NA_count, variable)) +
  geom_bar(stat = "identity") +
  facet_wrap(~pais)

