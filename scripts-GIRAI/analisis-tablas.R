# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("janitor")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(janitor)

# Fijo el dataset
attach(datos)

tabla_region <- tabyl(datos, GIRAI_region)

tabla_region %>% 
  arrange(desc(n)) %>%
  rename(
    "Región" = GIRAI_region,
    "Cant. paises" = n,
    "% paises" = percent
  ) %>% 
  adorn_totals() %>%
  adorn_pct_formatting(digits = 2)