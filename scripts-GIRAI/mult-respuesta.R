library(tidyverse)
library(janitor)

# Fijo el dataset
attach(datos)

# Respuesta múltiple
#p70_datpers, p70_suphum, p70_laboral, p70_segu, p70_transp

tabla <- datos %>%
  group_by(GIRAI_region) %>%
  summarise(
    across(c(p70_datpers, p70_suphum, p70_laboral, p70_segu, p70_transp),
           ~sum(.x, na.rm = TRUE)),
    ninguna = sum(ifelse(p70_datpers + p70_suphum + p70_laboral + p70_segu + p70_transp == 0, 1, 0)),
    total_region = n()
  ) %>%
  pivot_longer(cols = c(p70_datpers, p70_suphum, p70_laboral, p70_segu, p70_transp, ninguna),
               names_to = "variable",
               values_to = "cant") %>%
  group_by(GIRAI_region) %>%
  mutate(
    porcentaje = paste0(round(cant / total_region * 100, 2), "%")
  ) %>%
  arrange(GIRAI_region, desc(cant)) %>%
  select(-total_region)  # Opcional: eliminar columna total_region