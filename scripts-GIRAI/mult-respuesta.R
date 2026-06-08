library(tidyverse)
library(janitor)

# Fijo el dataset
attach(datos)

# Respuesta múltiple
#p70_datpers, p70_suphum, p70_laboral, p70_segu, p70_transp

# Calcular cantidad total de cada variable
datos_cantidad <- datos %>% 
  summarise(across(c(p70_datpers, p70_suphum, p70_laboral, p70_segu, p70_transp),
                   ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), 
               names_to = "variable", 
               values_to = "cantidad") %>%
  arrange(cantidad) %>%
  mutate(variable = recode(variable,
                           "p70_datpers" = "Protección de Datos y\n Privacidad",
                           "p70_suphum" = "Supervisión Humana",
                           "p70_laboral" = "Protección Laboral y\n Derecho al Trabajo",
                           "p70_segu" = "Seguridad, Precisión y\n Fiabilidad",
                           "p70_transp" = "Transparencia y\n Explicabilidad"))
  

# Gráfico de barras
datos_cantidad %>%
  arrange(cantidad) %>%
  mutate(variable = factor(variable, levels = variable)) %>%
  ggplot(aes(x = cantidad, y = variable, fill = variable)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = cantidad), 
            hjust = -0.5, 
            size = 4) +
  labs(x = "Cantidad de paises", 
       y = "Áreas temáticas", 
       title = "Cantidad de paises con índices mayores \n a 70 puntos por categoría. Año 2024") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 11),
        legend.position = "none")
