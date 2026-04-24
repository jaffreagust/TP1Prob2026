library(tidyverse)
library(ggridges)
library(gridExtra)

# Ordenamos los niveles de la variable ordinal para que el gráfico tenga sentido lógico
datos <- datos %>%
  mutate(sec_mng = factor(sec_mng, 
                          levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"),
                          ordered = TRUE))

datos %>%
  ggplot(aes(x = GIRAI_region, fill = sec_mng)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(option = "mako", name = "Nivel Normativo") + # Escala perceptualmente uniforme
  labs(title = "Distribución de Madurez Normativa por Región",
       subtitle = "Análisis de composición relativa (Normalizado al 100%)",
       x = "Región GIRAI",
       y = "Proporción de Países") +
  theme_minimal() +
  coord_flip() # Facilita la lectura de los nombres de las regione


#Girai_region vs Girai

library(ggplot2)
library(ggridges)


ggplot(datos, aes(x = reorder(GIRAI_region, GIRAI, FUN = median), y = GIRAI, fill = GIRAI_region)) +
  geom_boxplot(alpha = 0.7) +
  coord_flip() + # Horizontal para leer mejor los nombres de las regiones
  labs(title = "Comparativa del Índice GIRAI por Región",
       x = "Región (Ordenada por Mediana)",
       y = "Puntaje GIRAI") +
  theme_minimal() +
  theme(legend.position = "none")


#mng vs ag

ggplot(datos, aes(x = mng, y = ag)) +
  geom_point(aes(color = GIRAI_region), alpha = 0.6, size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") + # Línea de identidad (Referencia de coherencia)
  geom_smooth(method = "lm", color = "blue", fill = "lightgrey", alpha = 0.2) + # Tendencia global
  annotate("text", x = 70, y = 75, label = "Coherencia (y=x)", color = "red", angle = 45) +
  labs(title = "Relación entre Marcos Normativos (mng) y Acciones (ag)",
       subtitle = "Identificación de brechas de implementación (Gap Normativo-Ejecutivo)",
       x = "Puntaje Marcos Normativos (Teoría)",
       y = "Puntaje Acciones Gubernamentales (Práctica)",
       color = "Región") +
  theme_bw()