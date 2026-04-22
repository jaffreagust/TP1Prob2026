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



ggplot(datos, aes(x = GIRAI, y = GIRAI_region, fill = GIRAI_region)) +
  geom_density_ridges(alpha = 0.7, scale = 1.2, rel_min_height = 0.01) +
  geom_boxplot(width = 0.1, color = "white", outlier.shape = NA, alpha = 0.5) + # Boxplot embebido para ver medianas
  scale_fill_discrete(guide = "none") +
  labs(title = "Densidad del Índice GIRAI por Región",
       subtitle = "Detección de asimetrías y outliers regionales",
       x = "Puntaje GIRAI (0-100)",
       y = "Región") +
  theme_ridges()


ggplot(datos, aes(x = mng, y = ag)) +
  geom_point(aes(color = GIRAI_region), alpha = 0.6, size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") + # Línea de identidad (Referencia de coherencia)
  geom_smooth(method = "lm", color = "blue", fill = "lightgrey", alpha = 0.2) + # Tendencia global
  annotate("text", x = 80, y = 85, label = "Coherencia (y=x)", color = "red", angle = 45) +
  labs(title = "Relación entre Marcos Normativos (mng) y Acciones (ag)",
       subtitle = "Identificación de brechas de implementación (Gap Normativo-Ejecutivo)",
       x = "Puntaje Marcos Normativos (Teoría)",
       y = "Puntaje Acciones Gubernamentales (Práctica)",
       color = "Región") +
  theme_bw()