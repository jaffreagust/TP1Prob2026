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


# Calculamos la media global para usarla como referencia técnica
media_global <- mean(datos$GIRAI, na.rm = TRUE)

datos %>%
  ggplot(aes(x = GIRAI, fill = GIRAI_region)) +
  # Usamos histogramas con densidad superpuesta para rigor técnico
  geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.4, color = "white") +
  geom_density(size = 0.8) +
  
  # Línea de referencia: El benchmark global
  geom_vline(xintercept = media_global, linetype = "dashed", color = "red", size = 0.6) +
  
  # La clave: Facetado por región
  facet_wrap(~GIRAI_region, ncol = 4, axes = "all",labeller = label_wrap_gen(width = 15)) + 
  
  # Estética y anotaciones
  scale_fill_viridis_d(guide = "none") + # El color ya está implícito en el título del facet
  labs(
    title = "Distribución Regional del Índice GIRAI",
    subtitle = "Comparativa individual por región vs. Media Global (línea roja)",
    x = "Puntaje GIRAI (0-100)",
    y = "Densidad de Probabilidad"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "grey95", color = NA), # Fondo de los títulos de cada cuadro
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines") # Espaciado para que no se vea amontonado
  )


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