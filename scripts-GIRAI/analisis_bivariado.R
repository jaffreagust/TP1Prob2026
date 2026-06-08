library(ggplot2)
library(dplyr)
library(scales)

# 1. Asegurar la jerarquía ordinal de la variable
datos$sec_mng <- factor(datos$sec_mng, 
                        levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"),
                        ordered = TRUE)

# 2. Calcular frecuencias y porcentajes relativos por región
resumen_composición <- datos %>%
  group_by(GIRAI_region, sec_mng) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(GIRAI_region) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = ifelse(porcentaje >= 0.05, scales::percent(porcentaje, accuracy = 1), "")
  ) %>%
  ungroup()

# 3. EXTRAER EL ORDEN (Ranking de Excelencia Normativa)
# Calculamos qué porcentaje de nivel "Muy alto" tiene cada región
orden_excelencia <- resumen_composición %>%
  group_by(GIRAI_region) %>%
  # Sumamos el porcentaje solo donde sec_mng es "Muy alto". 
  # Si una región no tiene países en este nivel, la suma devolverá 0 de forma segura.
  summarise(pct_muy_alto = sum(porcentaje[sec_mng == "Muy alto"])) %>% 
  # Ordenamos de menor a mayor (porque coord_flip invierte el orden visual, 
  # dejando al mayor en la parte superior del gráfico)
  arrange(pct_muy_alto) %>% 
  pull(GIRAI_region)

# Aplicamos este nuevo orden estricto a la variable regional
resumen_composición$GIRAI_region <- factor(resumen_composición$GIRAI_region, 
                                           levels = orden_excelencia)

# 4. Construcción del gráfico
ggplot(resumen_composición, aes(x = GIRAI_region, y = porcentaje, fill = sec_mng)) +
  geom_bar(stat = "identity", position = "fill", color = "white", alpha = 0.9, width = 0.7) +
  
  geom_text(aes(label = etiqueta), 
            position = position_fill(vjust = 0.5), 
            size = 3.5, color = "gray10", fontface = "bold") +
  
  scale_fill_brewer(palette = "RdYlGn", name = "Nivel de Madurez") +
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Distribución de Madurez Normativa por Región\n establecida por el índice GIRAI",
    subtitle = "Análisis de composición relativa (Normalizado al 100%)",
    x = "Región",
    y = "Proporción de Países"
  ) +
  
  coord_flip() + 
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor = element_blank()
  )

#Girai_region vs Girai

library(ggplot2)
library(ggridges)


ggplot(datos, aes(x = reorder(GIRAI_region, GIRAI, FUN = median), y = GIRAI, fill = GIRAI_region)) +
  geom_boxplot(alpha = 0.7) +
  coord_flip() + # Horizontal para leer mejor los nombres de las regiones
  labs(title = "Comparativa de valor del Índice GIRAI por Región\n definida por el índice GIRAI",
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
  labs(title = "Relación entre Marcos Normativos (mng) y Acciones Gubernamentales (ag).",
       subtitle = "",
       x = "Puntaje Marcos Normativos (Teoría)",
       y = "Puntaje Acciones Gubernamentales (Práctica)",
       color = "Región") +
  theme_bw()