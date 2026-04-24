
#mng
# Calculamos la mediana de antemano
mediana_mng <- median(datos$mng, na.rm = TRUE)

ggplot(datos, aes(x = mng)) +
  geom_histogram(binwidth = 5, fill = "#4e79a7", color = "white", alpha = 0.7) +
  
  # Añadimos la línea de la mediana
  geom_vline(xintercept = mediana_mng, 
             color = "firebrick", 
             linetype = "dashed", 
             linewidth = 1) +
  
  # Añadimos un texto que indique el valor exacto
  annotate("text", 
           x = mediana_mng + 3, # Lo desplazamos un poco a la derecha para que no tape la línea
           y = 15,               # Ajusta este valor según la altura de tus barras
           label = paste("Mediana:", round(mediana_mng, 2)), 
           color = "firebrick", 
           fontface = "bold",
           hjust = 0) +
  
  labs(
    title = "Distribución de Marcos Normativos con Referencia Central",
    subtitle = "La línea discontinua representa el valor central (Mediana)",
    x = "Puntaje mng",
    y = "Frecuencia"
  ) +
  theme_minimal()



#areas_mng
library(ggplot2)
library(dplyr)

# 1. Calculamos las frecuencias y las métricas de dispersión por separado
resumen_frecuencias <- datos %>%
  group_by(areas_mng) %>%
  summarise(frecuencia = n()) %>%
  ungroup()

media_val <- mean(datos$areas_mng, na.rm = TRUE)
sd_val <- sd(datos$areas_mng, na.rm = TRUE)
mediana_val <- median(datos$areas_mng, na.rm = TRUE)

# 2. Graficamos usando la tabla ya resumida
ggplot(resumen_frecuencias, aes(x = areas_mng, y = frecuencia)) +
  # El área sombreada de dispersión (±1 Desv. Estándar)
  annotate("rect", 
           xmin = media_val - sd_val, xmax = media_val + sd_val, 
           ymin = 0, ymax = Inf, alpha = 0.15, fill = "royalblue") +
  
  # Bastones: Ahora usamos x, xend, y, yend de forma explícita
  geom_segment(aes(xend = areas_mng, yend = 0), 
               color = "grey60", linewidth = 1) +
  
  # Puntos en la cima
  geom_point(color = "#2c3e50", size = 3.5) +
  
  # Línea de la Mediana
  geom_vline(xintercept = mediana_val, 
             color = "firebrick", linetype = "dashed", linewidth = 1) +
  
  # Etiquetas de texto
  annotate("text", x = mediana_val + 0.3, y = max(resumen_frecuencias$frecuencia) * 0.9, 
           label = paste("Mediana:", mediana_val), color = "firebrick", fontface = "bold", hjust = 0) +
  
  labs(
    title = "Diversidad Legislativa: Áreas Temáticas (areas_mng)",
    subtitle = "Sombreado: Dispersión (Media ± 1 Desv. Estándar)",
    x = "Cantidad de Áreas Reguladas",
    y = "Número de Países (Frecuencia)"
  ) +
  scale_x_continuous(breaks = seq(0, max(datos$areas_mng, na.rm = TRUE), 1)) +
  theme_minimal()


#sec_mng
library(ggplot2)
library(dplyr)

# 1. Preparación de la variable ordinal
datos$sec_mng <- factor(datos$sec_mng, 
                        levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"),
                        ordered = TRUE)

# 2. Cálculo de la Mediana
# Convertimos a numérico para hallar la posición central y luego volvemos al nombre del nivel
mediana_idx <- round(median(as.numeric(datos$sec_mng), na.rm = TRUE))
mediana_cat <- levels(datos$sec_mng)[mediana_idx]

# 3. Gráfico de Barras
ggplot(datos, aes(x = sec_mng, fill = sec_mng)) +
  geom_bar(color = "white", alpha = 0.8) +
  
  # Resaltamos la barra de la mediana con un borde o anotación
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 3.5) +
  
  # Colores secuenciales (de rojo/bajo a verde/alto)
  scale_fill_brewer(palette = "RdYlGn") +
  
  labs(
    title = "Frecuencia de Países por Nivel de Madurez Normativa",
    subtitle = paste("Medida de posición central (Mediana):", mediana_cat),
    x = "Nivel de Desarrollo (sec_mng)",
    y = "Cantidad de Países",
    caption = "Nota: La dispersión se analiza mediante la amplitud de la distribución."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )


#GIRAI_region
library(ggplot2)
library(dplyr)

# 1. Preparación de datos y cálculo de la moda
resumen_region <- datos %>%
  group_by(GIRAI_region) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

moda_region <- resumen_region$GIRAI_region[1]

# 2. Gráfico de Barras
ggplot(resumen_region, aes(x = reorder(GIRAI_region, -n), y = n, fill = GIRAI_region)) +
  geom_bar(stat = "identity", color = "white", alpha = 0.8) +
  
  # Etiquetas de valor sobre cada barra para precisión numérica
  geom_text(aes(label = n), vjust = -0.5, size = 3.5, fontface = "bold") +
  
  # Estética profesional
  scale_fill_viridis_d(option = "mako", guide = "none") + 
  labs(
    title = "Distribución de Países por Región (GIRAI_region)",
    subtitle = paste("Medida de posición (Moda):", moda_region),
    x = "Región",
    y = "Cantidad de Países",
    caption = "Nota: La dispersión se evalúa según la equidad en la representación regional."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotación para etiquetas largas
    panel.grid.major.x = element_blank()
  )