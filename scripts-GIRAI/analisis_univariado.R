library(ggplot2)
library(dplyr)
#mng
# 1. Calculamos las estadísticas descriptivas previamente para usarlas en el gráfico
stats_mng <- datos %>%
  summarise(
    media = mean(mng, na.rm = TRUE),
    mediana = median(mng, na.rm = TRUE),
    desv_est = sd(mng, na.rm = TRUE)
  )

# 2. Generamos el gráfico base
ggplot(datos, aes(x = mng)) +
  
  # CAPA DE DISPERSIÓN: Sombreado de Media ± 1 Desviación Estándar
  # Lo ponemos de fondo para que no tape las barras
  annotate("rect", 
           xmin = stats_mng$media - stats_mng$desv_est, 
           xmax = stats_mng$media + stats_mng$desv_est, 
           ymin = 0, ymax = Inf, alpha = 0.15, fill = "royalblue") +
  
  # CAPA DEL HISTOGRAMA
  geom_histogram(binwidth = 5, fill = "#4e79a7", color = "white", alpha = 0.85) +
  
  
  # CAPA DE POSICIÓN: Línea de la Mediana
  geom_vline(xintercept = stats_mng$mediana, 
             color = "firebrick", linetype = "dashed", linewidth = 1) +
  
  # ETIQUETAS TÉCNICAS: Valores exactos en el gráfico
  annotate("text", x = stats_mng$mediana, y = max(table(cut(datos$mng, breaks=seq(0,100,5)))) * 0.95, 
           label = paste("Mediana:", round(stats_mng$mediana, 1)), 
           color = "firebrick", fontface = "bold", hjust = -0.1) +

  
  # Títulos y formato
  labs(
    title = "Distribución de Puntajes en Marcos Normativos (mng)\n por nivel. Año 2024",
    subtitle = "Análisis de tendencia central y dispersión global",
    x = "Puntaje mng (0 - 100)",
    y = "Frecuencia (Cantidad de Países)",
    caption = "Nota: El área sombreada representa Media ± 1 Desv. Estándar. \nLínea continua: Media | Línea discontinua: Mediana."
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(hjust = 0, color = "gray30"),
    panel.grid.minor = element_blank()
  )


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
    title = "Frecuencia de países por cantidad de areas temáticas reguladas en IA. Año 2024",
    subtitle = "Sombreado: Dispersión (Media ± 1 Desv. Estándar)",
    x = "Cantidad de Áreas Reguladas",
    y = "Número de Países (Frecuencia)"
  ) +
  scale_x_continuous(breaks = seq(0, max(datos$areas_mng, na.rm = TRUE), 1)) +
  theme_minimal()




#sec_mng
library(ggplot2)
library(dplyr)

# 1. Preparación y orden estricto de la variable ordinal
datos$sec_mng <- factor(datos$sec_mng, 
                        levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"),
                        ordered = TRUE)

# 2. Cálculos estadísticos para datos ordinales
sec_num <- as.numeric(datos$sec_mng)

q1_idx <- round(quantile(sec_num, 0.25, na.rm = TRUE))  
med_idx <- round(quantile(sec_num, 0.50, na.rm = TRUE)) 
q3_idx <- round(quantile(sec_num, 0.75, na.rm = TRUE))  

q1_cat <- levels(datos$sec_mng)[q1_idx]
med_cat <- levels(datos$sec_mng)[med_idx]
q3_cat <- levels(datos$sec_mng)[q3_idx]

# 3. Construcción del Gráfico con paleta divergente
ggplot(datos, aes(x = sec_mng, fill = sec_mng)) + # <-- Mapeamos el 'fill' a la variable
  
  # CAPA DE DISPERSIÓN: Sombreado del Rango Intercuartílico (Q1 a Q3)
  # Usamos un gris neutro para no interferir con la paleta semántica
  annotate("rect", 
           xmin = q1_idx - 0.45, 
           xmax = q3_idx + 0.45, 
           ymin = 0, ymax = Inf, alpha = 0.2, fill = "gray60") +
  
  # CAPA BASE: Gráfico de barras (quitamos el color fijo de aquí)
  geom_bar(color = "white", alpha = 0.9) +
  
  # APLICACIÓN DE LA PALETA: RdYlGn (Rojo a Verde)
  scale_fill_brewer(palette = "RdYlGn", guide = "none") + # guide = "none" quita la leyenda redundante
  
  # Añadimos las frecuencias exactas sobre cada barra para el rigor numérico
  geom_text(stat = "count", aes(label = after_stat(count)), 
            vjust = -1, size = 3.5, fontface = "bold", color = "black") +
  
  # Títulos y anotaciones técnicas
  labs(
    title = "Frecuencia de Países por Nivel de Madurez Normativa (sec_mng). Año 2024",
    subtitle = paste("Medida de posición: Moda en nivel '", med_cat, "'", sep=""),
    x = "Nivel de Desarrollo Normativo",
    y = "Cantidad de Países",
    caption = paste("Nota Técnica: El área sombreada gris representa la dispersión del 50% central\n",
                    "de los países (Rango Intercuartílico: de", q1_cat, "a", q3_cat, ").")
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.caption = element_text(hjust = 0, color = "gray40", face = "italic"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

#GIRAI_region
library(ggplot2)
library(dplyr)

# 1. Preparación de datos y cálculos estadísticos para variable nominal
resumen_region <- datos %>%
  count(GIRAI_region, name = "frecuencia") %>%
  arrange(desc(frecuencia)) # Ordenamos de mayor a menor para facilitar la lectura visual

# Cálculo de la Moda y el Total de observaciones
frec_moda <- resumen_region$frecuencia[1]
moda_cat <- resumen_region$GIRAI_region[1]
total_paises <- sum(resumen_region$frecuencia)

# Cálculo de la Dispersión (Razón de Variación)
# Fórmula: 1 - (Frecuencia de la Moda / Total de casos)
razon_variacion <- 1 - (frec_moda / total_paises)

# 2. Construcción del Gráfico
ggplot(resumen_region, aes(x = reorder(GIRAI_region, -frecuencia), y = frecuencia, 
                           # Usamos una condición lógica para resaltar la barra de la Moda
                           fill = GIRAI_region == moda_cat)) +
  
  geom_bar(stat = "identity", color = "white", alpha = 0.9) +
  
  # Añadimos las frecuencias numéricas sobre cada barra
  geom_text(aes(label = frecuencia), vjust = -0.8, size = 3.5, fontface = "bold") +
  
  # Estética: Resaltamos la Moda en rojo/fuego y el resto en un tono neutro (azul oscuro/grisáceo)
  scale_fill_manual(values = c("TRUE" = "#2c3e50", "FALSE" = "#2c3e50"), guide = "none") +
  
  # Títulos y anotaciones técnicas para el documento
  labs(
    title = "Frecuencia de Países por Región Global (GIRAI_region). Año 2024",
    subtitle = paste0("Medida de Posición (Moda): ", moda_cat, " (", frec_moda, " países)"),
    x = "Región Geográfica",
    y = "Cantidad de Países",
    caption = paste0("Nota Técnica: La dispersión geográfica se evalúa mediante la Razón de Variación (RV = ", 
                     round(razon_variacion, 2), ").\n",
                     "Una RV más alta indica una distribución más heterogénea entre los distintos continentes.")
  ) +
  
  # Ampliamos el eje Y para que los números no se corten
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.caption = element_text(hjust = 0, color = "gray40", face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), # Inclinamos el texto por si los nombres son largos
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
