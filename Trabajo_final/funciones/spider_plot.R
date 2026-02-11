# -------------------------------------------------------------------------
# 0. LIBRERÍAS NECESARIAS
# -------------------------------------------------------------------------
library(fmsb)
library(tidyverse)
library(scales)

# -------------------------------------------------------------------------
# 1. FUNCIÓN PARA VARIABLES CUANTITATIVAS (Normalización Min-Max)
# -------------------------------------------------------------------------
plot_spider_quanti_norm <- function(df, vars, title = "Perfil Cuantitativo (Normalizado)") {
  
  # A. Calcular promedios por clúster
  df_means <- df %>%
    group_by(cluster) %>%
    summarise(across(all_of(vars), \(x) mean(x, na.rm = TRUE))) %>%
    column_to_rownames("cluster")
  
  # B. Obtener Mínimos y Máximos GLOBALES
  df_min_max <- df %>%
    summarise(across(all_of(vars), list(min = \(x) min(x, na.rm = TRUE), 
                                        max = \(x) max(x, na.rm = TRUE))))
  
  # C. Normalizar: (Valor - Min) / (Max - Min) -> Escala 0 a 1
  df_scaled <- df_means
  for(v in vars) {
    min_val <- df_min_max[[paste0(v, "_min")]]
    max_val <- df_min_max[[paste0(v, "_max")]]
    
    if(max_val == min_val) {
      df_scaled[[v]] <- 0.5 
    } else {
      df_scaled[[v]] <- (df_means[[v]] - min_val) / (max_val - min_val)
    }
  }
  
  # D. Estructura para fmsb (Tope=1, Piso=0)
  df_plot <- rbind(rep(1, length(vars)), 
                   rep(0, length(vars)), 
                   df_scaled)
  
  # E. Configuración Visual
  colores <- c("#1B9E77", "#D95F02", "#7570B3") 
  colores_relleno <- scales::alpha(colores, 0.2)
  
  radarchart(df_plot, 
             pcol = colores, 
             pfcol = colores_relleno, 
             plwd = 2, 
             plty = 1,               # <--- CAMBIO AQUÍ: 1 = Línea Continua Sólida
             cglcol = "grey80", 
             cglty = 1,              # Tipo de línea de la red (1 = continua)
             axislabcol = "grey40", 
             caxislabels = c("Min", "", "Med", "", "Max"), 
             cglwd = 0.8, 
             vlcex = 0.6, 
             title = title)
  
  legend(x = "bottomright", legend = paste("Clúster", rownames(df_means)), 
         col = colores, lwd = 3, bty = "n", cex = 0.8)
}

# -------------------------------------------------------------------------
# 2. FUNCIÓN PARA VARIABLES CUALITATIVAS (Porcentaje de Presencia)
# -------------------------------------------------------------------------
plot_spider_quali_perc <- function(df, vars, title = "Prevalencia de Factores (%)") {
  
  # A. Calcular porcentaje de 'Sí' por clúster
  df_perc <- df %>%
    group_by(cluster) %>%
    summarise(across(all_of(vars), \(x) mean(as.character(x) == "Sí", na.rm = TRUE) * 100)) %>%
    column_to_rownames("cluster")
  
  # B. Estructura para fmsb (Tope=100%, Piso=0%)
  df_plot <- rbind(rep(100, length(vars)), 
                   rep(0, length(vars)), 
                   df_perc)
  
  # C. Configuración Visual
  colores <- c("#1B9E77", "#D95F02", "#7570B3")
  colores_relleno <- scales::alpha(colores, 0.2)
  
  radarchart(df_plot, 
             pcol = colores, 
             pfcol = colores_relleno, 
             plwd = 2, 
             plty = 1,               # <--- CAMBIO AQUÍ: 1 = Línea Continua Sólida
             cglcol = "grey80", 
             cglty = 1, 
             axislabcol = "grey40", 
             caxislabels = seq(0, 100, 25), 
             cglwd = 0.8, 
             vlcex = 0.7, 
             title = title)
  
  legend(x = "bottomright", legend = paste("Clúster", rownames(df_perc)), 
         col = colores, lwd = 3, bty = "n", cex = 0.8)
}