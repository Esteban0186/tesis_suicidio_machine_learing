homocedasticidad <- function(df, vars, grupo = "cluster") {
  
  resultados <- map_dfr(vars, function(v) {
    # Fórmula dinámica
    formula_test <- as.formula(paste(v, "~", grupo))
    
    # Cálculo de Levene (center = median es la clave para robustez)
    levene_res <- tryCatch(
      leveneTest(formula_test, data = df, center = median),
      error = function(e) NULL
    )
    
    # Extraer valor p
    p_valor <- if(!is.null(levene_res)) levene_res$`Pr(>F)`[1] else NA
    
    # Interpretación
    cumple <- ifelse(!is.na(p_valor) & p_valor > 0.05, "Sí", "No")
    metodo <- ifelse(cumple == "Sí", "ANOVA", "Kruskal-Wallis")
    
    data.frame(
      Variable = v,
      Valor_p = p_valor,
      Homocedasticidad = cumple,
      Metodo_Sugerido = metodo
    )
  })
  
  return(resultados)
}

# -------------------------------------------------------------------------
# FUNCIÓN: NORMALIDAD (Shapiro-Wilk en Residuos)
# -------------------------------------------------------------------------

normalidad <- function(df, vars, grupo = "cluster") {
  
  resultados <- map_dfr(vars, function(v) {
    
    # 1. Ajustar el modelo (ANOVA teórico)
    formula_modelo <- as.formula(paste(v, "~", grupo))
    modelo <- lm(formula_modelo, data = df)
    
    # 2. Extraer residuos
    residuos <- residuals(modelo)
    
    # 3. Test de Shapiro-Wilk
    shapiro_res <- tryCatch(
      shapiro.test(residuos),
      error = function(e) NULL
    )
    
    p_valor <- if(!is.null(shapiro_res)) shapiro_res$p.value else NA
    
    # 4. Interpretación
    # Si p > 0.05 -> Es Normal
    # Si p < 0.05 -> No es Normal
    es_normal <- ifelse(!is.na(p_valor) & p_valor > 0.05, "Sí (Normal)", "No (No Normal)")
    
    data.frame(
      Variable = v,
      W_Statistic = if(!is.null(shapiro_res)) round(shapiro_res$statistic, 4) else NA,
      P_Valor = p_valor,
      Distribucion = es_normal
    )
  })
  
  return(resultados)
}

# -------------------------------------------------------------------------
# 1. FUNCIÓN: ANOVA (Específica para Edad)
# -------------------------------------------------------------------------
anova <- function(df, grupo = "cluster") {
  
  # A. Ajustar modelo ANOVA
  # Usamos 'edad' directo porque es la única variable normal
  f <- as.formula(paste("edad ~", grupo))
  modelo <- aov(f, data = df)
  resumen <- summary(modelo)[[1]]
  
  # B. Extraer Estadísticos
  f_stat <- resumen[["F value"]][1]
  p_val <- resumen[["Pr(>F)"]][1]
  
  # C. CÁLCULO MANUAL DE ETA CUADRADO (A prueba de errores)
  # Fórmula: Suma de Cuadrados Entre Grupos / Suma Total
  ss_entre <- resumen[["Sum Sq"]][1]
  ss_total <- sum(resumen[["Sum Sq"]])
  eta2 <- ss_entre / ss_total
  
  # D. Interpretación
  mag <- case_when(
    eta2 < 0.01 ~ "Insignificante",
    eta2 < 0.06 ~ "Pequeño",
    eta2 < 0.14 ~ "Mediano",
    TRUE ~ "Grande"
  )
  
  # E. Retornar Fila
  data.frame(
    Variable = "edad",
    Prueba = "ANOVA (Paramétrico)",
    Estadistico = round(f_stat, 2),
    P_Valor = p_val,
    Significativo = ifelse(p_val < 0.05, "SÍ", "No"),
    Tamano_Efecto = round(eta2, 3),
    Magnitud = mag
  )
}

# -------------------------------------------------------------------------
# 2. FUNCIÓN: KRUSKAL (Para el resto de variables)
# -------------------------------------------------------------------------
kruskal <- function(df, vars, grupo = "cluster") {
  
  map_dfr(vars, function(v) {
    
    # A. Fórmula dinámica
    f <- as.formula(paste(v, "~", grupo))
    
    # B. Test Kruskal-Wallis
    test <- kruskal.test(f, data = df)
    
    # C. Tamaño del Efecto (Epsilon cuadrado - rstatix)
    efecto <- df %>% kruskal_effsize(f)
    
    # D. Interpretación
    mag <- case_when(
      efecto$effsize < 0.01 ~ "Insignificante",
      efecto$effsize < 0.06 ~ "Pequeño",
      efecto$effsize < 0.14 ~ "Mediano",
      TRUE ~ "Grande"
    )
    
    data.frame(
      Variable = v,
      Prueba = "Kruskal-Wallis (No Paramétrico)",
      Estadistico = round(test$statistic, 2),
      P_Valor = test$p.value,
      Significativo = ifelse(test$p.value < 0.05, "SÍ", "No"),
      Tamano_Efecto = round(efecto$effsize, 3),
      Magnitud = mag
    )
  })
}

post_hoc_anova <- function(df, var = "edad", grupo = "cluster") {
  
  # 1. Ajustar Modelo ANOVA
  f <- as.formula(paste(var, "~", grupo))
  modelo <- aov(f, data = df)
  
  # 2. Prueba de Tukey
  tukey <- TukeyHSD(modelo)
  
  # 3. Formatear Resultados
  # Convertimos la salida compleja de Tukey en una tabla simple
  tabla <- as.data.frame(tukey[[grupo]]) %>%
    rownames_to_column("Comparacion") %>%
    rename(
      Diferencia = diff,
      P_Ajustado = `p adj`
    ) %>%
    mutate(
      Variable = var,
      Prueba = "Tukey HSD",
      Significativo = ifelse(P_Ajustado < 0.05, "SÍ", "No")
    ) %>%
    select(Variable, Comparacion, Diferencia, P_Ajustado, Significativo)
  
  return(tabla)
}

post_hoc_kruskal <- function(df, vars, grupo = "cluster") {
  
  map_dfr(vars, function(v) {
    
    # 1. Fórmula
    f <- as.formula(paste(v, "~", grupo))
    
    # 2. Test de Dunn (Post-hoc no paramétrico)
    # Usamos Bonferroni para ser rigurosos y evitar falsos positivos
    dunn <- df %>% 
      dunn_test(f, p.adjust.method = "bonferroni")
    
    # 3. Formatear Resultados
    dunn %>%
      mutate(
        Comparacion = paste(group1, group2, sep = " - "),
        Significativo = ifelse(p.adj < 0.05, "SÍ", "No")
      ) %>%
      select(
        Variable = .y.,
        Comparacion,
        Z_Statistic = statistic,
        P_Ajustado = p.adj,
        Significativo
      )
  })
}