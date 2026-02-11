analisis_cuali_robusto <- function(df, vars, grupo = "cluster") {
  
  map_dfr(vars, function(v) {
    
    # 1. LIMPIEZA Y CONVERSIÓN FUERTE
    # Convertimos a character y luego a factor para eliminar niveles fantasmas
    # y asegurar que R entienda que es categórica.
    datos_limpios <- df %>%
      select(Var = all_of(v), Grupo = all_of(grupo)) %>%
      mutate(
        Var = as.factor(as.character(Var)),
        Grupo = as.factor(as.character(Grupo))
      ) %>%
      na.omit() 
    
    # 2. FILTRO DE SEGURIDAD (El paso clave que faltaba)
    # Si la variable es constante (solo 1 respuesta posible) o el grupo es constante
    # NO se puede hacer Chi-Cuadrado ni Fisher. Retornamos aviso.
    if(n_distinct(datos_limpios$Var) < 2 || n_distinct(datos_limpios$Grupo) < 2) {
      return(data.frame(
        Variable = v,
        Prueba = "No Calculable (Constante)",
        P_Valor = NA,
        Significativo = "No",
        V_Cramer = 0,
        Magnitud = "N/A"
      ))
    }
    
    # 3. CREAR TABLA
    tabla <- table(datos_limpios$Var, datos_limpios$Grupo)
    
    # Validar dimensión mínima de la tabla (debe ser al menos 2x2 para asociación)
    if(nrow(tabla) < 2 || ncol(tabla) < 2) {
      return(data.frame(Variable = v, Prueba = "Tabla < 2x2", P_Valor = NA, Significativo = "No", V_Cramer = 0, Magnitud = "N/A"))
    }
    
    # 4. PRUEBA EXACTA DE FISHER (Con manejo de errores)
    test_res <- tryCatch({
      fisher.test(tabla, simulate.p.value = TRUE, B = 2000)
    }, error = function(e) {
      # Si falla Fisher, intentamos Chi2, y si falla Chi2, devolvemos NA
      tryCatch(chisq.test(tabla, correct = TRUE), error = function(e2) list(p.value = NA, method = "Error Matemático"))
    })
    
    # 5. CÁLCULO SEGURO DE V DE CRAMER
    cramer <- tryCatch({
      chi_stat <- suppressWarnings(chisq.test(tabla)$statistic)
      n <- sum(tabla)
      k <- min(dim(tabla)) - 1
      sqrt(chi_stat / (n * k))
    }, error = function(e) 0) # Si falla el cálculo, asumimos efecto 0
    
    # 6. INTERPRETACIÓN
    mag <- case_when(
      cramer < 0.10 ~ "Insignificante",
      cramer < 0.30 ~ "Pequeño",
      cramer < 0.50 ~ "Mediano",
      TRUE ~ "Grande"
    )
    
    data.frame(
      Variable = v,
      Prueba = test_res$method,
      P_Valor = test_res$p.value,
      Significativo = ifelse(!is.na(test_res$p.value) & test_res$p.value < 0.05, "SÍ", "No"),
      V_Cramer = round(as.numeric(cramer), 3),
      Magnitud = mag
    )
  })
}