# ============================================================================
# Análisis de datos faltantes
# ENDES 2024 | Diagnóstico descriptivo y Test de Little (MCAR)
# ============================================================================

#' Análisis de datos faltantes
#'
#' @param data data.frame o data.table
#' @param cols vector de nombres de variables a evaluar
#' @param hacer_grafico TRUE/FALSE
#' @param hacer_little TRUE/FALSE
#' @return Lista con tabla_missing y little_test
analisis_missingness <- function(
    data,
    cols = names(data),
    hacer_grafico = TRUE,
    hacer_little = TRUE
) {
  
  # 1. Selección de variables ------------------------------------------------
  datos <- data[, ..cols]
  
  # 2. Conteo de faltantes ---------------------------------------------------
  n_missing <- colSums(is.na(datos))
  
  # 3. Porcentaje de faltantes -----------------------------------------------
  pct_missing <- round(colMeans(is.na(datos)) * 100, 2)
  
  # 4. Tabla resumen ---------------------------------------------------------
  tabla_missing <- data.frame(
    Variable = names(n_missing),
    N_Faltantes = as.numeric(n_missing),
    Pct_Faltantes = as.numeric(pct_missing)
  )
  
  tabla_missing <- tabla_missing[
    order(-tabla_missing$Pct_Faltantes),
  ]
  
  cat("\n")
  cat("==================================================\n")
  cat("RESUMEN DE DATOS FALTANTES\n")
  cat("==================================================\n")
  
  print(tabla_missing)
  
  # 5. Visualización ---------------------------------------------------------
  if (hacer_grafico) {
    
    grafico_missing <- ggplot(
      tabla_missing,
      aes(
        x = reorder(
          Variable,
          Pct_Faltantes
        ),
        y = Pct_Faltantes
      )
    ) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        title = "Porcentaje de datos faltantes por variable",
        x = NULL,
        y = "% Faltantes"
      ) +
      theme_minimal()
    
    print(grafico_missing)
    
  }
  
  # 6. Test de Little (MCAR) -------------------------------------------------
  little_test <- NULL
  
  if (hacer_little) {
    
    total_missing <- sum(n_missing)
    
    if (total_missing > 0) {
      
      cat("\n")
      cat("==================================================\n")
      cat("TEST MCAR (MISS MECH)\n")
      cat("==================================================\n")
      
      little_test <- TestMCARNormality(
        as.data.frame(datos)
      )
      
      print(little_test)
      
    } else {
      
      cat("\n")
      cat("No se encontraron valores faltantes.\n")
      cat("Se omite el Test MCAR.\n")
      
    }
    
  }
  
  # 7. Retorno ---------------------------------------------------------------
  return(
    list(
      tabla_missing = tabla_missing,
      little_test = little_test
    )
  )
  
}