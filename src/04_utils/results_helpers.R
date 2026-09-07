# ============================================================================
# Utilidades para resultados y visualización
# ENDES 2024 | Extracción, formato y visualización de resultados
# ============================================================================

# 1. Formato y extracción ------------------------------------------------------

#' Formatear odds ratios con intervalos de confianza
#'
#' @param OR Odds Ratio
#' @param IC_lower Límite inferior del IC
#' @param IC_upper Límite superior del IC
#' @param digits Número de decimales
#' @return String formateado "OR (IC_lower-IC_upper)"
format_OR <- function(OR, IC_lower, IC_upper, digits = 3) {
  paste0(
    round(OR, digits),
    " (",
    round(IC_lower, digits),
    "-",
    round(IC_upper, digits),
    ")"
  )
}

#' Extraer tabla de resultados del modelo
#'
#' @param model_result Lista con resultados del modelo (de extract_results)
#' @return data.frame con coeficientes, ORs e ICs
extract_model_table <- function(model_result) {
  
  if (!model_result$converged) {
    return(NULL)
  }
  
  coefs <- model_result$coefficients
  
  # Verificar si confidence_intervals existe
  if (is.null(model_result$confidence_intervals)) {
    warning("No hay intervalos de confianza en model_result")
    # Crear intervalos aproximados
    se <- summary(model_result$model)$coefficients[, 2]
    ci <- cbind(
      coefs - 1.96 * se,
      coefs + 1.96 * se
    )
    colnames(ci) <- c("2.5 %", "97.5 %")
    rownames(ci) <- names(coefs)
  } else {
    ci <- model_result$confidence_intervals
  }
  
  OR <- exp(coefs)
  OR_IC <- exp(ci)
  
  data.frame(
    Variable = names(coefs),
    Coef = round(coefs, 4),
    OR = round(OR, 4),
    OR_CI_lower = round(OR_IC[, 1], 4),
    OR_CI_upper = round(OR_IC[, 2], 4),
    OR_CI = format_OR(OR, OR_IC[, 1], OR_IC[, 2]),
    row.names = NULL
  )
}

#' Añadir significancia a tabla de resultados
#'
#' @param results data.frame de extract_model_table
#' @return data.frame con columna Signif añadida
add_significance <- function(results) {
  results$Signif <- ifelse(
    results$OR_CI_lower > 1 | results$OR_CI_upper < 1,
    ifelse(results$OR_CI_lower > 1.5 | results$OR_CI_upper < 0.67, "***", "**"),
    ifelse(results$OR_CI_lower > 1.1 | results$OR_CI_upper < 0.91, "*", "")
  )
  return(results)
}

#' Calcular pseudo-R² para modelos
#'
#' @param model Modelo GLM o svyGLM
#' @param data data.frame usado en el modelo (para GLM)
#' @return data.frame con métricas
calculate_pseudo_R2 <- function(model, data = NULL) {
  
  if (inherits(model, "glm")) {
    # Para GLM, necesitamos los datos originales
    if (is.null(data)) {
      data <- model$data
    }
    null_model <- glm(apa_bin ~ 1, data = data, family = binomial())
    mcfadden <- 1 - logLik(model) / logLik(null_model)
    n <- length(model$fitted.values)
    cox_snell <- 1 - exp(-(model$null.deviance - model$deviance) / n)
    nagelkerke <- cox_snell / (1 - exp(-model$null.deviance / n))
    
    return(data.frame(
      McFadden = as.numeric(mcfadden),
      Cox_Snell = as.numeric(cox_snell),
      Nagelkerke = as.numeric(nagelkerke)
    ))
  }
  
  if (inherits(model, "svyglm")) {
    return(data.frame(
      AIC = AIC(model),
      BIC = BIC(model),
      Deviance = deviance(model),
      df_resid = df.residual(model)
    ))
  }
  
  return(NULL)
}


# 2. Visualización -------------------------------------------------------------

#' Crear forest plot de Odds Ratios
#'
#' @param results data.frame de extract_model_table
#' @param title Título del gráfico
#' @param color Color de los puntos
#' @return objeto ggplot
create_forest_plot <- function(results, title = "Odds Ratios", color = "steelblue") {
  
  # Filtrar intercepto
  plot_data <- results[!grepl("Intercept", results$Variable),]
  
  # Ordenar por OR
  plot_data <- plot_data[order(plot_data$OR),]
  
  ggplot(plot_data, aes(x = reorder(Variable, OR), y = OR)) +
    geom_point(size = 3, color = color) +
    geom_errorbar(aes(ymin = OR_CI_lower, ymax = OR_CI_upper), width = 0.2) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
    coord_flip() +
    scale_y_log10() +
    labs(
      title = title,
      x = "Variables",
      y = "Odds Ratio (escala logarítmica)"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 8),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
}

#' Comparar dos modelos en un gráfico
#'
#' @param model1_results data.frame de extract_model_table (modelo 1)
#' @param model2_results data.frame de extract_model_table (modelo 2)
#' @param model1_name Nombre del modelo 1
#' @param model2_name Nombre del modelo 2
#' @param title Título del gráfico
#' @return objeto ggplot
create_comparison_plot <- function(model1_results, model2_results, 
                                   model1_name = "Modelo 1", 
                                   model2_name = "Modelo 2",
                                   title = "Comparación de Modelos") {
  
  # Extraer coeficientes comunes
  common_vars <- intersect(model1_results$Variable, model2_results$Variable)
  common_vars <- common_vars[!grepl("Intercept", common_vars)]
  
  comp_data <- data.frame(
    Variable = common_vars,
    OR1 = model1_results$OR[match(common_vars, model1_results$Variable)],
    OR2 = model2_results$OR[match(common_vars, model2_results$Variable)]
  )
  
  comp_long <- comp_data %>%
    pivot_longer(cols = c(OR1, OR2), names_to = "Modelo", values_to = "OR")
  
  comp_long$Modelo <- ifelse(comp_long$Modelo == "OR1", model1_name, model2_name)
  
  ggplot(comp_long, aes(x = reorder(Variable, OR), y = OR, color = Modelo)) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
    coord_flip() +
    scale_y_log10() +
    labs(
      title = title,
      x = "Variables",
      y = "Odds Ratio (escala logarítmica)",
      color = "Modelo"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 8),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )
}

#' Crear gráfico de distribución de variable
#'
#' @param data data.frame
#' @param variable Nombre de variable
#' @param title Título
#' @param fill_colors Vector de colores
#' @return objeto ggplot
create_distribution_plot <- function(data, variable, 
                                     title = "Distribución",
                                     fill_colors = c("No" = "#D73027", "Si" = "#1A9850")) {
  
  counts <- table(data[[variable]])
  df_plot <- data.frame(
    Categoria = names(counts),
    n = as.numeric(counts),
    pct = as.numeric(prop.table(counts) * 100)
  )
  
  ggplot(df_plot, aes(x = Categoria, y = n, fill = Categoria)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(n, "\n(", round(pct, 1), "%)")),
              vjust = -0.5, size = 4) +
    scale_fill_manual(values = fill_colors) +
    labs(
      title = title,
      subtitle = paste0("n = ", nrow(data)),
      x = NULL,
      y = "Frecuencia"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "none"
    )
}


# 3. Tablas para exportación ---------------------------------------------------

#' Exportar tabla a CSV y LaTeX
#'
#' @param data data.frame
#' @param name Nombre base del archivo
#' @param caption Título para LaTeX
#' @param path Directorio de salida
export_table <- function(data, name, caption = NULL, path = "report/assets/tabs") {
  
  # Crear directorio
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  
  # Exportar a CSV
  write.csv(data, file.path(path, paste0(name, ".csv")), row.names = FALSE)
  
  # Exportar a LaTeX (solo si hay caption)
  if (!is.null(caption)) {
    
    # Limpiar caracteres conflictivos de LaTeX
    clean_latex_chars <- function(text) {
      if (is.null(text)) return(text)
      text <- gsub("\\\\", "\\\\textbackslash{}", text)
      text <- gsub("&", "\\\\&", text)
      text <- gsub("%", "\\\\%", text)
      text <- gsub("\\$", "\\\\$", text)
      text <- gsub("#", "\\\\#", text)
      text <- gsub("_", "\\\\_", text)
      text <- gsub("\\{", "\\\\{", text)
      text <- gsub("\\}", "\\\\}", text)
      return(text)
    }
    
    colnames(data) <- clean_latex_chars(colnames(data))
    
    data[] <- lapply(data, function(col) {
      if (is.character(col)) {
        return(clean_latex_chars(col))
      } else if (is.factor(col)) {
        levels(col) <- clean_latex_chars(levels(col))
        return(col)
      } else {
        return(col)
      }
    })
    
    # Formato numérico dinámico (máximo 3 decimales, sin ceros sobrantes)
    data[] <- lapply(data, function(col) {
      if (is.numeric(col)) {
        formatted <- format(round(col, 3), scientific = FALSE, trim = TRUE)
        formatted <- gsub("0+$", "", formatted)
        formatted <- gsub("\\.$", "", formatted)
        col <- formatted
      }
      col <- ifelse(is.na(col) | col == "NA", "--", col)
      col <- as.character(col)
      return(col)
    })
    
    # Imprimir archivo .tex
    print(xtable::xtable(data, caption = caption), 
          file = file.path(path, paste0(name, ".tex")), 
          include.rownames = FALSE, 
          booktabs = TRUE, 
          floating = FALSE,  
          sanitize.text.function = identity,      
          sanitize.colnames.function = identity)  
  }
}