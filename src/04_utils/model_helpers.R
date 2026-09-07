# ============================================================================
# Utilidades para modelos
# ENDES 2024 | Ajuste seguro de modelos con manejo de errores
# ============================================================================

#' Ajustar modelo con manejo de errores
#'
#' @param formula fórmula del modelo
#' @param data data.frame
#' @param design objeto svydesign (opcional)
#' @param use_weights lógico (TRUE = svyglm, FALSE = glm)
#' @return modelo ajustado o NULL
safe_model_fit <- function(formula, data, design = NULL, use_weights = FALSE) {
  
  if (use_weights) {
    # Modelo con diseño complejo
    modelo <- tryCatch(
      svyglm(formula, design = design, family = quasibinomial()),
      error = function(e) {
        warning("Error en svyglm: ", e$message)
        return(NULL)
      }
    )
  } else {
    # Modelo sin pesos (logística estándar)
    modelo <- tryCatch(
      glm(formula, data = data, family = binomial(link = "logit")),
      error = function(e) {
        warning("Error en glm: ", e$message)
        return(NULL)
      }
    )
  }
  
  return(modelo)
}


#' Extraer resultados del modelo con intervalos de confianza
#'
#' @param model modelo ajustado (glm o svyglm)
#' @param model_name nombre del modelo
#' @param formula fórmula utilizada
#' @return lista con coeficientes, IC y convergencia
extract_results <- function(model, model_name, formula) {
  
  if (is.null(model)) {
    return(list(converged = FALSE, model_name = model_name))
  }
  
  # Coeficientes
  coefficients <- coef(model)
  
  # Intervalos de confianza
  if (inherits(model, "svyglm")) {
    ci <- tryCatch(
      confint(model),
      error = function(e) {
        warning("Error calculando confint para svyglm: ", e$message)
        # Fallback: usar errores estándar
        se <- summary(model)$coefficients[, 2]
        ci <- cbind(
          coefficients - 1.96 * se,
          coefficients + 1.96 * se
        )
        colnames(ci) <- c("2.5 %", "97.5 %")
        rownames(ci) <- names(coefficients)
        return(ci)
      }
    )
  } else {
    # Para glm: confint.default (más rápido y robusto)
    ci <- tryCatch(
      confint.default(model),
      error = function(e) {
        # Fallback: usar errores estándar
        se <- summary(model)$coefficients[, 2]
        ci <- cbind(
          coefficients - 1.96 * se,
          coefficients + 1.96 * se
        )
        colnames(ci) <- c("2.5 %", "97.5 %")
        rownames(ci) <- names(coefficients)
        return(ci)
      }
    )
  }
  
  list(
    converged = TRUE,
    model_name = model_name,
    model = model,
    formula = formula,
    coefficients = coefficients,
    confidence_intervals = ci,
    n_coefficients = length(coefficients)
  )
}