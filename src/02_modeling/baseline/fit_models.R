# =============================================================================
# AJUSTE DE MODELOS BASELINE
# =============================================================================
#
# MODELOS:
#   1. GLM sin pesos CON stepwise por GRUPOS
#   2. svyGLM con pesos SIN stepwise (modelo completo)
#
# NOTA: Los grupos se definen en utils/group_definitions.R
# =============================================================================

source("src/04_utils/group_definitions.R")

#' Preparar datos con dummies para stepwise y BMA
#'
#' NOTA: Esta misma función se usará para BMA
prepare_data_dummies <- function(data) {
  
  # Convertir categóricas a factor
  categorical_vars <- get_categorical_vars()
  for (var in categorical_vars) {
    data[[var]] <- as.factor(data[[var]])
  }
  
  # Crear dummies (sin intercepto para tener todas las categorías)
  formula_dummies <- as.formula(
    paste("~", paste(categorical_vars, collapse = " + "), "- 1")
  )
  
  dummies <- model.matrix(formula_dummies, data = data)
  dummies <- as.data.frame(dummies)
  names(dummies) <- make.names(names(dummies))
  
  # Variables numéricas
  numeric_vars <- get_numeric_vars()
  numeric_data <- data[, numeric_vars]
  
  # Variable dependiente y diseño
  base_data <- data[, c("apa_bin", "estrato", "conglomerado", "factor_exp")]
  
  # Unir todo
  df_model <- cbind(base_data, dummies, numeric_data)
  
  return(df_model)
}

#' Preparar datos con factores para svyGLM
prepare_data_factors <- function(data) {
  
  categorical_vars <- get_categorical_vars()
  for (var in categorical_vars) {
    data[[var]] <- as.factor(data[[var]])
  }
  
  numeric_vars <- get_numeric_vars()
  for (var in numeric_vars) {
    data[[var]] <- as.numeric(data[[var]])
  }
  
  required_vars <- c("apa_bin", "estrato", "conglomerado", "factor_exp",
                     get_all_vars())
  
  return(data[, required_vars])
}

#' Modelo 1: GLM sin pesos CON stepwise por GRUPOS
fit_model_glm_stepwise <- function(data) {
  
  # Preparar datos con dummies
  df_step <- prepare_data_dummies(data)
  
  # Grupos (MISMOS que se usarán en BMA)
  groups <- get_groups()
  
  cat("\n=== MODELO 1: STEPWISE POR GRUPOS (SIN PESOS) ===\n")
  cat("Grupos totales:", length(groups), "\n")
  
  step_result <- stepwise_group_selection(
    data = df_step,
    groups = groups,
    criterion = "AIC",
    trace = TRUE
  )
  
  cat("\nGrupos seleccionados:", step_result$n_groups, "\n")
  if (step_result$n_groups > 0) {
    cat("  ", paste(step_result$groups_selected, collapse = "\n  "), "\n")
  }
  
  # Modelo final GLM
  if (step_result$n_groups == 0) {
    model_glm <- glm(apa_bin ~ 1, data = df_step, family = binomial())
    formula_final <- as.formula("apa_bin ~ 1")
  } else {
    model_glm <- safe_model_fit(
      formula = step_result$formula,
      data = df_step,
      use_weights = FALSE
    )
    formula_final <- step_result$formula
  }
  
  return(list(
    stepwise = step_result,
    model = extract_results(model_glm, "glm_stepwise", formula_final),
    data = df_step,
    groups = groups  # Guardar grupos para consistencia
  ))
}

#' Modelo 2: svyGLM con pesos SIN stepwise (todas variables)
fit_model_svyglm_complete <- function(data) {
  
  # Datos para modelo con pesos (con factores, sin dummies)
  df_svy <- prepare_data_factors(data)
  
  # Variables base (TODAS)
  vars_base <- get_all_vars()
  
  # Fórmula completa
  formula_complete <- as.formula(
    paste("apa_bin ~", paste(vars_base, collapse = " + "))
  )
  
  cat("\n=== MODELO 2: svyGLM (CON PESOS, TODAS VARIABLES) ===\n")
  cat("Variables:", length(vars_base), "\n")
  
  # Diseño muestral
  design <- create_survey_design(df_svy)
  
  # Modelo final svyGLM
  model_svy <- safe_model_fit(
    formula = formula_complete,
    data = df_svy,
    design = design,
    use_weights = TRUE
  )
  
  return(list(
    model = extract_results(model_svy, "svyglm_complete", formula_complete),
    data = df_svy,
    design = design,
    vars = vars_base  # Guardar variables para consistencia
  ))
}