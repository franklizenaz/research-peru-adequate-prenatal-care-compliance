# =============================================================================
# STEPWISE POR GRUPOS PARA GLM SIN PESOS
# =============================================================================
#
# OBJETIVO: Selección stepwise donde las variables categóricas
#           entran o salen como GRUPO COMPLETO
#
# NOTA: Los grupos se definen en utils/group_definitions.R
# =============================================================================

source("src/04_utils/group_definitions.R")

#' Stepwise por grupos
#'
#' @param data data.frame con dummies
#' @param groups lista de grupos (de get_groups())
#' @param criterion "AIC" o "BIC"
#' @param trace lógico
stepwise_group_selection <- function(
    data,
    groups = get_groups(),
    criterion = "AIC",
    trace = FALSE
) {
  
  # Obtener todas las variables (dummies)
  all_vars <- unlist(groups)
  
  # Fórmula completa
  formula_complete <- as.formula(
    paste("apa_bin ~", paste(all_vars, collapse = " + "))
  )
  
  # Modelo inicial
  model_initial <- tryCatch(
    glm(formula_complete, data = data, family = binomial()),
    error = function(e) {
      warning("Error en modelo inicial: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(model_initial)) {
    return(list(
      formula = as.formula("apa_bin ~ 1"),
      groups_selected = character(0),
      n_groups = 0,
      converged = FALSE
    ))
  }
  
  # Estado inicial: todos los grupos incluidos
  model_current <- model_initial
  groups_names <- names(groups)
  
  for (iter in 1:50) {
    
    current_terms <- attr(terms(model_current), "term.labels")
    
    # Identificar qué grupos están actualmente en el modelo
    groups_in_model <- character()
    for (g in groups_names) {
      group_terms <- groups[[g]]
      if (all(group_terms %in% current_terms)) {
        groups_in_model <- c(groups_in_model, g)
      }
    }
    
    if (length(groups_in_model) == 0) {
      break
    }
    
    # Evaluar eliminación de cada grupo
    best_model <- NULL
    best_stat <- Inf
    best_group <- NULL
    
    for (g in groups_in_model) {
      
      group_terms <- groups[[g]]
      terms_without <- current_terms[!current_terms %in% group_terms]
      
      if (length(terms_without) == 0) {
        formula_without <- as.formula("apa_bin ~ 1")
      } else {
        formula_without <- as.formula(
          paste("apa_bin ~", paste(terms_without, collapse = " + "))
        )
      }
      
      model_without <- tryCatch(
        update(model_current, formula_without),
        error = function(e) NULL
      )
      
      if (!is.null(model_without)) {
        stat <- ifelse(criterion == "AIC", AIC(model_without), BIC(model_without))
        
        if (stat < best_stat) {
          best_stat <- stat
          best_model <- model_without
          best_group <- g
        }
      }
    }
    
    # Decidir eliminación
    current_stat <- ifelse(criterion == "AIC", AIC(model_current), BIC(model_current))
    
    if (!is.null(best_model) && best_stat < current_stat) {
      model_current <- best_model
      if (trace) {
        cat("  - Eliminando grupo:", best_group, "(", criterion, "=", round(best_stat, 2), ")\n")
      }
    } else {
      break
    }
  }
  
  # Identificar grupos seleccionados
  final_terms <- attr(terms(model_current), "term.labels")
  
  groups_selected <- character()
  for (g in groups_names) {
    group_terms <- groups[[g]]
    if (all(group_terms %in% final_terms)) {
      groups_selected <- c(groups_selected, g)
    }
  }
  
  # Construir fórmula final
  if (length(groups_selected) == 0) {
    final_formula <- as.formula("apa_bin ~ 1")
    final_vars <- character(0)
  } else {
    final_vars <- unlist(groups[groups_selected])
    final_formula <- as.formula(
      paste("apa_bin ~", paste(final_vars, collapse = " + "))
    )
  }
  
  return(list(
    formula = final_formula,
    groups_selected = groups_selected,
    n_groups = length(groups_selected),
    terms = final_vars,
    n_terms = length(final_vars),
    model = model_current,
    converged = TRUE
  ))
}