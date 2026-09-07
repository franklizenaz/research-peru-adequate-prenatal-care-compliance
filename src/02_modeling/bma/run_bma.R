# ============================================================================
# Pseudo-BMA por enumeración completa
# ENDES 2024 | Cálculo de PIPs y coeficientes promediados
# ============================================================================
#
# NOTA METODOLÓGICA:
#   Pseudo-BMA basado en pBIC de svyglm con tamaño muestral efectivo (DEFF).
#   Lumley & Scott (2015).
# ============================================================================

# 0. Setup --------------------------------------------------------------------

library(survey)
library(dplyr)

source("src/utils/design_utils.R")
source("src/utils/group_definitions.R")

dir.create("src/models/bma", recursive = TRUE, showWarnings = FALSE)


# 1. Carga y preparación de datos ---------------------------------------------

cat("\n========================================\n")
cat("PSEUDO-BMA POR ENUMERACIÓN COMPLETA\n")
cat("========================================\n")

APA <- readRDS("data/processed/apa.rds")
df <- as.data.frame(APA)

cat("\nDatos:", nrow(df), "observaciones\n")

# Preparar dummies
vars_diseno <- c("estrato", "conglomerado", "factor_exp")
factores_a_dummificar <- get_categorical_vars()

for (f in factores_a_dummificar) {
  df[[f]] <- as.factor(df[[f]])
}

formula_dummies <- as.formula(paste("~", paste(factores_a_dummificar, collapse = " + "), "- 1"))
dummies <- model.matrix(formula_dummies, data = df)
dummies <- as.data.frame(dummies)
names(dummies) <- make.names(names(dummies))

vars_numericas <- get_numeric_vars()
numeric_data <- df[, vars_numericas]
base_data <- df[, c("apa_bin", vars_diseno)]

APA_model <- cbind(base_data, dummies, numeric_data)

predictores <- names(APA_model)[!names(APA_model) %in% c("apa_bin", vars_diseno)]
stopifnot(all(!sapply(APA_model[, predictores], anyNA)))


# 2. Grupos y diseño -----------------------------------------------------------

grupos <- get_groups()
G <- length(grupos)
nombres_grupos <- names(grupos)

cat("\nNúmero de grupos:", G)
cat("\nModelos posibles:", 2^G, "\n")

diseno <- create_survey_design(APA_model)


# 3. Función para construir fórmula --------------------------------------------

construir_formula <- function(incluir_grupos) {
  vars_seleccionadas <- unlist(grupos[incluir_grupos])
  if (length(vars_seleccionadas) == 0) {
    return(as.formula("apa_bin ~ 1"))
  }
  return(as.formula(paste("apa_bin ~", paste(vars_seleccionadas, collapse = " + "))))
}


# 4. Función para calcular pBIC (con DEFF global) -----------------------------

# DEFF global (se calcula una sola vez)
cat("\n--- Calculando DEFF global ---\n")

pesos <- APA_model$factor_exp
n_eff_global <- sum(pesos)^2 / sum(pesos^2)
deff_global <- nrow(APA_model) / n_eff_global
cat("DEFF global:", round(deff_global, 3), "\n")
cat("n efectivo global:", round(n_eff_global, 0), "\n")

calcular_pBIC <- function(incluir_grupos, cache_env) {
  
  clave <- paste(as.integer(incluir_grupos), collapse = "")
  
  # Verificar caché
  if (exists(clave, envir = cache_env)) {
    return(get(clave, envir = cache_env))
  }
  
  form <- construir_formula(incluir_grupos)
  
  # Modelo principal: svyGLM con diseño complejo
  modelo_svy <- tryCatch(
    svyglm(form, design = diseno, family = binomial()),
    error = function(e) return(NULL)
  )
  
  if (is.null(modelo_svy)) {
    assign(clave, list(pBIC = Inf, coef = NULL, diag_vcov = NULL), envir = cache_env)
    return(get(clave, envir = cache_env))
  }
  
  loglik <- as.numeric(logLik(modelo_svy))
  
  if (is.na(loglik) || is.infinite(loglik)) {
    assign(clave, list(pBIC = Inf, coef = NULL, diag_vcov = NULL), envir = cache_env)
    return(get(clave, envir = cache_env))
  }
  
  d <- length(coef(modelo_svy))
  
  # Pseudo-BIC (Lumley & Scott, 2015) usando DEFF global
  pBIC <- -2 * loglik + d * log(n_eff_global)
  
  if (is.na(pBIC) || is.infinite(pBIC)) {
    pBIC <- Inf
  }
  
  # Guardar en caché: pBIC + coeficientes + diagonal de vcov
  cache_obj <- list(
    pBIC = pBIC,
    coef = coef(modelo_svy),
    diag_vcov = diag(vcov(modelo_svy))
  )
  
  assign(clave, cache_obj, envir = cache_env)
  
  return(get(clave, envir = cache_env))
}


# 5. Enumerar todos los modelos ------------------------------------------------

cat("\n--- Generando todos los modelos posibles ---\n")

combinaciones <- expand.grid(rep(list(c(FALSE, TRUE)), G))
colnames(combinaciones) <- nombres_grupos

n_modelos <- nrow(combinaciones)
cat("Total de modelos a evaluar:", n_modelos, "\n")


# 6. Evaluar cada modelo (una sola pasada) -------------------------------------

cat("\n--- Evaluando modelos (cálculo de pBIC + almacenamiento en caché) ---\n")

cache_env <- new.env()

# Para almacenar pBIC de cada modelo
pBIC_vector <- numeric(n_modelos)

pb <- txtProgressBar(min = 1, max = n_modelos, style = 3)

for (i in 1:n_modelos) {
  incluir <- as.logical(combinaciones[i, ])
  obj <- calcular_pBIC(incluir, cache_env)
  pBIC_vector[i] <- obj$pBIC
  setTxtProgressBar(pb, i)
}

close(pb)


# 7. Calcular PIPs -------------------------------------------------------------

cat("\n--- Calculando PIPs ---\n")

idx_validos <- which(is.finite(pBIC_vector))
resultados_validos <- data.frame(
  modelo_id = idx_validos,
  pBIC = pBIC_vector[idx_validos]
)
combinaciones_validas <- combinaciones[idx_validos, ]

cat("Modelos válidos:", nrow(resultados_validos), "de", n_modelos, "\n")

# Calcular PMP
min_pBIC <- min(resultados_validos$pBIC)
exp_terms <- exp(-0.5 * (resultados_validos$pBIC - min_pBIC))
sum_exp <- sum(exp_terms)

PMP <- exp_terms / sum_exp
names(PMP) <- resultados_validos$modelo_id

# Diagnóstico de dominancia
top10_mass <- sum(sort(PMP, decreasing = TRUE)[1:min(10, length(PMP))])
cat("\n--- Diagnóstico de dominancia ---\n")
cat("Peso acumulado de los 10 mejores modelos:", round(top10_mass * 100, 1), "%\n")
if (top10_mass > 0.9) {
  cat("  → Alta concentración: pocos modelos dominan la evidencia.\n")
} else if (top10_mass > 0.5) {
  cat("  → Concentración moderada: hay incertidumbre entre varios modelos.\n")
} else {
  cat("  → Baja concentración: alta incertidumbre estructural.\n")
}

# Calcular PIP
PIP <- numeric(G)
names(PIP) <- nombres_grupos

for (j in 1:G) {
  idx_incluidos <- which(combinaciones_validas[, j] == TRUE)
  PIP[j] <- sum(PMP[idx_incluidos])
}


# 8. Calcular coeficientes promediados (usando caché) --------------------------

cat("\n--- Calculando coeficientes promediados por Pseudo-BMA (usando caché) ---\n")

# Obtener nombres de todas las variables
all_vars <- unique(unlist(grupos))

# Inicializar matriz de coeficientes
n_modelos_validos <- nrow(resultados_validos)
coef_matrix <- matrix(NA, nrow = n_modelos_validos, ncol = length(all_vars))
colnames(coef_matrix) <- all_vars

# Extraer coeficientes del caché
cat("Extrayendo coeficientes de caché para", n_modelos_validos, "modelos...\n")

for (i in 1:n_modelos_validos) {
  
  incluir <- as.logical(combinaciones_validas[i, ])
  clave <- paste(as.integer(incluir), collapse = "")
  
  if (exists(clave, envir = cache_env)) {
    obj <- get(clave, envir = cache_env)
    
    if (!is.null(obj$coef)) {
      coefs <- obj$coef
      vars_en_modelo <- names(coefs)
      vars_en_modelo <- vars_en_modelo[vars_en_modelo != "(Intercept)"]
      
      for (var in vars_en_modelo) {
        if (var %in% colnames(coef_matrix)) {
          coef_matrix[i, var] <- coefs[var]
        }
      }
    }
  }
}

# Coeficientes promediados (incondicionales)
coef_promedio <- numeric(length(all_vars))
names(coef_promedio) <- all_vars

for (j in 1:length(all_vars)) {
  var <- all_vars[j]
  coefs_var <- coef_matrix[, var]
  coefs_var[is.na(coefs_var)] <- 0
  coef_promedio[j] <- sum(coefs_var * PMP, na.rm = TRUE)
}

# Coeficientes condicionales
coef_promedio_condicional <- numeric(length(all_vars))
names(coef_promedio_condicional) <- all_vars

for (j in 1:length(all_vars)) {
  var <- all_vars[j]
  coefs_var <- coef_matrix[, var]
  idx_incluidos <- which(!is.na(coefs_var))
  
  if (length(idx_incluidos) > 0) {
    pmp_condicional <- PMP[idx_incluidos] / sum(PMP[idx_incluidos])
    coef_promedio_condicional[j] <- sum(coefs_var[idx_incluidos] * pmp_condicional, na.rm = TRUE)
  } else {
    coef_promedio_condicional[j] <- NA
  }
}

# OR_BMA (exp de coeficiente promedio)
OR_BMA <- exp(coef_promedio)
OR_BMA_condicional <- exp(coef_promedio_condicional)


# 9. Calcular incertidumbre BMA (varianza posterior completa) ------------------

cat("\n--- Calculando incertidumbre Pseudo-BMA (varianza posterior completa) ---\n")

var_posterior <- numeric(length(all_vars))
names(var_posterior) <- all_vars

for (j in 1:length(all_vars)) {
  var <- all_vars[j]
  
  var_intra <- 0
  var_entre <- 0
  
  for (i in 1:n_modelos_validos) {
    incluir <- as.logical(combinaciones_validas[i, ])
    clave <- paste(as.integer(incluir), collapse = "")
    
    if (exists(clave, envir = cache_env)) {
      obj <- get(clave, envir = cache_env)
      
      beta_m <- 0
      
      if (!is.null(obj$coef) && var %in% names(obj$coef)) {
        beta_m <- obj$coef[var]
      }
      
      if (!is.null(obj$diag_vcov) && var %in% names(obj$diag_vcov)) {
        var_intra <- var_intra + PMP[i] * obj$diag_vcov[var]
      }
      
      var_entre <- var_entre + PMP[i] * (beta_m - coef_promedio[var])^2
    }
  }
  
  var_posterior[var] <- var_intra + var_entre
}

# Desviación estándar posterior
sd_posterior <- sqrt(var_posterior)

# Intervalos aproximados (basados en normal)
IC95_inf <- coef_promedio - 1.96 * sd_posterior
IC95_sup <- coef_promedio + 1.96 * sd_posterior

OR_IC95_inf <- exp(IC95_inf)
OR_IC95_sup <- exp(IC95_sup)


# 10. Top modelos por PMP ------------------------------------------------------

cat("\n--- Top 10 modelos por PMP ---\n")

top10_idx <- order(PMP, decreasing = TRUE)[1:min(10, length(PMP))]
top10_modelos <- data.frame(
  Rango = 1:length(top10_idx),
  Modelo_ID = names(PMP)[top10_idx],
  PMP = round(PMP[top10_idx], 6),
  pBIC = resultados_validos$pBIC[match(names(PMP)[top10_idx], resultados_validos$modelo_id)],
  Variables = sapply(top10_idx, function(i) {
    incluir <- as.logical(combinaciones_validas[i, ])
    vars <- nombres_grupos[incluir]
    if (length(vars) == 0) return("Nulo")
    return(paste(vars, collapse = " + "))
  })
)

print(top10_modelos)


# 11. Mostrar resumen ----------------------------------------------------------

cat("\n========================================\n")
cat("RESUMEN DE RESULTADOS\n")
cat("========================================\n")

cat("\n--- PIPs ---\n")
print(round(sort(PIP, decreasing = TRUE), 4))

cat("\n--- Grupos con PIP > 0.5 ---\n")
cat(paste(names(PIP)[PIP > 0.5], collapse = ", "), "\n")

cat("\n--- Coeficientes promediados (top 10) ---\n")
top_coef <- sort(abs(coef_promedio), decreasing = TRUE)[1:min(10, length(coef_promedio))]
print(round(top_coef, 4))

cat("\n--- OR_BMA (top 10) ---\n")
top_or <- sort(OR_BMA, decreasing = TRUE)[1:min(10, length(OR_BMA))]
print(round(top_or, 4))


# 12. Guardar resultados -------------------------------------------------------

bma_results <- list(
  PIP = PIP,
  PMP = PMP,
  top10_modelos = top10_modelos,
  resultados = resultados_validos,
  combinaciones = combinaciones_validas,
  grupos = grupos,
  nombres_grupos = nombres_grupos,
  G = G,
  n_modelos_evaluados = nrow(resultados_validos),
  n_modelos_totales = n_modelos,
  n_eff_global = n_eff_global,
  deff_global = deff_global,
  coeficientes = list(
    incondicional = coef_promedio,
    condicional = coef_promedio_condicional,
    OR_BMA = OR_BMA,
    OR_BMA_condicional = OR_BMA_condicional,
    sd_posterior = sd_posterior,
    IC95_inf = IC95_inf,
    IC95_sup = IC95_sup,
    OR_IC95_inf = OR_IC95_inf,
    OR_IC95_sup = OR_IC95_sup
  ),
  coef_matrix = coef_matrix,
  nota_metodologica = paste(
    "Pseudo-BMA basado en pBIC de svyglm con tamaño muestral efectivo (DEFF global).",
    "Lumley & Scott (2015).",
    "Los intervalos son aproximaciones normales, no intervalos creíbles exactos.",
    "El posterior BMA es una mezcla que puede no ser normal."
  ),
  cache_keys = ls(cache_env)
)

saveRDS(bma_results, "src/models/bma/bma_results_exhaustive.rds")