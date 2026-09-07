# ============================================================================
# Pipeline baseline: modelos de regresión logística
# ENDES 2024 | GLM + svyGLM
# ============================================================================
#
# MODELOS:
#   1. GLM sin pesos con stepwise por grupos
#   2. svyGLM con pesos sin stepwise (todas variables)
#
# NOTA: Los grupos definidos aquí se reutilizan en BMA
# ============================================================================

# 0. Setup y utilidades --------------------------------------------------------

library(survey)
library(dplyr)

source("src/04_utils/design_utils.R")
source("src/04_utils/model_helpers.R")
source("src/04_utils/group_definitions.R")

source("src/02_modeling/baseline/stepwise.R")
source("src/02_modeling/baseline/fit_models.R")


# 1. Carga de datos ------------------------------------------------------------

cat("\n========================================\n")
cat("PIPELINE BASELINE\n")
cat("========================================\n")

APA <- readRDS("data/processed/apa.rds")
df <- as.data.frame(APA)

cat("\nDatos:", nrow(df), "observaciones,", ncol(df), "variables\n")


# 2. Preparación ---------------------------------------------------------------

cat("\nPreparando datos...\n")


# 3. Modelo 1: GLM sin pesos con stepwise por grupos --------------------------

result_glm <- fit_model_glm_stepwise(df)


# 4. Modelo 2: svyGLM con pesos sin stepwise ----------------------------------

result_svy <- fit_model_svyglm_complete(df)


# 5. Resultados ----------------------------------------------------------------

baseline_results <- list(
  glm_stepwise = result_glm,
  svyglm_complete = result_svy,
  groups = get_groups(),  # Guardar grupos para BMA
  metadata = list(
    date = Sys.Date(),
    n_obs = nrow(df),
    n_groups = get_n_groups(),
    model_1 = "GLM sin pesos con stepwise por grupos",
    model_2 = "svyGLM con pesos sin stepwise (todas variables)",
    note = "Los grupos definidos son los mismos que se usarán en BMA"
  )
)


# 6. Guardar resultados --------------------------------------------------------

output_dir <- "src/models/baseline/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)