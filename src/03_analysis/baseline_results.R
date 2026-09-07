# ============================================================================
# Pipeline de resultados baseline - versión paper
# ENDES 2024 | GLM + svyGLM | Tablas y figuras
# ============================================================================

# 0. Setup --------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(survey)
library(xtable)
library(scales)
library(pROC)

source("src/04_utils/results_helpers.R")

# Crear directorios de salida
dir.create("report/assets/tabs", recursive = TRUE, showWarnings = FALSE)
dir.create("report/assets/figs", recursive = TRUE, showWarnings = FALSE)
dir.create("report/assets/metrics", recursive = TRUE, showWarnings = FALSE)


# 1. Carga de resultados -------------------------------------------------------

cat("\n========================================\n")
cat("PIPELINE DE RESULTADOS BASELINE - PAPER\n")
cat("========================================\n")

baseline <- readRDS("src/models/baseline/baseline_models.rds")

cat("\nResultados cargados exitosamente\n")

df_sample <- baseline$glm_stepwise$data


# 2. Tabla 1: Características de la muestra ------------------------------------

cat("\n--- Generando Tabla 1: Características de la muestra ---\n")

sample_chars_list <- list()
idx <- 1

# Distribución de APA
apa_counts <- table(df_sample$apa_bin)
sample_chars_list[[idx]] <- data.frame(
  Caracteristica = "Atención Prenatal Adecuada",
  Categoria = c("No", "Si"),
  n = as.numeric(apa_counts),
  Porcentaje = as.numeric(round(prop.table(apa_counts) * 100, 1))
)
idx <- idx + 1

# Variables categóricas
vars_categoricas <- c("niv_educ", "quintil_riq", "area_res", "etnicidad", "seguro", "estado_civil")
for (var in vars_categoricas) {
  if (var %in% names(df_sample)) {
    dist <- table(df_sample[[var]])
    sample_chars_list[[idx]] <- data.frame(
      Caracteristica = var,
      Categoria = names(dist),
      n = as.numeric(dist),
      Porcentaje = as.numeric(round(prop.table(dist) * 100, 1))
    )
    idx <- idx + 1
  }
}

# Variables numéricas
vars_numericas <- c("edad", "hijos", "num_centros_medicos")
for (var in vars_numericas) {
  if (var %in% names(df_sample)) {
    x <- df_sample[[var]]
    x <- x[!is.na(x)]
    sample_chars_list[[idx]] <- data.frame(
      Caracteristica = var,
      Categoria = c("Mínimo", "Q1", "Mediana", "Media", "Q3", "Máximo"),
      n = as.numeric(c(
        min(x),
        quantile(x, 0.25, names = FALSE),
        median(x),
        mean(x),
        quantile(x, 0.75, names = FALSE),
        max(x)
      )),
      Porcentaje = NA_real_
    )
    idx <- idx + 1
  }
}

sample_chars <- bind_rows(sample_chars_list)

export_table(sample_chars, "table2_sample_characteristics", 
             caption = "Características de la muestra (ENDES 2024)")
cat("  -> Tabla 1 guardada\n")


# 3. Tabla 2: GLM + Stepwise (comparación) -------------------------------------

cat("\n--- Generando Tabla 2: Modelo GLM + Stepwise (comparación) ---\n")

glm_results <- extract_model_table(baseline$glm_stepwise$model)
glm_results <- add_significance(glm_results)

glm_report <- glm_results[, c("Variable", "OR", "OR_CI", "Signif")]

export_table(glm_report, "table5_glm_stepwise",
             caption = "Modelo GLM con selección Stepwise (sin pesos)")
cat("  -> Tabla 2 guardada\n")


# 4. Tabla 3: Modelo svyGLM completo (principal) -------------------------------

cat("\n--- Generando Tabla 3: Modelo svyGLM Completo (PRINCIPAL) ---\n")

svy_results <- extract_model_table(baseline$svyglm_complete$model)
svy_results <- add_significance(svy_results)

svy_report <- svy_results[, c("Variable", "OR", "OR_CI", "Signif")]

export_table(svy_report, "table6_svyglm_complete",
             caption = "Modelo svyGLM con diseño muestral complejo (todas variables)")
cat("  -> Tabla 3 guardada\n")


# 5. Tabla 4: Métricas comparativas (resumen) ----------------------------------

cat("\n--- Generando Tabla 4: Métricas Comparativas (RESUMEN) ---\n")

# GLM metrics
glm_metrics <- calculate_pseudo_R2(
  baseline$glm_stepwise$model$model,
  data = baseline$glm_stepwise$data
)

# svyGLM metrics
svy_metrics <- calculate_pseudo_R2(baseline$svyglm_complete$model$model)

# Calcular AUC para GLM
glm_pred <- predict(baseline$glm_stepwise$model$model, type = "response")
glm_auc <- roc(df_sample$apa_bin, glm_pred)$auc

# Calcular AUC para svyGLM
svy_pred <- predict(baseline$svyglm_complete$model$model, type = "response")
svy_auc <- roc(df_sample$apa_bin, svy_pred)$auc

metrics_paper <- data.frame(
  Modelo = c("GLM + Stepwise", "svyGLM Completo"),
  AIC = round(c(AIC(baseline$glm_stepwise$model$model), svy_metrics$AIC), 2),
  BIC = round(c(BIC(baseline$glm_stepwise$model$model), svy_metrics$BIC), 2),
  AUC = round(c(as.numeric(glm_auc), as.numeric(svy_auc)), 4),
  N_Variables = c(
    baseline$glm_stepwise$stepwise$n_terms,
    length(baseline$svyglm_complete$vars)
  )
)

export_table(metrics_paper, "table7_model_metrics_paper",
             caption = "Métricas de ajuste comparativas: GLM vs svyGLM")
cat("  -> Tabla 4 guardada\n")


# 6. Figura 1: Distribución APA ------------------------------------------------

cat("\n--- Generando Figura 1: Distribución de APA ---\n")

p1 <- create_distribution_plot(
  df_sample,
  variable = "apa_bin",
  title = "Distribución de Atención Prenatal Adecuada (APA)",
  fill_colors = c("No" = "#D73027", "Si" = "#1A9850")
)

ggsave("report/assets/figs/fig1_apa_distribution_paper.png", p1, width = 8, height = 6, dpi = 300)
ggsave("report/assets/figs/fig1_apa_distribution_paper.pdf", p1, width = 8, height = 6)
cat("  -> Figura 1 guardada\n")


# 7. Figura 2: Odds ratios comparativos (ambos modelos) ------------------------

cat("\n--- Generando Figura 2: Odds Ratios Comparativos (AMBOS modelos) ---\n")

# Variables comunes (excluyendo intercepto)
common_vars <- intersect(glm_results$Variable, svy_results$Variable)
common_vars <- common_vars[!grepl("Intercept", common_vars)]

# Preparar datos
comp_plot_data <- data.frame(
  Variable = rep(common_vars, 2),
  OR = c(
    glm_results$OR[match(common_vars, glm_results$Variable)],
    svy_results$OR[match(common_vars, svy_results$Variable)]
  ),
  CI_lower = c(
    glm_results$OR_CI_lower[match(common_vars, glm_results$Variable)],
    svy_results$OR_CI_lower[match(common_vars, svy_results$Variable)]
  ),
  CI_upper = c(
    glm_results$OR_CI_upper[match(common_vars, glm_results$Variable)],
    svy_results$OR_CI_upper[match(common_vars, svy_results$Variable)]
  ),
  Modelo = rep(c("GLM + Stepwise", "svyGLM"), each = length(common_vars))
)

# Ordenar por OR de svyGLM
orden <- unique(comp_plot_data$Variable[comp_plot_data$Modelo == "svyGLM"])
comp_plot_data$Variable <- factor(comp_plot_data$Variable, levels = orden[order(orden)])

p2 <- ggplot(comp_plot_data, aes(x = Variable, y = OR, color = Modelo)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
  coord_flip() +
  scale_y_log10() +
  labs(
    title = "Comparación de Odds Ratios: GLM vs svyGLM",
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

ggsave("report/assets/figs/fig4_odds_ratios_comparison_paper.png", p2, width = 10, height = 8, dpi = 300)
ggsave("report/assets/figs/fig4_odds_ratios_comparison_paper.pdf", p2, width = 10, height = 8)
cat("  -> Figura 2 guardada\n")


# 8. Figura 3: Curvas ROC (ambos modelos) --------------------------------------

cat("\n--- Generando Figura 3: Curvas ROC (AMBOS modelos) ---\n")

roc_glm <- roc(df_sample$apa_bin, glm_pred)
roc_svy <- roc(df_sample$apa_bin, svy_pred)

roc_data <- data.frame(
  FPR = c(1 - roc_glm$specificities, 1 - roc_svy$specificities),
  TPR = c(roc_glm$sensitivities, roc_svy$sensitivities),
  Modelo = c(
    rep("GLM + Stepwise", length(roc_glm$specificities)),
    rep("svyGLM", length(roc_svy$specificities))
  )
)

p3 <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Modelo)) +
  geom_line(size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("GLM + Stepwise" = "steelblue", "svyGLM" = "darkgreen")) +
  labs(
    title = "Curvas ROC - Comparación de Modelos",
    x = "1 - Especificidad (FPR)",
    y = "Sensibilidad (TPR)",
    subtitle = paste0(
      "AUC GLM: ", round(as.numeric(glm_auc), 3),
      " | AUC svyGLM: ", round(as.numeric(svy_auc), 3)
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

ggsave("report/assets/figs/fig5_roc_curves_paper.png", p3, width = 8, height = 7, dpi = 300)
ggsave("report/assets/figs/fig5_roc_curves_paper.pdf", p3, width = 8, height = 7)
cat("  -> Figura 3 guardada\n")


# 9. Métricas completas (referencia) -------------------------------------------

cat("\n--- Guardando métricas completas (referencia) ---\n")

if (baseline$glm_stepwise$model$converged) {
  glm_metrics <- calculate_pseudo_R2(
    baseline$glm_stepwise$model$model,
    data = baseline$glm_stepwise$data
  )
  write.csv(glm_metrics, "report/assets/metrics/glm_metrics_complete.csv", row.names = FALSE)
}

if (baseline$svyglm_complete$model$converged) {
  svy_metrics <- calculate_pseudo_R2(baseline$svyglm_complete$model$model)
  write.csv(svy_metrics, "report/assets/metrics/svyglm_metrics_complete.csv", row.names = FALSE)
}

# Métricas combinadas
metrics_complete <- data.frame(
  Modelo = c("GLM + Stepwise", "svyGLM Completo"),
  AIC = round(c(AIC(baseline$glm_stepwise$model$model), svy_metrics$AIC), 2),
  BIC = round(c(BIC(baseline$glm_stepwise$model$model), svy_metrics$BIC), 2),
  McFadden_R2 = round(c(glm_metrics$McFadden, NA), 4),
  Nagelkerke_R2 = round(c(glm_metrics$Nagelkerke, NA), 4),
  AUC = round(c(as.numeric(glm_auc), as.numeric(svy_auc)), 4),
  N_Variables = c(
    baseline$glm_stepwise$stepwise$n_terms,
    length(baseline$svyglm_complete$vars)
  )
)

write.csv(metrics_complete, "report/assets/metrics/model_metrics_complete.csv", row.names = FALSE)