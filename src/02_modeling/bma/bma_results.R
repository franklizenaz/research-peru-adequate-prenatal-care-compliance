# ============================================================================
# Resultados Pseudo-BMA - tablas y figuras
# ENDES 2024 | PIPs, top modelos y comparaciones
# ============================================================================

# 0. Setup --------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(xtable)

source("src/04_utils/results_helpers.R")
source("src/04_utils/group_definitions.R")

# Crear directorios
dir.create("report/assets/tabs", recursive = TRUE, showWarnings = FALSE)
dir.create("report/assets/figs", recursive = TRUE, showWarnings = FALSE)


# 1. Carga de resultados -------------------------------------------------------

cat("\n========================================\n")
cat("RESULTADOS PSEUDO-BMA\n")
cat("========================================\n")

bma <- readRDS("src/models/bma/bma_results_exhaustive.rds")
baseline <- readRDS("src/models/baseline/baseline_models.rds")

cat("\nDatos cargados exitosamente\n")

nombres_grupos <- bma$nombres_grupos
PIP <- bma$PIP


# 2. Tabla 7: PIPs por grupo ---------------------------------------------------

cat("\n--- Generando Tabla 7: PIPs por grupo ---\n")

pips_table <- data.frame(
  Grupo = nombres_grupos,
  PIP = round(PIP, 4),
  Seleccionado = ifelse(PIP >= 0.5, "Sí", "No")
)

pips_table <- pips_table[order(pips_table$PIP, decreasing = TRUE), ]
pips_table$Rango <- 1:nrow(pips_table)

pips_table <- pips_table[, c("Rango", "Grupo", "PIP", "Seleccionado")]

export_table(pips_table, "table8_bma_pips",
             caption = "Probabilidades de inclusión posterior (PIP) por grupo de variables")
cat("  -> Tabla 7 guardada\n")


# 3. Tabla 8: Top 5 modelos por PMP --------------------------------------------

cat("\n--- Generando Tabla 8: Top modelos por PMP ---\n")

top5 <- bma$top10_modelos[1:min(5, nrow(bma$top10_modelos)), ]
top5 <- top5[, c("Rango", "Modelo_ID", "PMP", "pBIC", "Variables")]

export_table(top5, "table9_bma_top_models",
             caption = "Top 5 modelos con mayor probabilidad posterior (PMP)")
cat("  -> Tabla 8 guardada\n")


# 4. Tabla 9: PIP vs Stepwise --------------------------------------------------

cat("\n--- Generando Tabla 9: Comparación PIP vs Stepwise ---\n")

stepwise_selected <- baseline$glm_stepwise$stepwise$groups_selected

comparison_table <- data.frame(
  Grupo = nombres_grupos,
  PIP = round(PIP, 4),
  Stepwise = ifelse(nombres_grupos %in% stepwise_selected, "Sí", "No"),
  Consistente = ifelse(
    (PIP >= 0.5 & nombres_grupos %in% stepwise_selected) |
      (PIP < 0.5 & !(nombres_grupos %in% stepwise_selected)),
    "Sí", "No"
  )
)

comparison_table <- comparison_table[order(comparison_table$PIP, decreasing = TRUE), ]

export_table(comparison_table, "table10_bma_vs_stepwise",
             caption = "Comparación: PIP vs selección Stepwise")
cat("  -> Tabla 10 guardada\n")


# 5. Tabla 10: PIP vs svyGLM (corregido) ---------------------------------------

cat("\n--- Generando Tabla 10: Comparación PIP vs significancia en svyGLM ---\n")

# Extraer p-values del svyGLM
svy_model <- baseline$svyglm_complete$model$model
svy_summary <- summary(svy_model)
svy_pvalues <- svy_summary$coefficients[, 4]
svy_pvalues <- svy_pvalues[!names(svy_pvalues) %in% "(Intercept)"]

# Identificar variables significativas
significant_vars <- names(svy_pvalues)[svy_pvalues < 0.05]

# Función: ¿el grupo tiene AL MENOS UNA categoría significativa?
is_group_significant <- function(grupo) {
  dummies_grupo <- get_groups()[[grupo]]
  any(dummies_grupo %in% significant_vars)
}

comparison_table <- data.frame(
  Grupo = nombres_grupos,
  PIP = round(PIP, 4),
  Significativo_svyGLM = ifelse(sapply(nombres_grupos, is_group_significant), "Sí", "No"),
  Consistente = ifelse(
    (PIP >= 0.5 & sapply(nombres_grupos, is_group_significant)) |
      (PIP < 0.5 & !sapply(nombres_grupos, is_group_significant)),
    "Sí", "No"
  )
)

comparison_table <- comparison_table[order(comparison_table$PIP, decreasing = TRUE), ]

export_table(comparison_table, "table11_bma_vs_svyglm",
             caption = "Comparación: PIP (BMA) vs significancia estadística (svyGLM)")
cat("  -> Tabla 11 guardada\n")


# 6. Figura 6: PIPs (forest plot) ----------------------------------------------

cat("\n--- Generando Figura 6: Forest plot de PIPs ---\n")

plot_data <- data.frame(
  Grupo = nombres_grupos,
  PIP = PIP
)
plot_data <- plot_data[order(plot_data$PIP), ]

p6 <- ggplot(plot_data, aes(x = reorder(Grupo, PIP), y = PIP)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", alpha = 0.7, size = 0.8) +
  geom_hline(yintercept = 0.8, linetype = "dotted", color = "orange", alpha = 0.5, size = 0.6) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "Probabilidades de Inclusión Posterior (PIP)",
    subtitle = paste0("Pseudo-BMA por enumeración completa (", bma$n_modelos_evaluados, " modelos evaluados)"),
    x = "Grupo de variables",
    y = "PIP"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    panel.grid.minor = element_blank()
  )

ggsave("report/assets/figs/fig6_bma_pips.png", p6, width = 9, height = 6, dpi = 300)
ggsave("report/assets/figs/fig6_bma_pips.pdf", p6, width = 9, height = 6)
cat("  -> Figura 6 guardada\n")


# 7. Figura 7: Comparación de coeficientes (svyGLM vs BMA) ---------------------

cat("\n--- Generando Figura 7: Comparación de coeficientes (svyGLM vs BMA) ---\n")

svy_coef <- baseline$svyglm_complete$model$coefficients
svy_coef <- svy_coef[!names(svy_coef) %in% "(Intercept)"]

bma_coef <- bma$coeficientes$incondicional

vars_comunes <- intersect(names(svy_coef), names(bma_coef))

if (length(vars_comunes) > 0) {
  
  comp_data <- data.frame(
    Variable = vars_comunes,
    svyGLM = svy_coef[vars_comunes],
    BMA = bma_coef[vars_comunes]
  )
  
  comp_data$Diferencia <- abs(comp_data$svyGLM - comp_data$BMA)
  comp_data <- comp_data[order(comp_data$Diferencia, decreasing = TRUE), ]
  comp_data <- comp_data[1:min(15, nrow(comp_data)), ]
  
  comp_long <- comp_data %>%
    select(Variable, svyGLM, BMA) %>%
    pivot_longer(cols = c(svyGLM, BMA), names_to = "Modelo", values_to = "Coeficiente")
  
  p7 <- ggplot(comp_long, aes(x = reorder(Variable, Coeficiente), y = Coeficiente, color = Modelo)) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    coord_flip() +
    labs(
      title = "Comparación de Coeficientes: svyGLM vs BMA",
      subtitle = "Top 15 variables con mayor diferencia",
      x = "Variable",
      y = "Coeficiente",
      color = "Modelo"
    ) +
    scale_color_manual(values = c("svyGLM" = "darkgreen", "BMA" = "steelblue")) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 8),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      legend.position = "bottom"
    )
  
  ggsave("report/assets/figs/fig7_bma_vs_svyglm.png", p7, width = 10, height = 7, dpi = 300)
  ggsave("report/assets/figs/fig7_bma_vs_svyglm.pdf", p7, width = 10, height = 7)
  cat("  -> Figura 7 guardada\n")
  
} else {
  cat("  -> No hay variables comunes para comparar. Figura 7 no generada.\n")
}


# 8. Resumen en consola --------------------------------------------------------

cat("\n========================================\n")
cat("RESUMEN DE RESULTADOS BMA\n")
cat("========================================\n")

cat("\n--- Grupos con PIP > 0.5 ---\n")
grupos_pip_alto <- names(PIP)[PIP > 0.5]
if (length(grupos_pip_alto) > 0) {
  cat(paste(grupos_pip_alto, collapse = ", "), "\n")
} else {
  cat("Ningún grupo supera el umbral de 0.5\n")
}

cat("\n--- Top 3 PIPs ---\n")
print(round(sort(PIP, decreasing = TRUE)[1:min(3, length(PIP))], 4))

cat("\n--- Top 3 OR_BMA ---\n")
OR_BMA <- bma$coeficientes$OR_BMA
top_or <- sort(OR_BMA, decreasing = TRUE)[1:min(3, length(OR_BMA))]
print(round(top_or, 4))

cat("\n--- Diagnóstico de dominancia ---\n")
top10_mass <- sum(sort(bma$PMP, decreasing = TRUE)[1:min(10, length(bma$PMP))])
cat("Peso acumulado de los 10 mejores modelos:", round(top10_mass * 100, 1), "%\n")
if (top10_mass > 0.9) {
  cat("  → Alta concentración: pocos modelos dominan la evidencia.\n")
} else if (top10_mass > 0.5) {
  cat("  → Concentración moderada: hay incertidumbre entre varios modelos.\n")
} else {
  cat("  → Baja concentración: alta incertidumbre estructural.\n")
}