# ============================================================================
# Análisis de dimensiones de APA
# ENDES 2024 | Frecuencia, Calidad y Competencia
# ============================================================================

# 0. Setup --------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(UpSetR)
library(xtable)

source("src/04_utils/results_helpers.R")

# Crear directorios de salida
dir.create("report/assets/tabs", recursive = TRUE, showWarnings = FALSE)
dir.create("report/assets/figs", recursive = TRUE, showWarnings = FALSE)
dir.create("report/assets/metrics", recursive = TRUE, showWarnings = FALSE)


# 1. Carga de datos ------------------------------------------------------------

cat("\n========================================\n")
cat("ANÁLISIS DE DIMENSIONES DE APA\n")
cat("========================================\n")

APA_dim <- readRDS("data/interim/apa_dimensions.rds")

cat("\nDatos cargados:", nrow(APA_dim), "observaciones\n")


# 2. Tabla 1: Cumplimiento de dimensiones --------------------------------------

cat("\n--- Generando Tabla 1: Cumplimiento de dimensiones ---\n")

dimensiones <- data.frame(
  Dimension = c("Frecuencia y oportunidad", "Calidad y tamizaje", "Competencia"),
  Cumple = c(
    sum(APA_dim$apa_freq == 1),
    sum(APA_dim$apa_qual == 1),
    sum(APA_dim$apa_spec == 1)
  ),
  No_cumple = c(
    sum(APA_dim$apa_freq == 0),
    sum(APA_dim$apa_qual == 0),
    sum(APA_dim$apa_spec == 0)
  )
)

dimensiones$Pct_cumple <- round(dimensiones$Cumple / nrow(APA_dim) * 100, 1)
dimensiones$Pct_no_cumple <- round(dimensiones$No_cumple / nrow(APA_dim) * 100, 1)

export_table(dimensiones, "table3_apa_dimensions", 
             caption = "Cumplimiento de dimensiones de APA")

cat("  -> Tabla guardada\n")
print(dimensiones)


# 3. Tabla 2: Combinaciones de dimensiones -------------------------------------

cat("\n--- Generando Tabla 2: Combinaciones de dimensiones ---\n")

combinaciones <- APA_dim %>%
  group_by(apa_freq, apa_qual, apa_spec) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    pct = round(n / nrow(APA_dim) * 100, 1),
    combinacion = case_when(
      apa_freq == 1 & apa_qual == 1 & apa_spec == 1 ~ "Tres dimensiones",
      apa_freq == 1 & apa_qual == 1 & apa_spec == 0 ~ "Frecuencia + Calidad",
      apa_freq == 1 & apa_qual == 0 & apa_spec == 1 ~ "Frecuencia + Competencia",
      apa_freq == 0 & apa_qual == 1 & apa_spec == 1 ~ "Calidad + Competencia",
      apa_freq == 1 & apa_qual == 0 & apa_spec == 0 ~ "Solo Frecuencia",
      apa_freq == 0 & apa_qual == 1 & apa_spec == 0 ~ "Solo Calidad",
      apa_freq == 0 & apa_qual == 0 & apa_spec == 1 ~ "Solo Competencia",
      TRUE ~ "Ninguna"
    )
  ) %>%
  arrange(desc(n))

export_table(combinaciones, "table4_apa_combinations", 
             caption = "Combinaciones de dimensiones de APA")

cat("  -> Tabla guardada\n")
print(combinaciones)


# 4. Figura 1: Gráfico de barras apiladas --------------------------------------

cat("\n--- Generando Figura 1: Cumplimiento de dimensiones ---\n")

dim_long <- dimensiones %>%
  select(Dimension, Cumple, No_cumple) %>%
  pivot_longer(cols = c(Cumple, No_cumple), names_to = "Estado", values_to = "n")

dim_long$Estado <- factor(dim_long$Estado, levels = c("No_cumple", "Cumple"))

p1 <- ggplot(dim_long, aes(x = Dimension, y = n, fill = Estado)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(n / nrow(APA_dim) * 100, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(
    values = c("Cumple" = "#1A9850", "No_cumple" = "#D73027"),
    labels = c("Cumple", "No cumple")
  ) +
  labs(
    title = "Cumplimiento de dimensiones de Atención Prenatal Adecuada",
    subtitle = paste0("ENDES 2024 (n = ", nrow(APA_dim), ")"),
    x = "Dimensión",
    y = "Frecuencia",
    fill = "Estado"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

ggsave("report/assets/figs/fig2_apa_dimensions_bars.png", p1, width = 8, height = 6, dpi = 300)
ggsave("report/assets/figs/fig2_apa_dimensions_bars.pdf", p1, width = 8, height = 6)
cat("  -> Figura guardada\n")


# 5. Figura 2: Diagrama de Upset -----------------------------------------------

cat("\n--- Generando Figura 2: Intersección de dimensiones ---\n")

upset_data <- APA_dim %>%
  mutate(
    Frecuencia = as.integer(apa_freq == 1),
    Calidad = as.integer(apa_qual == 1),
    Competencia = as.integer(apa_spec == 1)
  ) %>%
  select(Frecuencia, Calidad, Competencia)

# PNG
png("report/assets/figs/fig3_apa_dimensions_upset.png", width = 800, height = 600, res = 120)
upset(upset_data, 
      sets = c("Frecuencia", "Calidad", "Competencia"),
      keep.order = TRUE,
      sets.bar.color = c("#377EB8", "#4DAF4A", "#E41A1C"),
      mainbar.y.label = "Intersecciones",
      sets.x.label = "Tamaño del conjunto",
      text.scale = 1.2,
      mb.ratio = c(0.6, 0.4))
dev.off()

# PDF
pdf("report/assets/figs/fig3_apa_dimensions_upset.pdf", width = 10, height = 7)
upset(upset_data, 
      sets = c("Frecuencia", "Calidad", "Competencia"),
      keep.order = TRUE,
      sets.bar.color = c("#377EB8", "#4DAF4A", "#E41A1C"),
      mainbar.y.label = "Intersecciones",
      sets.x.label = "Tamaño del conjunto",
      text.scale = 1.2,
      mb.ratio = c(0.6, 0.4))
dev.off()

cat("  -> Figura guardada\n")


# 6. Métricas resumen ----------------------------------------------------------

cat("\n--- Generando métricas resumen ---\n")

summary_metrics <- data.frame(
  Metrica = c(
    "Total mujeres",
    "Cumple APA (todas dimensiones)",
    "No cumple APA",
    "Cumple frecuencia",
    "Cumple calidad",
    "Cumple competencia"
  ),
  Valor = c(
    nrow(APA_dim),
    sum(APA_dim$apa_bin == 1),
    sum(APA_dim$apa_bin == 0),
    sum(APA_dim$apa_freq == 1),
    sum(APA_dim$apa_qual == 1),
    sum(APA_dim$apa_spec == 1)
  ),
  Porcentaje = c(
    100,
    round(sum(APA_dim$apa_bin == 1) / nrow(APA_dim) * 100, 1),
    round(sum(APA_dim$apa_bin == 0) / nrow(APA_dim) * 100, 1),
    round(sum(APA_dim$apa_freq == 1) / nrow(APA_dim) * 100, 1),
    round(sum(APA_dim$apa_qual == 1) / nrow(APA_dim) * 100, 1),
    round(sum(APA_dim$apa_spec == 1) / nrow(APA_dim) * 100, 1)
  )
)

write.csv(summary_metrics, "report/assets/metrics/apa_dimensions_summary.csv", row.names = FALSE)