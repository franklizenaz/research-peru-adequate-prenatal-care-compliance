# ============================================================================
# Pipeline principal: construcción del dataset APA
# ENDES 2024
# ============================================================================

# 0. Paquetes -----------------------------------------------------------------

library(haven)
library(data.table)
library(dplyr)
library(naniar)
library(MissMech)
library(ggplot2)
library(arrow)


# 1. Construcción de variables ------------------------------------------------

# Variable dependiente: Atención Prenatal Adecuada (APA)
# Criterios: ≥6 visitas, inicio en el primer trimestre,
# calidad de atención y atención por personal especializado.
source("src/01_data/target_apa.R")

# Variables predictoras: covariables socioeconómicas, geográficas
# y reproductivas.
source("src/01_data/predictors.R")

# Integración y transformación: merge, recodificación y variables derivadas.
source("src/01_data/merge_transform.R")


# 2. Exportación ----------------------------------------------------------------

# Guarda el dataset final en RDS para conservar metadatos de R.
saveRDS(APA, "data/processed/apa.rds")
