# ============================================================================
# Construcción del target: Atención Prenatal Adecuada (APA)
# ENDES 2024 | Módulos REC41 + REC94
# ============================================================================

# 1. Carga e integración de datos ---------------------------------------------

# Lectura de módulos
REC41 <- as.data.table(read_sav("data/raw/endes/Modulo1633/REC41_2024.sav"))
REC94 <- as.data.table(read_sav("data/raw/endes/Modulo1633/REC94_2024.sav"))

# Limpieza de CASEID para consistencia en el merge
REC41[, CASEID := gsub(" ", "", CASEID)]
REC94[, CASEID := gsub(" ", "", CASEID)]

# Selección de variables
REC41_1 <- subset(REC41, select = c(CASEID, MIDX, M14, M13, M42C, M42D, M42E, M2A, M2B, M2C))
REC94_1 <- subset(REC94, select = c(CASEID, IDX94, S411G, S411H, S411BA, S411CA, S411DA, S411EA))

# Estandarización de clave
REC94_1$MIDX <- REC94_1$IDX94

# Verificación de unicidad
any(duplicated(REC41_1[, c("CASEID", "MIDX")]))
any(duplicated(REC94_1[, c("CASEID", "MIDX")]))

# Merge
APA_nac <- merge(REC41_1, REC94_1, by = c("CASEID", "MIDX"), all = FALSE)


# 2. Tratamiento de códigos especiales -----------------------------------------

# No sabe (98) -> NA
na98 <- c("M14", "M13", "S411BA", "S411CA", "S411DA", "S411EA")
APA_nac[, (na98) := lapply(.SD, function(x) {
  x <- as.numeric(x)
  x[x == 98] <- NA
  x
}), .SDcols = na98]

# No responde (8) -> NA
na8 <- c("M42C", "M42D", "M42E", "S411G", "S411H")
APA_nac[, (na8) := lapply(.SD, function(x) {
  x <- as.numeric(x)
  x[x == 8] <- NA
  x
}), .SDcols = na8]


# 3. Análisis de faltantes -----------------------------------------------------
# Los NA son estructurales (filtros del cuestionario) y tienen sentido
# sustantivo: madres sin controles o sin tamizajes.

source("src/03_analysis/missing_analysis.R")

missing_apa <- analisis_missingness(
  data = APA_nac,
  cols = names(APA_nac),
  hacer_grafico = TRUE,
  hacer_little = FALSE
)


# 4. Construcción del target ---------------------------------------------------
# Inicialización en 0: preserva NAs estructurales como "inadecuado".

APA_nac[, `:=`(
  apa_freq = 0,  # Frecuencia adecuada
  apa_qual = 0,  # Calidad adecuada
  apa_spec = 0,  # Especialista calificado
  apa_bin = 0    # APA completa
)]

# Frecuencia: ≥6 visitas + inicio en 1er trimestre
APA_nac[
  M14 >= 6 & M13 %in% 1:3,
  apa_freq := 1
]

# Calidad: exámenes básicos + tamizajes en 1er trimestre
APA_nac[
  M42C == 1 & M42D == 1 & M42E == 1 &
    S411G == 1 & S411H == 1 &
    S411BA %in% 1:3 & S411CA %in% 1:3 &
    S411DA %in% 1:3 & S411EA %in% 1:3,
  apa_qual := 1
]

# Especialista: médico, obstetriz o enfermera
APA_nac[
  M2A == 1 | M2B == 1 | M2C == 1,
  apa_spec := 1
]

# APA completa: cumple las tres dimensiones
APA_nac[
  apa_freq == 1 & apa_qual == 1 & apa_spec == 1,
  apa_bin := 1
]


# 5. Selección del último hijo por mujer ---------------------------------------
# Se conserva el nacimiento con MIDX más alto.

APA_nac <- APA_nac[order(CASEID, MIDX), ]
APA <- APA_nac[, .SD[which.max(MIDX)], by = CASEID]

any(duplicated(APA$CASEID))

APA <- subset(APA, select = c(CASEID, apa_bin, apa_freq, apa_qual, apa_spec))


# 6. Exportación ---------------------------------------------------------------

saveRDS(APA, "data/interim/apa_dimensions.rds")

# Dataset final: solo CASEID y target para el merge
APA <- subset(APA, select = c(CASEID, apa_bin))