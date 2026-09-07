# ============================================================================
# Integración, transformación y validación del dataset analítico
# ENDES 2024 | Target APA + Predictores
# ============================================================================

# 1. Joins --------------------------------------------------------------------

# Integración de componentes por CASEID
APA <- merge(APA, REC42_1, by = "CASEID")
APA <- merge(APA, REC0111_1, by = "CASEID")
APA <- merge(APA, RE516171_1, by = "CASEID")
APA <- merge(APA, RE223132_1, by = "CASEID")

# Integración geográfica por UBIGEO
APA[, UBIGEO := as.integer(UBIGEO)]
GEO_1[, UBIGEO := as.integer(UBIGEO)]
APA <- merge(APA, GEO_1, by = "UBIGEO", all.x = TRUE)

# Integración de infraestructura sanitaria (NA estructural -> 0)
RENIPRESS_1[, UBIGEO := as.integer(UBIGEO)]
APA <- merge(APA, RENIPRESS_1, by = "UBIGEO", all.x = TRUE)
APA[is.na(num_centros_medicos), num_centros_medicos := 0]

# Eliminación de llaves administrativas
APA <- subset(APA, select = -c(UBIGEO, CASEID))


# 2. Recodificación de variables categóricas -----------------------------------

# Target APA
APA$apa_bin <- ifelse(APA$apa_bin == 1, "Si", ifelse(APA$apa_bin == 0, "No", NA))

# Nivel educativo
APA$V106 <- ifelse(APA$V106 == 0, "Sin educación",
                   ifelse(APA$V106 == 1, "Primaria",
                          ifelse(APA$V106 == 2, "Secundaria",
                                 ifelse(APA$V106 == 3, "Superior", NA))))

# Quintil de riqueza
APA$V190 <- ifelse(APA$V190 == 1, "Muy pobre",
                   ifelse(APA$V190 == 2, "Pobre",
                          ifelse(APA$V190 == 3, "Medio",
                                 ifelse(APA$V190 == 4, "Rico",
                                        ifelse(APA$V190 == 5, "Muy rico", NA)))))

# Área de residencia
APA$V025 <- ifelse(APA$V025 == 1, "Urbano", ifelse(APA$V025 == 2, "Rural", NA))

# Etnicidad
APA$V131 <- ifelse(APA$V131 %in% c(1:9), "Etnia nativa",
                   ifelse(APA$V131 == 10, "Etnia castellana",
                          ifelse(APA$V131 %in% c(11,12), "Etnia Extranjera", NA)))

# Situación laboral
APA$V731 <- ifelse(APA$V731 %in% c(0,1), "Sin empleo activo",
                   ifelse(APA$V731 == 2, "Empleo activo",
                          ifelse(APA$V731 == 3, "Empleo en licencia", NA)))

# Estado civil: 1=Casada | 2=Conviviente | 0=Soltera | 3=Viuda | 4=Divorciada | 5=Separada
APA$V501 <- ifelse(APA$V501 %in% c(0,3,4,5), "Sola",
                   ifelse(APA$V501 %in% c(1,2), "Pareja Estable", NA))

# Embarazo planeado: 1=Deseado | 2=Deseado más tarde | 3=No deseado
APA$V367 <- ifelse(APA$V367 == 1, "Planeado",
                   ifelse(APA$V367 %in% c(2,3), "No Planeado", NA))


# 3. Renombramiento de variables -----------------------------------------------

names(APA)[names(APA) == "V106"] <- "niv_educ"
names(APA)[names(APA) == "V190"] <- "quintil_riq"
names(APA)[names(APA) == "V025"] <- "area_res"
names(APA)[names(APA) == "V131"] <- "etnicidad"
names(APA)[names(APA) == "V012"] <- "edad"
names(APA)[names(APA) == "V022"] <- "estrato"
names(APA)[names(APA) == "V021"] <- "conglomerado"
names(APA)[names(APA) == "V005"] <- "factor_exp"
names(APA)[names(APA) == "V731"] <- "situacion_lab"
names(APA)[names(APA) == "V501"] <- "estado_civil"
names(APA)[names(APA) == "V220"] <- "hijos"
names(APA)[names(APA) == "V367"] <- "emb_planeado"


# 4. Estandarización de pesos muestrales ---------------------------------------

APA$factor_exp <- APA$factor_exp / 1000000


# 5. Limpieza de metadatos SPSS ------------------------------------------------

APA[] <- lapply(APA, function(x) {
  attr(x, "label") <- NULL
  return(x)
})


# 6. Análisis de datos faltantes -----------------------------------------------
# Los NA son estructurales (filtros del cuestionario) y tienen sentido
# sustantivo en el proceso de atención prenatal.

source("src/03_analysis/missing_analysis.R")

missing_apa <- analisis_missingness(
  data = APA,
  cols = names(APA),
  hacer_grafico = TRUE,
  hacer_little = FALSE
)


# 7. Conversión de tipos de datos ----------------------------------------------

# Variables ordinales (con niveles y categoría de referencia)
APA$niv_educ <- factor(APA$niv_educ, levels = c("Sin educación", "Primaria", "Secundaria", "Superior"))
APA$quintil_riq <- factor(APA$quintil_riq, levels = c("Muy pobre", "Pobre", "Medio", "Rico", "Muy rico"))

# Variables nominales (primera categoría = referencia en regresión)
APA$apa_bin <- factor(APA$apa_bin, levels = c("No", "Si"))
APA$area_res <- factor(APA$area_res, levels = c("Rural", "Urbano"))
APA$seguro <- factor(APA$seguro, levels = c("Ninguno", "SIS", "ESSALUD"))
APA$etnicidad <- factor(APA$etnicidad, levels = c("Etnia nativa", "Etnia castellana", "Etnia Extranjera"))
APA$situacion_lab <- factor(APA$situacion_lab, levels = c("Sin empleo activo", "Empleo activo", "Empleo en licencia"))
APA$estado_civil <- factor(APA$estado_civil, levels = c("Sola", "Pareja Estable"))
APA$emb_planeado <- factor(APA$emb_planeado, levels = c("No Planeado", "Planeado"))
APA$SDEPARTAMENTO <- factor(APA$SDEPARTAMENTO, levels = c(
  "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
  "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO",
  "ICA", "JUNIN", "LA LIBERTAD", "LAMBAYEQUE", "LIMA PROVINCIA",
  "LIMA REGION", "LORETO", "MADRE DE DIOS", "MOQUEGUA", "PASCO",
  "PIURA", "PUNO", "SAN MARTIN", "TACNA", "TUMBES", "UCAYALI"
))

# Variables numéricas
num_cols <- c("edad", "hijos", "num_centros_medicos", "factor_exp")
APA[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

# Variables de diseño muestral
APA$conglomerado <- as.integer(APA$conglomerado)
APA$estrato <- as.integer(APA$estrato)

# Verificación final
str(APA)