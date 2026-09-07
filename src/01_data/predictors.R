# ============================================================================
# Extracción de variables predictoras
# ENDES 2024 | Módulos REC42, REC0111, RE516171, RE223132 + fuentes externas
# ============================================================================

# 1. Variables socioeconómicas, demográficas y de salud ------------------------

# Seguro de salud (REC42)
REC42 <- as.data.table(read_sav("data/raw/endes/Modulo1634/REC42_2024.sav"))
REC42[, CASEID := gsub(" ", "", CASEID)]

REC42_1 <- subset(REC42, select = c(CASEID, V481E, V481G))

# Recodificación: ESSALUD, SIS o Ninguno
REC42_1[, seguro := ifelse(V481E == 1, "ESSALUD",
                           ifelse(V481G == 1, "SIS", "Ninguno"))]

REC42_1 <- subset(REC42_1, select = c(CASEID, seguro))

# Características sociodemográficas (REC0111)
REC0111 <- as.data.table(read_sav("data/raw/endes/Modulo1631/REC0111_2024.sav"))
REC0111[, CASEID := gsub(" ", "", CASEID)]

REC0111_1 <- subset(REC0111, select = c(
  CASEID,
  V106,   # Nivel educativo
  V190,   # Índice de riqueza (quintil)
  V025,   # Área de residencia (urbano/rural)
  V131,   # Etnicidad
  V012,   # Edad actual
  UBIGEO, # Código geográfico
  V022,   # Estrato muestral
  V021,   # Conglomerado
  V005    # Factor de expansión
))

# Empleo y estado civil (RE516171)
RE516171 <- as.data.table(read_sav("data/raw/endes/Modulo1635/RE516171_2024.sav"))
RE516171[, CASEID := gsub(" ", "", CASEID)]

RE516171_1 <- subset(RE516171, select = c(
  CASEID,
  V731,   # Trabajó en últimos 12 meses
  V501    # Estado civil actual
))

# Paridad y planificación del embarazo (RE223132)
RE223132 <- as.data.table(read_sav("data/raw/endes/Modulo1632/RE223132_2024.sav"))
RE223132[, CASEID := gsub(" ", "", CASEID)]

RE223132_1 <- subset(RE223132, select = c(
  CASEID,
  V220,   # Número total de hijos vivos (paridad)
  V367    # Embarazo deseado/planeado
))


# 2. Variables geográficas -----------------------------------------------------

# Departamento por UBIGEO (INEI)
GEO <- as.data.table(read.csv("data/raw/inei/ubigeo_distrito.csv"))
GEO[, UBIGEO := gsub(" ", "", inei)]

GEO_1 <- subset(GEO, select = c(UBIGEO, SDEPARTAMENTO))


# 3. Infraestructura sanitaria (RENIPRESS) -------------------------------------

# Cobertura de centros de salud por distrito
RENIPRESS <- as.data.table(read.csv2("data/raw/renipress/RENIPRESS_2026_v2.csv"))
RENIPRESS[, UBIGEO := gsub(" ", "", UBIGEO)]

RENIPRESS_1 <- subset(RENIPRESS, select = c(UBIGEO, ESTADO, CONDICION, INICIO_ACTIVIDAD, CATEGORIA))
RENIPRESS_1 <- na.omit(RENIPRESS_1)

# Limpieza y filtrado
RENIPRESS_1[, INICIO_ACTIVIDAD := as.Date(INICIO_ACTIVIDAD, format = "%d/%m/%Y")]

RENIPRESS_1 <- RENIPRESS_1 %>%
  filter(ESTADO == "ACTIVO" & CONDICION == "ACTIVO" & INICIO_ACTIVIDAD <= as.Date("2024-12-31")) %>%
  filter(CATEGORIA %in% c("I-3", "I-4", "II-1", "II-2", "III-1")) %>%
  group_by(UBIGEO) %>%
  summarise(num_centros_medicos = n())


# Nota: Variables consideradas para análisis posteriores:
# - Educación del esposo: descartada por generar NAs en madres solteras/divorciadas
# - Ingresos familiares
# - Antecedentes de complicaciones obstétricas
# - Creencias culturales
# - Conocimiento sobre VIH/ITS (REC758081)
# - Violencia doméstica (REC84DV)
# - Índice de paridad de género del hogar (V743A, V743D, D105A)