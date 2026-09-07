# 📁 Data

Este directorio contiene las fuentes de datos utilizadas en el proyecto.  
**No se incluyen los archivos originales** por restricciones de licencia de ENDES y RENIPRESS.

---

## 📋 Fuentes de Datos

### 1. ENDES 2024 (Encuesta Demográfica y de Salud Familiar)

**Institución:** INEI - Instituto Nacional de Estadística e Informática  
**Año:** 2024  
**Diseño:** Muestral complejo (estratificado, conglomerado, probabilístico)  
**Unidad de análisis:** Mujeres en edad fértil (15-49 años)

| Módulo | Archivo | Contenido |
|--------|---------|-----------|
| REC0111 | `Modulo1631/REC0111_2024.sav` | Nivel educativo, quintil de riqueza, área de residencia, etnicidad, edad, estrato, conglomerado, factor de expansión |
| RE223132 | `Modulo1632/RE223132_2024.sav` | Número de hijos vivos, embarazo planeado/deseado |
| REC41 | `Modulo1633/REC41_2024.sav` | Frecuencia de controles, calidad de atención, personal especializado |
| REC94 | `Modulo1633/REC94_2024.sav` | Tamizajes (sífilis, VIH), pruebas de laboratorio |
| REC42 | `Modulo1634/REC42_2024.sav` | Tipo de seguro de salud (SIS, ESSALUD, etc.) |
| RE516171 | `Modulo1635/RE516171_2024.sav` | Situación laboral, estado civil |

---

### 2. UBIGEO INEI

**Origen:** Repositorio asociado a datos INEI y RENIEC (Castagnetto, 2021). 
**Archivo:** `ubigeo_distrito.csv`  
**Contenido:** Código de ubicación geográfica por distrito (departamento, provincia, distrito)  
**Uso:** Asignación geográfica a nivel departamental

---

### 3. RENIPRESS 2026

**Institución:** MINSA - Ministerio de Salud  
**Archivo:** `RENIPRESS_2026_v2.csv`  
**Contenido:** Infraestructura de centros de salud: estado, condición, categoría, fecha de inicio de actividades, ubicación (UBIGEO)  
**Uso:** Cobertura de servicios de salud por distrito

---

## 📂 Estructura

````bash
data/
├── raw/ # Datos sin procesar (fuentes originales)
│ ├── endes/ # ENDES 2024 - Módulos utilizados
│ │ ├── Modulo1631/REC0111_2024.sav # Características sociodemográficas
│ │ ├── Modulo1632/RE223132_2024.sav # Paridad y planificación familiar
│ │ ├── Modulo1633/ # Nacimientos y atención prenatal
│ │ │ ├── REC41_2024.sav # Características del nacimiento
│ │ │ └── REC94_2024.sav # Atención prenatal y salud
│ │ ├── Modulo1634/REC42_2024.sav # Seguro de salud
│ │ └── Modulo1635/RE516171_2024.sav # Empleo y estado civil
│ │
│ ├── inei/
│ │ └── ubigeo_distrito.csv # Ubigeo distrital (departamento, provincia, distrito)
│ │
│ └── renipress/
│ └── RENIPRESS_2026_v2.csv # Infraestructura sanitaria (centros de salud)
│
├── interim/ # Datos procesados (intermedios)
│ └── apa_dimensions.* # Dimensiones APA (freq, qual, spec)
│
└── processed/ # Datos finales para modelamiento
└── apa.rds # Dataset analítico integrado
````

---

## 📌 Módulos ENDES NO UTILIZADOS (Disponibles para análisis posteriores)

| Módulo | Archivo | Contenido |
|--------|---------|-----------|
| RE758081 | `RE758081_2024.sav` | Conocimiento sobre VIH/ITS |
| REC84DV | `REC84DV_2024.sav` | Violencia doméstica (V743A, V743D, D105A) |

Estos módulos **no están incorporados** en el pipeline actual, pero pueden ser considerados en futuras extensiones del estudio.

---

## 🔑 Variables de Diseño Muestral (ENDES)

| Variable | Descripción |
|----------|-------------|
| `estrato` | Estrato de muestreo (V022) |
| `conglomerado` | Conglomerado de muestreo (V021) |
| `factor_exp` | Factor de expansión (V005) / 1,000,000 |

---

## 🔗 Obtención de Datos

- **ENDES 2024:** [ENDES](https://proyectos.inei.gob.pe/endes/)
- **RENIPRESS:** [SUSALUD](http://datos.susalud.gob.pe/dataset/registro-nacional-de-ipress-renipress)
- **UBIGEO:** [CASTAGNETTO](https://github.com/jmcastagnetto/ubigeo)