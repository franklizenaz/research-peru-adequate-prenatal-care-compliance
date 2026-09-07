# 📊 Determinantes de la Atención Prenatal Adecuada (APA) en Perú

> Análisis de los determinantes de la **Atención Prenatal Adecuada (APA)** en mujeres peruanas utilizando datos de la **ENDES 2024**. El estudio aplica **Promediación de Modelos Bayesianos (Pseudo-BMA)** con ajuste por **diseño muestral complejo** para identificar factores robustos asociados a la calidad de la atención prenatal.

---

## 🎯 Objetivos

### General
Determinar los factores asociados a la APA en gestantes peruanas mediante regresión logística y Pseudo-BMA, considerando el diseño muestral complejo de la ENDES.

### Específicos
1. Estimar modelos de regresión logística con y sin diseño muestral complejo
2. Calcular Probabilidades de Inclusión Posterior (PIP) mediante Pseudo-BMA
3. Comparar la consistencia y estabilidad de coeficientes entre enfoques

---

## 📊 Metodología

### Fuente de Datos
- **ENDES 2024** (Encuesta Demográfica y de Salud Familiar)
- **Muestra analítica**: 17,608 mujeres de 14-49 años con parto/gestación en últimos 5 años
- **Diseño**: Probabilístico, estratificado, bietápico, por conglomerados

### Variable Dependiente (APA)
Variable binaria que cumple **simultáneamente** tres dimensiones:

| Dimensión | Criterio |
|-----------|----------|
| **Frecuencia + Oportunidad** | ≥6 controles + inicio en primer trimestre |
| **Calidad + Tamizaje** | 9 intervenciones clínicas (presión arterial, exámenes, VIH/sífilis, etc.) |
| **Competencia del personal** | Atención por médico/obstetra/enfermera |

### Análisis Estadístico

| Etapa | Método | Descripción |
|-------|--------|-------------|
| **1** | Regresión logística (GLM) | Modelo baseline sin diseño complejo + Stepwise |
| **2** | Regresión logística (svyGLM) | Con diseño muestral complejo (estratos, conglomerados, pesos) |
| **3** | Pseudo-BMA | Promediación de modelos con pseudo-BIC, PIP y coeficientes promedio |
| **4** | Comparación | Estabilidad de coeficientes y consistencia entre métodos |

### Ajuste por Diseño Complejo
- **Estratificación**: V022
- **Conglomerados**: V021
- **Ponderación**: V005 (normalizado)

---

## 📁 Estructura del Repositorio

```
research-adequate-prenatal-care-compliance/
│
├── README.md                               # Este archivo
│
├── data/                                   # Datos (no incluidos en el repo)
│   └── README.md                      
│
├── src/                                    # Código R para análisis
│   ├── 01_data/
│   ├── 02_modeling/
│   ├── 03_analysis/
│   └── 04_utils/
│
├── results/                                # Tablas, Gráficos & Métricas
│
└── report/                                 # Reporte
    └── peru-adequate-prenatal-care-compliance.pdf                            # Artículo completo
```

---

## 🛠️ Requisitos Técnicos

### Software
- **R** (v4.3+) con paquetes: `survey`, `srvyr`, `tidyverse`, `pROC`, `ggplot2`

