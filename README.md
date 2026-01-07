# mortalidadUTI-oncohemato
Mortality study of onco-hematologic patients in the ICU

# Predictores de Mortalidad en Pacientes Oncohematológicos en UTI

Este repositorio contiene el código fuente y los datos anonimizados para el análisis de factores pronósticos de mortalidad en la Unidad de Terapia Intensiva.

## 📂 Estructura del Repositorio

### 1. Datos (`data/`)
* `df_analisis.rds`: Dataset final anonimizado en formato nativo de R. Conserva la estructura de factores y niveles de referencia utilizados en el modelo logístico.

### 2. Código (`scripts/`)
Aquí se encuentra la transparencia del análisis:
* `analisis_completo.Rmd`: **Archivo Principal**. RMarkdown con la narrativa completa, limpieza de datos, y discusión.
* `01_model_development.R`: Script de selección de variables y evaluación de linealidad (Splines vs Lineal).
* `final_analysis.R`: Script limpio para reproducir el modelo final y la validación cruzada.

### 3. Resultados (`output/`)
* `reporte_analisis.pdf`: **Reporte Completo**. Documento estático generado desde el RMarkdown que incluye todas las tablas, gráficos y resultados estadísticos paso a paso, listo para lectura sin software adicional.

## 🛠 Metodología
Se utilizó un modelo de regresión logística multivariada validado mediante *5-fold Cross-Validation*. Se evaluó la no-linealidad de variables continuas mediante Splines Naturales.

## 💻 Requisitos
* R version 4.x
* Paquetes principales: `tidyverse`, `caret`, `sjPlot`, `performance`.

---
*Análisis realizado con fines de investigación académica y transparencia científica.*
