# Trabajo Final: Módulo de R


## Dataset Utilizado y Origen
* **Origen:** Los datos provienen de la **Encuesta Permanente de Hogares (EPH)** elaborada por el INDEC (Argentina). 
* **Extracción:** La descarga de la base de microdatos (individuos) correspondiente al **primer trimestre de 2026** se realiza de forma automatizada en el script mediante el paquete oficial `eph` en R.
* **Población objetivo:** Se filtró la muestra para trabajar exclusivamente con población asalariada ocupada en su ocupación principal, con edades comprendidas entre los 18 y 65 años.

## Técnicas Aplicadas
El proyecto aborda la modelización de la oferta laboral combinando econometría tradicional y aprendizaje estadístico:
1. **Análisis Exploratorio de Datos (EDA):** Procesamiento de variables, tratamiento de valores atípicos y análisis ponderado utilizando el ponderador poblacional.
2. **Inferencia por Mínimos Cuadrados Ordinarios (MCO):** Estimaciones base y extendidas (incorporando interacciones) evaluando significancia, supuestos y diagnósticos de residuos.
3. **Modelos de Regularización (Ridge y Lasso):** Implementación de validación cruzada y partición de datos para evaluar la estabilidad de los coeficientes, el control de sobreajuste y la capacidad predictiva fuera de muestra medida a través del Error Cuadrático Medio.

## Cómo Correr el Proyecto

1. **Clonar o descargar** este repositorio en tu computadora.
2. Abrir el archivo `tp_r_Calderón_Sosinowicz.R` en RStudio.
3. Asegurarte particularmente de tener instaladas las librerías necesarias ejecutando las primeras lineas del script.
