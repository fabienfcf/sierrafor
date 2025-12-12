# SIERRAFOR

**Sistema Integrado de Estimación y Regulación de Recursos Forestales**

Modelo de crecimiento, reclutamiento y mortalidad para bosques de pino-encino de zonas montañosas del noreste de México.

## Descripción

SIERRAFOR permite calcular e inferir las principales variables necesarias para la implementación de un Programa de Manejo Forestal según la **NOM-152-SEMARNAT-2023**.

### Características principales

- 🌲 Modelos poblacionales específicos para *Pinus* y *Quercus*
- 📊 Cálculo de ICA (Incremento Corriente Anual) biológico
- 🎯 Optimización de cortas con método ICA-Liocourt
- 📈 Simulación a 10 años con/sin manejo
- 📄 Generación automática de tablas LaTeX para PMF
- 📉 Gráficos de distribuciones diamétricas, erosión, sanidad

### Estructura del proyecto
```
modelov5/
├── config/          # Configuración (especies, códigos, parámetros)
├── core/            # Modelos poblacionales (crecimiento, mortalidad, reclutamiento)
├── analisis/        # Análisis descriptivo y estadístico
├── simulaciones/    # Simulación forestal 10 años
├── generadores/     # Generación de tablas/gráficos PMF
├── reportes/        # Sistema de reportes
├── workflows/       # Workflows principales
└── utils/           # Utilidades compartidas
```

## Requisitos

- R >= 4.0
- tidyverse, readxl, janitor, xtable, patchwork

## Uso
```r
# Ejecutar workflow completo
setwd("modelov5")
source("workflows/40_WORKFLOW_COMPLETO.R")
```

## Outputs

- 📋 14+ tablas LaTeX (NOM-152 compliant)
- 📊 Gráficos de distribución, erosión, sanidad
- 📈 Métricas dendrométricas por rodal
- 🌳 Historial de simulación 10 años
- ✂️ Registro detallado de cortas

## Metodología

### Modelos de crecimiento
- Chapman-Richards calibrados con datos INFyS 2015-2020
- Ecuaciones alométricas específicas por especie

### Método de cortas
1. ICA define volumen objetivo (biológicamente realista)
2. Liocourt identifica clases sobrepobladas (Q-factor 1.7)
3. Priorización: árboles suprimidos → regeneración natural

### NOM-152 Compliance
Genera todas las tablas requeridas:
- Posibilidad anual por especie y rodal
- Programa cronológico de cortas
- Distribución de productos por género
- Clasificación de superficies

## Autor

Dr Fabien Charbonnier  
Facultad de Ciencias Forestales - UANL

## Licencia

[Especificar licencia]
