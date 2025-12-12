# SIERRAFOR - Sistema de Simulación Forestal

**Sistema de Crecimiento, Reclutamiento y Mortalidad para Bosques de Pino-Encino**

Desarrollado para bosques de montaña del noreste de México. Permite calcular e inferir las principales variables necesarias para la implementación de un Programa de Manejo Forestal según la NOM-152-SEMARNAT-2006.

---

## 📋 Tabla de Contenidos

- [Descripción General](#descripción-general)
- [Estructura del Proyecto](#estructura-del-proyecto)
- [Módulos Principales](#módulos-principales)
- [Modelo de Simulación](#modelo-de-simulación)
- [Instalación y Uso](#instalación-y-uso)
- [Flujo de Trabajo](#flujo-de-trabajo)
- [Salidas y Resultados](#salidas-y-resultados)
- [Cambios Recientes](#cambios-recientes)

---

## 📖 Descripción General

SIERRAFOR es un sistema modular en R para la simulación del crecimiento forestal y optimización de programas de corta. El sistema implementa:

- **Modelos de crecimiento individual** basados en tasas diferenciales por género y dominancia
- **Modelo de mortalidad** con tasas ajustadas por clase de dominancia
- **Modelo de reclutamiento** que simula el ingreso de nuevos árboles
- **Optimizador de cortas** basado en el método Liocourt (distribución balanceada)
- **Cálculo de ICA** (Incremento Corriente Anual) y sostenibilidad

### Características Principales

✅ **Arquitectura modular refactorizada** - Código limpio sin duplicaciones
✅ **Funciones compartidas** - Utilidades centralizadas para validación y cálculos
✅ **Compatible con SIPLAFOR** - Usa códigos oficiales de dominancia, sanidad, erosión
✅ **Método ICA-Liocourt** - Cortas basadas en crecimiento real, no en metas arbitrarias
✅ **Reproducible** - Semillas fijas para simulaciones estocásticas

---

## 📁 Estructura del Proyecto

### Archivos de Configuración

| Archivo | Descripción |
|---------|-------------|
| `01_parametros_configuracion.R` | Carga centralizada de toda la configuración |
| `02_config_especies.R` | Catálogo de especies, ecuaciones alométricas y parámetros h-d |
| `03_config_codigos.R` | Códigos SIPLAFOR (dominancia, sanidad, erosión, etc.) |
| `04_config_simulacion.R` | Parámetros de simulación (mortalidad, reclutamiento, periodo) |
| `05_config_programa_cortas.R` | Calendario de intervenciones y parámetros de corta |

### Módulos Core

| Archivo | Descripción |
|---------|-------------|
| `15_core_calculos.R` | Funciones puras para cálculos dasométricos (volumen, área basal, filtros) |
| `utils_validacion.R` | **NUEVO** - Funciones de validación compartidas |
| `utils_metricas.R` | **NUEVO** - Cálculo de métricas sin duplicación |

### Modelos de Simulación

| Archivo | Descripción |
|---------|-------------|
| `10_modelos_crecimiento.R` | Incremento diamétrico y altura por árbol individual |
| `11_modelo_mortalidad.R` | Aplicación de mortalidad con probabilidades diferenciales |
| `12_modelo_reclutamiento.R` | Ingreso de nuevos árboles según composición actual |
| `13_simulador_crecimiento.R` | Simulador principal que integra los 3 procesos |
| `14_optimizador_cortas.R` | Optimización de cortas según método Liocourt |
| `16_calcular_ica.R` | Cálculo del Incremento Corriente Anual |

### Flujos de Trabajo

| Archivo | Descripción |
|---------|-------------|
| `00_importar_inventario.R` | Importación de datos desde Excel |
| `20_analisis_descriptivo.R` | Estadísticas y tablas descriptivas del inventario |
| `30_SIMULACION_10AÑOS_COMPLETA.R` | Simulación completa de 10 años con cortas |
| `40_WORKFLOW_COMPLETO.R` | **Punto de entrada principal** |
| `41_WORKFLOW_calcular_ica.r` | Flujo para calcular ICA específicamente |

### Análisis y Reportes

| Archivo | Descripción |
|---------|-------------|
| `31_stat x rodal.R` | Estadísticas por rodal |
| `32_tablas_pmf.R` | Generación de tablas para PMF (LaTeX) |
| `33_graficos_pmf.R` | Gráficos para PMF |
| `35_GENERAR_REPORTE_PMF.R` | Generación automática de reporte |

---

## 🧩 Módulos Principales

### 1. Módulo de Crecimiento (`10_modelos_crecimiento.R`)

**Funciones principales:**
- `calcular_incremento_diametro(arbol, config)` - Incremento anual en diámetro
- `calcular_incremento_altura(arbol, incremento_d, config)` - Incremento en altura (proporcional a dh/dd)
- `aplicar_crecimiento_anual(arbol, config)` - Aplica crecimiento a un árbol individual
- `aplicar_crecimiento_poblacion(arboles_df, config, año)` - Aplica a toda la población

**Parámetros de crecimiento:**
```r
CONFIG$crecimiento_base <- list(
  Pinus = 0.40,    # cm/año
  Quercus = 0.28,
  Juniperus = 0.25,
  Arbutus = 0.30,
  Otros = 0.30
)
```

**Modificadores por dominancia:**
```r
# Dominante (1):     factor = 1.00
# Codominante (2):   factor = 0.90
# Intermedio (3):    factor = 0.75
# Suprimido (6):     factor = 0.40
# Muertos (7,8,9):   factor = 0.00 (no crecen)
```

### 2. Módulo de Mortalidad (`11_modelo_mortalidad.R`)

**Funciones principales:**
- `calcular_probabilidad_muerte(arbol, config)` - Probabilidad anual de mortalidad
- `aplicar_mortalidad_arbol(arbol, config, valor_aleatorio)` - Decide si un árbol muere
- `aplicar_mortalidad_poblacion(arboles_df, config, año)` - Aplica a toda la población

**Parámetros:**
```r
CONFIG$mortalidad_base <- 0.01  # 1% anual base

# Modificadores por dominancia:
# Dominante:     0.5× (0.5% anual)
# Codominante:   0.7×
# Intermedio:    1.0×
# Suprimido:     2.0× (2% anual)
```

### 3. Módulo de Reclutamiento (`12_modelo_reclutamiento.R`)

**Funciones principales:**
- `calcular_n_reclutas(arboles_rodal, config)` - Número de reclutas según población viva
- `calcular_composicion_reclutas(arboles_rodal, n_reclutas, config)` - Distribución por especie
- `generar_reclutas(rodal_id, composicion, config, año)` - Genera árboles nuevos
- `aplicar_reclutamiento(arboles_df, config, año)` - Aplica a toda la población

**Parámetros:**
```r
CONFIG$tasa_reclutamiento <- 0.02  # 2% de la población viva
CONFIG$reclut_d_min <- 7.5         # cm
CONFIG$reclut_d_max <- 12.0        # cm
CONFIG$reclut_dominancia <- 6      # Suprimido
CONFIG$reclut_altura <- list(Pinus = 3.5, Quercus = 2.8, ...)
```

### 4. Simulador Integrado (`13_simulador_crecimiento.R`)

**Función principal:**
```r
resultado <- simular_crecimiento_rodal(
  arboles_inicial = arboles_df,
  config = CONFIG,
  años = 10
)
```

**Ciclo anual de simulación:**
```
Para cada año (1 a 10):
  1. Aplicar crecimiento
  2. Actualizar volúmenes
  3. Aplicar mortalidad
  4. Aplicar reclutamiento
  5. Guardar estado en historial
```

**Salidas:**
- `poblacion_inicial` - Estado al inicio
- `poblacion_final` - Estado después de N años
- `historial` - Todos los árboles en cada año
- `historial_metricas` - Métricas agregadas por año

### 5. Optimizador de Cortas (`14_optimizador_cortas.R`)

**Método Liocourt:**
- Distribución balanceada: `N(d+1) = q × N(d)`
- Q-factor define la forma de la distribución
- Cortas solo en clases con exceso
- Volumen cortado ≤ ICA (sostenibilidad garantizada)

**Función principal:**
```r
resultado_corta <- optimizar_corta_rodal(
  arboles_rodal,
  config,
  año_corta,
  aplicar_corta = TRUE
)
```

---

## 🔄 Modelo de Simulación

### Ecuaciones Fundamentales

#### 1. Crecimiento Diamétrico
```
Δd = tasa_base[género] × factor_dominancia[dominancia]
```

#### 2. Crecimiento en Altura
```
dh/dd = Chapman-Richards(d, especie, dominancia)
Δh = (dh/dd) × Δd
```

#### 3. Volumen Individual
Ecuaciones alométricas por especie:
```
Potencia: V = a × d^b × h^c
Exponencial: V = exp(a + b×ln(d) + c×ln(h))
```

#### 4. Mortalidad
```
P(muerte) = mortalidad_base × factor_dominancia
```

#### 5. Reclutamiento
```
N_reclutas = N_vivos × tasa_reclutamiento
Composición de reclutas = Composición actual del rodal
```

### Flujo de Simulación Detallado

```
AÑO 0 (Inventario Inicial)
├─ Importar datos
├─ Calcular volúmenes
├─ Asignar ecuaciones alométricas
└─ Estado inicial guardado

PARA cada año t = 1 a 10:

  ├─ CRECIMIENTO
  │  ├─ Calcular Δd para cada árbol vivo
  │  ├─ Calcular Δh según relación h-d
  │  ├─ Actualizar d, h
  │  └─ Recalcular volumen

  ├─ MORTALIDAD
  │  ├─ Calcular P(muerte) para cada vivo
  │  ├─ Generar valor aleatorio ~ U(0,1)
  │  ├─ Si U < P(muerte) → marcar como muerto (dom = 7)
  │  └─ Registrar año de muerte

  ├─ RECLUTAMIENTO
  │  ├─ Contar N_vivos por rodal
  │  ├─ Calcular N_reclutas = tasa × N_vivos
  │  ├─ Determinar composición según inventario
  │  ├─ Generar árboles con d ~ U(7.5, 12) cm
  │  └─ Agregar a población

  ├─ CORTAS (si año programado)
  │  ├─ Calcular ICA
  │  ├─ Calcular distribución Liocourt
  │  ├─ Identificar excesos por clase diamétrica
  │  ├─ Seleccionar árboles a cortar
  │  ├─ Verificar: Vol_corta ≤ ICA
  │  └─ Marcar árboles como cortados (dom = 8)

  └─ Guardar estado del año
```

---

## 🚀 Instalación y Uso

### Requisitos

```r
# Librerías necesarias
install.packages(c(
  "tidyverse",
  "readxl",
  "janitor",
  "xtable",
  "patchwork"
))
```

### Preparación de Datos

El inventario debe estar en formato Excel con las hojas:
- `F01` - Información de sitios
- `F02` - Regeneración
- `F03` - Árboles individuales
- `F04` - Virutas (incremento)
- `F05` - Regeneración adicional
- `F06` - Combustibles

Además, archivo CSV con estadísticas de muestreo: `UMM_stats.csv`

### Ejecución Básica

```r
# 1. Establecer directorio de trabajo
setwd("/ruta/a/tu/proyecto")

# 2. Ejecutar workflow completo
source("modelov5/40_WORKFLOW_COMPLETO.R")
```

Este workflow ejecuta automáticamente:
1. Carga de configuración
2. Importación de inventario
3. Construcción del dataset
4. Análisis descriptivo
5. Simulación de 10 años
6. Generación de tablas y gráficos

### Ejecución Personalizada

```r
# Cargar configuración
source("modelov5/01_parametros_configuracion.R")

# Importar inventario
source("modelov5/00_importar_inventario.R")
inventario <- importar_inventario_completo(
  ruta_archivo = "inventario_forestal.xlsx",
  ruta_umm = "UMM_stats.csv"
)

# Construir dataset
source("modelov5/15_core_calculos.R")
arboles <- construir_arboles_analisis(inventario, CONFIG)

# Simular 10 años
source("modelov5/10_modelos_crecimiento.R")
source("modelov5/11_modelo_mortalidad.R")
source("modelov5/12_modelo_reclutamiento.R")
source("modelov5/13_simulador_crecimiento.R")

resultado <- simular_crecimiento_rodal(
  arboles_inicial = arboles,
  config = CONFIG,
  años = 10
)

# Ver resultados
comparar_estados(resultado)
comparar_estados_por_genero(resultado)
```

---

## 📊 Salidas y Resultados

### Estructura de Archivos Generada

```
proyecto/
├─ datos_intermedios/
│  ├─ arboles_analisis.rds
│  └─ inventario_completo.rds
│
├─ resultados/
│  ├─ analisis_descriptivo.rds
│  ├─ historial_completo_10años.rds
│  ├─ metricas_10años.rds
│  └─ registro_cortas.rds
│
├─ tablas_latex/
│  ├─ desc_01_resumen_rodal.tex
│  ├─ desc_02_composicion_genero.tex
│  ├─ desc_03_top_especies.tex
│  ├─ 01_inventario_inicial.tex
│  ├─ 02_comparacion_inicial_final.tex
│  ├─ 03_intensidad_corte_rodal.tex
│  └─ ...
│
└─ graficos/
   ├─ desc_01_distribucion_diametrica.png
   ├─ desc_02_erosion.png
   ├─ desc_03_sanidad.png
   ├─ evolucion_10años_rodales.png
   └─ ...
```

### Métricas Calculadas

Para cada año y rodal:
- **Población**: N° árboles vivos, densidad/ha
- **Dimensiones**: Diámetro medio, altura media
- **Volumen**: Total muestreado (m³), volumen/ha (m³/ha)
- **Área basal**: Total muestreada (m²), área basal/ha (m²/ha)
- **Incrementos**: Δd medio, Δh medio, ΔV total
- **Mortalidad**: N° muertos, tasa (%)
- **Reclutamiento**: N° reclutas, distribución por género
- **Cortas**: N° cortados, volumen extraído, intensidad (%)

### Gráficos Generados

1. **Evolución temporal** (10 años):
   - Volumen/ha por rodal
   - Población total
   - Diámetro medio

2. **Distribución diamétrica**:
   - Histograma por clase (5 cm)
   - Por género (Pinus vs Quercus)
   - Comparación inicial vs final

3. **Análisis de cortas**:
   - Distribución Liocourt (observada vs ideal)
   - Intensidad de corta por clase diamétrica
   - Excesos y defectos por clase

---

## 🔄 Cambios Recientes - Refactorización

### ✅ Eliminación de Código Duplicado

**Antes:**
- `validar_crecimiento()` duplicada en `10_modelos_crecimiento.R` y `13_simulador_crecimiento.R`
- `validar_mortalidad()` duplicada
- `validar_reclutamiento()` duplicada
- `calcular_metricas_*()` triplicadas con lógica idéntica

**Después:**
- ✅ **Nuevo módulo** `utils_validacion.R` con todas las validaciones centralizadas
- ✅ **Nuevo módulo** `utils_metricas.R` con función genérica de métricas
- ✅ Todos los módulos ahora importan funciones compartidas
- ✅ **~400 líneas de código eliminadas**

### ✅ Limpieza de Archivos

- ❌ Eliminado `20_analisis_descriptivo_old.R` (990 líneas duplicadas)
- ✅ Eliminado código comentado sin uso
- ✅ Estandarizado idioma de comentarios (español)
- ✅ Simplificado mensajes de carga de módulos

### ✅ Arquitectura Mejorada

**Nueva estructura modular:**

```
CONFIG (configuración centralizada)
  │
  ├─ core_calculos.R (funciones puras)
  ├─ utils_validacion.R (validaciones compartidas)
  └─ utils_metricas.R (cálculos de métricas)
       │
       ├─ modelos_crecimiento.R
       ├─ modelo_mortalidad.R
       ├─ modelo_reclutamiento.R
       └─ simulador_crecimiento.R
```

### Funciones Compartidas Nuevas

#### `utils_validacion.R`
```r
validar_crecimiento(arboles_antes, arboles_despues)
validar_mortalidad(arboles_antes, arboles_despues)
validar_reclutamiento(arboles_antes, arboles_despues, config)
```

#### `utils_metricas.R`
```r
# Función genérica con agrupamiento flexible
calcular_metricas(arboles_df, agrupar_por, config)

# Wrappers para compatibilidad
calcular_metricas_estado(arboles_df, config)
calcular_metricas_por_genero(arboles_df, config)
calcular_metricas_por_especie(arboles_df, config)
```

### Beneficios de la Refactorización

1. **Mantenibilidad** - Un solo lugar para actualizar cada función
2. **Consistencia** - Todas las validaciones usan la misma lógica
3. **Menos errores** - No hay riesgo de versiones inconsistentes
4. **Más simple** - Módulos más cortos y enfocados
5. **Testeable** - Funciones compartidas fáciles de probar

---

## 📝 Notas Técnicas

### Reproducibilidad

Las simulaciones usan semillas fijas:
```r
CONFIG$semilla_mortalidad <- 42
set.seed(CONFIG$semilla_mortalidad + año_actual)
```

### Expansión a Hectárea

El sistema diferencia claramente:
- **Valores muestreados**: Medidos directamente en parcelas
- **Valores/ha**: Expandidos usando factor = 1 / área_total_muestreada

```r
area_total <- area_parcela × n_muestreos
valor_ha <- valor_muestreado × (1 / area_total)
```

### Compatibilidad SIPLAFOR

Todos los códigos son compatibles con SIPLAFOR:
- Dominancia (1-9)
- Sanidad (1-5)
- Erosión (0-3)
- Vigor (1-3)
- Y 15+ tablas adicionales

---

## 📧 Soporte y Contribuciones

Para preguntas, sugerencias o reporte de errores, contactar al equipo de desarrollo.

---

## 📜 Licencia

Este proyecto está bajo licencia MIT. Ver archivo `LICENSE` para más detalles.

---

**SIERRAFOR v2.0** - Sistema Refactorizado y Optimizado
_Última actualización: Noviembre 2025_
