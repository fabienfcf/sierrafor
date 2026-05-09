# SIERRAFOR — Sistema de Simulación Forestal
### PMF Las Alazanas · Ciclo 2026–2036

Sistema individual-árbol en R para calcular crecimiento, mortalidad, reclutamiento y programas de corta en bosques mixtos de pino-encino. Genera todas las tablas y gráficos requeridos por la NOM-152-SEMARNAT-2006.

---

## Inicio Rápido

```r
# Desde RStudio o R, con el directorio apuntando a modelov5/
source("workflows/40_WORKFLOW_COMPLETO.R")
```

Eso es todo. El script corre las 6 fases, genera ~40 tablas LaTeX, ~15 gráficos PNG y los CSVs de respaldo. Si quieres saltar fases costosas (p.ej. ya corriste la simulación), lee la sección [Control de Fases](#control-de-fases).

---

## Tabla de Contenidos

1. [¿Qué hace este sistema?](#1-qué-hace-este-sistema)
2. [Requisitos](#2-requisitos)
3. [Estructura de carpetas](#3-estructura-de-carpetas)
4. [Datos de entrada](#4-datos-de-entrada)
5. [Control de Fases](#5-control-de-fases)
6. [Configuración](#6-configuración)
7. [Cómo funciona la simulación](#7-cómo-funciona-la-simulación)
8. [Archivos generados](#8-archivos-generados)
9. [Módulos avanzados](#9-módulos-avanzados)
10. [Preguntas frecuentes](#10-preguntas-frecuentes)

---

## 1. ¿Qué hace este sistema?

SIERRAFOR simula el crecimiento de cada árbol del inventario año por año durante 10 años. En cada ciclo anual aplica tres procesos biológicos:

```
Cada año:
  ┌─────────────────────────────────┐
  │  1. Crecimiento                 │  Cada árbol vivo crece en diámetro y altura
  │  2. Mortalidad natural          │  Algunos árboles mueren (más los suprimidos)
  │  3. Reclutamiento               │  Nuevos árboles ingresan a la clase mínima
  └─────────────────────────────────┘
       ↓ (si hay corta programada ese año)
  ┌─────────────────────────────────┐
  │  4. Programa de cortas          │  Extracción según método Liocourt + ICA
  └─────────────────────────────────┘
```

Con los resultados calcula:

| Producto | Descripción |
|----------|-------------|
| ICA | Incremento Corriente Anual (m³/ha/año) por género y UMM |
| Tiempo de paso | Años que tarda un árbol en subir una clase diamétrica |
| Posibilidad | Volumen de corta sostenible por UMM y periodo |
| Tablas NOM-152 | Tablas 5, 6, 7, 8 y 9 del PMF en formato LaTeX |
| Distribución Liocourt | Estructura diamétrica de equilibrio |
| Análisis descriptivo | Composición, sanidad, erosión, regeneración |

---

## 2. Requisitos

### Software

- R ≥ 4.2
- RStudio (recomendado)

### Paquetes R

```r
install.packages(c(
  "tidyverse",   # Manipulación de datos y gráficos
  "readxl",      # Lectura del inventario Excel
  "janitor",     # Limpieza de nombres de columnas
  "xtable",      # Exportación de tablas a LaTeX
  "patchwork",   # Composición de gráficos
  "openxlsx"     # Escritura de Excel (para tabla densidad)
))
```

### Archivos de datos obligatorios

Deben estar en la raíz de `modelov5/`:

| Archivo | Descripción |
|---------|-------------|
| `inventario_forestal.xlsx` | Inventario con hojas F01–F06 en formato SIPLAFOR |
| `UMM_stats.csv` | Superficie y número de muestreos por UMM |

---

## 3. Estructura de Carpetas

```
modelov5/
│
├── workflows/                   ← PUNTO DE ENTRADA
│   ├── 40_WORKFLOW_COMPLETO.R  ← Script principal (corre todo)
│   └── 41_WORKFLOW_calcular_ica.r  ← ICA standalone (ya integrado en 40)
│
├── config/                      ← Parámetros del modelo
│   ├── 00_importar_inventario.R    Lee el Excel y construye las tablas F01-F06
│   ├── 01_parametros_configuracion.R  Orquestador: carga los 4 configs y valida
│   ├── 02_config_especies.R         Catálogo de especies, ecuaciones de volumen,
│   │                                tasas de crecimiento diamétrico y altura
│   ├── 03_config_codigos.R          Tablas de códigos SIPLAFOR (18 tablas)
│   ├── 04_config_simulacion.R       Mortalidad, reclutamiento, clases diamétricas
│   └── 05_config_programa_cortas.R  Q-factor, DMC, N_ref, intensidades por UMM
│
├── core/                        ← Motor de simulación
│   ├── 10_modelos_crecimiento.R     Incremento Δd y Δh por árbol individual
│   ├── 11_modelo_mortalidad.R       Probabilidad de muerte por dominancia
│   ├── 12_modelo_reclutamiento.R    Ingreso de nuevos individuos
│   ├── 13_simulador_crecimiento.R   Integra los 3 procesos en un loop anual
│   ├── 14_optimizador_cortas.R      Selección de árboles según Liocourt + ICA
│   ├── 15_core_calculos.R           Funciones puras (volumen, área basal, filtros)
│   └── 16_calcular_ica.R            Simulación sin cortas → ICA y tiempo de paso
│
├── analisis/                    ← Análisis del inventario inicial
│   ├── 20_analisis_descriptivo.R    Estadísticas dasométricas, sanidad, erosión
│   ├── 21_ANALISIS_RESULTADOS_DETALLADO.R
│   ├── 31_stat x rodal.R
│   ├── Fichas.R                     Fichas PDF por sitio (mapa + fotos + tabla)
│   ├── FichasUMM.R                  Fichas PDF por UMM
│   └── ANÁLISIS DE REGENERACIÓN...R
│
├── simulaciones/
│   └── 30_SIMULACION_10AÑOS_COMPLETA.R  Simulación con cortas → historial 10 años
│
├── generadores/                 ← Tablas y gráficos para el PMF
│   ├── 53_TABLA_DENSIDAD_ESPECIES.R     Densidad por especie (rodal y sitio) → xlsx
│   ├── Tabla_ICA_Posibilidad.R          ICA y posibilidad por UMM y género
│   ├── TABLA_5_COMPLETA_CON_ICA.R       Tabla 5 NOM-152 — Existencias por UMM
│   ├── TABLA_6.R                        Tabla 6 NOM-152 — Resumen predio
│   ├── TABLA_7_densidad e incrementos.R Tabla 7 NOM-152 — Densidad e ICA
│   ├── TABLA_8 y 9 - Posibilidad...R    Tablas 8-9 NOM-152 — Posibilidad anual
│   ├── TABLA ANEXA - DISTRIBUCIÓN...R   Distribución diamétrica de cortas por UMM
│   ├── 20_analisis_dasometrico_FINAL.R
│   ├── 32_tablas_pmf.R
│   ├── 33_graficos_pmf.R
│   ├── 50_GENERADOR_TABLAS.R
│   ├── 51_GENERADOR_GRAFICOS.R
│   ├── 52_CALCULOS_ESPECIFICOS.R
│   ├── 60_graficos_distrib_ini.R
│   ├── 60_graficos_distrib_ini_fin_corta.R
│   ├── 60_graficos_distrib_ini_fin_corta_V2.R
│   └── 61_generar_tabla_descriptiva_sitios.R
│
├── utils/                       ← Funciones compartidas
│   ├── utils_metricas.R             calcular_metricas() — densidad, AB, volumen/ha
│   └── utils_validacion.R           Chequeos de crecimiento, mortalidad, reclutas
│
├── opcional/
│   └── 23_Main_incendio.R           Riesgo de incendio (desactivado por defecto)
│
├── reportes/
│   ├── 35_GENERAR_REPORTE_PMF.R
│   └── 70_CONFIG_REPORTES.R
│
├── Calibracion/
│   └── Calibracion_chapman.R        Ajuste de Chapman-Richards
│
├── tests/                       ← Verificación y regresión
│   ├── baseline_output.md           Valores de referencia para detectar regresiones
│   ├── 22_VERIFICACION_TABLAS_LATEX.R
│   ├── TEST_INTEGRACION_TS_TC.R
│   └── test_refactorizacion.R
│
├── datos_intermedios/           ← Generado automáticamente
├── resultados/                  ← Generado automáticamente
├── tablas_latex/                ← Generado automáticamente
└── graficos/                    ← Generado automáticamente
```

---

## 4. Datos de Entrada

### `inventario_forestal.xlsx`

Debe contener las siguientes hojas en formato SIPLAFOR:

| Hoja | Contenido | Columnas clave |
|------|-----------|----------------|
| `F01` | Sitios de muestreo | rodal, muestreo, pendiente, exposición, erosión |
| `F02` | Regeneración (conteo) | rodal, muestreo, especie, categoría |
| `F03` | Árboles individuales | rodal, muestreo, especie, diámetro_normal, altura_total, dominancia |
| `F04` | Virutas (incremento) | árbol_id, incremento_5años |
| `F05` | Regeneración adicional | rodal, muestreo, especie, altura |
| `F06` | Combustibles | rodal, muestreo, carga_fina, carga_gruesa |

### `UMM_stats.csv`

Una fila por UMM (rodal). Columnas mínimas:

```
id, SUPERFICIE, num_muestreos, ...
```

- `id` — número de rodal (entero)
- `SUPERFICIE` — área de la UMM en hectáreas
- `num_muestreos` — número de sitios de muestreo realizados (se usa como denominador fijo para expansión a hectárea)

---

## 5. Control de Fases

Al inicio de `40_WORKFLOW_COMPLETO.R` hay un panel de control. Cambia `TRUE`/`FALSE` para decidir qué calcular en cada ejecución:

```r
FASES <- list(
  importar    = TRUE,  # Lee el Excel → arboles_analisis.rds       ~10s   necesita: Excel
  descriptivo = TRUE,  # Estadísticas dasométricas del inventario   ~30s   necesita: importar o .rds
  incendio    = FALSE, # Riesgo de incendio (combustibles)          ~5s    necesita: importar o .rds
  ica         = TRUE,  # Simulación sin cortas → ICA CSV            ~60s   necesita: importar o .rds
  simulacion  = TRUE,  # Simulación con cortas → PMF               ~90s   necesita: ica o CSV en disco
  tablas      = TRUE   # Tablas 5-9, ICA, densidad → LaTeX          ~30s   necesita: ica+simulacion o CSVs
)
```

### Escenarios comunes

**Primera vez o cuando cambia el inventario:**
```r
# Corre todo (valores por defecto)
FASES <- list(importar=TRUE, descriptivo=TRUE, incendio=FALSE,
              ica=TRUE, simulacion=TRUE, tablas=TRUE)
```

**Solo regenerar las tablas finales** (ya tienes los CSVs en `resultados/`):
```r
FASES <- list(importar=FALSE, descriptivo=FALSE, incendio=FALSE,
              ica=FALSE, simulacion=FALSE, tablas=TRUE)
```

**Solo recalcular el ICA y las tablas** (cambiaste parámetros de crecimiento):
```r
FASES <- list(importar=FALSE, descriptivo=FALSE, incendio=FALSE,
              ica=TRUE, simulacion=TRUE, tablas=TRUE)
```

**Solo el análisis descriptivo** (cambió el inventario, quieres ver estadísticas rápido):
```r
FASES <- list(importar=TRUE, descriptivo=TRUE, incendio=FALSE,
              ica=FALSE, simulacion=FALSE, tablas=FALSE)
```

> **Nota:** Si desactivas `ica=FALSE` y también `simulacion=TRUE`, el sistema verifica que exista `resultados/31_ica_por_rodal.csv` y detiene con un mensaje claro si no existe.

---

## 6. Configuración

Todos los parámetros del modelo están en la carpeta `config/`. El objeto `CONFIG` centraliza todo.

### 6.1 Parámetros de crecimiento (`config/02_config_especies.R`)

```r
CRECIMIENTO_DIAMETRICO <- tribble(
  ~genero,    ~tasa_base_cm_año,  ~tasa_altura_m_año,  ~altura_max_m,
  "Pinus",    0.40,               0.20,                 22,
  "Quercus",  0.25,               0.15,                 17
)
```

- **tasa_base_cm_año** — crecimiento diamétrico promedio (cm/año) para un árbol dominante
- **tasa_altura_m_año** — crecimiento en altura base (m/año)
- **altura_max_m** — techo biológico; por encima no crece en altura

El crecimiento real de cada árbol se ajusta multiplicando por el factor de su clase de dominancia:

| Dominancia | Factor diamétrico | Factor altura |
|------------|-------------------|---------------|
| 1 — Dominante | 1.00 | 1.00 |
| 2 — Intermedio | 0.80 | 1.00 |
| 3 — Suprimido | 0.60 | 0.75 |
| 4 — Libre sin supresión | 1.00 | 1.00 |
| 5 — Libre con supresión | 0.70 | 0.75 |
| 6 — Aislado con piso alto | 0.50 | 0.50 |
| 7, 8, 9 — Muertos/tocones | 0.00 | 0.00 |

### 6.2 Mortalidad (`config/04_config_simulacion.R`)

```r
MORTALIDAD_BASE <- 0.005  # 0.5% anual para árboles dominantes
```

Los suprimidos mueren hasta 1.2× más rápido (ver `factor_mortalidad` en `CODIGOS_DOMINANCIA`).

### 6.3 Reclutamiento (`config/04_config_simulacion.R`)

```r
TASA_RECLUTAMIENTO   <- 0.01   # 1% de los vivos ingresa por año
RECLUTAMIENTO_D_MIN  <- 7.5    # cm — diámetro mínimo de ingreso
RECLUTAMIENTO_D_MAX  <- 8.5    # cm
RECLUTAMIENTO_DOMINANCIA <- 3  # ingresan como suprimidos
```

### 6.4 Programa de cortas (`config/05_config_programa_cortas.R`)

```r
Q_FACTOR              <- 1.5   # Cociente de Liocourt (estructura objetivo)
TOLERANCIA_EQUILIBRIO <- 20    # ±20% — margen de equilibrio aceptado
DMC <- list(Pinus = 20, Quercus = 2)   # Diámetro Mínimo de Corta (cm)
```

El `N_REF` (número de referencia en la clase de 40 cm) y el `año_corta` de cada UMM se definen manualmente en `PROGRAMA_CORTAS` para que el técnico los ajuste a la realidad del predio.

---

## 7. Cómo funciona la simulación

### 7.1 El ciclo anual

Para cada año del periodo (2026–2035) el simulador:

```
1. CRECIMIENTO
   Para cada árbol vivo:
     Δd = tasa_base[género] × factor_dominancia
     Δh = tasa_altura[género] × factor_dom_h × factor_tamaño
     Se actualiza diámetro, altura, volumen y área basal

2. MORTALIDAD NATURAL
   Para cada árbol vivo:
     P(muerte) = mortalidad_base × factor_mortalidad[dominancia]
     Se sortea un U(0,1); si U < P → árbol muere (dominancia = 7)

3. RECLUTAMIENTO
   Por rodal:
     n_reclutas = round(n_vivos × tasa_reclutamiento)
     Se generan árboles nuevos con d ~ U(7.5, 8.5) cm
     La composición de géneros sigue la del rodal

4. CORTAS (solo en el año programado para cada UMM)
   Se calcula la distribución Liocourt objetivo con Q y N_ref
   Se identifican clases diamétricas con exceso de individuos
   Se extraen árboles hasta acercarse al equilibrio
   Restricción: solo árboles con d ≥ DMC del género
```

### 7.2 El método Liocourt (optimizador de cortas)

La distribución de equilibrio dice que el número de árboles en cada clase debe ser:

```
N(clase k) = N_ref × Q^(-(k - k_ref))
```

Donde `k_ref` es la clase de referencia (40 cm por defecto). Si una clase tiene más árboles que la distribución objetivo (±tolerancia), los excedentes son candidatos a corta.

El volumen cortado nunca supera el ICA del rodal para garantizar sostenibilidad.

### 7.3 El ICA (Incremento Corriente Anual)

El ICA se calcula con una simulación paralela sin cortas durante 10 años:

```
ICA [m³/ha/año] = (Volumen_final − Volumen_inicial) / (n_años × superficie_ha)
```

Esto incluye crecimiento de los árboles existentes, más el volumen de los reclutas que ingresan, menos las pérdidas por mortalidad natural.

---

## 8. Archivos Generados

Después de correr el workflow completo encontrarás:

### 8.1 Datos intermedios (`datos_intermedios/`)

| Archivo | Descripción |
|---------|-------------|
| `arboles_analisis.rds` | Dataset principal: un árbol por fila con todas las variables calculadas |
| `inventario_completo.rds` | Listas F01–F06 tal como vienen del Excel |

### 8.2 Resultados (`resultados/`)

**Del análisis descriptivo:**

| Archivo | Contenido |
|---------|-----------|
| `analisis_descriptivo.rds` | Todos los resultados descriptivos en una lista |
| `desc_01_resumen_general.csv` | Resumen global del predio |
| `desc_02_por_rodal.csv` | Métricas por UMM |
| `desc_03_por_genero.csv` | Métricas por género |
| `desc_05_distribucion_diametrica.csv` | Histograma de clases |
| `desc_06_erosion.csv` | Índice de erosión por sitio |
| `desc_07/08_sanidad.csv` | Infestación de muérdago y descortezadores |
| `desc_09/10_regeneracion.csv` | Densidad y composición de regeneración |

**Del ICA (FASE 4.5):**

| Archivo | Contenido |
|---------|-----------|
| `31_ica_por_rodal.csv` | ICA total por UMM |
| `31_ica_por_genero_rodal.csv` | ICA por género y UMM ← usado por Tablas 6-9 |
| `31_ica_por_especie_rodal.csv` | ICA por especie y UMM |
| `31_tiempo_paso_rodal.csv` | Años para subir una clase diamétrica |
| `31_resumen_predio.csv` | ICA y crecimiento a nivel predio |

**De la simulación con cortas (FASE 5):**

| Archivo | Contenido |
|---------|-----------|
| `evolucion_rodal_10anos.csv` | Métricas anuales por UMM y género ← usado por Tablas 5-7 |
| `cortas_distribucion_diametrica.csv` | Árboles cortados por clase y UMM |
| `cortas_resumen_rodal_genero.csv` | Resumen de cortas por UMM y género |
| `cortas_detalle_completo.csv` | Un árbol cortado por fila |
| `historial_completo_10anos.rds` | Estado completo de cada árbol en cada año |

### 8.3 Tablas LaTeX (`tablas_latex/`)

Listas para insertar en tu documento con `\input{tablas_latex/nombre.tex}`:

**Análisis descriptivo:**

| Archivo `.tex` | Tabla |
|----------------|-------|
| `desc_01_resumen_rodal.tex` | Resumen general por UMM |
| `desc_02_composicion_genero.tex` | Composición Pinus/Quercus |
| `desc_03_top_especies.tex` | Especies más abundantes |
| `desc_04_erosion.tex` | Erosión por sitio |
| `desc_05_sanidad.tex` | Sanidad forestal |
| `desc_06_regeneracion.tex` | Regeneración natural |

**Tablas NOM-152:**

| Archivo `.tex` | Tabla |
|----------------|-------|
| `tabla_05_existencias_umm.tex` | Tabla 5 — Existencias por UMM |
| `tabla_06_resumen_predio.tex` | Tabla 6 — Resumen a nivel predio |
| `tabla_07_densidad_incrementos.tex` | Tabla 7 — Densidad e ICA |
| `tabla_08_posibilidad_anual.tex` | Tabla 8 — Posibilidad anual |
| `tabla_09_plan_cortas.tex` | Tabla 9 — Plan decenal de cortas |
| `tabla_ica_posibilidad.tex` | ICA y posibilidad por UMM y género |
| `tabla_anexa_distribucion_cortas.tex` | Distribución diamétrica de cortas |
| `01_inventario_inicial.tex` | Inventario inicial consolidado |
| `02_comparacion_inicial_final.tex` | Estado inicial vs final de la simulación |
| `03_intensidad_corte_rodal.tex` | Intensidad de corta por UMM |

**Del ICA:**

| Archivo `.tex` | Tabla |
|----------------|-------|
| `ica_01_por_rodal.tex` | ICA total por UMM |
| `ica_02_por_genero_rodal.tex` | ICA desglosado por género |
| `ica_03_tiempo_paso.tex` | Tiempo de paso por género y UMM |
| `ica_04_resumen_predio.tex` | ICA a nivel predio |

### 8.4 Gráficos (`graficos/`)

| Archivo `.png` | Gráfico |
|----------------|---------|
| `desc_01_distribucion_diametrica.png` | Histograma diamétrico inicial |
| `desc_02_erosion.png` | Mapa de erosión |
| `desc_03_sanidad.png` | Infestación por sitio |
| `desc_04_regeneracion.png` | Densidad de regeneración |
| `evolucion_10años_rodales.png` | Volumen/ha por UMM a lo largo del periodo |
| `distrib_ini_fin_corta_*.png` | Distribuciones diamétricas inicio/fin con cortas |

### 8.5 Excel de densidad por especie

El script `53_TABLA_DENSIDAD_ESPECIES.R` genera dos pestañas en `inventario_forestal.xlsx`:

- **Resumen_Especies** — densidad (N/ha) por especie y UMM, con subtotales por género
- **Resumen_Sitios** — misma tabla pero por sitio de muestreo individual

---

## 9. Módulos Avanzados

Esta sección es para quien quiera correr partes del sistema por separado o entender el código.

### 9.1 Funciones core (disponibles siempre que se cargue `15_core_calculos.R`)

```r
# Filtrado de árboles
es_arbol_vivo(dominancia)              # Devuelve TRUE/FALSE
filtrar_arboles_vivos(df)              # Filtra filas vivas (dominancia 1-6)
filtrar_arboles_muertos(df)            # Filtra filas muertas (dominancia 7-9)

# Cálculos dasométricos
calcular_volumen_arbol(d_cm, h_m, tipo, a, b, c)
calcular_area_basal(d_cm)
```

### 9.2 Calcular métricas por grupo (`utils/utils_metricas.R`)

```r
source("utils/utils_metricas.R")

# Genérico — agrupar_por puede ser cualquier vector de columnas
calcular_metricas(arboles_df, agrupar_por = c("rodal", "genero_grupo"), config = CONFIG)

# Atajos
calcular_metricas_estado(arboles_df, config)          # Global
calcular_metricas_por_genero(arboles_df, config)      # Por género
calcular_metricas_por_especie(arboles_df, config)     # Por especie
calcular_metricas_predio(arboles_df, config)          # Resumen predio
```

Las métricas incluyen: `n_arboles`, `densidad_ha`, `ab_m2ha`, `vol_m3ha`, `dg_cm`, `h_media_m`.

### 9.3 Calcular ICA por separado (`core/16_calcular_ica.R`)

```r
source("core/10_modelos_crecimiento.R")
source("core/11_modelo_mortalidad.R")
source("core/12_modelo_reclutamiento.R")
source("core/16_calcular_ica.R")

resultado_ica <- calcular_ica_sin_cortes(
  arboles_inicial = arboles_analisis,
  config = CONFIG,
  años = 10
)

# Exportar tablas LaTeX
exportar_tablas_latex_ica(resultado_ica, directorio = "tablas_latex")

# Guardar CSVs en resultados/
guardar_resultados_ica(resultado_ica, directorio = "resultados")
```

### 9.4 Simular solo crecimiento (sin cortas)

```r
source("core/13_simulador_crecimiento.R")

resultado <- simular_crecimiento_rodal(
  arboles_inicial = arboles_analisis %>% filter(rodal == 3),
  config = CONFIG,
  años = 10
)

# Ver evolución
resultado$historial_metricas   # Métricas por año
resultado$poblacion_final       # Estado al año 10
```

### 9.5 Ecuaciones de volumen

El sistema usa ecuaciones alométricas por especie. Los tipos disponibles son:

```r
# Tipo "potencia":  V = a × d^b × h^c
# Tipo "exp":       V = exp(a + b×ln(d) + c×ln(h))
# Tipo "local":     V = a + b×d² (solo usa diámetro)
```

Los coeficientes `a`, `b`, `c` están en `config/02_config_especies.R` y se asignan a cada árbol al construir el dataset.

### 9.6 Reproducibilidad de la mortalidad

La mortalidad es estocástica. La semilla se fija por año:

```r
set.seed(CONFIG$semilla_mortalidad + año_actual)  # semilla = 42
```

Esto garantiza que la misma configuración siempre produce los mismos resultados. Para explorar variabilidad, cambia `SEMILLA_MORTALIDAD` en `config/04_config_simulacion.R`.

---

## 10. Preguntas Frecuentes

**¿Por qué los valores de densidad/ha no cambian aunque cambien el número de árboles muertos?**

El sistema usa un denominador fijo para expandir a hectárea: el número de sitios de muestreo realizados en el inventario. Esto es metodológicamente correcto — el esfuerzo de muestreo no cambia con la simulación. Ver `utils/utils_metricas.R` para detalles.

**¿Puedo cambiar los parámetros de crecimiento?**

Sí, edita `config/02_config_especies.R`. Las tasas base (`tasa_base_cm_año`) y las alturas máximas (`altura_max_m`) se propagan automáticamente a todo el sistema vía `CONFIG`.

**¿Puedo añadir una nueva UMM al programa de cortas?**

Agrega una fila al tribble `PROGRAMA_CORTAS` en `config/05_config_programa_cortas.R` con el rodal, año de corta, N_ref, intensidad e información complementaria.

**¿Qué pasa si el inventario cambia (nuevos muestreos)?**

Pon el nuevo archivo `inventario_forestal.xlsx` en la raíz, activa `FASES$importar = TRUE` y corre el workflow. Todo se recalcula desde cero.

**¿Los árboles muertos siguen en el dataset?**

Sí, nunca se eliminan filas. Los muertos se marcan con `dominancia %in% c(7, 8, 9)` y todas las funciones de cálculo los excluyen automáticamente usando `filtrar_arboles_vivos()`. Esto permite rastrear la mortalidad acumulada.

**¿Cómo incluyo las tablas en LaTeX?**

```latex
\input{tablas_latex/tabla_05_existencias_umm.tex}
\input{tablas_latex/tabla_07_densidad_incrementos.tex}
% etc.
```

---

## Valores de referencia (baseline)

Para detectar si algún cambio al código alteró los resultados, estos son los valores esperados con la configuración actual:

| Métrica | Valor |
|---------|-------|
| Árboles en inventario | 2 119 |
| Volumen inventario (vivos) | 437.30 m³ |
| Árboles cortados (10 años) | 270 |
| Volumen cortado total | 90.98 m³ |
| ICA medio predio | ~2.17 m³/ha/año |

Si corrés el workflow y los números difieren, compará con `tests/baseline_output.md`.

---

**SIERRAFOR v5** · PMF Las Alazanas 2026–2036  
Actualizado: abril 2026
