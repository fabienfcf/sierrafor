# SIERRAFOR - Sistema de Gestión Forestal Dinámica
## Documentación Técnica Completa v2.0

**Proyecto**: PMF Las Alazanas 2026-2036  
**Norma**: SEMARNAT-152  
**Última actualización**: Octubre 2025

---

## TABLA DE CONTENIDOS

1. [Arquitectura del Sistema](#arquitectura)
2. [Archivos de Configuración](#configuracion)
3. [Módulos de Simulación](#simulacion)
4. [Ecuaciones y Modelos](#ecuaciones)
5. [Flujo de Ejecución](#flujo)
6. [Estrategias de Corte](#cortes)
7. [Tareas Futuras](#futuras)

---

## 1. ARQUITECTURA DEL SISTEMA {#arquitectura}

### Estructura Modular

```
SIERRAFOR/
├── 40_WORKFLOW_COMPLETO.R          [Orquestador principal]
│
├── 01_parametros_configuracion.R   [Integrador de CONFIG]
│   ├── 02_config_especies.R        [Biología y alometrías]
│   ├── 03_config_codigos.R         [Códigos SIPLAFOR]
│   ├── 04_config_simulacion.R      [Parámetros demográficos]
│   └── 05_config_programa_cortas.R [Calendario de intervenciones]
│
├── 00_importar_inventario.R        [Lectura desde Excel]
│
├── 15_core_calculos.R              [Funciones puras reutilizables]
│
├── 10_modelos_crecimiento.R        [Incremento individual]
├── 11_modelo_mortalidad.R          [Tasa de muerte diferencial]
├── 12_modelo_reclutamiento.R       [Ingreso de nuevos árboles]
├── 13_simulador_crecimiento.R      [Orquestador de simulación]
├── 14_optimizador_cortas.R         [Estrategias de aprovechamiento]
│
├── 20_analisis_descriptivo.R       [Análisis inicial del inventario]
├── 21_ANALISIS_RESULTADOS_DETALLADO.R [Análisis post-simulación]
├── 22_VERIFICACION_TABLAS_LATEX.R  [Generación de tablas PMF]
│
└── 30_SIMULACION_10AÑOS_COMPLETA.R [Simulación con cortas programadas]
```

### Objeto Central: CONFIG

CONFIG es una lista maestra que contiene toda la configuración:

```r
CONFIG <- list(
  # Temporal
  periodo = 10,
  inicio = 2026,
  
  # Especies
  especies = ESPECIES,                      # 21 especies
  ecuaciones_volumen = ECUACIONES_VOLUMEN,  # Alometrías
  parametros_altura = PARAMETROS_...,       # Chapman-Richards
  generos = c("Pinus", "Quercus"),
  
  # Crecimiento
  crecimiento_base = list(Pinus=0.40, Quercus=0.30),  # cm/año
  modificadores_dominancia = CODIGOS_DOMINANCIA,
  
  # Mortalidad
  mortalidad_base = 0.02,  # 2% anual
  
  # Reclutamiento
  tasa_reclutamiento = 0.03,  # 3% de densidad
  reclut_d_min = 7.5,
  reclut_d_max = 12.5,
  reclut_dominancia = 6,  # Suprimidos
  reclut_altura = list(Pinus=3.0, Quercus=2.5),
  
  # Cortas
  dmc = list(Pinus=30, Quercus=25),
  q_factor = 1.7,
  tolerancia = 20,
  programa_cortas = PROGRAMA_CORTAS,
  
  # Muestreo
  area_parcela_ha = 0.1,
  clases_d = seq(5, 100, by=5),
  
  # Funciones
  interpolar_dhdd = function(especie, d, dom) {...}
)
```

---

## 2. ARCHIVOS DE CONFIGURACIÓN {#configuracion}

### 02_config_especies.R

#### ESPECIES (21 especies)
```r
codigo  nombre_cientifico        genero    grupo         maderable
11      Pinus pseudostrobus      Pinus     Coníferas     SI
12      Pinus teocote            Pinus     Coníferas     SI
13      Pinus cembroides         Pinus     Coníferas     NO
61      Quercus rysophylla       Quercus   Latifoliadas  SI
62      Quercus laeta            Quercus   Latifoliadas  SI
...
```

#### ECUACIONES_VOLUMEN
**Tipo "potencia"**: V = a × d^b × h^c  
**Tipo "exp"**: V = exp(a + b×ln(d) + c×ln(h))

```r
Especie                  tipo       a           b          c
Pinus pseudostrobus      potencia   0.00004     1.93694    1.03169
Quercus laceyi           exp        -9.48686    1.82408    0.96893
```

#### PARAMETROS_ALTURA_DIAMETRO (Chapman-Richards)
**Ecuación**: h = a × (1 - exp(-b×d))^c  
**Derivada**: dh/dd = a × c × b × exp(-b×d) × (1-exp(-b×d))^(c-1)

```r
Especie              a_q1     b_q1     c_q1    a_q3     b_q3     c_q3
                   (suprim.)                  (domin.)
Pinus pseudostrobus  15.794   0.01349  0.4746  26.323   0.01649  0.6421
Pinus cembroides     15.250   0.01251  0.5994  25.417   0.01529  0.8109
Quercus rysophylla   9.830    0.03180  0.6991  16.384   0.03886  0.9459
```

#### CRECIMIENTO_DIAMETRICO
```r
Género    Tasa base (cm/año)
Pinus     0.40
Quercus   0.30
```

### 03_config_codigos.R

#### CODIGOS_DOMINANCIA (CRÍTICO)
```r
codigo  etiqueta              factor_crecimiento  factor_mortalidad
1       Dominante             1.00                1.0
2       Codominante           1.00                1.0
3       Intermedio            0.70                1.5
4       Libre sin supresión   1.00                1.0
5       Libre con supresión   0.70                1.5
6       Suprimido             0.40                3.0
7       Muerto en pie         0.00                NA
8       Muerto caído          0.00                NA
9       Tocón                 0.00                NA
```

**Impacto**:
- Dom 1: crece 100%, muere 2%
- Dom 6: crece 40%, muere 6%

### 04_config_simulacion.R

```r
PERIODO_SIMULACION <- 10
INICIO_SIMULACION <- 2026
GENEROS_OBJETIVO <- c("Pinus", "Quercus")
MORTALIDAD_BASE <- 0.02
TASA_RECLUTAMIENTO <- 0.03
AREA_PARCELA_HA <- 0.1
CLASES_DIAMETRICAS <- seq(5, 100, by=5)
```

### 05_config_programa_cortas.R

#### Diámetros Mínimos de Corta
```r
DMC <- list(Pinus = 30, Quercus = 25)  # cm
```

#### Parámetros Liocourt
```r
Q_FACTOR <- 1.7              # Cociente J invertida
TOLERANCIA_EQUILIBRIO <- 20  # ±20%
```

**Interpretación Q_FACTOR**:
- 1.3: Pendiente SUAVE → corta MENOS
- 1.5: Pendiente MODERADA (estándar)
- **1.7**: Pendiente PRONUNCIADA (recomendado) → corta MÁS
- 2.0: Pendiente MUY PRONUNCIADA → corta MUCHO MÁS

#### Programa de Cortas
```r
PROGRAMA_CORTAS <- tribble(
  ~rodal, ~año_corta, ~metodo,    ~intensidad_pct, ~d_min, ~prioridad,
  1,      1,          "LIOCOURT", 20,              15,     "suprimidos",
  2,      2,          "LIOCOURT", 20,              15,     "suprimidos",
  ...
  11,     10,         "LIOCOURT", 20,              15,     "suprimidos"
)
```

---

## 3. MÓDULOS DE SIMULACIÓN {#simulacion}

### 10_modelos_crecimiento.R

#### Incremento Diamétrico
```r
calcular_incremento_diametro(arbol, config)
```

**Algoritmo**:
1. Obtener tasa base por género (Pinus: 0.40, Quercus: 0.30)
2. Obtener factor dominancia (0.00-1.00)
3. Calcular: **Δd = tasa_base × factor_dominancia**

**Ejemplos**:
- Pinus dominante (dom=1): Δd = 0.40 × 1.00 = **0.40 cm/año**
- Pinus suprimido (dom=6): Δd = 0.40 × 0.40 = **0.16 cm/año**
- Quercus intermedio (dom=3): Δd = 0.30 × 0.70 = **0.21 cm/año**

#### Incremento en Altura
```r
calcular_incremento_altura(arbol, incremento_d, config)
```

**Algoritmo**:
1. Si Δd = 0 → Δh = 0 (muertos no crecen)
2. Obtener parámetros Chapman-Richards según dominancia
3. Calcular tasa: **dh/dd = a × c × b × exp(-b×d) × (1-exp(-b×d))^(c-1)**
4. Calcular: **Δh = (dh/dd) × Δd**
5. Limitar máximo: Δh ≤ 0.5 m/año

**Ejemplo**:
- Pinus pseudostrobus 30 cm, dominante:
  - a=26.323, b=0.01649, c=0.6421
  - dh/dd ≈ 0.18 m/cm
  - Δd = 0.40 cm
  - **Δh = 0.18 × 0.40 = 0.072 m/año**

#### Aplicar Crecimiento
```r
aplicar_crecimiento_anual(arbol, config)
```

**Secuencia**:
1. Calcular Δd y Δh
2. d_nuevo = d_actual + Δd
3. h_nueva = h_actual + Δh
4. V_nuevo = calcular_volumen(d_nuevo, h_nueva, tipo, a, b, c)
5. ΔV = V_nuevo - V_actual
6. Actualizar arbol$diametro, $altura, $volumen, $area_basal

### 11_modelo_mortalidad.R

```r
aplicar_mortalidad_poblacion(arboles_df, config, año_actual)
```

**Algoritmo por árbol**:
1. Si ya muerto (dom ∈ {7,8,9}) → mantener muerto
2. Si vivo:
   - prob_muerte = mortalidad_base × factor_mortalidad_dominancia
   - aleatorio ~ U(0,1)
   - Si aleatorio < prob_muerte → marcar muerto (dom=7)

**Tasas efectivas**:
```r
Dominancia  Factor  Mortalidad efectiva
1,2,4       1.0     2.0% anual
3,5         1.5     3.0% anual
6           3.0     6.0% anual
```

### 12_modelo_reclutamiento.R

```r
aplicar_reclutamiento(arboles_df, config, año_actual)
```

**Algoritmo por rodal**:
1. n_reclutas = round(n_vivos × 0.03)
2. Limitar: máximo 10% del rodal
3. Determinar composición según especies actuales
4. PARA cada especie:
   - Generar d ~ U(7.5, 12.5) cm
   - Asignar h según género: Pinus=3.0m, Quercus=2.5m (±20%)
   - Asignar dominancia = 6 (suprimido)
   - Calcular volumen
   - Crear ID: "RECLUTA_R03_A04_Pinus_pseudostrobus_001"
5. Agregar reclutas a población

### 13_simulador_crecimiento.R

```r
simular_crecimiento_rodal(arboles_inicial, config, años=10)
```

**Bucle principal**:
```
PARA año = 1 HASTA 10:
  1. arboles ← aplicar_crecimiento_poblacion(arboles)
  2. arboles ← actualizar_volumenes(arboles)
  3. arboles ← aplicar_mortalidad_poblacion(arboles)
  4. arboles ← aplicar_reclutamiento(arboles)
  5. historial[[año]] ← arboles
  6. metricas[[año]] ← calcular_metricas_estado(arboles)
FIN PARA
```

### 14_optimizador_cortas.R

#### Método LIOCOURT

**Paso 1**: Calcular distribución objetivo
```r
calcular_distribucion_liocourt(arboles_vivos, config)
```

```
Fórmula: N_i = N_ref / q^(i-1)

Ejemplo con Q=1.7:
Clase   N_actual  N_objetivo  Diagnóstico    Exceso
10-15   120       120         Equilibrado    0
15-20   95        71          Sobrepoblado   24
20-25   78        42          Sobrepoblado   36
25-30   45        25          Sobrepoblado   20
30-35   18        15          Equilibrado    0
```

**Paso 2**: Calcular volumen objetivo
```r
PARA clase sobrepoblada:
  n_cortar = round(exceso × intensidad% / 100)
  vol_objetivo += n_cortar × vol_medio_clase
FIN PARA
```

**Paso 3**: Marcar árboles
```r
marcar_arboles(arboles, vol_objetivo, corte_config, dist_obj)
```

Secuencia:
1. Filtrar solo clases sobrepobladas
2. Aplicar d_min (DMC por género)
3. Aplicar d_max si existe
4. Excluir semilleros (dom ∈ {1,2,3} Y d ≥ P75)
5. Priorizar según estrategia
6. Seleccionar hasta alcanzar vol_objetivo

#### Método ICA
```r
ICA_anual = (vol_actual - vol_inicial) / años_transcurridos
vol_disponible = ICA_anual × periodo_PMF
vol_objetivo = vol_disponible × intensidad% / 100
```

#### Método EXISTENCIAS
```r
vol_objetivo = vol_actual × intensidad% / 100
```

---

## 4. ECUACIONES Y MODELOS {#ecuaciones}

### Ecuaciones Alométricas de Volumen

**Tipo Potencia** (Pinus pseudostrobus):
```
V = 0.00004 × d^1.93694 × h^1.03169
```

**Tipo Exponencial** (Quercus laceyi):
```
V = exp(-9.48686 + 1.82408×ln(d) + 0.96893×ln(h))
```

### Modelo Chapman-Richards

**Ecuación altura-diámetro**:
```
h = a × (1 - exp(-b×d))^c
```

**Derivada (tasa dh/dd)**:
```
dh/dd = a × c × b × exp(-b×d) × (1 - exp(-b×d))^(c-1)
```

**Ejemplo numérico** (Pinus pseudostrobus dominante, d=30cm):
```
a = 26.323, b = 0.01649, c = 0.6421

h = 26.323 × (1 - exp(-0.01649×30))^0.6421
  = 26.323 × (1 - 0.6105)^0.6421
  = 26.323 × 0.5216
  ≈ 13.73 m

dh/dd = 26.323 × 0.6421 × 0.01649 × exp(-0.01649×30) × (0.3895)^(-0.3579)
      ≈ 0.18 m/cm
```

### Cálculo de Área Basal

```
AB = π × (d/200)²

Ejemplo: d=30cm
AB = π × (30/200)² = 0.0707 m²
```

### Factores de Expansión

```
Factor = 1 / area_parcela_ha

Arbolado: 0.1 ha → factor = 10
Valor_ha = valor_parcela × 10
```

### Distribución Liocourt

```
N_i = N_ref / q^(i-1)

Q = 1.7, N_ref = 120:
Clase 0: N = 120 / 1.7^0 = 120
Clase 1: N = 120 / 1.7^1 = 71
Clase 2: N = 120 / 1.7^2 = 42
Clase 3: N = 120 / 1.7^3 = 25
```

---

## 5. FLUJO DE EJECUCIÓN {#flujo}

### Workflow Completo (40_WORKFLOW_COMPLETO.R)

```
┌─────────────────────────────────────────┐
│ FASE 0: CONFIGURACIÓN                   │
│ source("01_parametros_configuracion.R") │
│ → CONFIG creado y validado              │
└─────────────────────────────────────────┘
            ↓
┌─────────────────────────────────────────┐
│ FASE 1: IMPORTACIÓN                     │
│ source("00_importar_inventario.R")      │
│ → inventario_forestal.xlsx              │
│ → arboles_analisis creado               │
└─────────────────────────────────────────┘
            ↓
┌─────────────────────────────────────────┐
│ FASE 2: ANÁLISIS DESCRIPTIVO            │
│ source("20_analisis_descriptivo.R")     │
│ → Tablas LaTeX + gráficos PNG           │
└─────────────────────────────────────────┘
            ↓
┌─────────────────────────────────────────┐
│ FASE 3: SIMULACIÓN 10 AÑOS              │
│ source("30_SIMULACION_10AÑOS...")       │
│ PARA año = 1..10:                       │
│   1. Crecimiento                        │
│   2. Mortalidad                         │
│   3. Reclutamiento                      │
│   4. Cortas (si programadas)            │
│   5. Guardar historial                  │
│ FIN PARA                                │
└─────────────────────────────────────────┘
            ↓
┌─────────────────────────────────────────┐
│ FASE 4: ANÁLISIS RESULTADOS             │
│ source("21_ANALISIS_RESULTADOS...")     │
│ source("22_VERIFICACION_TABLAS...")     │
│ → Comparaciones                         │
│ → Tablas para PMF                       │
└─────────────────────────────────────────┘
```

### Ciclo Anual de Simulación

```
AÑO n
  ↓
┌──────────────────────────────────┐
│ 1. CRECIMIENTO                   │
│ aplicar_crecimiento_poblacion()  │
│                                  │
│ PARA cada árbol vivo:            │
│   Δd ← tasa × factor_dom         │
│   Δh ← dh/dd × Δd                │
│   V_nuevo ← alometría(d+Δd, h+Δh)│
│   Actualizar árbol               │
│ FIN PARA                         │
└──────────────────────────────────┘
  ↓
┌──────────────────────────────────┐
│ 2. MORTALIDAD                    │
│ aplicar_mortalidad_poblacion()   │
│                                  │
│ PARA cada árbol vivo:            │
│   prob ← 0.02 × factor_mort      │
│   r ~ U(0,1)                     │
│   SI r < prob:                   │
│     dom ← 7 (muerto)             │
│ FIN PARA                         │
└──────────────────────────────────┘
  ↓
┌──────────────────────────────────┐
│ 3. RECLUTAMIENTO                 │
│ aplicar_reclutamiento()          │
│                                  │
│ PARA cada rodal:                 │
│   n_reclutas ← n_vivos × 0.03    │
│   Generar nuevos árboles:        │
│     d ~ U(7.5, 12.5)             │
│     h ~ género_base              │
│     dom = 6                      │
│   Agregar a población            │
│ FIN PARA                         │
└──────────────────────────────────┘
  ↓
┌──────────────────────────────────┐
│ 4. CORTAS (si programadas)       │
│ calcular_plan_cortas()           │
│ aplicar_cortas()                 │
│                                  │
│ SI hay corta este año:           │
│   Calcular vol_objetivo          │
│   Identificar candidatos         │
│   Marcar árboles                 │
│   dom ← 8 (cortado)              │
│ FIN SI                           │
└──────────────────────────────────┘
  ↓
┌──────────────────────────────────┐
│ 5. GUARDAR ESTADO                │
│ historial[[n]] ← población       │
│ metricas[[n]] ← estadísticas     │
└──────────────────────────────────┘
```

---

## 6. ESTRATEGIAS DE CORTE {#cortes}

### Comparación de Métodos

| Aspecto | LIOCOURT | ICA | EXISTENCIAS |
|---------|----------|-----|-------------|
| **Objetivo** | Estructura J invertida | Cosechar crecimiento | Reducir existencias |
| **Volumen** | % exceso en clases sobrepobladas | % del ICA acumulado | % del volumen actual |
| **Filtros** | Solo clases sobrepobladas | Todos los candidatos | Todos los candidatos |
| **Complejidad** | Alta (requiere cálculos) | Media | Baja |
| **Uso típico** | Manejo irregular | Bosques productivos | Cortas sanitarias |

### Prioridades de Corta

#### Suprimidos Primero
```r
prioridad = "suprimidos"
```
- Ordena: desc(dominancia) → 6, 5, 4, 3, 2, 1
- Libera espacio para árboles dominantes
- Mejora estructura y sanidad
- **Recomendado para LIOCOURT**

#### Dominantes Primero
```r
prioridad = "dominantes"
```
- Ordena: dominancia → 1, 2, 3, 4, 5, 6
- Aprovechamiento tradicional
- Madera de mayor valor
- **Uso en cortas de producción**

#### Intermedios (Aleatorio)
```r
prioridad = "intermedios"
```
- Sin priorización específica
- **Uso en cortas sanitarias**

### Exclusión de Semilleros

```r
excluir_semilleros = TRUE
```

Protege automáticamente:
- Árboles dominantes (dom ∈ {1, 2, 3})
- Con diámetro ≥ percentil 75
- Garantiza regeneración natural

---

## 7. TAREAS FUTURAS {#futuras}

### Pendientes de Desarrollo

#### 1. MEJORAS AL MODELO DE RECLUTAMIENTO (PRIORIDAD ALTA)
**Archivo**: `12_modelo_reclutamiento.R`  
**Documento técnico completo**: Ver artefacto "Mejoras al Modelo de Reclutamiento"

**Estado actual**: Tasa constante de 3% sin considerar factores ambientales

**Limitaciones**:
- No responde a apertura del dosel tras cortas
- No considera supresión por alta densidad
- Ignora impacto de ganadería

**Mejoras propuestas**:

##### a) Estimular regeneración tras aclareos
```r
factor_apertura = 1.8 en años 1-3 post-corta
                  → Decae a 1.0 en años 4-7
                  
Efecto: +80% regeneración tras corta
```

**Justificación**: Apertura del dosel aumenta luz, temperatura del suelo, germinación y supervivencia de plántulas.

##### b) Reducir regeneración por alta densidad
```r
factor_densidad = 1.0 si 80-150 m³/ha (óptimo)
                  0.05 si >250 m³/ha (crítico)
                  
Efecto: -95% regeneración en bosques densos
```

**Justificación**: Dosel cerrado (<10% luz) suprime germinación y supervivencia de regeneración.

##### c) Reducir regeneración por ganadería
```r
factor_ganaderia según uso_pecuario (F01):
  Baja: 0.80 (-20%)
  Moderada: 0.50 (-50%)
  Intensa: 0.20 (-80%)
  
Efecto: Reducción proporcional a intensidad
```

**Justificación**: Pisoteo, ramoneo y compactación del suelo por ganado reducen 40-80% la supervivencia de plántulas (literatura).

**Datos requeridos**:
- ✅ Años desde última corta (rastrable)
- ✅ Volumen/ha por rodal (calculable)
- ✅ Intensidad de ganadería (disponible en F01: `uso_pecuario`, `perturbacion1/2/3`)

**Implementación**:
```r
# Combinación de factores:
factor_sitio = max(factor_apertura, factor_densidad)
factor_final = factor_sitio × factor_ganaderia
tasa_ajustada = tasa_base × factor_final

# Ejemplo rodal post-corta + ganadería moderada:
factor_apertura = 1.8, factor_ganaderia = 0.5
→ factor_final = 1.8 × 0.5 = 0.9
Corta estimula pero ganado limita el efecto
```

**Prioridad**:
1. **ALTA**: Apertura post-corta (más impactante, datos disponibles)
2. **MEDIA**: Densidad (importante para realismo)
3. **BAJA-MEDIA**: Ganadería (si hay pastoreo activo)

**Acción**: Ver documento técnico completo con ecuaciones, ejemplos numéricos y código detallado.

---

#### 2. Análisis de Riesgo de Incendio
**Archivo**: `23_Main_incendio.R` (comentado en workflow)

**Estado**: Código completo pero no integrado

**Funcionalidades**:
- Cálculo de carga de combustibles (1h, 10h, 100h, 1000h)
- Índice de riesgo (combustibles + pendiente + continuidad)
- Velocidad de propagación estimada
- Categorización: BAJO, MODERADO, ALTO, EXTREMO

**Datos requeridos**: Formato FIREMON (hoja F06)

**Acción necesaria**:
- Descomentar en `40_WORKFLOW_COMPLETO.R` línea ~45
- Verificar datos de entrada disponibles
- Ajustar parámetros según región

#### 3. Validación de Parámetros

**Tareas**:
- Calibrar tasas de crecimiento con datos locales
- Validar factores de mortalidad con inventarios sucesivos
- Ajustar Q_FACTOR según objetivos de manejo
- Verificar ecuaciones alométricas por especie

#### 4. Mejoras Estructurales

**Código**:
- Implementar tests unitarios para funciones críticas
- Agregar logging detallado de simulación
- Crear función de exportación a bases de datos
- Mejorar manejo de errores y warnings

**Documentación**:
- Manual de usuario para técnicos forestales
- Guía de interpretación de resultados
- Ejemplos de calibración regional

#### 4. Extensiones del Modelo

**Potenciales**:
- Modelado espacial (coordenadas UTM disponibles)
- Incorporar cambio climático (precipitación, temperatura)
- Simulación de plagas y enfermedades
- Análisis económico de escenarios de manejo
- Carbono almacenado y captura

---

## REFERENCIAS RÁPIDAS

### Códigos de Dominancia Críticos
```
1,2,4 → Crecimiento 100%, Mortalidad 2%
3,5   → Crecimiento 70%,  Mortalidad 3%
6     → Crecimiento 40%,  Mortalidad 6%
7,8,9 → Muertos (no crecen)
```

### Parámetros Clave
```
Pinus:   0.40 cm/año, DMC=30cm
Quercus: 0.30 cm/año, DMC=25cm
Mortalidad base: 2%
Reclutamiento: 3%
Q-factor: 1.7
Tolerancia: ±20%
```

### Funciones Más Usadas
```r
# Filtrado
filtrar_arboles_vivos(df)

# Cálculos
calcular_volumen_arbol(d, h, tipo, a, b, c)
calcular_area_basal(d)
expandir_a_hectarea(valor, area_parcela)

# Simulación
aplicar_crecimiento_anual(arbol, config)
aplicar_mortalidad_arbol(arbol, config, r)
aplicar_reclutamiento(arboles, config, año)

# Cortas
calcular_plan_cortas(arboles, config, corte_config)
aplicar_cortas(arboles, plan, año)
```

### Estructura de Datos Típica
```r
arbol <- tibble(
  arbol_id = "R03_M067_A012",
  rodal = 3,
  muestreo = 67,
  arbol = 12,
  nombre_cientifico = "Pinus pseudostrobus",
  genero_grupo = "Pinus",
  dominancia = 1,
  diametro_normal = 32.5,
  altura_total = 14.2,
  volumen_m3 = 0.45,
  area_basal = 0.083,
  tipo = "potencia",
  a = 0.00004,
  b = 1.93694,
  c = 1.03169
)
```

---

## NOTAS IMPORTANTES

1. **Nunca modificar CONFIG directamente** - Usar archivos de configuración

2. **Siempre usar `filtrar_arboles_vivos()`** antes de análisis poblacionales

3. **Volumenes se recalculan** después de cada cambio en d o h

4. **Coordenadas UTM disponibles** pero no usadas actualmente

5. **Árboles muertos permanecen en población** (dom=7,8,9) para trazabilidad

6. **IDs únicos son permanentes** a lo largo de la simulación

7. **Valores /ha requieren expansión** usando `expandir_a_hectarea()`

8. **Ecuaciones validadas** para rango 7.5-60 cm diámetro

---

**Fin del documento técnico SIERRAFOR v2.0**