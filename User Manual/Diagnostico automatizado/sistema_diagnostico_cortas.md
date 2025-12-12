# SISTEMA DE DIAGNÓSTICO AUTOMATIZADO PARA ESTRATEGIAS DE CORTA
## Modelo SIERRAFOR - Ciclos de 10 años

---

## ÍNDICE

1. [Introducción](#introduccion)
2. [Marco Conceptual](#marco-conceptual)
3. [Sistema de Clasificación](#sistema-clasificacion)
4. [Algoritmo de Diagnóstico](#algoritmo-diagnostico)
5. [Matriz de Decisión](#matriz-decision)
6. [Implementación en R](#implementacion)
7. [Ejemplos de Salidas](#ejemplos)
8. [Integración con SIERRAFOR](#integracion)

---

## 1. INTRODUCCIÓN {#introduccion}

### Problema identificado

El método de **Liocourt no es universalmente aplicable**:
- ✓ Funciona bien en rodales con estructura J invertida existente
- ❌ Falla en rodales degradados o con distribución "campana"
- ❌ No apropiado para transformación estructural

### Solución propuesta

Sistema de **diagnóstico automatizado** que:
1. Analiza estructura diamétrica actual
2. Evalúa densidad (G_actual vs G_potencial)
3. Diagnostica estado del rodal
4. Recomienda método de corta apropiado
5. Genera estrategia a 30 años (3 ciclos de 10 años)

### Principios de diseño

- **Adaptativo**: Método varía según estado del rodal
- **Temporal**: Estrategia evoluciona en 3 fases (10, 20, 30 años)
- **Automatizado**: Decisiones basadas en métricas objetivas
- **Documentado**: Genera reporte técnico para PMF

---

## 2. MARCO CONCEPTUAL {#marco-conceptual}

### 2.1 Tipos de estructura diamétrica

#### A. J INVERTIDA (Ideal para Liocourt)

```
Características:
- Muchos árboles pequeños (>40% en 5-15cm)
- Pocos árboles grandes (<10% en >40cm)
- Decrecimiento exponencial: N_i+1 / N_i ≈ 1.3-2.0

Estrategia: MANTENIMIENTO
Método: LIOCOURT
│
│ ████████
│ ████████
│ ████████  ██████
│ ████████  ██████  ████
│ ████████  ██████  ████  ██
└─────────────────────────────
  5-15    15-25   25-35  35+
```

#### B. CAMPANA / BIMODAL (Degradado)

```
Características:
- Pico en clases intermedias (20-35cm)
- Poca regeneración (<20% en 5-15cm)
- Pocos árboles grandes (<5% en >40cm)

Estrategia: TRANSFORMACIÓN
Método: EXISTENCIAS → ICA → LIOCOURT
│
│         ████████
│   ████  ████████  ████
│   ████  ████████  ████
│ ██████  ████████  ████████
│ ██████  ████████  ████████  ██
└─────────────────────────────────
  5-15    15-25     25-35    35+
```

#### C. UNIFORME (Rodal coetáneo)

```
Características:
- Concentración en 1-2 clases
- Ausencia de otras clases
- Resultado de plantación o regeneración natural única

Estrategia: TRANSICIÓN A INCOETÁNEO
Método: ICA o EXISTENCIAS conservador
│
│                   ████████
│                   ████████
│                   ████████
│         ██        ████████
│   ██    ██  ██    ████████    ██
└─────────────────────────────────
  5-15   15-25     25-35      35+
```

#### D. IRREGULAR EMPOBRECIDA (Baja densidad)

```
Características:
- G < 15 m²/ha
- Distribución irregular
- Posible sobreexplotación histórica

Estrategia: REGENERACIÓN + PROTECCIÓN
Método: SIN CORTA o SANITARIO mínimo
│
│
│   ██
│   ██    ██        ████
│   ██    ██  ██    ████      ██
└─────────────────────────────────
  5-15   15-25     25-35      35+
```

### 2.2 Métricas de diagnóstico

#### Índices estructurales

| Métrica | Fórmula | Interpretación |
|---------|---------|----------------|
| **Coeficiente de forma (CF)** | Σ\|pct_i - pct_objetivo_i\| / 2 | CF < 20%: Buena estructura<br>CF 20-40%: Estructura mejorable<br>CF > 40%: Mala estructura |
| **Índice de regeneración (IR)** | % árboles en 5-15cm | IR > 30%: Buena regeneración<br>IR 15-30%: Regeneración moderada<br>IR < 15%: Regeneración insuficiente |
| **Índice de reserva (IRe)** | % árboles > DMC | IRe > 20%: Buena reserva<br>IRe 10-20%: Reserva moderada<br>IRe < 10%: Reserva pobre |
| **Índice de concentración (IC)** | max(pct_clase_i) | IC < 30%: Bien distribuido<br>IC 30-50%: Concentración moderada<br>IC > 50%: Alta concentración |

#### Índices de densidad

| Métrica | Interpretación |
|---------|----------------|
| **G_actual / G_potencial** | > 0.9: Sobrepoblado<br>0.7-0.9: Óptimo<br>0.5-0.7: Subpoblado<br>< 0.5: Muy degradado |
| **Densidad (árb/ha)** | > 600: Alta<br>300-600: Media<br>< 300: Baja |

---

## 3. SISTEMA DE CLASIFICACIÓN {#sistema-clasificacion}

### 3.1 Árbol de decisión automatizado

```
INICIO: Analizar rodal

┌──────────────────────────────────┐
│ PASO 1: Evaluar densidad         │
│ G_actual vs G_potencial          │
└──────────────────────────────────┘
         │
         ├─ G_actual < 0.5 × G_pot ──► CLASE D: Empobrecida
         │                              ► SIN CORTA
         │
         ├─ G_actual 0.5-0.7 × G_pot ─► Continuar a PASO 2
         │
         └─ G_actual > 0.7 × G_pot ───► Continuar a PASO 2

┌──────────────────────────────────┐
│ PASO 2: Analizar estructura      │
│ Calcular IR, IRe, IC, CF         │
└──────────────────────────────────┘
         │
         ├─ IR > 30% Y CF < 20% ──────► CLASE A: J invertida
         │                              ► MÉTODO: LIOCOURT
         │
         ├─ IC > 50% Y IRe < 10% ─────► CLASE B: Campana/Bimodal
         │                              ► MÉTODO: EXISTENCIAS → ICA → LIOCOURT
         │
         ├─ IC > 60% en 1 clase ──────► CLASE C: Uniforme
         │                              ► MÉTODO: ICA conservador
         │
         └─ Otros ────────────────────► Evaluar caso por caso

┌──────────────────────────────────┐
│ PASO 3: Determinar intensidad    │
│ Según G_actual / G_potencial     │
└──────────────────────────────────┘
         │
         ├─ Ratio > 1.0 ──────► Intensidad alta (25-30%)
         │
         ├─ Ratio 0.9-1.0 ────► Intensidad media (15-20%)
         │
         ├─ Ratio 0.7-0.9 ────► Intensidad baja (10-15%)
         │
         └─ Ratio < 0.7 ──────► Intensidad muy baja (5-10%)

┌──────────────────────────────────┐
│ PASO 4: Generar estrategia       │
│ 3 ciclos × 10 años               │
└──────────────────────────────────┘
```

### 3.2 Matriz de transición de métodos

| Ciclo actual | Estado rodal | Método actual | Ciclo siguiente (10 años) | Método futuro |
|--------------|--------------|---------------|---------------------------|---------------|
| 1 | Empobrecida (D) | SIN CORTA | ► | SIN CORTA o SANITARIO |
| 1 | Campana (B) | EXISTENCIAS | ► | ICA o EXISTENCIAS |
| 2 | Campana → Irregular | ICA | ► | LIOCOURT (si mejora) o ICA |
| 3 | Irregular → J invertida | LIOCOURT | ► | LIOCOURT (mantenimiento) |
| 1 | J invertida (A) | LIOCOURT | ► | LIOCOURT |
| 1 | Uniforme (C) | ICA | ► | ICA → EXISTENCIAS |
| 2 | Uniforme → Irregular | EXISTENCIAS | ► | ICA → LIOCOURT |

---

## 4. ALGORITMO DE DIAGNÓSTICO {#algoritmo-diagnostico}

### 4.1 Pseudocódigo

```
FUNCIÓN diagnosticar_rodal(arboles, config):
  
  # FASE 1: CALCULAR MÉTRICAS
  G_actual = calcular_area_basimetrica(arboles)
  G_potencial = estimar_G_potencial(config, calidad_sitio)
  
  distribucion = calcular_distribucion_clases(arboles)
  
  IR = porcentaje_clase(5, 15)
  IRe = porcentaje_clase(DMC, Inf)
  IC = max(porcentajes_por_clase)
  CF = calcular_coeficiente_forma(distribucion, liocourt_referencia)
  
  ratio_G = G_actual / G_potencial
  
  # FASE 2: CLASIFICAR ESTRUCTURA
  SI ratio_G < 0.5:
    clase = "D_EMPOBRECIDA"
    metodo_ciclo1 = "SIN_CORTA"
  
  SI NO Y IR > 30 Y CF < 20:
    clase = "A_J_INVERTIDA"
    metodo_ciclo1 = "LIOCOURT"
  
  SI NO Y IC > 50 Y IRe < 10:
    clase = "B_CAMPANA"
    metodo_ciclo1 = "EXISTENCIAS"
  
  SI NO Y IC > 60:
    clase = "C_UNIFORME"
    metodo_ciclo1 = "ICA"
  
  SI NO:
    clase = "INDETERMINADA"
    metodo_ciclo1 = "ICA"  # Conservador por defecto
  
  # FASE 3: DETERMINAR INTENSIDAD
  SI ratio_G > 1.0:
    intensidad = 25
  SI NO Y ratio_G > 0.9:
    intensidad = 18
  SI NO Y ratio_G > 0.7:
    intensidad = 12
  SI NO:
    intensidad = 5
  
  # FASE 4: GENERAR ESTRATEGIA 3 CICLOS
  estrategia = generar_estrategia_30_anos(clase, metodo_ciclo1, intensidad)
  
  # FASE 5: CREAR REPORTE
  reporte = generar_reporte_tecnico(
    rodal_id,
    metricas = [G_actual, IR, IRe, IC, CF],
    clase,
    estrategia
  )
  
  RETORNAR [clase, metodo_ciclo1, intensidad, estrategia, reporte]

FIN FUNCIÓN
```

### 4.2 Generación de estrategia a 30 años

```
FUNCIÓN generar_estrategia_30_anos(clase, metodo_inicial, intensidad):
  
  SEGÚN clase:
    
    CASO "A_J_INVERTIDA":
      ciclo1 = [año=2, método=LIOCOURT, intensidad=intensidad, filtros=estandar]
      ciclo2 = [año=12, método=LIOCOURT, intensidad=intensidad-2, filtros=estandar]
      ciclo3 = [año=22, método=LIOCOURT, intensidad=intensidad-5, filtros=estandar]
      objetivo = "MANTENIMIENTO de estructura J invertida"
    
    CASO "B_CAMPANA":
      ciclo1 = [año=2, método=EXISTENCIAS, intensidad=intensidad, 
                filtros=[d_min=15, d_max=35, prioridad=intermedios]]
      ciclo2 = [año=12, método=ICA, intensidad=intensidad*0.7, 
                filtros=[d_min=18, prioridad=suprimidos]]
      ciclo3 = [año=22, método=LIOCOURT, intensidad=15, 
                filtros=estandar]
      objetivo = "TRANSFORMACIÓN a J invertida en 20-30 años"
    
    CASO "C_UNIFORME":
      ciclo1 = [año=2, método=ICA, intensidad=intensidad*0.5, 
                filtros=[prioridad=suprimidos]]
      ciclo2 = [año=12, método=EXISTENCIAS, intensidad=intensidad*0.6, 
                filtros=[d_min=clase_dominante-5, d_max=clase_dominante+5]]
      ciclo3 = [año=22, método=ICA, intensidad=intensidad*0.7, 
                filtros=[prioridad=intermedios]]
      objetivo = "DIVERSIFICACIÓN estructural progresiva"
    
    CASO "D_EMPOBRECIDA":
      ciclo1 = [año=NULL, método=SIN_CORTA]
      ciclo2 = [año=12, método=SANITARIO, intensidad=5, 
                filtros=[solo_muertos_y_enfermos]]
      ciclo3 = [año=22, método=ICA, intensidad=8, 
                filtros=[prioridad=suprimidos]]
      objetivo = "REGENERACIÓN y densificación antes de cosechar"
    
  FIN SEGÚN
  
  RETORNAR [ciclo1, ciclo2, ciclo3, objetivo]

FIN FUNCIÓN
```

---

## 5. MATRIZ DE DECISIÓN {#matriz-decision}

### 5.1 Tabla de referencia rápida

| Clase | IR | IRe | IC | CF | G_actual/G_pot | Ciclo 1 (0-10 años) | Ciclo 2 (10-20 años) | Ciclo 3 (20-30 años) |
|-------|----|----|----|----|----------------|---------------------|----------------------|----------------------|
| **A** | >30% | >15% | <30% | <20% | 0.7-1.0 | LIOCOURT 15-20% | LIOCOURT 15-20% | LIOCOURT 12-15% |
| **B** | <20% | <10% | >50% | >40% | 0.6-0.9 | EXISTENCIAS 18-25% | ICA 50-70% | LIOCOURT 15-20% |
| **C** | - | - | >60% | - | 0.7-1.0 | ICA 40-50% | EXISTENCIAS 15-20% | ICA 50-60% |
| **D** | <15% | <5% | - | - | <0.5 | SIN CORTA | SANITARIO 5% | ICA 8-12% |

### 5.2 Filtros diamétricos por método y clase

| Clase | Ciclo | Método | d_min | d_max | Prioridad | Excluir semilleros |
|-------|-------|--------|-------|-------|-----------|-------------------|
| A | 1-3 | LIOCOURT | DMC | - | suprimidos | TRUE |
| B | 1 | EXISTENCIAS | 15 | 35 | intermedios | TRUE |
| B | 2 | ICA | 18 | - | suprimidos | TRUE |
| B | 3 | LIOCOURT | DMC | - | suprimidos | TRUE |
| C | 1 | ICA | clase_dom-5 | clase_dom+10 | suprimidos | TRUE |
| C | 2 | EXISTENCIAS | clase_dom | clase_dom+5 | intermedios | FALSE |
| D | 2 | SANITARIO | - | - | muertos | FALSE |

---

## 6. IMPLEMENTACIÓN EN R {#implementacion}

### 6.1 Funciones principales

```r
# 16_diagnostico_automatizado.R

#' @title Sistema de diagnóstico automatizado
diagnosticar_rodal_automatico <- function(arboles_rodal, 
                                          rodal_id,
                                          config = CONFIG,
                                          verbose = TRUE) {
  
  # Validaciones
  if (nrow(arboles_rodal) < 20) {
    return(list(
      clase = "INSUFICIENTE",
      recomendacion = "Rodal con muy pocos árboles. Evaluar manualmente."
    ))
  }
  
  vivos <- arboles_rodal %>% filtrar_arboles_vivos()
  
  # ========================================
  # PASO 1: CALCULAR MÉTRICAS
  # ========================================
  
  metricas <- calcular_metricas_diagnostico(vivos, config)
  
  if (verbose) {
    cat(sprintf("\n[DIAGNÓSTICO RODAL %d]\n", rodal_id))
    cat("═══════════════════════════════════════════════════════════\n")
    cat(sprintf("G actual:      %.2f m²/ha\n", metricas$G_actual))
    cat(sprintf("G potencial:   %.2f m²/ha\n", metricas$G_potencial))
    cat(sprintf("Ratio G:       %.2f\n", metricas$ratio_G))
    cat(sprintf("IR (regen):    %.1f%%\n", metricas$IR))
    cat(sprintf("IRe (reserva): %.1f%%\n", metricas$IRe))
    cat(sprintf("IC (concent):  %.1f%%\n", metricas$IC))
    cat(sprintf("CF (forma):    %.1f%%\n", metricas$CF))
  }
  
  # ========================================
  # PASO 2: CLASIFICAR ESTRUCTURA
  # ========================================
  
  clasificacion <- clasificar_estructura(metricas, verbose)
  
  # ========================================
  # PASO 3: GENERAR ESTRATEGIA
  # ========================================
  
  estrategia <- generar_estrategia_30_anos(
    clasificacion$clase,
    metricas,
    rodal_id,
    config
  )
  
  # ========================================
  # PASO 4: GENERAR REPORTE
  # ========================================
  
  reporte <- generar_reporte_tecnico(
    rodal_id,
    metricas,
    clasificacion,
    estrategia,
    vivos
  )
  
  # ========================================
  # RETORNO
  # ========================================
  
  resultado <- list(
    rodal_id = rodal_id,
    metricas = metricas,
    clasificacion = clasificacion,
    estrategia = estrategia,
    reporte = reporte,
    programa_cortas = estrategia$programa_cortas
  )
  
  if (verbose) {
    cat("\n")
    cat(reporte$resumen)
  }
  
  return(resultado)
}

#' @title Calcular métricas de diagnóstico
calcular_metricas_diagnostico <- function(vivos, config) {
  
  # Área basimétrica
  G_actual <- sum(vivos$area_basal, na.rm = TRUE) / 
              (config$area_parcela_ha * n_distinct(vivos$muestreo))
  
  # G potencial (calibrar según calidad de sitio)
  G_potencial <- config$g_objetivo  # Usar del config o calcular
  
  # Distribución por clases
  dist <- vivos %>%
    mutate(clase = cut(diametro_normal, 
                       breaks = seq(5, 100, 5),
                       include.lowest = TRUE)) %>%
    count(clase, .drop = FALSE) %>%
    mutate(pct = n / sum(n) * 100)
  
  # Índice de regeneración (5-15 cm)
  IR <- sum(dist$pct[dist$clase %in% c("[5,10]", "(10,15]")], na.rm = TRUE)
  
  # Índice de reserva (> DMC promedio = 30cm)
  IRe <- sum(dist$pct[as.numeric(dist$clase) > 6], na.rm = TRUE)
  
  # Índice de concentración
  IC <- max(dist$pct, na.rm = TRUE)
  
  # Coeficiente de forma (vs Liocourt ideal)
  dist_liocourt_ideal <- generar_distribucion_liocourt_ideal(
    config$q_factor, 
    nrow(dist)
  )
  CF <- sum(abs(dist$pct - dist_liocourt_ideal)) / 2
  
  # Ratio G
  ratio_G <- G_actual / G_potencial
  
  return(list(
    G_actual = G_actual,
    G_potencial = G_potencial,
    ratio_G = ratio_G,
    IR = IR,
    IRe = IRe,
    IC = IC,
    CF = CF,
    distribucion = dist,
    n_arboles = nrow(vivos),
    densidad_ha = nrow(vivos) / (config$area_parcela_ha * n_distinct(vivos$muestreo))
  ))
}

#' @title Clasificar estructura del rodal
clasificar_estructura <- function(metricas, verbose = TRUE) {
  
  # Lógica de clasificación
  if (metricas$ratio_G < 0.5) {
    clase <- "D_EMPOBRECIDA"
    descripcion <- "Rodal degradado con baja densidad"
    
  } else if (metricas$IR > 30 && metricas$CF < 20) {
    clase <- "A_J_INVERTIDA"
    descripcion <- "Estructura irregular balanceada (J invertida)"
    
  } else if (metricas$IC > 50 && metricas$IRe < 10) {
    clase <- "B_CAMPANA"
    descripcion <- "Estructura en campana o bimodal (degradada)"
    
  } else if (metricas$IC > 60) {
    clase <- "C_UNIFORME"
    descripcion <- "Estructura uniforme/coetánea"
    
  } else {
    clase <- "INDETERMINADA"
    descripcion <- "Estructura irregular no clasificable"
  }
  
  if (verbose) {
    cat("\n[CLASIFICACIÓN]\n")
    cat(sprintf("Clase:       %s\n", clase))
    cat(sprintf("Descripción: %s\n", descripcion))
  }
  
  return(list(
    clase = clase,
    descripcion = descripcion
  ))
}

#' @title Generar estrategia a 30 años (3 ciclos)
generar_estrategia_30_anos <- function(clase, metricas, rodal_id, config) {
  
  # Implementar lógica según matriz de decisión
  # Ver sección 4.2 del documento
  
  estrategia <- switch(
    clase,
    
    "A_J_INVERTIDA" = list(
      objetivo = "MANTENIMIENTO de estructura J invertida",
      ciclo1 = list(año = 2, metodo = "LIOCOURT", intensidad = 18),
      ciclo2 = list(año = 12, metodo = "LIOCOURT", intensidad = 16),
      ciclo3 = list(año = 22, metodo = "LIOCOURT", intensidad = 14)
    ),
    
    "B_CAMPANA" = list(
      objetivo = "TRANSFORMACIÓN a estructura J invertida",
      ciclo1 = list(año = 2, metodo = "EXISTENCIAS", intensidad = 20, 
                    d_min = 15, d_max = 35, prioridad = "intermedios"),
      ciclo2 = list(año = 12, metodo = "ICA", intensidad = 60, 
                    d_min = 18, prioridad = "suprimidos"),
      ciclo3 = list(año = 22, metodo = "LIOCOURT", intensidad = 15)
    ),
    
    "C_UNIFORME" = list(
      objetivo = "DIVERSIFICACIÓN estructural",
      ciclo1 = list(año = 2, metodo = "ICA", intensidad = 50),
      ciclo2 = list(año = 12, metodo = "EXISTENCIAS", intensidad = 15),
      ciclo3 = list(año = 22, metodo = "ICA", intensidad = 60)
    ),
    
    "D_EMPOBRECIDA" = list(
      objetivo = "REGENERACIÓN antes de cosecha",
      ciclo1 = list(año = NULL, metodo = "SIN_CORTA"),
      ciclo2 = list(año = 12, metodo = "SANITARIO", intensidad = 5),
      ciclo3 = list(año = 22, metodo = "ICA", intensidad = 10)
    ),
    
    # Default
    list(
      objetivo = "Evaluación manual requerida",
      ciclo1 = list(año = 2, metodo = "ICA", intensidad = 50),
      ciclo2 = list(año = 12, metodo = "ICA", intensidad = 50),
      ciclo3 = list(año = 22, metodo = "ICA", intensidad = 50)
    )
  )
  
  # Convertir a formato PROGRAMA_CORTAS
  programa <- generar_programa_cortas_from_estrategia(rodal_id, estrategia)
  
  estrategia$programa_cortas <- programa
  
  return(estrategia)
}

#' @title Generar reporte técnico
generar_reporte_tecnico <- function(rodal_id, metricas, clasificacion, 
                                     estrategia, arboles) {
  
  # Crear texto del reporte
  texto <- sprintf("
═══════════════════════════════════════════════════════════
REPORTE DE DIAGNÓSTICO Y ESTRATEGIA DE MANEJO
Rodal: %d
Fecha: %s
═══════════════════════════════════════════════════════════

1. DIAGNÓSTICO ACTUAL
──────────────────────────────────────────────────────────
Clasificación estructural: %s
%s

Métricas clave:
  • Área basimétrica:    %.2f m²/ha (objetivo: %.2f m²/ha)
  • Densidad:            %.0f árb/ha
  • Regeneración (IR):   %.1f%% (objetivo: >30%%)
  • Reserva (IRe):       %.1f%% (objetivo: >15%%)
  • Concentración (IC):  %.1f%% (objetivo: <30%%)
  • Forma (CF):          %.1f%% (objetivo: <20%%)

Evaluación general: %s

2. ESTRATEGIA RECOMENDADA (30 AÑOS / 3 CICLOS)
──────────────────────────────────────────────────────────
Objetivo a largo plazo: %s

CICLO 1 (Años 1-10):
  Año de corta:  %s
  Método:        %s
  Intensidad:    %s
  Filtros:       %s
  Justificación: %s

CICLO 2 (Años 11-20):
  Año de corta:  %s
  Método:        %s
  Intensidad:    %s
  Justificación: %s

CICLO 3 (Años 21-30):
  Año de corta:  %s
  Método:        %s
  Intensidad:    %s
  Justificación: %s

3. RECOMENDACIONES ADICIONALES
──────────────────────────────────────────────────────────
%s

4. MONITOREO Y REEVALUACIÓN
──────────────────────────────────────────────────────────
• Realizar inventario intermedio en año 5 y año 15
• Ajustar estrategia según evolución de la estructura
• Evaluar éxito de regeneración natural post-corta
• Considerar tratamientos silvícolas complementarios si necesario

═══════════════════════════════════════════════════════════
",
    rodal_id,
    format(Sys.Date(), "%d/%m/%Y"),
    clasificacion$clase,
    clasificacion$descripcion,
    metricas$G_actual,
    metricas$G_potencial,
    metricas$densidad_ha,
    metricas$IR,
    metricas$IRe,
    metricas$IC,
    metricas$CF,
    evaluar_estado_general(metricas),
    estrategia$objetivo,
    # Ciclo 1
    ifelse(is.null(estrategia$ciclo1$año), "SIN CORTA", 
           sprintf("Año %d", estrategia$ciclo1$año)),
    estrategia$ciclo1$metodo,
    sprintf("%d%%", estrategia$ciclo1$intensidad),
    generar_texto_filtros(estrategia$ciclo1),
    generar_justificacion(clasificacion$clase, 1),
    # Ciclo 2
    ifelse(is.null(estrategia$ciclo2$año), "SIN CORTA", 
           sprintf("Año %d", estrategia$ciclo2$año)),
    estrategia$ciclo2$metodo,
    sprintf("%d%%", estrategia$ciclo2$intensidad),
    generar_justificacion(clasificacion$clase, 2),
    # Ciclo 3
    ifelse(is.null(estrategia$ciclo3$año), "SIN CORTA", 
           sprintf("Año %d", estrategia$ciclo3$año)),
    estrategia$ciclo3$metodo,
    sprintf("%d%%", estrategia$ciclo3$intensidad),
    generar_justificacion(clasificacion$clase, 3),
    # Recomendaciones
    generar_recomendaciones_adicionales(clasificacion$clase, metricas)
  )
  
  return(list(
    resumen = texto,
    grafico = generar_grafico_diagnostico(metricas, clasificacion)
  ))
}
```

### 6.2 Funciones auxiliares

```r
#' Helpers para generación de textos

evaluar_estado_general <- function(metricas) {
  if (metricas$ratio_G < 0.5) {
    return("DEFICIENTE - Requiere regeneración prioritaria")
  } else if (metricas$ratio_G < 0.7 || metricas$CF > 40) {
    return("REGULAR - Requiere transformación estructural")
  } else if (metricas$CF < 20 && metricas$IR > 30) {
    return("BUENO - Estructura equilibrada para manejo")
  } else {
    return("ACEPTABLE - Con potencial de mejora")
  }
}

generar_texto_filtros <- function(ciclo) {
  filtros <- c()
  
  if (!is.null(ciclo$d_min)) {
    filtros <- c(filtros, sprintf("d_min=%dcm", ciclo$d_min))
  }
  if (!is.null(ciclo$d_max)) {
    filtros <- c(filtros, sprintf("d_max=%dcm", ciclo$d_max))
  }
  if (!is.null(ciclo$prioridad)) {
    filtros <- c(filtros, sprintf("prioridad=%s", ciclo$prioridad))
  }
  
  if (length(filtros) == 0) {
    return("Estándar")
  } else {
    return(paste(filtros, collapse=", "))
  }
}

generar_justificacion <- function(clase, ciclo_num) {
  justificaciones <- list(
    "A_J_INVERTIDA" = c(
      "Mantener estructura mediante corta selectiva en clases sobrepobladas",
      "Continuar mantenimiento con intensidad ligeramente menor",
      "Consolidar estructura J invertida equilibrada"
    ),
    "B_CAMPANA" = c(
      "Reducir clases intermedias sobrerrepresentadas (15-35cm)",
      "Aprovechar ICA acumulado favoreciendo clases suprimidas",
      "Transicionar a Liocourt una vez mejorada la estructura"
    ),
    "C_UNIFORME" = c(
      "Iniciar diversificación aprovechando ICA conservadoramente",
      "Reducir concentración en clase dominante",
      "Continuar diversificación estructural"
    ),
    "D_EMPOBRECIDA" = c(
      "Permitir regeneración natural sin intervención",
      "Corta sanitaria mínima de árboles muertos/enfermos",
      "Iniciar aprovechamiento conservador si estructura mejora"
    )
  )
  
  if (clase %in% names(justificaciones)) {
    return(justificaciones[[clase]][ciclo_num])
  } else {
    return("Evaluación caso por caso según evolución")
  }
}

generar_recomendaciones_adicionales <- function(clase, metricas) {
  recos <- c()
  
  if (metricas$IR < 15) {
    recos <- c(recos, "• CRÍTICO: Promover regeneración natural mediante aperturas de dosel")
  }
  
  if (metricas$IRe < 10) {
    recos <- c(recos, "• Proteger estrictamente árboles semilleros (>DMC, dominantes)")
  }
  
  if (metricas$IC > 50) {
    recos <- c(recos, "• Priorizar diversificación estructural en primeros ciclos")
  }
  
  if (metricas$ratio_G > 1.0) {
    recos <- c(recos, "• Considerar corta de liberación adicional si competencia excesiva")
  }
  
  if (clase == "B_CAMPANA") {
    recos <- c(recos, "• Evaluar necesidad de tratamientos de liberación de regeneración")
    recos <- c(recos, "• Monitorear respuesta de clases pequeñas post-corta")
  }
  
  if (length(recos) == 0) {
    return("• Seguir estrategia propuesta y monitorear evolución")
  } else {
    return(paste(recos, collapse = "\n"))
  }
}
```

---

## 7. EJEMPLOS DE SALIDAS {#ejemplos}

### 7.1 Rodal Clase B (Campana) - Como tu Rodal 4

```
═══════════════════════════════════════════════════════════
REPORTE DE DIAGNÓSTICO Y ESTRATEGIA DE MANEJO
Rodal: 4
Fecha: 03/11/2025
═══════════════════════════════════════════════════════════

1. DIAGNÓSTICO ACTUAL
──────────────────────────────────────────────────────────
Clasificación estructural: B_CAMPANA
Estructura en campana o bimodal (degradada)

Métricas clave:
  • Área basimétrica:    18.60 m²/ha (objetivo: 25.00 m²/ha)
  • Densidad:            163 árb/ha
  • Regeneración (IR):   16.6% (objetivo: >30%)
  • Reserva (IRe):       5.5% (objetivo: >15%)
  • Concentración (IC):  33.1% (objetivo: <30%)
  • Forma (CF):          45.2% (objetivo: <20%)

Evaluación general: REGULAR - Requiere transformación estructural

2. ESTRATEGIA RECOMENDADA (30 AÑOS / 3 CICLOS)
──────────────────────────────────────────────────────────
Objetivo a largo plazo: TRANSFORMACIÓN a estructura J invertida

CICLO 1 (Años 1-10):
  Año de corta:  Año 2
  Método:        EXISTENCIAS
  Intensidad:    20%
  Filtros:       d_min=15cm, d_max=35cm, prioridad=intermedios
  Justificación: Reducir clases intermedias sobrerrepresentadas (15-35cm)

CICLO 2 (Años 11-20):
  Año de corta:  Año 12
  Método:        ICA
  Intensidad:    60%
  Justificación: Aprovechar ICA acumulado favoreciendo clases suprimidas

CICLO 3 (Años 21-30):
  Año de corta:  Año 22
  Método:        LIOCOURT
  Intensidad:    15%
  Justificación: Transicionar a Liocourt una vez mejorada la estructura

3. RECOMENDACIONES ADICIONALES
──────────────────────────────────────────────────────────
• CRÍTICO: Promover regeneración natural mediante aperturas de dosel
• Proteger estrictamente árboles semilleros (>DMC, dominantes)
• Priorizar diversificación estructural en primeros ciclos
• Evaluar necesidad de tratamientos de liberación de regeneración
• Monitorear respuesta de clases pequeñas post-corta

4. MONITOREO Y REEVALUACIÓN
──────────────────────────────────────────────────────────
• Realizar inventario intermedio en año 5 y año 15
• Ajustar estrategia según evolución de la estructura
• Evaluar éxito de regeneración natural post-corta
• Considerar tratamientos silvícolas complementarios si necesario

═══════════════════════════════════════════════════════════
```

### 7.2 Rodal Clase A (J invertida)

```
1. DIAGNÓSTICO ACTUAL
──────────────────────────────────────────────────────────
Clasificación estructural: A_J_INVERTIDA
Estructura irregular balanceada (J invertida)

Métricas clave:
  • Área basimétrica:    22.50 m²/ha (objetivo: 25.00 m²/ha)
  • Densidad:            450 árb/ha
  • Regeneración (IR):   38.5% (objetivo: >30%)
  • Reserva (IRe):       18.2% (objetivo: >15%)
  • Concentración (IC):  25.1% (objetivo: <30%)
  • Forma (CF):          15.8% (objetivo: <20%)

Evaluación general: BUENO - Estructura equilibrada para manejo

2. ESTRATEGIA RECOMENDADA (30 AÑOS / 3 CICLOS)
──────────────────────────────────────────────────────────
Objetivo a largo plazo: MANTENIMIENTO de estructura J invertida

CICLO 1 (Años 1-10):
  Método:        LIOCOURT con G_objetivo = 25 m²/ha
  Intensidad:    18%
  
CICLO 2 (Años 11-20):
  Método:        LIOCOURT con G_objetivo = 25 m²/ha
  Intensidad:    16%
  
CICLO 3 (Años 21-30):
  Método:        LIOCOURT con G_objetivo = 25 m²/ha
  Intensidad:    14%
```

---

## 8. INTEGRACIÓN CON SIERRAFOR {#integracion}

### 8.1 Workflow modificado

```r
# En 40_WORKFLOW_COMPLETO.R

# FASE NUEVA: DIAGNÓSTICO AUTOMATIZADO
cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║      FASE 3.5: DIAGNÓSTICO Y ESTRATEGIA AUTOMATIZADA      ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")

source("modelov5/16_diagnostico_automatizado.R")

# Diagnosticar todos los rodales
rodales <- unique(arboles_analisis$rodal)
diagnosticos <- list()

for (r in rodales) {
  arboles_r <- arboles_analisis %>% filter(rodal == r)
  diag <- diagnosticar_rodal_automatico(arboles_r, r, CONFIG, verbose = TRUE)
  diagnosticos[[as.character(r)]] <- diag
  
  # Guardar reporte individual
  writeLines(diag$reporte$resumen, 
             sprintf("reportes/diagnostico_rodal_%02d.txt", r))
}

# Consolidar programas de corta recomendados
PROGRAMA_CORTAS_RECOMENDADO <- bind_rows(
  lapply(diagnosticos, function(d) d$programa_cortas)
)

# Guardar
saveRDS(diagnosticos, "resultados/diagnosticos_automatizados.rds")
write.csv(PROGRAMA_CORTAS_RECOMENDADO, 
          "resultados/programa_cortas_recomendado.csv")

# OPCIONAL: Usar recomendaciones en lugar de configuración manual
# CONFIG$programa_cortas <- PROGRAMA_CORTAS_RECOMENDADO
```

### 8.2 Modo interactivo (opcional)

```r
# Para casos dudosos o usuario desea refinar

diagnosticar_rodal_interactivo <- function(rodal_id, arboles, config) {
  
  # Diagnóstico automático
  diag_auto <- diagnosticar_rodal_automatico(arboles, rodal_id, config)
  
  cat("\n¿Aceptar diagnóstico automático? (s/n): ")
  respuesta <- readline()
  
  if (tolower(respuesta) == "n") {
    cat("\nOpciones de clase:")
    cat("\n  1. A_J_INVERTIDA")
    cat("\n  2. B_CAMPANA")
    cat("\n  3. C_UNIFORME")
    cat("\n  4. D_EMPOBRECIDA")
    cat("\nSeleccione clase (1-4): ")
    
    opcion <- as.integer(readline())
    clases <- c("A_J_INVERTIDA", "B_CAMPANA", "C_UNIFORME", "D_EMPOBRECIDA")
    clase_manual <- clases[opcion]
    
    # Regenerar estrategia con clase manual
    estrategia <- generar_estrategia_30_anos(
      clase_manual, 
      diag_auto$metricas, 
      rodal_id, 
      config
    )
    
    diag_auto$clasificacion$clase <- clase_manual
    diag_auto$clasificacion$descripcion <- paste(
      diag_auto$clasificacion$descripcion, 
      "(MANUAL)"
    )
    diag_auto$estrategia <- estrategia
  }
  
  return(diag_auto)
}
```

---

## 9. CONCLUSIONES Y RECOMENDACIONES

### 9.1 Ventajas del sistema

✅ **Objetividad**: Decisiones basadas en métricas cuantificables
✅ **Adaptabilidad**: Estrategia evoluciona según estado del rodal
✅ **Reproducibilidad**: Proceso documentado y auditable
✅ **Visión a largo plazo**: Planificación a 30 años
✅ **Flexibilidad**: Permite ajustes manuales si necesario

### 9.2 Limitaciones y precauciones

⚠️ **No reemplaza criterio profesional**: Sistema es apoyo a la decisión
⚠️ **Requiere calibración local**: G_potencial debe ajustarse al sitio
⚠️ **Evaluación intermedia necesaria**: Inventarios en año 5 y 15
⚠️ **Factores externos**: Incendios, plagas pueden alterar estrategia

### 9.3 Próximos desarrollos

1. **Incorporar índice de sitio** para calibrar G_potencial automáticamente
2. **Machine learning** para clasificación más precisa
3. **Simulación probabilística** de evolución estructural
4. **Módulo de optimización económica** integrado

---

## REFERENCIAS

- De Liocourt, F. (1898). De l'aménagement des sapinières
- Schütz, J.P. (2001). Gestion des forêts irrégulières et mélangées
- O'Hara, K.L. (2014). Multiaged Silviculture
- CONAFOR (2017). Norma SEMARNAT-152

---

**Documento técnico**  
**Fecha:** Noviembre 2025  
**Proyecto:** SIERRAFOR v2.0  
**Estado:** Propuesta de implementación
