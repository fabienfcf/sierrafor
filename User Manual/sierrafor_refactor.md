# SIERRAFOR - Análisis de Redundancias y Mejoras

## 📋 ÍNDICE
1. [Redundancias Detectadas](#redundancias)
2. [Mejoras desde la Literatura](#literatura)
3. [Priorización de Acciones](#priorizacion)

---

## 🔍 REDUNDANCIAS DETECTADAS {#redundancias}

### 1. ARCHIVOS DUPLICADOS (CRÍTICO)

#### Problema:
```
20_analisis_descriptivo.R
20_analisis_descriptivo (Copie).R
```

**Acción**:
- ✅ Comparar versiones con `diff`
- ✅ Mantener solo la versión correcta
- ❌ Eliminar duplicado
- 📝 Si hay diferencias útiles, documentar y fusionar

---

### 2. CÁLCULO DE VOLUMEN REPETIDO

#### Redundancia detectada:
```r
# En 10_modelos_crecimiento.R (línea ~45):
V_nuevo = calcular_volumen_arbol(d_nuevo, h_nueva, tipo, a, b, c)

# En 13_simulador_crecimiento.R (línea ~30):
arboles <- actualizar_volumenes(arboles)
  # Llama a calcular_volumenes_vectorizado()
    # Que llama a calcular_volumen_arbol()
```

**Problema**: Se recalcula volumen dos veces en el mismo ciclo anual.

**Solución propuesta**:
```r
# EN aplicar_crecimiento_anual():
# ELIMINAR cálculo de volumen individual
# Solo actualizar d y h

aplicar_crecimiento_anual <- function(arbol, config) {
  # ...
  arbol$diametro_normal <- diametro_nuevo
  arbol$altura_total <- altura_nueva
  # NO calcular volumen aquí
  arbol$incremento_d_cm <- delta_d
  arbol$incremento_h_m <- delta_h
  # Volumen se calculará después en actualizar_volumenes()
  return(arbol)
}

# MANTENER actualizar_volumenes() como único punto de cálculo
```

**Beneficio**:
- ✅ Una sola fuente de verdad
- ✅ Más eficiente (un cálculo vectorizado vs n individuales)
- ✅ Más fácil de mantener

---

### 3. FILTRADO DE ÁRBOLES VIVOS REPETIDO

#### Redundancia detectada:
```r
# Se repite en múltiples funciones:
arboles_vivos <- arboles %>% filter(!dominancia %in% c(7,8,9))
```

**Ya existe solución**: `filtrar_arboles_vivos()` en `15_core_calculos.R`

**Problema**: No se usa consistentemente.

**Acción**:
```bash
# Buscar y reemplazar en TODOS los archivos:
grep -r "filter(!dominancia %in% c(7,8,9))" modelov5/

# Reemplazar con:
filtrar_arboles_vivos(arboles)
```

**Archivos afectados** (estimado):
- `10_modelos_crecimiento.R`
- `11_modelo_mortalidad.R`
- `12_modelo_reclutamiento.R`
- `13_simulador_crecimiento.R`
- `14_optimizador_cortas.R`
- `20_analisis_descriptivo.R`

---

### 4. CÁLCULO DE MÉTRICAS POR RODAL DUPLICADO

#### Redundancia detectada:
```r
# En 13_simulador_crecimiento.R:
calcular_metricas_estado(arboles_df)
  # Calcula densidad, volumen/ha, AB/ha por rodal

# En 20_analisis_descriptivo.R:
analizar_estructura_poblacional(arboles_df)
  # Calcula lo mismo + más cosas
```

**Problema**: Lógica similar en dos lugares.

**Solución propuesta**:
```r
# Mover TODO a 15_core_calculos.R
calcular_metricas_rodal <- function(arboles_df, config) {
  # Versión unificada y completa
  # Usable en simulación Y análisis descriptivo
}

# Especializar según necesidad:
calcular_metricas_simulacion <- function(arboles_df, config) {
  calcular_metricas_rodal(arboles_df, config) %>%
    select(rodal, n_vivos, densidad_ha, vol_ha_m3, ab_ha_m2)
  # Solo lo esencial para simulación
}

calcular_metricas_descriptivas <- function(arboles_df, config) {
  calcular_metricas_rodal(arboles_df, config) %>%
    left_join(calcular_composicion_especies(...)) %>%
    left_join(calcular_distribucion_diametrica(...))
  # Versión completa para análisis
}
```

---

### 5. VALIDACIÓN DE CONFIG REPETIDA

#### Redundancia detectada:
```r
# En múltiples archivos:
if (!exists("CONFIG")) {
  stop("CONFIG no está cargado...")
}

if (!exists("filtrar_arboles_vivos")) {
  source("modelov5/15_core_calculos.R")
}
```

**Problema**: Validaciones defensivas repetidas.

**Solución propuesta**:
```r
# Crear 00_init_sistema.R
verificar_sistema <- function() {
  errores <- c()
  
  if (!exists("CONFIG")) {
    errores <- c(errores, "CONFIG no cargado")
  }
  
  if (!exists("filtrar_arboles_vivos")) {
    errores <- c(errores, "core_calculos no cargado")
  }
  
  if (length(errores) > 0) {
    stop(paste("Errores del sistema:\n", paste(errores, collapse="\n")))
  }
  
  return(TRUE)
}

# LLAMAR UNA SOLA VEZ en 40_WORKFLOW_COMPLETO.R
verificar_sistema()
```

---

### 6. CONVERSIÓN DE CÓDIGOS SIPLAFOR

#### Redundancia detectada:
```r
# Múltiples archivos hacen joins manuales con CODIGOS_*
left_join(CODIGOS_DOMINANCIA, by="codigo")
left_join(CODIGOS_EROSION, by="codigo")
```

**Ya existe solución parcial**: `traducir_codigo()` en `03_config_codigos.R`

**Problema**: No se usa consistentemente.

**Mejora propuesta**:
```r
# Agregar función vectorizada conveniente:
enriquecer_con_codigos <- function(df, columnas_codigos) {
  # columnas_codigos = list(dominancia="dominancia", erosion="erosion_laminar")
  
  for (col_nombre in names(columnas_codigos)) {
    tipo_codigo <- columnas_codigos[[col_nombre]]
    nueva_col <- paste0(col_nombre, "_etiqueta")
    
    df[[nueva_col]] <- traducir_codigos(df[[tipo_codigo]], col_nombre)
  }
  
  return(df)
}

# Uso:
arboles <- enriquecer_con_codigos(
  arboles,
  list(dominancia = "dominancia", sanidad = "sanidad")
)
```

---

### 7. EXPANSIÓN A HECTÁREA

#### Redundancia detectada:
```r
# Se repite el cálculo manual en varios lugares:
valor_ha <- (valor_parcela / n_sitios) / config$area_parcela_ha
```

**Ya existe solución**: `expandir_a_hectarea()` en `15_core_calculos.R`

**Problema**: Cálculo manual persiste en varios archivos.

**Acción**: Buscar y reemplazar con función estándar.

---

## 📚 MEJORAS DESDE LA LITERATURA {#literatura}

### 1. MODELO DE CRECIMIENTO MÁS SOFISTICADO

#### Estado actual:
```r
Δd = tasa_base × factor_dominancia
```
Tasa constante sin considerar:
- ❌ Edad del árbol
- ❌ Competencia local
- ❌ Calidad de sitio
- ❌ Clima

#### Mejora propuesta: MODELO DE WYKOFF (1990)

**Ecuación**:
```
Δd = (a × d^b) × exp(c × BAL) × SI_factor × dominancia_factor

Donde:
- d = diámetro actual
- BAL = Basal Area Larger (área basal de árboles más grandes)
- SI = Site Index (índice de sitio)
```

**Ventajas**:
- ✅ Crecimiento disminuye con la edad (d^b con b<1)
- ✅ Incorpora competencia explícita (BAL)
- ✅ Ajustable por calidad de sitio

**Referencias**:
- Wykoff, W.R. (1990). "A basal area increment model for individual conifers"
- Stage, A.R. (1973). "Prognosis Model for Stand Development"
- Monserud, R.A. & Sterba, H. (1996). "A basal area increment model for trees"

**Implementación**:
```r
# Nuevos parámetros en CONFIG:
CRECIMIENTO_WYKOFF <- tribble(
  ~genero,   ~a,      ~b,       ~c,
  "Pinus",   1.2,     0.8,     -0.015,
  "Quercus", 0.9,     0.75,    -0.012
)

calcular_incremento_wykoff <- function(arbol, arboles_vecinos, config) {
  # Calcular BAL (área basal de árboles más grandes)
  BAL <- arboles_vecinos %>%
    filter(diametro_normal > arbol$diametro_normal) %>%
    summarise(bal = sum(area_basal)) %>%
    pull(bal)
  
  # Obtener parámetros
  params <- config$crecimiento_wykoff %>%
    filter(genero == arbol$genero_grupo)
  
  # Ecuación de Wykoff
  d <- arbol$diametro_normal
  incremento_base <- params$a * (d ^ params$b)
  competencia_factor <- exp(params$c * BAL)
  dominancia_factor <- config$modificadores_dominancia %>%
    filter(codigo == arbol$dominancia) %>%
    pull(factor_crecimiento)
  
  delta_d <- incremento_base * competencia_factor * dominancia_factor
  
  return(delta_d)
}
```

**Datos requeridos**:
- ✅ Ya tenemos: d, dominancia, área basal
- ⚠️ Faltaría: Calcular BAL por árbol (factible)
- ⚠️ Opcional: Índice de sitio (se puede estimar de altura dominante)

**Prioridad**: MEDIA-ALTA

---

### 2. MORTALIDAD BASADA EN COMPETENCIA

#### Estado actual:
```r
prob_muerte = 0.02 × factor_dominancia
```
Solo considera dominancia estática.

#### Mejora propuesta: MODELO DE HAMILTON (1986)

**Ecuación**:
```
logit(prob_muerte) = β0 + β1×(1/d) + β2×BAL + β3×suprimido

Donde:
- 1/d = inverso del diámetro (árboles pequeños más vulnerables)
- BAL = competencia
- suprimido = dummy (1 si dominancia = 6)
```

**Ventajas**:
- ✅ Árboles pequeños más vulnerables
- ✅ Competencia aumenta mortalidad
- ✅ Supresión explícita

**Referencias**:
- Hamilton, D.A. (1986). "A logistic model of mortality in thinned stands"
- Monserud, R.A. (1976). "Simulation of forest tree mortality"
- Yao, X. et al. (2001). "A generalized logistic model of mortality"

**Implementación**:
```r
calcular_probabilidad_muerte_hamiltion <- function(arbol, BAL, config) {
  
  if (arbol$dominancia %in% c(7,8,9)) return(1.0)
  
  # Parámetros calibrados (ejemplo)
  beta0 <- -5.0
  beta1 <- 8.0   # Árboles pequeños más vulnerables
  beta2 <- 0.02  # Competencia aumenta mortalidad
  beta3 <- 1.5   # Suprimidos mueren más
  
  suprimido <- if_else(arbol$dominancia == 6, 1, 0)
  
  logit_prob <- beta0 + 
                beta1 * (1/arbol$diametro_normal) + 
                beta2 * BAL + 
                beta3 * suprimido
  
  # Convertir logit a probabilidad
  prob <- 1 / (1 + exp(-logit_prob))
  
  # Limitar a rango razonable
  return(max(0.001, min(prob, 0.15)))
}
```

**Calibración necesaria**: Ajustar β0-β3 con datos locales.

**Prioridad**: MEDIA

---

### 3. ÍNDICE DE COMPETENCIA ESPACIAL

#### Estado actual:
```
No hay competencia explícita entre árboles vecinos
```

#### Mejora propuesta: ÍNDICE DE HEGYI (1974)

**Ecuación**:
```
CI_i = Σ(d_j / d_i) / dist_ij

Donde:
- d_j = diámetro del competidor j
- d_i = diámetro del árbol focal i
- dist_ij = distancia entre árboles i y j
```

**Ventajas**:
- ✅ Considera tamaño relativo de competidores
- ✅ Pondera por distancia
- ✅ Usado ampliamente en la literatura

**Problema**: ⚠️ **Requiere coordenadas xy de cada árbol**

**Datos disponibles**: Solo coordenadas de sitios (no de árboles individuales)

**Alternativa viable**: Usar **área basal del rodal** como proxy de competencia

```r
# Competencia a nivel de rodal (más simple):
calcular_competencia_rodal <- function(arboles_rodal) {
  arboles_rodal %>%
    group_by(rodal) %>%
    mutate(
      AB_rodal = sum(area_basal, na.rm=TRUE),
      AB_mas_grandes = sapply(diametro_normal, function(d) {
        sum(area_basal[diametro_normal > d], na.rm=TRUE)
      }),
      indice_competencia = AB_mas_grandes / AB_rodal
    ) %>%
    ungroup()
}
```

**Referencias**:
- Hegyi, F. (1974). "A simulation model for managing jack-pine"
- Daniels, R.F. (1976). "Simple competition indices"
- Biging & Dobbertin (1995). "Evaluation of competition indices"

**Prioridad**: BAJA (requiere datos espaciales)

---

### 4. ÍNDICE DE SITIO (SITE INDEX)

#### Estado actual:
```
Todos los rodales crecen igual (solo varía por género)
```

#### Mejora propuesta: CALCULAR SI POR RODAL

**Método estándar**:
```
SI = h_dom_100 = altura dominante a edad base (100 años)

Para estimar sin edad:
SI ≈ h_dom_actual × (100 / edad_estimada)
```

**Estimación sin edad conocida** (Chapman-Richards invertido):
```
Dado: h_dom, d_dom actuales
Estimar: edad → luego SI
```

**Uso en crecimiento**:
```r
calcular_incremento_con_SI <- function(arbol, SI_rodal, config) {
  # SI alto → más crecimiento
  # SI bajo → menos crecimiento
  
  SI_ref <- 20  # Referencia (m a 100 años)
  SI_factor <- SI_rodal / SI_ref
  
  delta_d_base <- calcular_incremento_diametro(arbol, config)
  delta_d_ajustado <- delta_d_base * SI_factor
  
  return(delta_d_ajustado)
}
```

**Ventajas**:
- ✅ Diferencia entre rodales productivos y pobres
- ✅ No requiere datos adicionales (estimable de inventario)

**Referencias**:
- Avery & Burkhart (2002). "Forest Measurements" - Capítulo Site Index
- Carmean et al. (1989). "Site index curves for forest tree species"
- Clutter et al. (1983). "Timber Management: A Quantitative Approach"

**Implementación**:
```r
estimar_SI_rodal <- function(arboles_rodal, config) {
  # Obtener árboles dominantes
  dominantes <- arboles_rodal %>%
    filter(dominancia %in% c(1, 2)) %>%
    arrange(desc(altura_total)) %>%
    head(10)  # Top 10 más altos
  
  if (nrow(dominantes) == 0) return(15)  # Default si no hay dominantes
  
  h_dom <- mean(dominantes$altura_total)
  d_dom <- mean(dominantes$diametro_normal)
  
  # Estimar edad usando ecuación inversa de Chapman-Richards
  # (requiere calibración específica)
  edad_estimada <- estimar_edad_desde_hd(h_dom, d_dom, config)
  
  # Proyectar a edad base
  SI <- h_dom * (100 / edad_estimada)
  
  return(SI)
}
```

**Prioridad**: MEDIA

---

### 5. CALIBRACIÓN Y VALIDACIÓN ESTADÍSTICA

#### Estado actual:
```
Parámetros basados en literatura general
No hay validación con datos locales
```

#### Mejora propuesta: FRAMEWORK DE VALIDACIÓN

**Métricas de ajuste**:
```r
validar_modelo <- function(observado, predicho) {
  
  # 1. Sesgo (Bias)
  bias <- mean(predicho - observado)
  
  # 2. RMSE (Root Mean Square Error)
  rmse <- sqrt(mean((predicho - observado)^2))
  
  # 3. R² (coeficiente determinación)
  ss_res <- sum((observado - predicho)^2)
  ss_tot <- sum((observado - mean(observado))^2)
  r2 <- 1 - (ss_res / ss_tot)
  
  # 4. Eficiencia del modelo (Nash-Sutcliffe)
  E <- 1 - (ss_res / ss_tot)
  
  # 5. AIC (si usamos máxima verosimilitud)
  # AIC = 2k - 2ln(L)
  
  return(list(
    bias = bias,
    rmse = rmse,
    r2 = r2,
    efficiency = E
  ))
}
```

**Validación cruzada**:
```r
# Si hay inventarios sucesivos:
# 1. Calibrar con 70% de datos
# 2. Validar con 30% restante
# 3. Reportar métricas

cross_validate_crecimiento <- function(arboles_t0, arboles_t1, años) {
  # Simular crecimiento
  simulado <- simular_crecimiento(arboles_t0, años, CONFIG)
  
  # Comparar con observado
  metricas <- validar_modelo(
    observado = arboles_t1$diametro_normal,
    predicho = simulado$diametro_normal
  )
  
  return(metricas)
}
```

**Referencias**:
- Vanclay (1994). "Modelling Forest Growth and Yield"
- Mayer & Butler (1993). "Statistical validation"
- Reynolds et al. (1988). "Goodness of fit tests"

**Prioridad**: ALTA (si hay datos para validar)

---

### 6. MODELO DE DISTRIBUCIÓN DIAMÉTRICA

#### Estado actual:
```
Se simula árbol individual
```

#### Alternativa: MODELO DE DISTRIBUCIÓN (más eficiente)

**Concepto**: 
En lugar de simular cada árbol, ajustar una distribución paramétrica (Weibull) que describe el rodal completo.

**Ecuación Weibull**:
```
f(d) = (c/b) × ((d-a)/b)^(c-1) × exp(-((d-a)/b)^c)

Parámetros:
- a = ubicación (diámetro mínimo)
- b = escala
- c = forma
```

**Ventajas**:
- ✅ Mucho más rápido (3 parámetros vs 1000 árboles)
- ✅ Apropiado para rodales grandes
- ✅ Útil para planificación a largo plazo

**Desventajas**:
- ❌ Menos detalle individual
- ❌ No apto si necesitas rastrear árboles específicos

**Referencias**:
- Bailey & Dell (1973). "Quantifying diameter distributions with Weibull"
- Cao (2004). "Predicting parameters of Weibull distribution"
- Nord-Larsen & Cao (2006). "A diameter distribution model"

**Prioridad**: BAJA (modelo individual es más apropiado para este PMF)

---

### 7. SIMULACIÓN ESTOCÁSTICA CON MÚLTIPLES RUNS

#### Estado actual:
```
Simulación determinística (un solo resultado)
```

#### Mejora propuesta: SIMULACIÓN MONTE CARLO

**Concepto**:
Ejecutar simulación 100-1000 veces con diferentes semillas aleatorias para:
- Mortalidad estocástica
- Reclutamiento estocástico
- Variabilidad en crecimiento

**Resultado**: Intervalos de confianza para proyecciones

```r
simulacion_monte_carlo <- function(arboles_inicial, config, n_runs = 100) {
  
  resultados <- list()
  
  for (run in 1:n_runs) {
    set.seed(run)  # Reproducible pero diferente
    
    sim <- simular_crecimiento_rodal(arboles_inicial, config, años=10)
    
    resultados[[run]] <- sim$poblacion_final %>%
      summarise(
        run = run,
        n_vivos = sum(!dominancia %in% c(7,8,9)),
        vol_total = sum(volumen_m3, na.rm=TRUE)
      )
  }
  
  resultados_df <- bind_rows(resultados)
  
  # Calcular intervalos de confianza
  intervalos <- resultados_df %>%
    summarise(
      vol_media = mean(vol_total),
      vol_q025 = quantile(vol_total, 0.025),
      vol_q975 = quantile(vol_total, 0.975),
      n_media = mean(n_vivos),
      n_q025 = quantile(n_vivos, 0.025),
      n_q975 = quantile(n_vivos, 0.975)
    )
  
  return(list(
    runs = resultados_df,
    intervalos = intervalos
  ))
}
```

**Ventajas**:
- ✅ Cuantifica incertidumbre
- ✅ Más robusto para toma de decisiones
- ✅ Detecta escenarios extremos

**Referencias**:
- Haight & Monserud (1990). "Evaluating uncertainty in forest growth"
- Kangas & Kangas (2004). "Probability, possibility and evidence: approaches to uncertainty"

**Prioridad**: MEDIA-BAJA (útil pero no esencial)

---

### 8. OPTIMIZACIÓN MULTIOBJETIVO PARA CORTAS

#### Estado actual:
```
Optimización enfocada en volumen y estructura
```

#### Mejora propuesta: ALGORITMOS MULTIOBJETIVO

**Objetivos simultáneos**:
1. Maximizar producción de madera
2. Maximizar biodiversidad (estructura irregular)
3. Minimizar erosión
4. Maximizar carbono almacenado
5. Maximizar valor económico neto

**Algoritmo**: NSGA-II (Non-dominated Sorting Genetic Algorithm)

**Concepto**: Frontera de Pareto
```
No hay una solución única "óptima"
Hay un conjunto de soluciones "no dominadas"

Ejemplo:
Solución A: Vol alto, Carbono bajo
Solución B: Vol medio, Carbono alto
Solución C: Vol medio, Carbono medio → DOMINADA (eliminar)
```

**Referencias**:
- Deb et al. (2002). "A fast elitist multi-objective genetic algorithm: NSGA-II"
- Ducheyne et al. (2004). "Multi-objective optimization in forestry"
- Pukkala (2002). "Multi-objective forest planning"

**Implementación**: Requiere paquete `mco` o `nsga2R` en R

**Prioridad**: BAJA (muy avanzado, no necesario para PMF básico)

---

## 🎯 PRIORIZACIÓN DE ACCIONES {#priorizacion}

### INMEDIATO (hacer YA)
1. ✅ **Eliminar archivo duplicado** `20_analisis_descriptivo (Copie).R`
2. ✅ **Eliminar cálculo de volumen duplicado** en crecimiento individual
3. ✅ **Unificar uso de `filtrar_arboles_vivos()`** en todo el código
4. ✅ **Centralizar validación de sistema** en un solo lugar

**Esfuerzo**: 2-4 horas  
**Beneficio**: Código más limpio y mantenible

---

### CORTO PLAZO (próximo mes)
1. 🌱 **Implementar mejoras de reclutamiento** (ya documentadas)
2. 📊 **Framework de validación estadística** (si hay datos)
3. 🌲 **Calcular índice de sitio por rodal**
4. 📝 **Unificar cálculo de métricas** en `15_core_calculos.R`

**Esfuerzo**: 1-2 semanas  
**Beneficio**: Modelo más realista y validable

---

### MEDIANO PLAZO (2-6 meses)
1. 📈 **Modelo de Wykoff para crecimiento** (competencia explícita)
2. 💀 **Modelo de Hamilton para mortalidad** (basado en competencia)
3. 🎲 **Simulación Monte Carlo** (cuantificar incertidumbre)
4. 📊 **Dashboard interactivo** de resultados (Shiny app)

**Esfuerzo**: 1-2 meses  
**Beneficio**: Modelo de clase mundial

---

### LARGO PLAZO (opcional)
1. 🗺️ **Modelo espacial** (si se obtienen coordenadas de árboles)
2. 🎯 **Optimización multiobjetivo** (NSGA-II)
3. 🌡️ **Incorporar cambio climático** (escenarios de temperatura/precipitación)
4. 🐛 **Módulo de plagas y enfermedades**

**Esfuerzo**: 3-6 meses  
**Beneficio**: Investigación de frontera

---

## 📊 MATRIZ ESFUERZO vs IMPACTO

```
Alto Impacto
     │
     │  Eliminar         Mejoras         Modelo Wykoff
     │  redundancias    reclutamiento
     │                                    
     │                  SI por rodal     Monte Carlo
     │                  
     │  Código           Validación      Espacial
     │  unificado        estadística     
     │
Bajo │____________________________________________
     Bajo                                    Alto
                    Esfuerzo
```

**Recomendación**: Empezar por **cuadrante superior izquierdo** (alto impacto, bajo esfuerzo).

---

## 📚 REFERENCIAS CLAVE

### Libros fundamentales:
1. **Vanclay, J.K. (1994)**. "Modelling Forest Growth and Yield"
2. **Pretzsch, H. (2009)**. "Forest Dynamics, Growth and Yield"
3. **Avery & Burkhart (2002)**. "Forest Measurements"
4. **Weiskittel et al. (2011)**. "Forest Growth and Yield Modeling"

### Artículos seminales:
1. **Wykoff (1990)** - Modelo de crecimiento con competencia
2. **Hamilton (1986)** - Modelo logístico de mortalidad
3. **Hegyi (1974)** - Índice de competencia espacial
4. **Stage (1973)** - Prognosis model (base de muchos modelos)

### Revistas especializadas:
- *Forest Ecology and Management*
- *Canadian Journal of Forest Research*
- *Forest Science*
- *Ecological Modelling*

---

## ✅ CHECKLIST DE IMPLEMENTACIÓN

### Redundancias (HOY):
```
[ ] Comparar y eliminar archivo duplicado
[ ] Eliminar cálculo volumen en aplicar_crecimiento_anual()
[ ] Buscar/reemplazar filter(!dominancia...) con filtrar_arboles_vivos()
[ ] Crear verificar_sistema() y centralizar validaciones
[ ] Unificar uso de expandir_a_hectarea()
[ ] Consolidar cálculo de métricas en core_calculos.R
```

### Mejoras (PRÓXIMO MES):
```
[ ] Implementar mejoras reclutamiento (doc separado)
[ ] Calcular índice de sitio por rodal
[ ] Framework de validación estadística
[ ] Documentar cambios en README
```

### Avanzadas (SI HAY TIEMPO):
```
[ ] Modelo Wykoff con BAL
[ ] Modelo Hamilton para mortalidad
[ ] Simulación Monte Carlo (100 runs)
[ ] Calibración con datos locales
```

---

**IMPORTANTE**: No intentar hacer todo a la vez. Implementar incrementalmente y validar cada mejora antes de pasar a la siguiente.

**Principio de Pareto**: 20% del esfuerzo (eliminar redundancias + mejoras reclutamiento) dará 80% del beneficio.

---

**Fin del análisis de redundancias y mejoras**