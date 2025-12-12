# SIERRAFOR - Mejoras Propuestas al Modelo de Reclutamiento

## 📋 DOCUMENTO TÉCNICO DE MEJORA

**Módulo afectado**: `12_modelo_reclutamiento.R`  
**Estado actual**: Tasa constante de 3% sin factores ambientales  
**Fecha**: Octubre 2025  
**Prioridad**: MEDIA-ALTA

---

## 🎯 OBJETIVO DE LAS MEJORAS

Hacer el modelo de reclutamiento **más realista y ecológicamente robusto** incorporando tres factores clave:

1. **Efecto de apertura del dosel** tras cortas (estimula regeneración)
2. **Efecto de competencia** por alta densidad/volumen (inhibe regeneración)  
3. **Efecto de ganadería** (impacto negativo en plántulas)

---

## 📊 ESTADO ACTUAL DEL MODELO

### Función Principal Actual
```r
calcular_n_reclutas <- function(arboles_rodal, config = CONFIG) {
  n_vivos <- sum(!arboles_rodal$dominancia %in% c(7, 8, 9))
  n_reclutas <- round(n_vivos * config$tasa_reclutamiento)
  n_reclutas <- max(0, min(n_reclutas, n_vivos * 0.1))
  return(n_reclutas)
}
```

### Limitaciones Actuales

❌ **Tasa constante**: Siempre 3%, sin importar condiciones  
❌ **Ignora manejo**: Cortas no estimulan regeneración  
❌ **Ignora competencia**: Alta densidad no reduce regeneración  
❌ **Ignora disturbios**: Pastoreo no afecta plántulas  

### Parámetros Actuales
```r
TASA_RECLUTAMIENTO <- 0.03  # 3% constante
RECLUT_D_MIN <- 7.5         # cm
RECLUT_D_MAX <- 12.5        # cm
RECLUT_DOMINANCIA <- 6      # Suprimidos
RECLUT_ALTURA <- list(Pinus=3.0, Quercus=2.5)  # m
```

---

## 🌲 MEJORA 1: ESTIMULAR REGENERACIÓN TRAS ACLAREOS

### Fundamento Ecológico

**Cuando se cortan árboles**:
- ↑ Luz en sotobosque
- ↑ Temperatura del suelo
- ↓ Competencia por agua/nutrientes
- ↑ Germinación de semillas
- ↑ Supervivencia de plántulas

**Literatura forestal**: Incrementos de 50-300% en regeneración post-corta en bosques templados.

### Implementación Propuesta

#### Paso 1: Detectar si hubo corta reciente en el rodal

```r
# En 30_SIMULACION_10AÑOS_COMPLETA.R
# Después de aplicar cortas, marcar el rodal

SI hubo corta en rodal X en año Y:
  arboles$tuvo_corta_reciente[rodal==X] <- TRUE
  arboles$año_ultima_corta[rodal==X] <- Y
```

#### Paso 2: Calcular factor de apertura

```r
calcular_factor_apertura_dosel <- function(arboles_rodal, año_actual, config) {
  
  # Verificar si hubo corta reciente
  if (!"tuvo_corta_reciente" %in% names(arboles_rodal)) {
    return(1.0)  # Sin efecto si no hay info
  }
  
  tuvo_corta <- any(arboles_rodal$tuvo_corta_reciente, na.rm=TRUE)
  
  if (!tuvo_corta) {
    return(1.0)  # Factor neutro
  }
  
  # Calcular años desde última corta
  año_corta <- max(arboles_rodal$año_ultima_corta, na.rm=TRUE)
  años_desde_corta <- año_actual - año_corta
  
  # Efecto decae exponencialmente con el tiempo
  # Máximo efecto en años 1-3, luego decae
  
  if (años_desde_corta <= 0) {
    factor <- 1.0  # Año de corta: aún no hay efecto
    
  } else if (años_desde_corta <= 3) {
    # Años 1-3: máximo efecto (150-200%)
    factor <- config$reclut_factor_apertura_max
    
  } else if (años_desde_corta <= 7) {
    # Años 4-7: decaimiento lineal hacia 1.0
    años_decaimiento <- años_desde_corta - 3
    factor <- config$reclut_factor_apertura_max - 
              (config$reclut_factor_apertura_max - 1.0) * 
              (años_decaimiento / 4)
    
  } else {
    # Año 8+: efecto desaparece
    factor <- 1.0
  }
  
  return(factor)
}
```

#### Paso 3: Nuevos parámetros CONFIG

```r
# En 04_config_simulacion.R

RECLUT_FACTOR_APERTURA_MAX <- 1.8  # 80% más regeneración post-corta
RECLUT_AÑOS_EFECTO_APERTURA <- 7   # Efecto dura 7 años
```

#### Paso 4: Aplicar en cálculo de reclutas

```r
calcular_n_reclutas <- function(arboles_rodal, config, año_actual) {
  
  # Cálculo base
  n_vivos <- sum(!arboles_rodal$dominancia %in% c(7, 8, 9))
  tasa_base <- config$tasa_reclutamiento
  
  # NUEVO: Factor de apertura
  factor_apertura <- calcular_factor_apertura_dosel(
    arboles_rodal, año_actual, config
  )
  
  # Aplicar factor
  tasa_ajustada <- tasa_base * factor_apertura
  
  n_reclutas <- round(n_vivos * tasa_ajustada)
  n_reclutas <- max(0, min(n_reclutas, n_vivos * 0.15))  # Máx 15% ahora
  
  return(n_reclutas)
}
```

### Ejemplo Numérico

```
Rodal 3, cortado en año 5:

Año 4 (antes de corta):
  n_vivos = 92
  factor_apertura = 1.0
  tasa = 0.03 × 1.0 = 0.03
  n_reclutas = 92 × 0.03 = 3

Año 5 (año de corta):
  n_vivos = 75 (post-corta)
  factor_apertura = 1.0 (aún no hay efecto)
  n_reclutas = 75 × 0.03 = 2

Año 6 (1 año post-corta):
  n_vivos = 77
  factor_apertura = 1.8
  tasa = 0.03 × 1.8 = 0.054
  n_reclutas = 77 × 0.054 = 4 (↑33% vs sin corta)

Año 7 (2 años post-corta):
  n_vivos = 79
  factor_apertura = 1.8
  n_reclutas = 79 × 0.054 = 4

Año 8 (3 años post-corta):
  n_vivos = 82
  factor_apertura = 1.8
  n_reclutas = 82 × 0.054 = 4

Año 9 (4 años post-corta):
  n_vivos = 85
  factor_apertura = 1.6 (empieza a decaer)
  n_reclutas = 85 × 0.048 = 4

Año 12 (7 años post-corta):
  factor_apertura = 1.0 (efecto desapareció)
  Vuelve a tasa normal
```

---

## 🌳 MEJORA 2: REDUCIR REGENERACIÓN POR ALTA DENSIDAD

### Fundamento Ecológico

**Cuando hay mucho volumen en pie**:
- ↓ Luz en sotobosque (<10% luz disponible)
- ↓ Germinación de semillas
- ↓ Supervivencia de plántulas por falta de luz
- ↑ Competencia con árboles establecidos

**Umbrales críticos**:
- Volumen "óptimo": 80-150 m³/ha → regeneración normal
- Volumen "alto": 150-250 m³/ha → regeneración reducida
- Volumen "muy alto": >250 m³/ha → regeneración casi nula

### Implementación Propuesta

#### Paso 1: Calcular volumen actual del rodal

```r
calcular_factor_densidad <- function(arboles_rodal, config) {
  
  # Calcular volumen/ha actual
  vivos <- arboles_rodal %>% filter(!dominancia %in% c(7,8,9))
  n_sitios <- n_distinct(vivos$muestreo)
  
  if (n_sitios == 0) return(1.0)
  
  vol_muestreado <- sum(vivos$volumen_m3, na.rm=TRUE)
  vol_ha <- vol_muestreado / (n_sitios * config$area_parcela_ha)
  
  # Definir umbrales
  vol_optimo_min <- config$reclut_vol_optimo_min  # 80 m³/ha
  vol_optimo_max <- config$reclut_vol_optimo_max  # 150 m³/ha
  vol_critico <- config$reclut_vol_critico        # 250 m³/ha
  
  # Calcular factor según volumen
  if (vol_ha < vol_optimo_min) {
    # Muy poca densidad: también reduce regeneración
    # (falta de árboles semilleros)
    factor <- 0.5 + (vol_ha / vol_optimo_min) * 0.5
    
  } else if (vol_ha <= vol_optimo_max) {
    # Densidad óptima: factor = 1.0
    factor <- 1.0
    
  } else if (vol_ha <= vol_critico) {
    # Alta densidad: decaimiento lineal
    factor <- 1.0 - ((vol_ha - vol_optimo_max) / 
                     (vol_critico - vol_optimo_max)) * 0.9
    # Va de 1.0 a 0.1
    
  } else {
    # Muy alta densidad: regeneración casi nula
    factor <- 0.05  # 5% de tasa normal
  }
  
  return(max(0.05, min(factor, 1.0)))
}
```

#### Paso 2: Nuevos parámetros CONFIG

```r
# En 04_config_simulacion.R

RECLUT_VOL_OPTIMO_MIN <- 80   # m³/ha - mínimo para regeneración normal
RECLUT_VOL_OPTIMO_MAX <- 150  # m³/ha - máximo para regeneración normal
RECLUT_VOL_CRITICO <- 250     # m³/ha - umbral de supresión severa
```

#### Paso 3: Integrar en cálculo de reclutas

```r
calcular_n_reclutas <- function(arboles_rodal, config, año_actual) {
  
  n_vivos <- sum(!arboles_rodal$dominancia %in% c(7, 8, 9))
  tasa_base <- config$tasa_reclutamiento
  
  # Factor apertura (post-corta)
  factor_apertura <- calcular_factor_apertura_dosel(
    arboles_rodal, año_actual, config
  )
  
  # NUEVO: Factor densidad
  factor_densidad <- calcular_factor_densidad(arboles_rodal, config)
  
  # IMPORTANTE: Apertura y densidad son COMPLEMENTARIOS
  # Si cortaste recientemente, factor_apertura > 1 pero factor_densidad también mejora
  # Usar el MÁXIMO de los dos (no multiplicar)
  factor_final <- max(factor_apertura, factor_densidad)
  
  # Si no hay corta reciente, densidad puede reducir regeneración
  if (factor_apertura <= 1.0) {
    factor_final <- factor_densidad
  }
  
  tasa_ajustada <- tasa_base * factor_final
  
  n_reclutas <- round(n_vivos * tasa_ajustada)
  n_reclutas <- max(0, min(n_reclutas, n_vivos * 0.15))
  
  return(n_reclutas)
}
```

### Ejemplo Numérico

```
Rodal 7 sin cortas:

Año 3:
  n_vivos = 145
  vol_ha = 120 m³/ha (óptimo)
  factor_densidad = 1.0
  tasa = 0.03 × 1.0 = 0.03
  n_reclutas = 145 × 0.03 = 4

Año 8:
  n_vivos = 167 (creció sin manejo)
  vol_ha = 185 m³/ha (alto)
  factor_densidad = 1.0 - (185-150)/(250-150) × 0.9
                  = 1.0 - (35/100) × 0.9
                  = 1.0 - 0.315 = 0.685
  tasa = 0.03 × 0.685 = 0.021
  n_reclutas = 167 × 0.021 = 4 (similar, pero tasa reducida)

Año 12:
  n_vivos = 189 (sigue creciendo)
  vol_ha = 265 m³/ha (crítico)
  factor_densidad = 0.05 (supresión severa)
  tasa = 0.03 × 0.05 = 0.0015
  n_reclutas = 189 × 0.0015 = 0 (prácticamente nula)
```

---

## 🐄 MEJORA 3: REDUCIR REGENERACIÓN POR GANADERÍA

### Fundamento Ecológico

**Impacto del pastoreo en regeneración**:
- Pisoteo de plántulas jóvenes
- Ramoneo de brotes tiernos
- Compactación del suelo
- Reducción 40-80% en supervivencia de regeneración

### Datos Disponibles

En el archivo `inventario_forestal.xlsx`, hoja **F01** tenemos:

```r
uso_pecuario          # Código de intensidad (1-4)
perturbacion1         # Código 8 = "Pastoreo"
perturbacion2         # Código 8 = "Pastoreo"
perturbacion3         # Código 8 = "Pastoreo"
```

**CODIGOS_INTENSIDAD**:
```
1 = Nula
2 = Baja
3 = Moderada
4 = Intensa
```

**CODIGOS_PERTURBACIONES**:
```
8 = Pastoreo
```

### Implementación Propuesta

#### Paso 1: Detectar presencia y intensidad de ganadería

```r
detectar_ganaderia <- function(inventario_f01, rodal_id) {
  
  # Filtrar sitios del rodal
  sitios_rodal <- inventario_f01 %>% 
    filter(rodal == rodal_id)
  
  if (nrow(sitios_rodal) == 0) {
    return(list(hay_ganaderia = FALSE, intensidad = 0))
  }
  
  # Verificar uso pecuario
  uso_pec <- sitios_rodal$uso_pecuario
  intensidad_uso <- max(uso_pec, na.rm=TRUE)
  
  # Verificar perturbaciones
  tiene_pastoreo <- any(
    sitios_rodal$perturbacion1 == 8 | 
    sitios_rodal$perturbacion2 == 8 | 
    sitios_rodal$perturbacion3 == 8,
    na.rm=TRUE
  )
  
  # Determinar si hay ganadería activa
  hay_ganaderia <- (intensidad_uso >= 2) | tiene_pastoreo
  
  # Si no hay código de intensidad pero sí perturbación,
  # asumir intensidad moderada (3)
  if (tiene_pastoreo & (is.na(intensidad_uso) | intensidad_uso == 0)) {
    intensidad_uso <- 3
  }
  
  return(list(
    hay_ganaderia = hay_ganaderia,
    intensidad = intensidad_uso
  ))
}
```

#### Paso 2: Calcular factor de reducción

```r
calcular_factor_ganaderia <- function(inventario_f01, rodal_id, config) {
  
  # Detectar ganadería
  ganaderia <- detectar_ganaderia(inventario_f01, rodal_id)
  
  if (!ganaderia$hay_ganaderia) {
    return(1.0)  # Sin impacto
  }
  
  # Factores de reducción según intensidad
  # Basado en literatura: Baja 20%, Moderada 50%, Intensa 80%
  
  factor <- switch(
    as.character(ganaderia$intensidad),
    "1" = 1.00,  # Nula → sin efecto
    "2" = 0.80,  # Baja → reduce 20%
    "3" = 0.50,  # Moderada → reduce 50%
    "4" = 0.20,  # Intensa → reduce 80%
    1.00  # Default: sin efecto
  )
  
  return(factor)
}
```

#### Paso 3: Nuevos parámetros CONFIG

```r
# En 04_config_simulacion.R

RECLUT_FACTOR_GANADERIA <- list(
  nula = 1.00,      # Sin impacto
  baja = 0.80,      # -20%
  moderada = 0.50,  # -50%
  intensa = 0.20    # -80%
)
```

#### Paso 4: Integrar en cálculo de reclutas

```r
calcular_n_reclutas <- function(arboles_rodal, inventario_f01, config, año_actual) {
  
  rodal_id <- unique(arboles_rodal$rodal)[1]
  n_vivos <- sum(!arboles_rodal$dominancia %in% c(7, 8, 9))
  tasa_base <- config$tasa_reclutamiento
  
  # Factor apertura
  factor_apertura <- calcular_factor_apertura_dosel(
    arboles_rodal, año_actual, config
  )
  
  # Factor densidad
  factor_densidad <- calcular_factor_densidad(arboles_rodal, config)
  
  # NUEVO: Factor ganadería
  factor_ganaderia <- calcular_factor_ganaderia(
    inventario_f01, rodal_id, config
  )
  
  # Combinar factores
  # Apertura y densidad son alternativos (usar max)
  # Ganadería es MULTIPLICATIVO (siempre aplica)
  
  factor_sitio <- max(factor_apertura, factor_densidad)
  factor_final <- factor_sitio * factor_ganaderia
  
  tasa_ajustada <- tasa_base * factor_final
  
  n_reclutas <- round(n_vivos * tasa_ajustada)
  n_reclutas <- max(0, min(n_reclutas, n_vivos * 0.15))
  
  return(n_reclutas)
}
```

### Ejemplo Numérico

```
Rodal 5 con ganadería moderada:

Sin ganadería (teórico):
  tasa_base = 0.03
  factor_densidad = 1.0 (densidad óptima)
  factor_ganaderia = 1.0 (sin ganado)
  n_reclutas = 98 × 0.03 = 3

Con ganadería moderada:
  tasa_base = 0.03
  factor_densidad = 1.0
  factor_ganaderia = 0.50 (reduce 50%)
  tasa_ajustada = 0.03 × 1.0 × 0.50 = 0.015
  n_reclutas = 98 × 0.015 = 1 (↓67%)

Rodal 5 después de corta + ganadería:
  factor_apertura = 1.8 (post-corta año 2)
  factor_densidad = 0.9 (vol moderado)
  factor_sitio = max(1.8, 0.9) = 1.8
  factor_ganaderia = 0.50
  factor_final = 1.8 × 0.50 = 0.90
  tasa_ajustada = 0.03 × 0.90 = 0.027
  n_reclutas = 95 × 0.027 = 3
  
  # La corta estimula, pero ganadería limita el efecto
```

---

## 🔄 INTERACCIÓN ENTRE FACTORES

### Lógica de Combinación

```
┌─────────────────────────────────────────────────┐
│ DECISIÓN: ¿Hubo corta reciente?                │
└─────────────────────────────────────────────────┘
         │
         ├─── SÍ ──→ factor_apertura > 1.0
         │           (estimula regeneración)
         │
         └─── NO ──→ factor_apertura = 1.0
                     Evaluar factor_densidad
                     
┌─────────────────────────────────────────────────┐
│ Factor de sitio = max(apertura, densidad)      │
│                                                 │
│ Apertura y densidad son ALTERNATIVOS:          │
│ - Si cortaste: apertura > 1 domina             │
│ - Si no cortaste: densidad puede reducir       │
└─────────────────────────────────────────────────┘
         │
         ↓
┌─────────────────────────────────────────────────┐
│ Factor ganadería es MULTIPLICATIVO              │
│                                                 │
│ factor_final = factor_sitio × factor_ganaderia │
│                                                 │
│ Ganadería SIEMPRE reduce (si está presente)    │
└─────────────────────────────────────────────────┘
         │
         ↓
┌─────────────────────────────────────────────────┐
│ tasa_ajustada = tasa_base × factor_final       │
│                                                 │
│ n_reclutas = round(n_vivos × tasa_ajustada)    │
└─────────────────────────────────────────────────┘
```

### Tabla de Escenarios

| Escenario | Apertura | Densidad | Ganadería | Factor Final | Efecto |
|-----------|----------|----------|-----------|--------------|--------|
| **Óptimo** | 1.8 (post-corta) | 1.0 | 1.0 (sin ganado) | 1.8 | ↑80% |
| **Post-corta + ganado moderado** | 1.8 | 1.0 | 0.5 | 0.9 | ↓10% |
| **Alta densidad** | 1.0 | 0.3 | 1.0 | 0.3 | ↓70% |
| **Alta densidad + ganado** | 1.0 | 0.3 | 0.5 | 0.15 | ↓85% |
| **Baja densidad** | 1.0 | 0.7 | 1.0 | 0.7 | ↓30% |
| **Óptimo sin manejo** | 1.0 | 1.0 | 1.0 | 1.0 | Normal |
| **Peor caso** | 1.0 | 0.05 | 0.2 | 0.01 | ↓99% |

---

## 💻 IMPLEMENTACIÓN PASO A PASO

### Paso 1: Modificar `04_config_simulacion.R`

```r
# Agregar al final del archivo, antes de crear_configuracion_simulacion()

# ==============================================================================
# PARÁMETROS DE RECLUTAMIENTO DINÁMICO
# ==============================================================================

# Efecto de apertura post-corta
RECLUT_FACTOR_APERTURA_MAX <- 1.8  # 80% más regeneración
RECLUT_AÑOS_EFECTO_APERTURA <- 7   # Efecto dura 7 años

# Umbrales de densidad
RECLUT_VOL_OPTIMO_MIN <- 80   # m³/ha
RECLUT_VOL_OPTIMO_MAX <- 150  # m³/ha
RECLUT_VOL_CRITICO <- 250     # m³/ha

# Factores de ganadería
RECLUT_FACTOR_GANADERIA <- list(
  nula = 1.00,
  baja = 0.80,
  moderada = 0.50,
  intensa = 0.20
)
```

Luego agregar a CONFIG:
```r
config$reclut_factor_apertura_max <- RECLUT_FACTOR_APERTURA_MAX
config$reclut_años_efecto_apertura <- RECLUT_AÑOS_EFECTO_APERTURA
config$reclut_vol_optimo_min <- RECLUT_VOL_OPTIMO_MIN
config$reclut_vol_optimo_max <- RECLUT_VOL_OPTIMO_MAX
config$reclut_vol_critico <- RECLUT_VOL_CRITICO
config$reclut_factor_ganaderia <- RECLUT_FACTOR_GANADERIA
```

### Paso 2: Crear archivo `12_modelo_reclutamiento_v2.R`

Crear versión mejorada con todas las funciones nuevas:
- `calcular_factor_apertura_dosel()`
- `calcular_factor_densidad()`
- `detectar_ganaderia()`
- `calcular_factor_ganaderia()`
- `calcular_n_reclutas()` [MEJORADA]

### Paso 3: Modificar `30_SIMULACION_10AÑOS_COMPLETA.R`

```r
# Después de aplicar cortas:
if (hubo_corta) {
  arboles_actual <- arboles_actual %>%
    mutate(
      tuvo_corta_reciente = if_else(rodal == rodal_cortado, TRUE, FALSE),
      año_ultima_corta = if_else(rodal == rodal_cortado, año, año_ultima_corta)
    )
}

# En la llamada a reclutamiento:
arboles_actual <- aplicar_reclutamiento(
  arboles_actual, 
  inventario$f01,  # ← NUEVO: pasar F01 para info de ganadería
  config, 
  año
)
```

### Paso 4: Actualizar `aplicar_reclutamiento()` en `12_modelo_reclutamiento_v2.R`

```r
aplicar_reclutamiento <- function(arboles_df, inventario_f01, config, año_actual) {
  
  # ... código existente ...
  
  for (rodal_id in rodales_unicos) {
    arboles_rodal <- arboles_df %>% filter(rodal == rodal_id)
    
    # NUEVA FIRMA con inventario_f01
    n_reclutas <- calcular_n_reclutas(
      arboles_rodal, 
      inventario_f01,  # ← NUEVO
      config, 
      año_actual
    )
    
    # ... resto del código ...
  }
}
```

---

## 📊 VALIDACIÓN Y CALIBRACIÓN

### Tests a Realizar

#### Test 1: Efecto Apertura
```r
# Comparar rodal cortado vs no cortado
# Años 1-3 post-corta debe tener ~80% más reclutas
```

#### Test 2: Efecto Densidad
```r
# Verificar que rodal >250 m³/ha tiene ~95% menos reclutas
```

#### Test 3: Efecto Ganadería
```r
# Rodal con uso_pecuario=4 debe tener ~80% menos reclutas
```

### Calibración de Parámetros

Si los resultados no son realistas, ajustar:

```r
# Demasiada regeneración post-corta:
RECLUT_FACTOR_APERTURA_MAX <- 1.5  # reducir de 1.8

# Muy poca regeneración en alta densidad:
RECLUT_VOL_CRITICO <- 300  # aumentar umbral

# Ganadería muy severa:
RECLUT_FACTOR_GANADERIA$intensa <- 0.30  # subir de 0.20
```

---

## 📈 RESULTADOS ESPERADOS

### Antes de las Mejoras

```
Todos los rodales:
  Tasa fija 3%
  Sin variación por manejo
  
Rodal cortado año 5:
  Año 4: 3 reclutas
  Año 6: 2 reclutas  ← No refleja apertura
  Año 8: 3 reclutas
  
Rodal denso (270 m³/ha):
  Sigue generando 3-4 reclutas  ← Irrealista
```

### Después de las Mejoras

```
Rodal cortado año 5:
  Año 4: 3 reclutas (normal)
  Año 6: 5 reclutas  ← +67% por apertura ✓
  Año 7: 5 reclutas  ← Efecto persiste
  Año 8: 4 reclutas
  Año 12: 3 reclutas  ← Vuelve a normal
  
Rodal denso sin cortar (270 m³/ha):
  Año 5: 0 reclutas  ← Supresión severa ✓
  
Rodal con ganadería intensa:
  Año 3: 1 recluta  ← Reducción 80% ✓
  
Rodal óptimo (120 m³/ha, sin ganado):
  Año 3: 3 reclutas  ← Normal ✓
```

---

## ⚠️ ADVERTENCIAS Y CONSIDERACIONES

### 1. Datos Requeridos

✅ **Ya disponibles**:
- Volumen por rodal (calculado)
- Años desde corta (se puede rastrear)
- Uso pecuario (F01)
- Perturbación pastoreo (F01)

❌ **No disponibles actualmente**:
- Intensidad real de pastoreo (usar códigos existentes)
- Cobertura del dosel (se puede estimar de volumen)

### 2. Limitaciones del Modelo

- **Asume relación lineal** entre volumen y luz (simplificación)
- **No considera bancos de semillas** (puede subestimar respuesta)
- **Efecto ganadería constante** (en realidad varía estacionalmente)
- **No modela dispersión espacial** de semillas

### 3. Sensibilidad de Parámetros

**MÁS SENSIBLES** (ajustar con cuidado):
- `RECLUT_FACTOR_APERTURA_MAX`: Impacto directo en regeneración post-corta
- `RECLUT_VOL_CRITICO`: Define cuándo hay supresión severa
- `RECLUT_FACTOR_GANADERIA$intensa`: Puede eliminar regeneración

**MENOS SENSIBLES** (más robustos):
- `RECLUT_AÑOS_EFECTO_APERTURA`: 5-10 años son razonables
- `RECLUT_VOL_OPTIMO_MIN/MAX`: Rango amplio es aceptable

---

## 🎯 PRIORIDAD DE IMPLEMENTACIÓN

### Fase 1: ALTA PRIORIDAD (implementar primero)
✅ **Mejora 1**: Efecto apertura post-corta
- Más impactante
- Datos disponibles
- Implementación directa

### Fase 2: MEDIA PRIORIDAD
✅ **Mejora 2**: Efecto densidad
- Importante para realismo
- Requiere cálculo de volumen/ha
- Implementación moderada

### Fase 3: BAJA-MEDIA PRIORIDAD
⚠️ **Mejora 3**: Efecto ganadería
- Importante si hay pastoreo
- Datos ya capturados en F01
- Implementación sencilla una vez que Fase 1-2 funcionan

---

## 📚 REFERENCIAS CIENTÍFICAS

### Literatura sobre Regeneración Post-Corta

- Nyland (2002): "Silviculture: Concepts and Applications" - Efecto apertura dosel
- Oliver & Larson (1996): "Forest Stand Dynamics" - Dinámica regeneración
- Hawley & Smith (1972): "Silvicultura Práctica" - Respuesta a aclareos

### Umbrales de Densidad

- Zeide (2001): "Thinning and growth: a full turnaround"
- Smith et al. (1997): "The Practice of Silviculture" - Densidad óptima

### Impacto de Ganadería

- Belsky & Blumenthal (1997): "Effects of livestock grazing on stand dynamics"
- Motta (1996): "Impact of wild ungulates on forest regeneration"

---

## ✅ CHECKLIST DE IMPLEMENTACIÓN

```
[ ] Agregar parámetros a 04_config_simulacion.R
[ ] Crear 12_modelo_reclutamiento_v2.R con funciones nuevas
[ ] Modificar firma de aplicar_reclutamiento() para recibir inventario_f01
[ ] Agregar campos tuvo_corta_reciente y año_ultima_corta a arboles_df
[ ] Actualizar 30_SIMULACION_10AÑOS_COMPLETA.R para marcar cortas
[ ] Ejecutar tests unitarios de cada función
[ ] Ejecutar simulación completa y validar resultados
[ ] Comparar historial con versión anterior
[ ] Ajustar parámetros si es necesario
[ ] Documentar cambios en README
[ ] Actualizar documentación técnica
```

---

**IMPORTANTE**: Implementar estas mejoras DESPUÉS de validar que el modelo actual funciona correctamente. No hacer cambios estructurales y de parámetros simultáneamente.

**Fecha de implementación sugerida**: Una vez que PMF esté aprobado y se tenga experiencia con el modelo base.

---

**Fin del documento técnico de mejoras**