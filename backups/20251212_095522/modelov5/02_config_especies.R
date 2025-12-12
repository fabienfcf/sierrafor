# ==============================================================================
# 01_CONFIG_ESPECIES.R
# Información biológica y dasométrica por especie
# ==============================================================================

library(tidyverse)

cat("\n[1/4] Cargando configuración de especies...\n")

# ==============================================================================
# 1. CATÁLOGO DE ESPECIES
# ==============================================================================

ESPECIES <- tribble(
  ~codigo, ~nombre_cientifico,      ~genero,           ~grupo,          ~maderable,
  10,      "Pinus sp.",             "Pinus",           "Coníferas",     "SI",
  11,      "Pinus pseudostrobus",   "Pinus",           "Coníferas",     "SI",
  12,      "Pinus teocote",         "Pinus",           "Coníferas",     "SI",
  13,      "Pinus cembroides",      "Pinus",           "Coníferas",     "NO",
  20,      "Juniperus sp",          "Juniperus",       "Coníferas",     "NO",
  21,      "Juniperus flaccida",    "Juniperus",       "Coníferas",     "NO",
  30,      "Cupressus sp",          "Cupressus",       "Coníferas",     "NO",
  40,      "Pseudotsuga sp",        "Pseudotsuga",     "Coníferas",     "NO",
  50,      "otras coniferas",       "otras coniferas", "Coníferas",     "NO",
  61,      "Quercus rysophylla",    "Quercus",         "Latifoliadas",  "SI",
  62,      "Quercus laeta",         "Quercus",         "Latifoliadas",  "SI",
  63,      "Quercus affinis",       "Quercus",         "Latifoliadas",  "SI",
  64,      "Quercus laceyi",        "Quercus",         "Latifoliadas",  "SI",
  70,      "Alnus sp",              "Alnus",           "Latifoliadas",  "NO",
  81,      "Arbutus xalapensis",    "Arbutus",         "Latifoliadas",  "NO",
  90,      "Fraxinus sp",           "Fraxinus",        "Latifoliadas",  "NO",
  101,     "Crataegus mexicana",    "Crataegus",       "Latifoliadas",  "NO",
  68,       "Amelanchier denticulata", "Amelanchier",        "Latifoliadas",  "NO",
  67, "Cercis canadensis", "Cercis",        "Latifoliadas",  "NO",
  65, "Juglans sp", "Juglans",        "Latifoliadas",  "NO",
  66, "Carya sp", "Carya",        "Latifoliadas",  "NO"
  
)

# ==============================================================================
# 2. ECUACIONES ALOMÉTRICAS DE VOLUMEN
# ==============================================================================

ECUACIONES_VOLUMEN <- tribble(
  ~nombre_cientifico,    ~tipo,       ~a,            ~b,          ~c,          ~fuente,
  "Quercus affinis",     "potencia",  0.00006,       1.85604,     0.97898,     "SIPLAFOR",
  "Quercus laceyi",      "exp",       -9.48686252,   1.82408096,  0.96892639,  "1er INFYS",
  "Quercus rysophylla",  "exp",       -9.48686252,   1.82408096,  0.96892639,  "1er INFYS",
  "Quercus laeta",       "exp",       -9.48686252,   1.82408096,  0.96892639,  "1er INFYS",
  "Pinus cembroides",    "exp",       -9.8207876,    1.89180185,  1.08048365,  "1er INFYS",
  "Pinus pseudostrobus", "potencia",  0.00004,       1.93694,     1.03169,     "SIPLAFOR",
  "Pinus teocote",       "exp",       -8.72641434,   1.43032994,  1.19541675,  "1er INFYS",
  "Juniperus flaccida",  "potencia",  0.00013,       1.77192,     0.73487,     "SIPLAFOR",
  "Arbutus xalapensis",  "potencia",  0.00006,       1.82154,     0.96124,     "SIPLAFOR",
  "Fraxinus sp",         "exp",       -9.80434696,   1.91033696,  1.03262007,  "1er INFYS",
  "Carya sp",         "exp",       -9.98222558,   1.94239763,  1.02228707,  "1er INFYS",
  "Crataegus mexicana",     "potencia",  0.00006,       1.98226,     0.84224,     "SIPLAFOR",
  "Juglans sp",    "exp",       -9.82944377,    1.9060093,  1.04047533,  "1er INFYS"
  
)

# ==============================================================================
# 3. PARÁMETROS CHAPMAN-RICHARDS (ALTURA-DIÁMETRO)
# Ecuación: h = a × (1 - exp(-b×d))^c
# Derivada: dh/dd = a × c × b × exp(-b×d) × (1 - exp(-b×d))^(c-1)
# ==============================================================================

PARAMETROS_ALTURA_DIAMETRO <- tribble(
  ~especie,                ~n,   ~r2,      ~a_intermedio, ~b_intermedio, ~c_intermedio, ~a_q1,   ~b_q1,   ~c_q1,   ~a_q3,   ~b_q3,   ~c_q3,
  "Pinus pseudostrobus",   138,  0.5716,   21.058,        0.01499,       0.5583,        15.794,  0.01349, 0.4746,  26.323,  0.01649, 0.6421,
  "Pinus teocote",   138,  0.5716,   21.058,        0.01499,       0.5583,        15.794,  0.01349, 0.4746,  26.323,  0.01649, 0.6421,  #buscar verdaderos parametros
    "Pinus cembroides",      744,  0.6309,   20.334,        0.01390,       0.7051,        15.250,  0.01251, 0.5994,  25.417,  0.01529, 0.8109,
  "Quercus rysophylla",    101,  0.7416,   13.107,        0.03533,       0.8225,        9.830,   0.03180, 0.6991,  16.384,  0.03886, 0.9459,
  "Quercus laceyi",    101,  0.7416,   13.107,        0.03533,       0.8225,        9.830,   0.03180, 0.6991,  16.384,  0.03886, 0.9459, #buscar verdaderos parametros
  "Quercus affinis",    101,  0.7416,   13.107,        0.03533,       0.8225,        9.830,   0.03180, 0.6991,  16.384,  0.03886, 0.9459, #buscar verdaderos parametros
   "Quercus laeta",         306,  0.2877,   8.611,         0.06131,       0.6720,        9.000,   0.01839, 0.6000,  10.800,  0.09800, 0.8000
)

# Mapeo de especies sin parámetros específicos a especies equivalentes
MAPEO_ESPECIES_ALTURA <- c(
  # "Quercus affinis" = "Quercus rysophylla",
  # "Quercus laceyi"  = "Quercus rysophylla",
  # "Pinus teocote"   = "Pinus pseudostrobus"
)

# ==============================================================================
# 4. TASAS DE CRECIMIENTO DIAMÉTRICO BASE (cm/año)
# ==============================================================================

CRECIMIENTO_DIAMETRICO <- tribble(
  ~genero,   ~tasa_base_cm_año, ~fuente,
  "Pinus",   0.40,              "Observaciones campo + literatura regional",
  "Quercus", 0.30,              "Observaciones campo + literatura regional"
)

# ==============================================================================
# 5. FUNCIONES DE ACCESO RÁPIDO
# ==============================================================================

#' @title Obtener ecuación de volumen para una especie
#' @param nombre_cientifico Nombre científico de la especie
#' @return Tibble con parámetros (tipo, a, b, c, fuente) o NULL
#' @examples
#' obtener_ecuacion_volumen("Pinus pseudostrobus")
obtener_ecuacion_volumen <- function(nombre_cientifico) {
  resultado <- ECUACIONES_VOLUMEN %>%
    filter(nombre_cientifico == !!nombre_cientifico)
  
  if (nrow(resultado) == 0) {
    warning(sprintf("No se encontró ecuación para '%s'", nombre_cientifico))
    return(NULL)
  }
  
  return(resultado)
}

#' @title Obtener parámetros de altura para una especie
#' @param nombre_cientifico Nombre científico
#' @param dominancia Código de dominancia (1-6) para seleccionar cuartil
#' @return Lista con parámetros (a, b, c) o tibble completo si dominancia=NULL
#' @examples
#' obtener_parametros_altura("Pinus pseudostrobus", dominancia = 1)
obtener_parametros_altura <- function(nombre_cientifico, dominancia = NULL) {
  
  # Aplicar mapeo si existe
  especie_buscar <- nombre_cientifico
  if (nombre_cientifico %in% names(MAPEO_ESPECIES_ALTURA)) {
    especie_buscar <- MAPEO_ESPECIES_ALTURA[nombre_cientifico]
    message(sprintf("Usando parámetros de '%s' para '%s'", 
                    especie_buscar, nombre_cientifico))
  }
  
  params <- PARAMETROS_ALTURA_DIAMETRO %>%
    filter(especie == especie_buscar)
  
  if (nrow(params) == 0) {
    warning(sprintf("Especie '%s' no encontrada en parámetros altura-diámetro", 
                    nombre_cientifico))
    return(NULL)
  }
  
  # Si no se especifica dominancia, retornar toda la fila
  if (is.null(dominancia)) {
    return(params)
  }
  
  # Seleccionar parámetros según dominancia
  if (dominancia == 6) {
    # Suprimidos → Q1 (árboles pequeños)
    return(list(a = params$a_q1, b = params$b_q1, c = params$c_q1))
  } else if (dominancia %in% c(3, 5)) {
    # Intermedios → Q2
    return(list(a = params$a_intermedio, b = params$b_intermedio, c = params$c_intermedio))
  } else if (dominancia %in% c(1, 2, 4)) {
    # Dominantes → Q3 (árboles grandes)
    return(list(a = params$a_q3, b = params$b_q3, c = params$c_q3))
  } else {
    # Default: intermedios
    return(list(a = params$a_intermedio, b = params$b_intermedio, c = params$c_intermedio))
  }
}

#' @title Obtener tasa de crecimiento diamétrico para un género
#' @param genero "Pinus" o "Quercus"
#' @return Tasa en cm/año
#' @examples
#' obtener_tasa_crecimiento("Pinus")
obtener_tasa_crecimiento <- function(genero) {
  tasa <- CRECIMIENTO_DIAMETRICO %>%
    filter(genero == !!genero) %>%
    pull(tasa_base_cm_año)
  
  if (length(tasa) == 0) {
    warning(sprintf("Género '%s' no encontrado, usando 0.30 cm/año por defecto", genero))
    return(0.30)
  }
  
  return(tasa)
}

#' @title Calcular dh/dd desde parámetros Chapman-Richards
#' @param d Diámetro normal (cm)
#' @param a,b,c Parámetros de Chapman-Richards
#' @return Tasa de cambio dh/dd (m/cm)
calcular_dhdd_chapman_richards <- function(d, a, b, c) {
  if (is.na(a) | is.na(b) | is.na(c) | d <= 0) {
    return(NA_real_)
  }
  
  exp_term <- exp(-b * d)
  base_term <- 1 - exp_term
  
  if (base_term <= 0) return(0)
  
  dhdd <- a * c * b * exp_term * (base_term ^ (c - 1))
  
  # Limitar a rango razonable
  return(max(0.05, min(dhdd, 0.50)))
}

# ==============================================================================
# RESUMEN DE CARGA
# ==============================================================================

cat(sprintf("  ✓ Especies catalogadas:      %d\n", nrow(ESPECIES)))
cat(sprintf("  ✓ Ecuaciones de volumen:     %d\n", nrow(ECUACIONES_VOLUMEN)))
cat(sprintf("  ✓ Modelos altura-diámetro:   %d\n", nrow(PARAMETROS_ALTURA_DIAMETRO)))
cat(sprintf("  ✓ Géneros con tasa de crec.: %d\n", nrow(CRECIMIENTO_DIAMETRICO)))
cat("  ✓ Funciones disponibles:\n")
cat("      obtener_ecuacion_volumen(especie)\n")
cat("      obtener_parametros_altura(especie, dominancia)\n")
cat("      obtener_tasa_crecimiento(genero)\n")
cat("      calcular_dhdd_chapman_richards(d, a, b, c)\n")