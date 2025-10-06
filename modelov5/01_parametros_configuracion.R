# ==============================================================================
# MÓDULO 1: CONFIGURACIÓN Y PARÁMETROS DEL SISTEMA
# Gestión Forestal Dinámica - Las Alazanas
# ==============================================================================

library(tidyverse)

# ==============================================================================
# 1. PARÁMETROS TEMPORALES
# ==============================================================================

PERIODO_SIMULACION <- 10  # años del ciclo de corta
INICIO_SIMULACION <- 2025

# ==============================================================================
# 2. ESPECIES Y GÉNEROS OBJETIVO
# ==============================================================================

GENEROS_OBJETIVO <- c("Pinus", "Quercus")

# Mapeo de especies a parámetros específicos
ESPECIES_CONFIG <- tribble(
  ~especie,                ~genero,    ~grupo,
  "Pinus pseudostrobus",   "Pinus",    "Coníferas",
  "Pinus teocote",         "Pinus",    "Coníferas",
  "Pinus cembroides",      "Pinus",    "Coníferas",
  "Quercus rysophylla",    "Quercus",  "Latifoliadas",
  "Quercus laeta",         "Quercus",  "Latifoliadas",
  "Quercus affinis",       "Quercus",  "Latifoliadas",
  "Quercus laceyi",        "Quercus",  "Latifoliadas"
)

# ==============================================================================
# 3. CRECIMIENTO DIAMÉTRICO ANUAL BASE (cm/año)
# ==============================================================================

CRECIMIENTO_BASE <- list(
  "Pinus" = 0.40,      # 4 mm/año
  "Quercus" = 0.30     # 3 mm/año
)

# Modificadores por dominancia (multiplicadores de la tasa base)
MODIFICADORES_DOMINANCIA <- tribble(
  ~codigo_dom, ~nombre_dom,              ~factor_crecimiento, ~factor_mortalidad,
  1,           "Dominante",               1.00,                1.0,
  2,           "Codominante",             1.00,                1.0,
  3,           "Intermedio",              0.70,                1.5,
  4,           "Libre sin supresión",     1.00,                1.0,
  5,           "Libre con supresión",     0.70,                1.5,
  6,           "Suprimido",               0.40,                3.0,
  7,           "Muerto en pie",           0.00,                NA,
  8,           "Muerto caído",            0.00,                NA,
  9,           "Tocón",                   0.00,                NA
)

# ELIMINADO: Factor por edad de simulación (biológicamente incorrecto)
# El crecimiento ya se reduce naturalmente por:
# - Dominancia (suprimidos crecen 40% de la tasa)
# - Mortalidad diferencial (elimina árboles senescentes)
# - Curvas altura-diámetro (incorporan asíntota biológica)

# ==============================================================================
# 4. MORTALIDAD
# ==============================================================================

MORTALIDAD_BASE <- 0.005  # 0.5% anual

# Semilla para reproducibilidad de eventos estocásticos
SEMILLA_MORTALIDAD <- 42

# NOTA: El incremento de mortalidad se maneja vía:
# - Modificadores por dominancia (suprimidos mueren 3× más)
# - Competencia capturada en cambios de dominancia
# NO se incrementa artificialmente por "año de simulación"

# ==============================================================================
# 5. RECLUTAMIENTO
# ==============================================================================

TASA_RECLUTAMIENTO <- 0.02  # 2% de la densidad actual por año

# Rango de clases de ingreso (diámetro en cm)
RECLUTAMIENTO_D_MIN <- 7.5
RECLUTAMIENTO_D_MAX <- 12.5

# Dominancia inicial de nuevos árboles
RECLUTAMIENTO_DOMINANCIA <- 6  # Suprimidos

# Altura inicial aproximada (m) por género
RECLUTAMIENTO_ALTURA <- list(
  "Pinus" = 3.5,
  "Quercus" = 2.5
)

# ==============================================================================
# 6. DISTRIBUCIÓN OBJETIVO (LIOCOURT)
# ==============================================================================

Q_FACTOR <- 1.5  # Cociente de Liocourt

# Clases diamétricas (límites en cm)
CLASES_DIAMETRICAS <- seq(5, 100, by = 5)

# ==============================================================================
# 7. DIÁMETRO MÍNIMO DE CORTA (DMC) por género (cm)
# ==============================================================================

DMC <- list(
  "Pinus" = 30,
  "Quercus" = 25
)

# ==============================================================================
# 8. REGLAS DE CORTA
# ==============================================================================

# Proporción del exceso a remover en clases sobrepobladas
INTENSIDAD_CORTA <- 0.70  # 70% del exceso vs J invertida

# Tolerancia para considerar "equilibrado" (%)
TOLERANCIA_EQUILIBRIO <- 20  # ±20% del ideal

# OPCIÓN 1: FAVORECER REGENERACIÓN
PRIORIDAD_CORTA <- c(
  "dominancia_baja",   # Cortar suprimidos primero
  "diametro_menor"     # Dentro de cada clase, los más pequeños primero
)

# # OPCIÓN 2: APROVECHAMIENTO TRADICIONAL
# PRIORIDAD_CORTA <- c(
#   "dominancia_alta",   # Cortar dominantes primero
#   "diametro_mayor"     # Los más grandes primero
# )

# ==============================================================================
# 9. ORDEN DE CORTA POR RODAL
# ==============================================================================

# Define qué rodal se corta cada año
ORDEN_CORTA_RODALES <- tribble(
  ~rodal,  ~año_corta,
  1,       1,
  2,       2,
  3,       3,
  4,       4,
  5,       5,
  6,       6,
  7,       7,
  8,       8,
  9,       9,
  10,      10
)

# ==============================================================================
# 10. PARÁMETROS DE EXPANSIÓN Y MUESTREO
# ==============================================================================

AREA_PARCELA_HA <- 0.05  # 500 m² por sitio de muestreo

# ==============================================================================
# 11. MODELOS DE CRECIMIENTO EN ALTURA (Chapman-Richards)
# ==============================================================================

# Ruta al archivo con parámetros ajustados manualmente
RUTA_PARAMETROS_CR <- "2_parametros_finales_ajustados_manualmente.csv"

# Función para calcular dh/dd desde parámetros Chapman-Richards
# Ecuación: h = a × (1 - exp(-b×d))^c
# Derivada: dh/dd = a × c × b × exp(-b×d) × (1 - exp(-b×d))^(c-1)
calcular_dhdd_chapman_richards <- function(d, a, b, c) {
  if (is.na(a) | is.na(b) | is.na(c) | d <= 0) {
    return(NA_real_)
  }
  
  exp_term <- exp(-b * d)
  base_term <- 1 - exp_term
  
  # Evitar división por cero o términos inválidos
  if (base_term <= 0) {
    return(0)
  }
  
  dhdd <- a * c * b * exp_term * (base_term ^ (c - 1))
  
  return(dhdd)
}

# Función para cargar parámetros de Chapman-Richards
cargar_parametros_cr <- function() {
  
  if (!file.exists(RUTA_PARAMETROS_CR)) {
    stop("❌ No se encontró archivo: ", RUTA_PARAMETROS_CR,
         "\n   Este archivo es CRÍTICO para calcular crecimiento en altura.")
  }
  
  parametros <- read_csv(RUTA_PARAMETROS_CR, show_col_types = FALSE)
  
  # Validar columnas requeridas
  columnas_requeridas <- c("especie", 
                           "a_q1", "b_q1", "c_q1",
                           "a_intermedio", "b_intermedio", "c_intermedio",
                           "a_q3", "b_q3", "c_q3")
  
  if (!all(columnas_requeridas %in% names(parametros))) {
    stop("❌ El archivo de parámetros no tiene las columnas requeridas:\n   ",
         paste(columnas_requeridas, collapse=", "))
  }
  
  cat(sprintf("✓ Parámetros Chapman-Richards cargados: %d especies\n",
              nrow(parametros)))
  
  return(parametros)
}

# Función para interpolar dh/dd según diámetro y dominancia
interpolar_dhdd <- function(especie, diametro, dominancia, parametros_cr) {
  
  # MAPEO DE ESPECIES SIN PARÁMETROS ESPECÍFICOS
  mapeo_especies <- c(
    "Quercus afinis" = "Quercus rysophylla",
    "Quercus affinis" = "Quercus rysophylla" , # Por si aparece con doble f
    "Quercus laceyi" = "Quercus rysophylla"  # Por si aparece con doble f
    
  )
  
  # Aplicar mapeo si existe
  especie_a_buscar <- if_else(
    especie %in% names(mapeo_especies),
    mapeo_especies[especie],
    especie
  )
  
  # Filtrar parámetros de la especie
  params <- parametros_cr %>% 
    filter(especie == !!especie_a_buscar)
  
  # Si no existe la especie, usar valor genérico conservador
  if (nrow(params) == 0) {
    warning(sprintf("Especie '%s' no encontrada en parámetros. Usando dh/dd = 0.15 m/cm por defecto.", 
                    especie))
    return(0.15)
  }
  

  # Seleccionar parámetros según dominancia
  if (dominancia %in% c(6)) {
    # Suprimidos (Q1)
    a <- params$a_q1
    b <- params$b_q1
    c <- params$c_q1
  } else if (dominancia %in% c(3, 5)) {
    # Intermedios (Q2)
    a <- params$a_intermedio
    b <- params$b_intermedio
    c <- params$c_intermedio
  } else if (dominancia %in% c(1, 2, 4)) {
    # Dominantes (Q3)
    a <- params$a_q3
    b <- params$b_q3
    c <- params$c_q3
  } else {
    # Default: intermedios
    a <- params$a_intermedio
    b <- params$b_intermedio
    c <- params$c_intermedio
  }
  
  # Calcular dh/dd
  dhdd <- calcular_dhdd_chapman_richards(diametro, a, b, c)
  
  # Si el cálculo falló, usar valor conservador
  if (is.na(dhdd) | dhdd <= 0) {
    warning(sprintf("Cálculo dh/dd falló para '%s' d=%.1f dom=%d. Usando 0.15 m/cm.", 
                    especie, diametro, dominancia))
    return(0.15)
  }
  
  # Limitar a rango razonable (seguridad)
  dhdd <- max(0.05, min(dhdd, 0.50))  # Entre 5 y 50 cm de altura por cm de diámetro
  
  return(dhdd)
}

# ==============================================================================
# 12. CONFIGURACIÓN COMPLETA (LISTA MAESTRA)
# ==============================================================================

crear_configuracion <- function() {
  
  config <- list(
    # Temporal
    periodo = PERIODO_SIMULACION,
    inicio = INICIO_SIMULACION,
    
    # Biológico
    especies = ESPECIES_CONFIG,
    generos = GENEROS_OBJETIVO,
    crecimiento_base = CRECIMIENTO_BASE,
    modificadores_dominancia = MODIFICADORES_DOMINANCIA,
    
    # Mortalidad
    mortalidad_base = MORTALIDAD_BASE,
    semilla_mortalidad = SEMILLA_MORTALIDAD,
    
    # Reclutamiento
    tasa_reclutamiento = TASA_RECLUTAMIENTO,
    reclut_d_min = RECLUTAMIENTO_D_MIN,
    reclut_d_max = RECLUTAMIENTO_D_MAX,
    reclut_dominancia = RECLUTAMIENTO_DOMINANCIA,
    reclut_altura = RECLUTAMIENTO_ALTURA,
    
    # Estructura
    q_factor = Q_FACTOR,
    clases_d = CLASES_DIAMETRICAS,
    
    # Cortas
    dmc = DMC,
    intensidad_corta = INTENSIDAD_CORTA,
    tolerancia = TOLERANCIA_EQUILIBRIO,
    prioridad = PRIORIDAD_CORTA,
    orden_corta = ORDEN_CORTA_RODALES,
    
    # Muestreo
    area_parcela_ha = AREA_PARCELA_HA,
    
    # Modelos de crecimiento en altura (Chapman-Richards)
    parametros_cr = cargar_parametros_cr(),
    interpolar_dhdd = interpolar_dhdd  # Función disponible en CONFIG
  )
  
  return(config)
}

# ==============================================================================
# 13. FUNCIÓN DE VALIDACIÓN
# ==============================================================================

validar_configuracion <- function(config) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║        VALIDACIÓN DE CONFIGURACIÓN DEL SISTEMA            ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  errores <- c()
  
  # Validar periodo
  if (config$periodo <= 0) {
    errores <- c(errores, "❌ Periodo de simulación debe ser > 0")
  } else {
    cat(sprintf("✓ Periodo simulación: %d años\n", config$periodo))
  }
  
  # Validar géneros
  if (length(config$generos) == 0) {
    errores <- c(errores, "❌ Debe haber al menos un género objetivo")
  } else {
    cat(sprintf("✓ Géneros objetivo: %s\n", paste(config$generos, collapse=", ")))
  }
  
  # Validar tasas de crecimiento
  if (any(unlist(config$crecimiento_base) <= 0)) {
    errores <- c(errores, "❌ Tasas de crecimiento deben ser > 0")
  } else {
    cat("✓ Tasas crecimiento diamétrico:\n")
    for (g in names(config$crecimiento_base)) {
      cat(sprintf("  - %s: %.2f cm/año\n", g, config$crecimiento_base[[g]]))
    }
  }
  
  # Validar mortalidad
  if (config$mortalidad_base < 0 | config$mortalidad_base > 1) {
    errores <- c(errores, "❌ Mortalidad base debe estar entre 0 y 1")
  } else {
    cat(sprintf("✓ Mortalidad base: %.2f%%\n", config$mortalidad_base * 100))
  }
  
  # Validar DMC
  if (any(unlist(config$dmc) < 0)) {
    errores <- c(errores, "❌ DMC debe ser ≥ 0")
  } else {
    cat("✓ Diámetro mínimo de corta:\n")
    for (g in names(config$dmc)) {
      cat(sprintf("  - %s: %.0f cm\n", g, config$dmc[[g]]))
    }
  }
  
  # Validar orden de corta
  n_rodales <- nrow(config$orden_corta)
  años_unicos <- n_distinct(config$orden_corta$año_corta)
  
  if (n_rodales != años_unicos) {
    warning(sprintf("⚠️  Hay %d rodales pero %d años únicos de corta", 
                    n_rodales, años_unicos))
  }
  cat(sprintf("✓ Programa de corta: %d rodales en %d años\n", 
              n_rodales, config$periodo))
  
  # Validar modelos de altura
  if (is.null(config$parametros_cr)) {
    errores <- c(errores, "❌ No se cargaron parámetros Chapman-Richards (CRÍTICO)")
  } else {
    cat(sprintf("✓ Parámetros Chapman-Richards: %d especies\n", 
                nrow(config$parametros_cr)))
  }
  
  # Resumen final
  cat("\n")
  if (length(errores) > 0) {
    cat("══════════════════════════════════════════════════════════════\n")
    cat("ERRORES ENCONTRADOS:\n")
    cat("══════════════════════════════════════════════════════════════\n")
    for (e in errores) cat(e, "\n")
    stop("Configuración inválida. Corrige los errores.")
  } else {
    cat("╔════════════════════════════════════════════════════════════╗\n")
    cat("║    ✓ CONFIGURACIÓN VÁLIDA - SISTEMA LISTO                 ║\n")
    cat("╚════════════════════════════════════════════════════════════╝\n\n")
  }
  
  return(TRUE)
}

# ==============================================================================
# 14. FUNCIÓN DE REPORTE DE CONFIGURACIÓN
# ==============================================================================

imprimir_configuracion <- function(config) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║           CONFIGURACIÓN DEL SISTEMA DE SIMULACIÓN         ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  cat("[PARÁMETROS TEMPORALES]\n")
  cat(sprintf("  Periodo:           %d años (%d-%d)\n", 
              config$periodo, config$inicio, config$inicio + config$periodo - 1))
  
  cat("\n[ESPECIES Y GÉNEROS]\n")
  cat(sprintf("  Géneros objetivo:  %s\n", paste(config$generos, collapse=", ")))
  cat(sprintf("  Especies totales:  %d\n", nrow(config$especies)))
  
  cat("\n[CRECIMIENTO DIAMÉTRICO]\n")
  for (g in names(config$crecimiento_base)) {
    cat(sprintf("  %s:             %.2f cm/año\n", g, config$crecimiento_base[[g]]))
  }
  
  cat("\n[MODIFICADORES POR DOMINANCIA]\n")
  print(config$modificadores_dominancia %>% select(nombre_dom, factor_crecimiento, factor_mortalidad))
  
  cat("\n[MORTALIDAD]\n")
  cat(sprintf("  Base anual:        %.2f%%\n", config$mortalidad_base * 100))
  
  cat("\n[RECLUTAMIENTO]\n")
  cat(sprintf("  Tasa anual:        %.1f%% de densidad\n", config$tasa_reclutamiento * 100))
  cat(sprintf("  Rango ingreso:     %.1f - %.1f cm\n", 
              config$reclut_d_min, config$reclut_d_max))
  
  cat("\n[ESTRUCTURA OBJETIVO]\n")
  cat(sprintf("  Factor q:          %.2f (Liocourt)\n", config$q_factor))
  cat(sprintf("  Clases diam:       %d (%.0f-%.0f cm)\n", 
              length(config$clases_d)-1, 
              min(config$clases_d), 
              max(config$clases_d)))
  
  cat("\n[REGLAS DE CORTA]\n")
  for (g in names(config$dmc)) {
    cat(sprintf("  DMC %s:         %.0f cm\n", g, config$dmc[[g]]))
  }
  cat(sprintf("  Intensidad:        %.0f%% del exceso\n", config$intensidad_corta * 100))
  
  cat("\n[PROGRAMA DE CORTA]\n")
  print(config$orden_corta %>% arrange(año_corta))
  
  cat("\n")
}

# ==============================================================================
# 15. INICIALIZACIÓN AUTOMÁTICA
# ==============================================================================

# Crear configuración global al cargar el módulo
CONFIG <- crear_configuracion()

# Validar automáticamente
validar_configuracion(CONFIG)

cat("\n✓ Módulo de configuración cargado exitosamente\n")
cat("  Accede a la configuración con: CONFIG\n")
cat("  Ver resumen completo: imprimir_configuracion(CONFIG)\n\n")