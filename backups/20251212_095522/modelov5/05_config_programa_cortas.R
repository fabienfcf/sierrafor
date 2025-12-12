# ==============================================================================
# 05_CONFIG_PROGRAMA_CORTAS.R - CORREGIDO
# ==============================================================================

library(tidyverse)

cat("\n[4/4] Cargando programa de cortas...\n")

# ==============================================================================
# 1. DIÁMETROS MÍNIMOS DE CORTA (DMC) por género
# ==============================================================================

DMC <- list(
  "Pinus" = 30,
  "Quercus" = 25
)

# ==============================================================================
# 2. PARÁMETROS DEL MÉTODO LIOCOURT
# ==============================================================================

Q_FACTOR <- 1.7
TOLERANCIA_EQUILIBRIO <- 20

# ==============================================================================
# 3. PROGRAMA DE CORTAS - ✅ CON COLUMNA excluir_semilleros
# ==============================================================================

PROGRAMA_CORTAS <- tribble(
  ~rodal, ~año_corta, ~metodo, ~intensidad_pct, ~d_min, ~d_max, ~prioridad, ~excluir_semilleros,
  #-------|-----------|---------|----------------|--------|--------|------------|--------------------
  1,      1,          "ICA",   80,               NULL,   NULL,   "suprimidos", TRUE,
  2,      3,          "ICA",   80,               NULL,   NULL,   "suprimidos", TRUE,
  3,      5,          "ICA",   80,               NULL,   NULL,   "suprimidos", TRUE,
  4,      7,          "ICA",   80,               NULL,   NULL,   "suprimidos", TRUE,
  5,      9,          "ICA",   80,               NULL,   NULL,   "suprimidos", TRUE,
  6,      10,          "ICA",   80,               NULL,   NULL,   "suprimidos", TRUE
  )

# ==============================================================================
# 4. FUNCIÓN HELPER
# ==============================================================================

configurar_corte <- function(metodo = "ICA",
                             intensidad_pct = 80,
                             d_min = NULL,
                             d_max = NULL,
                             prioridad = "suprimidos",
                             excluir_semilleros = TRUE,
                             q_factor = Q_FACTOR,
                             tolerancia_equilibrio = TOLERANCIA_EQUILIBRIO,
                             años_ica = 10) {
  
  if (!metodo %in% c("ICA", "EXISTENCIAS", "LIOCOURT")) {
    stop(sprintf("Método '%s' no válido. Usa: ICA o EXISTENCIAS", metodo))
  }
  
  if (intensidad_pct <= 0 || intensidad_pct > 100) {
    stop("intensidad_pct debe estar entre 0 y 100")
  }
  
  if (!prioridad %in% c("suprimidos", "dominantes", "intermedios")) {
    stop("prioridad debe ser: suprimidos, dominantes, o intermedios")
  }
  
  config <- list(
    metodo = metodo,
    intensidad_pct = intensidad_pct,
    d_min = d_min,
    d_max = d_max,
    prioridad = prioridad,
    excluir_semilleros = excluir_semilleros,
    años_ica = años_ica,
    q_factor = q_factor,
    tolerancia = tolerancia_equilibrio
  )
  
  return(config)
}

# ==============================================================================
# 5. VALIDACIÓN
# ==============================================================================

validar_programa_cortas <- function(programa, config = CONFIG) {
  
  cat("\n[VALIDACIÓN PROGRAMA DE CORTAS]\n")
  cat("═══════════════════════════════════════════════════════════\n")
  
  errores <- c()
  
  # Verificar columnas requeridas
  cols_requeridas <- c("rodal", "año_corta", "metodo", "intensidad_pct", 
                       "prioridad", "excluir_semilleros")
  cols_faltantes <- setdiff(cols_requeridas, names(programa))
  
  if (length(cols_faltantes) > 0) {
    errores <- c(errores, sprintf("Columnas faltantes: %s", 
                                  paste(cols_faltantes, collapse=", ")))
  }
  
  # Verificar métodos válidos
  metodos_invalidos <- setdiff(unique(programa$metodo), 
                               c("ICA", "EXISTENCIAS", "LIOCOURT"))
  if (length(metodos_invalidos) > 0) {
    errores <- c(errores, sprintf("Métodos inválidos: %s", 
                                  paste(metodos_invalidos, collapse=", ")))
  }
  
  # Verificar intensidades
  if (any(programa$intensidad_pct <= 0 | programa$intensidad_pct > 100)) {
    errores <- c(errores, "Intensidades deben estar entre 0 y 100")
  }
  
  if (length(errores) > 0) {
    cat("\n❌ ERRORES:\n")
    for (e in errores) cat(sprintf("  • %s\n", e))
    stop("Programa de cortas inválido")
  }
  
  cat("\n✓ Validación exitosa\n")
  cat(sprintf("  Rodales programados: %d\n", length(unique(programa$rodal))))
  cat(sprintf("  Años con cortas: %s\n\n", 
              paste(sort(unique(programa$año_corta)), collapse=", ")))
  
  return(TRUE)
}

# ==============================================================================
# RESUMEN
# ==============================================================================

cat(sprintf("  ✓ DMC definido para %d géneros\n", length(DMC)))
for (g in names(DMC)) {
  cat(sprintf("    • %s: %d cm\n", g, DMC[[g]]))
}
cat(sprintf("  ✓ Q_FACTOR: %.1f\n", Q_FACTOR))
cat(sprintf("  ✓ Tolerancia: ±%d%%\n", TOLERANCIA_EQUILIBRIO))
cat(sprintf("  ✓ Rodales programados: %d\n", nrow(PROGRAMA_CORTAS)))
cat(sprintf("  ✓ Columnas: %s\n\n", paste(names(PROGRAMA_CORTAS), collapse=", ")))