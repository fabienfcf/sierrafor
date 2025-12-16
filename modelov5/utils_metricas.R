# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# ==============================================================================
# MÓDULO DE UTILIDADES: FUNCIONES DE CÁLCULO DE MÉTRICAS COMPARTIDAS
# Centraliza el cálculo de métricas para evitar duplicación
# VERSIÓN CORREGIDA: Método de densidad global
# ==============================================================================

library(tidyverse)

# Cargar core_calculos si no está disponible
if (!exists("filtrar_arboles_vivos")) {
  source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
}

# ==============================================================================
# MÉTODO CORRECTO: DENSIDAD GLOBAL POR UMM
# ==============================================================================
#
# PROBLEMA ANTERIOR:
#   densidad_ha = n_arboles / (n_sitios_donde_presente × 0.1)
#   → n_sitios diferente para cada especie
#
# SOLUCIÓN:
#   densidad_ha = n_arboles / (n_sitios_TOTALES_UMM × 0.1)
#   → n_sitios = TODOS los sitios del UMM, igual para todas las especies
#
# EJEMPLO:
#   UMM 1: 16 sitios totales
#   Pinus teocote: 9 árboles (presente en solo 3 sitios)
#   Densidad = 9 / (16 × 0.1) = 5.6 árb/ha  ✓ CORRECTO
#   NO: 9 / (3 × 0.1) = 30 árb/ha  ✗ INCORRECTO
#
# ==============================================================================

#' @title Calcular métricas dasométricas con densidad global
#' @description Función genérica que calcula métricas usando n_sitios TOTALES
#' @param arboles_df Data frame de árboles
#' @param agrupar_por Vector de columnas para agrupar (ej: c("rodal", "genero_grupo"))
#' @param config Configuración del sistema
#' @return Data frame con métricas calculadas correctamente
calcular_metricas <- function(arboles_df, agrupar_por = NULL, config = CONFIG) {
  
  vivos <- filtrar_arboles_vivos(arboles_df)
  
  # PASO 1: Obtener n_sitios TOTALES por rodal
  # ============================================
  sitios_por_rodal <- arboles_df %>%
    group_by(rodal) %>%
    summarise(n_sitios_total = n_distinct(muestreo), .groups = "drop")
  
  # PASO 2: Si no hay agrupación, calcular global
  # ==============================================
  if (is.null(agrupar_por) || length(agrupar_por) == 0) {
    n_sitios_global <- n_distinct(arboles_df$muestreo)
    
    metricas <- vivos %>%
      summarise(
        n_sitios = n_sitios_global,
        n_arboles = n(),
        vol_total_m3 = sum(volumen_m3, na.rm = TRUE),
        ab_total_m2 = sum(area_basal, na.rm = TRUE),
        d_medio_cm = mean(diametro_normal, na.rm = TRUE),
        h_media_m = mean(altura_total, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    # PASO 3: Calcular por grupo
    # ===========================
    metricas <- vivos %>%
      group_by(across(all_of(agrupar_por))) %>%
      summarise(
        n_arboles = n(),
        vol_total_m3 = sum(volumen_m3, na.rm = TRUE),
        ab_total_m2 = sum(area_basal, na.rm = TRUE),
        d_medio_cm = mean(diametro_normal, na.rm = TRUE),
        h_media_m = mean(altura_total, na.rm = TRUE),
        .groups = "drop"
      )
    
    # PASO 4: Unir con n_sitios_total del rodal
    # ==========================================
    if ("rodal" %in% agrupar_por) {
      metricas <- metricas %>%
        left_join(sitios_por_rodal, by = "rodal") %>%
        rename(n_sitios = n_sitios_total)
    } else {
      # Si no agrupa por rodal, usar total global
      metricas$n_sitios <- n_distinct(arboles_df$muestreo)
    }
  }
  
  # PASO 5: Calcular métricas /ha con densidad global
  # ==================================================
  metricas <- metricas %>%
    mutate(
      area_total_ha = n_sitios * config$area_parcela_ha,
      
      # DENSIDAD GLOBAL: n_arboles / (n_sitios_TOTALES × 0.1)
      densidad_ha = n_arboles / area_total_ha,
      
      # AB GLOBAL: suma_AB / (n_sitios_TOTALES × 0.1)
      ab_ha_m2 = ab_total_m2 / area_total_ha,
      
      # VOL GLOBAL: suma_Vol / (n_sitios_TOTALES × 0.1)
      vol_ha_m3 = vol_total_m3 / area_total_ha
    )
  
  return(metricas)
}

# ==============================================================================
# WRAPPERS ESPECÍFICOS (para compatibilidad con código existente)
# ==============================================================================

#' @title Calcular métricas de estado por rodal
#' @description Métricas globales por UMM (n_sitios = todos los sitios del UMM)
calcular_metricas_estado <- function(arboles_df, config = CONFIG) {
  calcular_metricas(arboles_df, agrupar_por = "rodal", config = config)
}

#' @title Calcular métricas por género
#' @description Métricas por género y UMM (n_sitios = todos los sitios del UMM)
calcular_metricas_por_genero <- function(arboles_df, config = CONFIG) {
  calcular_metricas(arboles_df, agrupar_por = c("rodal", "genero_grupo"), config = config)
}

#' @title Calcular métricas por especie
#' @description Métricas por especie y UMM (n_sitios = todos los sitios del UMM)
calcular_metricas_por_especie <- function(arboles_df, config = CONFIG) {
  calcular_metricas(arboles_df, agrupar_por = c("rodal", "genero_grupo", "nombre_cientifico"), config = config)
}

#' @title Calcular métricas globales del predio
#' @description Métricas a nivel de todo el predio
calcular_metricas_predio <- function(arboles_df, config = CONFIG) {
  calcular_metricas(arboles_df, agrupar_por = NULL, config = config)
}

# ==============================================================================
# VALIDACIÓN DE MÉTODO
# ==============================================================================

#' Validar que el método de densidad global está funcionando
#'
#' Compara resultados con cálculo manual para verificar corrección
#'
#' @param arboles_df Data frame de árboles
#' @param config Configuración
#' @return Logical indicando si la validación pasó
validar_metodo_densidad_global <- function(arboles_df, config = CONFIG) {
  
  cat("\n[VALIDACIÓN] Método de densidad global...\n")
  
  # Test 1: Verificar que n_sitios es igual para todas las especies de un UMM
  metricas_especie <- calcular_metricas_por_especie(arboles_df, config)
  
  n_sitios_por_umm <- metricas_especie %>%
    group_by(rodal) %>%
    summarise(
      n_sitios_unico = n_distinct(n_sitios),
      n_sitios = first(n_sitios)
    )
  
  if (any(n_sitios_por_umm$n_sitios_unico != 1)) {
    cat("  ✗ ERROR: n_sitios NO es constante por UMM\n")
    return(FALSE)
  }
  
  cat("  ✓ n_sitios es constante por UMM\n")
  
  # Test 2: Verificar cálculo manual
  vivos <- filtrar_arboles_vivos(arboles_df)
  umm_test <- first(metricas_especie$rodal)
  especie_test <- first(metricas_especie$nombre_cientifico)
  
  # Cálculo manual
  n_arboles_manual <- vivos %>%
    filter(rodal == umm_test, nombre_cientifico == especie_test) %>%
    nrow()
  
  n_sitios_manual <- n_distinct(arboles_df %>% filter(rodal == umm_test) %>% pull(muestreo))
  densidad_manual <- n_arboles_manual / (n_sitios_manual * config$area_parcela_ha)
  
  # Cálculo función
  densidad_funcion <- metricas_especie %>%
    filter(rodal == umm_test, nombre_cientifico == especie_test) %>%
    pull(densidad_ha)
  
  diferencia <- abs(densidad_manual - densidad_funcion)
  
  if (diferencia > 0.01) {
    cat(sprintf("  ✗ ERROR: Diferencia entre manual (%.2f) y función (%.2f)\n",
                densidad_manual, densidad_funcion))
    return(FALSE)
  }
  
  cat(sprintf("  ✓ Cálculo correcto: %.2f árb/ha (test en %s, %s)\n",
              densidad_funcion, umm_test, especie_test))
  
  return(TRUE)
}

# ==============================================================================
# MENSAJE DE CARGA
# ==============================================================================

cat("\n✓ Módulo de métricas compartidas cargado (MÉTODO DENSIDAD GLOBAL)\n")
cat("  MÉTODO CORRECTO:\n")
cat("    • n_sitios = TODOS los sitios del UMM (constante para todas las especies)\n")
cat("    • densidad_ha = n_arboles / (n_sitios_TOTALES × 0.1 ha)\n")
cat("    • AB/ha = Σ(AB_individuales) / (n_sitios_TOTALES × 0.1 ha)\n")
cat("    • Vol/ha = Σ(Vol_individuales) / (n_sitios_TOTALES × 0.1 ha)\n\n")
cat("  Funciones disponibles:\n")
cat("    - calcular_metricas(arboles_df, agrupar_por, config)\n")
cat("    - calcular_metricas_estado(arboles_df, config)\n")
cat("    - calcular_metricas_por_genero(arboles_df, config)\n")
cat("    - calcular_metricas_por_especie(arboles_df, config)\n")
cat("    - calcular_metricas_predio(arboles_df, config)\n")
cat("    - validar_metodo_densidad_global(arboles_df, config)\n\n")