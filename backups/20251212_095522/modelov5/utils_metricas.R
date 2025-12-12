# ==============================================================================
# MÓDULO DE UTILIDADES: FUNCIONES DE CÁLCULO DE MÉTRICAS COMPARTIDAS
# Centraliza el cálculo de métricas para evitar duplicación
# ==============================================================================

library(tidyverse)

# Cargar core_calculos si no está disponible
if (!exists("filtrar_arboles_vivos")) {
  source("modelov5/15_core_calculos.R")
}

# ==============================================================================
# FUNCIÓN GENÉRICA PARA CALCULAR MÉTRICAS
# ==============================================================================

#' @title Calcular métricas dasométricas
#' @description Función genérica que calcula métricas con agrupamiento flexible
#' @param arboles_df Data frame de árboles
#' @param agrupar_por Vector de columnas para agrupar (ej: c("rodal"), c("rodal", "genero_grupo"))
#' @param config Configuración del sistema
calcular_metricas <- function(arboles_df, agrupar_por = "rodal", config = CONFIG) {

  vivos <- filtrar_arboles_vivos(arboles_df)

  if (!"num_muestreos_realizados" %in% names(vivos)) {
    stop("❌ ERROR: Columna 'num_muestreos_realizados' no existe en los datos")
  }

  # Agrupar según parámetros
  if (length(agrupar_por) > 0) {
    vivos_agrupados <- vivos %>% group_by(across(all_of(agrupar_por)))
  } else {
    vivos_agrupados <- vivos
  }

  # Calcular métricas base
  metricas <- vivos_agrupados %>%
    summarise(
      # Conteo
      n_vivos = n(),

      # Dimensiones medias
      d_medio_cm = mean(diametro_normal, na.rm = TRUE),
      h_media_m = mean(altura_total, na.rm = TRUE),

      # VALORES MEDIDOS EN PARCELAS (no expandidos)
      vol_muestreado_m3 = sum(volumen_m3, na.rm = TRUE),
      ab_muestreada_m2 = sum(area_basal, na.rm = TRUE),

      # Información de muestreo
      n_muestreos = first(num_muestreos_realizados),
      .groups = "drop"
    ) %>%
    mutate(
      # Calcular área TOTAL muestreada
      area_total_ha = config$area_parcela_ha * n_muestreos,

      # Expandir usando función base
      vol_ha_m3 = expandir_a_hectarea(vol_muestreado_m3, area_total_ha),
      ab_ha_m2 = expandir_a_hectarea(ab_muestreada_m2, area_total_ha),
      densidad_ha = expandir_a_hectarea(n_vivos, area_total_ha)
    )

  return(metricas)
}

# ==============================================================================
# WRAPPERS ESPECÍFICOS (para compatibilidad con código existente)
# ==============================================================================

#' @title Calcular métricas de estado por rodal
calcular_metricas_estado <- function(arboles_df, config = CONFIG) {
  calcular_metricas(arboles_df, agrupar_por = "rodal", config = config)
}

#' @title Calcular métricas por género
calcular_metricas_por_genero <- function(arboles_df, config = CONFIG) {
  calcular_metricas(arboles_df, agrupar_por = c("rodal", "genero_grupo"), config = config)
}

#' @title Calcular métricas por especie
calcular_metricas_por_especie <- function(arboles_df, config = CONFIG) {
  calcular_metricas(arboles_df, agrupar_por = c("rodal", "genero_grupo", "nombre_cientifico"), config = config)
}

# ==============================================================================
# MENSAJE DE CARGA
# ==============================================================================

cat("\n✓ Módulo de métricas compartidas cargado\n")
cat("  Funciones disponibles:\n")
cat("    - calcular_metricas(arboles_df, agrupar_por, config)\n")
cat("    - calcular_metricas_estado(arboles_df, config)\n")
cat("    - calcular_metricas_por_genero(arboles_df, config)\n")
cat("    - calcular_metricas_por_especie(arboles_df, config)\n\n")
