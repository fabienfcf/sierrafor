# ==============================================================================
# REFACTORIZACIÓN RECOMENDADA: 16_calcular_ica.R
# ==============================================================================

# ANTES (líneas 161-215 duplicadas):
calcular_metricas_detalladas_OLD <- function(arboles_df, config) {
  por_rodal <- calcular_metricas_estado(arboles_df, config)

  # DUPLICACIÓN 1: por género
  por_genero <- arboles_vivos %>%
    group_by(rodal, genero_grupo) %>%
    summarise(vol_muestreado_m3 = sum(volumen_m3), ...) %>%
    mutate(vol_ha_m3 = expandir_a_hectarea(...))

  # DUPLICACIÓN 2: por especie
  por_especie <- arboles_vivos %>%
    group_by(rodal, genero_grupo, nombre_cientifico) %>%
    summarise(vol_muestreado_m3 = sum(volumen_m3), ...) %>%
    mutate(vol_ha_m3 = expandir_a_hectarea(...))

  return(list(por_rodal, por_genero, por_especie))
}

# ============================================================================
# DESPUÉS (usando utils_metricas.R): ✅ SIMPLIFICADO
# ============================================================================

calcular_metricas_detalladas <- function(arboles_df, config) {
  list(
    por_rodal = calcular_metricas_estado(arboles_df, config),
    por_genero = calcular_metricas_por_genero(arboles_df, config),
    por_especie = calcular_metricas_por_especie(arboles_df, config)
  )
}

# Reducción: ~50 líneas de código duplicado → 5 líneas
