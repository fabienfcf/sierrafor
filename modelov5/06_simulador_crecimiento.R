# ==============================================================================
# PARCHE 2: VERSIÓN CORREGIDA DE 06_simulador_crecimiento.R
# Sección de métricas corregida para usar num_muestreos_realizados
# ==============================================================================

# INSTRUCCIONES:
# 1. Guardar este archivo como: modelov5/06_simulador_crecimiento_CORREGIDO.R
# 2. Revisar los cambios marcados con # ✓ CORREGIDO
# 3. Si todo funciona, reemplazar el original

library(tidyverse)

# Cargar funciones puras
source("modelov5/01_parametros_configuracion.R")
source("modelov5/core_calculos.R")

# ==============================================================================
# 1. CALCULAR MÉTRICAS DE ESTADO (NOMENCLATURA CLARA)
# ==============================================================================

#' @title Calcular métricas básicas del estado actual
#' @description Nomenclatura clara: vol_muestreado vs vol_ha
calcular_metricas_estado <- function(arboles_df, config = CONFIG) {
  
  vivos <- filtrar_arboles_vivos(arboles_df)
  
  if (!"num_muestreos_realizados" %in% names(vivos)) {
    stop("❌ ERROR: Columna 'num_muestreos_realizados' no existe en los datos\n",
         "   Ejecuta primero: PARCHE 1 (añadir num_muestreos_realizados)")
  }
  
  metricas <- vivos %>%
    group_by(rodal) %>%
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
      # Cálculo de expansión
      area_muestreada_ha = config$area_parcela_ha * n_muestreos,
      factor_expansion = 1 / area_muestreada_ha,
      
      # VALORES POR HECTÁREA (expandidos)
      vol_ha_m3 = vol_muestreado_m3 * factor_expansion,
      ab_ha_m2 = ab_muestreada_m2 * factor_expansion,
      densidad_ha = n_vivos * factor_expansion
    )
  
  return(metricas)
}

# ==============================================================================
# 2. CALCULAR MÉTRICAS POR GÉNERO
# ==============================================================================

calcular_metricas_por_genero <- function(arboles_df, config = CONFIG) {
  
  vivos <- filtrar_arboles_vivos(arboles_df)
  
  if (!"num_muestreos_realizados" %in% names(vivos)) {
    stop("❌ ERROR: Columna 'num_muestreos_realizados' no existe")
  }
  
  metricas <- vivos %>%
    group_by(rodal, genero_grupo) %>%
    summarise(
      n_vivos = n(),
      d_medio_cm = mean(diametro_normal, na.rm = TRUE),
      h_media_m = mean(altura_total, na.rm = TRUE),
      
      # MEDIDO en parcelas
      vol_muestreado_m3 = sum(volumen_m3, na.rm = TRUE),
      ab_muestreada_m2 = sum(area_basal, na.rm = TRUE),
      
      n_muestreos = first(num_muestreos_realizados),
      .groups = "drop"
    ) %>%
    mutate(
      area_muestreada_ha = config$area_parcela_ha * n_muestreos,
      factor_expansion = 1 / area_muestreada_ha,
      
      # EXPANDIDO por hectárea
      vol_ha_m3 = vol_muestreado_m3 * factor_expansion,
      ab_ha_m2 = ab_muestreada_m2 * factor_expansion,
      densidad_ha = n_vivos * factor_expansion
    )
  
  return(metricas)
}

# ==============================================================================
# 3. CALCULAR MÉTRICAS POR ESPECIE
# ==============================================================================

calcular_metricas_por_especie <- function(arboles_df, config = CONFIG) {
  
  vivos <- filtrar_arboles_vivos(arboles_df)
  
  if (!"num_muestreos_realizados" %in% names(vivos)) {
    stop("❌ ERROR: Columna 'num_muestreos_realizados' no existe")
  }
  
  metricas <- vivos %>%
    group_by(rodal, genero_grupo, nombre_cientifico) %>%
    summarise(
      n_vivos = n(),
      d_medio_cm = mean(diametro_normal, na.rm = TRUE),
      h_media_m = mean(altura_total, na.rm = TRUE),
      
      # MEDIDO
      vol_muestreado_m3 = sum(volumen_m3, na.rm = TRUE),
      ab_muestreada_m2 = sum(area_basal, na.rm = TRUE),
      
      n_muestreos = first(num_muestreos_realizados),
      .groups = "drop"
    ) %>%
    mutate(
      area_muestreada_ha = config$area_parcela_ha * n_muestreos,
      factor_expansion = 1 / area_muestreada_ha,
      
      # EXPANDIDO
      vol_ha_m3 = vol_muestreado_m3 * factor_expansion,
      ab_ha_m2 = ab_muestreada_m2 * factor_expansion,
      densidad_ha = n_vivos * factor_expansion
    )
  
  return(metricas)
}

# ==============================================================================
# 4. ACTUALIZAR VOLÚMENES
# ==============================================================================

actualizar_volumenes <- function(arboles_df) {
  
  cat("\n[VOLUMEN] Recalculando con ecuaciones alométricas...\n")
  
  arboles_actualizado <- arboles_df %>%
    mutate(area_basal = calcular_area_basal(diametro_normal))
  
  arboles_actualizado <- calcular_volumenes_vectorizado(arboles_actualizado)
  
  arboles_actualizado <- arboles_actualizado %>%
    mutate(volumen_m3 = if_else(dominancia %in% c(7, 8, 9), 0, volumen_m3))
  
  vol_total <- sum(arboles_actualizado$volumen_m3, na.rm = TRUE)
  vivos <- filtrar_arboles_vivos(arboles_actualizado)
  vol_vivos <- sum(vivos$volumen_m3, na.rm = TRUE)
  
  cat(sprintf("  Volumen total: %.2f m³\n", vol_total))
  cat(sprintf("  Volumen vivos: %.2f m³\n", vol_vivos))
  
  return(arboles_actualizado)
}

# ==============================================================================
# 5. SIMULADOR PRINCIPAL
# ==============================================================================

simular_crecimiento_rodal <- function(arboles_inicial, config, años = NULL) {
  
  if (is.null(años)) {
    años <- config$periodo
  }
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat(sprintf("║     SIMULACIÓN DE CRECIMIENTO - %d AÑOS                   ║\n", años))
  cat("╚════════════════════════════════════════════════════════════╝\n")
  
  arboles_actual <- arboles_inicial
  historial <- list()
  historial_metricas <- list()
  
  historial[[1]] <- arboles_actual %>% mutate(año_simulacion = 0)
  historial_metricas[[1]] <- calcular_metricas_estado(arboles_actual) %>% 
    mutate(año_simulacion = 0)
  
  for (año in 1:años) {
    
    cat(sprintf("\n════════════════ AÑO %d ════════════════\n", año))
    
    arboles_actual <- aplicar_crecimiento_poblacion(arboles_actual, config, año)
    arboles_actual <- actualizar_volumenes(arboles_actual)
    arboles_actual <- aplicar_mortalidad_poblacion(arboles_actual, config, año)
    arboles_actual <- aplicar_reclutamiento(arboles_actual, config, año)
    
    historial[[año + 1]] <- arboles_actual %>% mutate(año_simulacion = año)
    historial_metricas[[año + 1]] <- calcular_metricas_estado(arboles_actual) %>%
      mutate(año_simulacion = año)
  }
  
  historial_completo <- bind_rows(historial)
  historial_metricas_completo <- bind_rows(historial_metricas)
  
  cat("\n✓ Simulación completada\n")
  
  return(list(
    poblacion_inicial = arboles_inicial,
    poblacion_final = arboles_actual,
    historial = historial_completo,
    historial_metricas = historial_metricas_completo,
    años_simulados = años
  ))
}

# ==============================================================================
# 6. COMPARAR ESTADOS (NOMENCLATURA ACTUALIZADA)
# ==============================================================================

comparar_estados <- function(resultado_simulacion) {
  
  cat("\n╔═══════════════════════════════════════════════════════════╗\n")
  cat("║         COMPARACIÓN ESTADO INICIAL vs FINAL               ║\n")
  cat("╚═══════════════════════════════════════════════════════════╝\n\n")
  
  inicial <- resultado_simulacion$poblacion_inicial
  final <- resultado_simulacion$poblacion_final
  años <- resultado_simulacion$años_simulados
  
  metricas_inicial <- calcular_metricas_estado(inicial)
  metricas_final <- calcular_metricas_estado(final)
  
  comparacion <- metricas_inicial %>%
    select(rodal, 
           n_vivos_ini = n_vivos,
           vol_muestreado_ini = vol_muestreado_m3,
           vol_ha_ini = vol_ha_m3,
           d_medio_ini = d_medio_cm,
           h_media_ini = h_media_m) %>%
    left_join(
      metricas_final %>%
        select(rodal,
               n_vivos_fin = n_vivos,
               vol_muestreado_fin = vol_muestreado_m3,
               vol_ha_fin = vol_ha_m3,
               d_medio_fin = d_medio_cm,
               h_media_fin = h_media_m),
      by = "rodal"
    ) %>%
    mutate(
      delta_n = n_vivos_fin - n_vivos_ini,
      delta_vol_muestreado = vol_muestreado_fin - vol_muestreado_ini,
      delta_vol_ha = vol_ha_fin - vol_ha_ini,
      delta_d = d_medio_fin - d_medio_ini,
      delta_h = h_media_fin - h_media_ini,
      
      cambio_n_pct = (delta_n / n_vivos_ini) * 100,
      cambio_vol_pct = (delta_vol_muestreado / vol_muestreado_ini) * 100,
      
      ima_vol_muestreado = delta_vol_muestreado / años,
      ima_vol_ha = delta_vol_ha / años,
      ima_d = delta_d / años,
      ima_h = delta_h / años
    )
  
  # Reporte
  cat("[POBLACIÓN]\n")
  for (i in 1:nrow(comparacion)) {
    r <- comparacion[i,]
    cat(sprintf("  Rodal %d: %d → %d árboles (%+d, %+.1f%%)\n",
                r$rodal, r$n_vivos_ini, r$n_vivos_fin, 
                r$delta_n, r$cambio_n_pct))
  }
  
  cat("\n[VOLUMEN MEDIDO EN PARCELAS]\n")
  for (i in 1:nrow(comparacion)) {
    r <- comparacion[i,]
    cat(sprintf("  Rodal %d: %.2f → %.2f m³ (%+.2f, %+.1f%%) | IMA: %.2f m³/año\n",
                r$rodal, r$vol_muestreado_ini, r$vol_muestreado_fin,
                r$delta_vol_muestreado, r$cambio_vol_pct, r$ima_vol_muestreado))
  }
  
  cat("\n[VOLUMEN POR HECTÁREA]\n")
  for (i in 1:nrow(comparacion)) {
    r <- comparacion[i,]
    cat(sprintf("  Rodal %d: %.2f → %.2f m³/ha | IMA: %.2f m³/ha/año\n",
                r$rodal, r$vol_ha_ini, r$vol_ha_fin, r$ima_vol_ha))
  }
  
  cat("\n[DIMENSIONES]\n")
  for (i in 1:nrow(comparacion)) {
    r <- comparacion[i,]
    cat(sprintf("  Rodal %d: Ø %.1f → %.1f cm (%+.1f) | h %.1f → %.1f m (%+.1f)\n",
                r$rodal, r$d_medio_ini, r$d_medio_fin, r$delta_d,
                r$h_media_ini, r$h_media_fin, r$delta_h))
  }
  
  cat("\n")
  
  return(comparacion)
}

# ==============================================================================
# 7. COMPARAR POR GÉNERO
# ==============================================================================

comparar_estados_por_genero <- function(resultado_simulacion) {
  
  cat("\n╔═══════════════════════════════════════════════════════════╗\n")
  cat("║         COMPARACIÓN POR GÉNERO - INICIAL vs FINAL         ║\n")
  cat("╚═══════════════════════════════════════════════════════════╝\n\n")
  
  inicial <- resultado_simulacion$poblacion_inicial
  final <- resultado_simulacion$poblacion_final
  años <- resultado_simulacion$años_simulados
  
  metricas_inicial <- calcular_metricas_por_genero(inicial)
  metricas_final <- calcular_metricas_por_genero(final)
  
  comparacion <- metricas_inicial %>%
    select(rodal, genero_grupo,
           n_vivos_ini = n_vivos,
           vol_muestreado_ini = vol_muestreado_m3,
           vol_ha_ini = vol_ha_m3,
           d_medio_ini = d_medio_cm) %>%
    left_join(
      metricas_final %>%
        select(rodal, genero_grupo,
               n_vivos_fin = n_vivos,
               vol_muestreado_fin = vol_muestreado_m3,
               vol_ha_fin = vol_ha_m3,
               d_medio_fin = d_medio_cm),
      by = c("rodal", "genero_grupo")
    ) %>%
    mutate(
      delta_n = n_vivos_fin - n_vivos_ini,
      delta_vol_muestreado = vol_muestreado_fin - vol_muestreado_ini,
      delta_vol_ha = vol_ha_fin - vol_ha_ini,
      delta_d = d_medio_fin - d_medio_ini,
      
      cambio_n_pct = (delta_n / n_vivos_ini) * 100,
      cambio_vol_pct = (delta_vol_muestreado / vol_muestreado_ini) * 100,
      
      ima_vol_muestreado = delta_vol_muestreado / años,
      ima_vol_ha = delta_vol_ha / años
    )
  
  # Reporte por rodal y género
  for (rodal_id in unique(comparacion$rodal)) {
    cat(sprintf("\n═══ RODAL %d ═══\n\n", rodal_id))
    
    datos_rodal <- comparacion %>% filter(rodal == rodal_id)
    
    for (gen in unique(datos_rodal$genero_grupo)) {
      r <- datos_rodal %>% filter(genero_grupo == gen)
      
      cat(sprintf("[%s]\n", gen))
      cat(sprintf("  Población:      %d → %d árboles (%+d, %+.1f%%)\n",
                  r$n_vivos_ini, r$n_vivos_fin, r$delta_n, r$cambio_n_pct))
      cat(sprintf("  Vol. muestreado: %.2f → %.2f m³ (%+.2f, %+.1f%%) | IMA: %.2f m³/año\n",
                  r$vol_muestreado_ini, r$vol_muestreado_fin, 
                  r$delta_vol_muestreado, r$cambio_vol_pct, r$ima_vol_muestreado))
      cat(sprintf("  Vol/ha:         %.2f → %.2f m³/ha | IMA: %.2f m³/ha/año\n",
                  r$vol_ha_ini, r$vol_ha_fin, r$ima_vol_ha))
      cat(sprintf("  Ø medio:        %.1f → %.1f cm (%+.1f)\n\n",
                  r$d_medio_ini, r$d_medio_fin, r$delta_d))
    }
  }
  
  # Resumen general
  cat("╔═══════════════════════════════════════════════════════════╗\n")
  cat("║              RESUMEN GENERAL POR GÉNERO                   ║\n")
  cat("╚═══════════════════════════════════════════════════════════╝\n\n")
  
  resumen_genero <- comparacion %>%
    group_by(genero_grupo) %>%
    summarise(
      n_rodales = n(),
      vol_muestreado_ini = sum(vol_muestreado_ini),
      vol_muestreado_fin = sum(vol_muestreado_fin),
      vol_ha_medio_ini = mean(vol_ha_ini),
      vol_ha_medio_fin = mean(vol_ha_fin),
      .groups = "drop"
    ) %>%
    mutate(
      cambio_vol = vol_muestreado_fin - vol_muestreado_ini,
      cambio_pct = (cambio_vol / vol_muestreado_ini) * 100
    )
  
  print(resumen_genero)
  
  cat("\n")
  
  return(comparacion)
}

# ==============================================================================
# 8. VISUALIZACIÓN
# ==============================================================================

graficar_evolucion_temporal <- function(resultado_simulacion) {
  
  if (!"historial_metricas" %in% names(resultado_simulacion)) {
    warning("No hay historial de métricas disponible")
    return(NULL)
  }
  
  resumen_anual <- resultado_simulacion$historial_metricas %>%
    group_by(año_simulacion) %>%
    summarise(
      n_vivos_total = sum(n_vivos),
      vol_muestreado_m3 = sum(vol_muestreado_m3),
      vol_medio_ha = mean(vol_ha_m3),
      d_medio_cm = mean(d_medio_cm, na.rm = TRUE),
      h_media_m = mean(h_media_m, na.rm = TRUE),
      .groups = "drop"
    )
  
  library(patchwork)
  
  p1 <- ggplot(resumen_anual, aes(x = año_simulacion, y = vol_medio_ha)) +
    geom_area(fill = "#06A77D", alpha = 0.3) +
    geom_line(color = "#06A77D", linewidth = 1.2) +
    geom_point(color = "#06A77D", size = 2.5) +
    labs(
      title = "Volumen Promedio por Hectárea",
      x = "Año de Simulación",
      y = "Volumen (m³/ha)"
    ) +
    theme_minimal(base_size = 11)
  
  p2 <- ggplot(resumen_anual, aes(x = año_simulacion, y = n_vivos_total)) +
    geom_line(color = "#2E86AB", linewidth = 1.2) +
    geom_point(color = "#2E86AB", size = 2.5) +
    labs(
      title = "Población Total",
      x = "Año de Simulación",
      y = "Número de Árboles Vivos"
    ) +
    theme_minimal(base_size = 11)
  
  p3 <- ggplot(resumen_anual, aes(x = año_simulacion, y = d_medio_cm)) +
    geom_line(color = "#F18F01", linewidth = 1.2) +
    geom_point(color = "#F18F01", size = 2.5) +
    labs(
      title = "Diámetro Medio",
      x = "Año de Simulación",
      y = "Diámetro (cm)"
    ) +
    theme_minimal(base_size = 11)
  
  p_combined <- (p1 / p2 / p3) +
    plot_annotation(
      title = "Simulación de Crecimiento Forestal",
      subtitle = sprintf("%d años de proyección", max(resumen_anual$año_simulacion))
    )
  
  return(p_combined)
}

# ==============================================================================
# 9. VALIDACIÓN
# ==============================================================================

validar_crecimiento <- function(arboles_antes, arboles_despues) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║           VALIDACIÓN DE CRECIMIENTO APLICADO              ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  if (nrow(arboles_antes) != nrow(arboles_despues)) {
    warning("⚠ Número de árboles cambió durante crecimiento!")
  }
  
  vivos_antes <- filtrar_arboles_vivos(arboles_antes)
  vivos_despues <- filtrar_arboles_vivos(arboles_despues)
  
  diametros_decrecieron <- sum(
    vivos_despues$diametro_normal < vivos_antes$diametro_normal,
    na.rm = TRUE
  )
  
  if (diametros_decrecieron > 0) {
    warning(sprintf("⚠ %d árboles con diámetro decreciente!", diametros_decrecieron))
  } else {
    cat("✓ Todos los diámetros aumentaron correctamente\n")
  }
  
  incrementos <- vivos_despues %>%
    summarise(
      delta_d_min = min(incremento_d_cm, na.rm = TRUE),
      delta_d_max = max(incremento_d_cm, na.rm = TRUE),
      delta_h_min = min(incremento_h_m, na.rm = TRUE),
      delta_h_max = max(incremento_h_m, na.rm = TRUE)
    )
  
  cat(sprintf("✓ Rango incremento diámetro: [%.3f - %.3f] cm\n", 
              incrementos$delta_d_min, incrementos$delta_d_max))
  cat(sprintf("✓ Rango incremento altura:   [%.3f - %.3f] m\n", 
              incrementos$delta_h_min, incrementos$delta_h_max))
  
  if (incrementos$delta_d_max > 1.0) {
    warning("⚠ Incremento diamétrico muy alto (>1 cm/año)")
  }
  
  if (incrementos$delta_h_max > 0.8) {
    warning("⚠ Incremento altura muy alto (>0.8 m/año)")
  }
  
  cat("\n✓ Validación completada\n\n")
  
  return(TRUE)
}

# ==============================================================================
# MENSAJE DE CARGA
# ==============================================================================

cat("\n✓ 06_simulador_crecimiento.R (v2.2 - NOMENCLATURA CLARA) cargado\n")
cat("  CAMBIO IMPORTANTE:\n")
cat("    vol_total_m3 → vol_muestreado_m3 (valor medido en parcelas)\n")
cat("    vol_ha_m3 → vol_ha_m3 (valor expandido por hectárea)\n\n")
cat("  Funciones disponibles:\n")
cat("    - simular_crecimiento_rodal(arboles, config, años)\n")
cat("    - calcular_metricas_estado(arboles_df)\n")
cat("    - calcular_metricas_por_genero(arboles_df)\n")
cat("    - calcular_metricas_por_especie(arboles_df)\n")
cat("    - comparar_estados(resultado)\n")
cat("    - comparar_estados_por_genero(resultado)\n")
cat("    - graficar_evolucion_temporal(resultado)\n\n")