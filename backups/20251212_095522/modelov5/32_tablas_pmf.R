# ==============================================================================
# TABLAS PARA PMF - SECCIONES 7.1, 7.2, 7.3
# ==============================================================================

library(tidyverse)
library(xtable)

# Función para formatear números con 3 cifras significativas
formatear_3_significativas <- function(df) {
  df %>%
    mutate(across(where(is.numeric), ~ {
      # Primero reemplazar NAs
      x <- ifelse(is.na(.) | . == 0, 0, .)
      
      # Aplicar 3 cifras significativas
      formatted <- signif(x, 3)
      
      # Formatear adecuadamente según el tipo de número
      ifelse(formatted == round(formatted), 
             as.character(round(formatted)),  # Enteros sin decimales
             # Para decimales, determinar cuántos dígitos mostrar
             ifelse(abs(formatted) >= 100, sprintf("%.0f", formatted),
                    ifelse(abs(formatted) >= 10, sprintf("%.1f", formatted),
                           ifelse(abs(formatted) >= 1, sprintf("%.2f", formatted),
                                  sprintf("%.3f", formatted)))))
    }))
}

# ==============================================================================
# TABLA 5: EXISTENCIAS REALES Y VOLUMEN DE APROVECHAMIENTO
# ==============================================================================

tabla_5_existencias_aprovechamiento <- function(arboles_df, config = CONFIG, 
                                                vol_aprovechamiento = NULL) {
  
  cat("\n[TABLA 5] Existencias reales y volumen de aprovechamiento...\n")
  
  # Filtrar vivos
  vivos <- arboles_df %>% filter(!dominancia %in% c(7, 8, 9))
  
  # Calcular métricas por rodal y especie principal (Pinus/Quercus)
  por_rodal_especie <- vivos %>%
    filter(genero_grupo %in% c("Pinus", "Quercus")) %>%
    group_by(rodal, genero = genero_grupo) %>%
    summarise(
      n_arboles = n(),
      vol_muestreado_m3 = sum(volumen_m3, na.rm = TRUE),
      ab_muestreada_m2 = sum(area_basal, na.rm = TRUE),
      d_medio = mean(diametro_normal, na.rm = TRUE),
      n_muestreos = first(num_muestreos_realizados),
      superficie_ha = first(superficie_ha),
      .groups = "drop"
    ) %>%
    mutate(
      area_total_ha = config$area_parcela_ha * n_muestreos,
      
      # Expandir a hectárea
      vol_ha_m3 = expandir_a_hectarea(vol_muestreado_m3, area_total_ha),
      ab_ha_m2 = expandir_a_hectarea(ab_muestreada_m2, area_total_ha),
      densidad_ha = expandir_a_hectarea(n_arboles, area_total_ha),
      
      # Volumen total por rodal
      vol_total_rodal_m3 = vol_ha_m3 * superficie_ha,
      
      # Calcular DMC (Diámetro Medio Cuadrático)
      dmc_cm = sqrt((ab_ha_m2 * 10000) / (pi/4) / densidad_ha)
    )
  
  # Crear datos separados para Quercus y Pinus
  quercus_data <- por_rodal_especie %>%
    filter(genero == "Quercus") %>%
    select(rodal,
           N_quercus = n_arboles,
           D_medio_quercus = d_medio,
           AB_quercus = ab_ha_m2,
           Vol_ha_quercus = vol_ha_m3,
           Vol_rodal_quercus = vol_total_rodal_m3)
  
  pinus_data <- por_rodal_especie %>%
    filter(genero == "Pinus") %>%
    select(rodal,
           N_pinus = n_arboles,
           D_medio_pinus = d_medio,
           AB_pinus = ab_ha_m2,
           Vol_ha_pinus = vol_ha_m3,
           Vol_rodal_pinus = vol_total_rodal_m3)
  
  # Resumen por rodal (total)
  por_rodal_total <- vivos %>%
    group_by(rodal) %>%
    summarise(
      n_arboles_total = n(),
      vol_muestreado_m3 = sum(volumen_m3, na.rm = TRUE),
      ab_muestreada_m2 = sum(area_basal, na.rm = TRUE),
      n_muestreos = first(num_muestreos_realizados),
      superficie_ha = first(superficie_ha),
      .groups = "drop"
    ) %>%
    mutate(
      area_total_ha = config$area_parcela_ha * n_muestreos,
      vol_ha_m3 = expandir_a_hectarea(vol_muestreado_m3, area_total_ha),
      ab_ha_m2 = expandir_a_hectarea(ab_muestreada_m2, area_total_ha),
      densidad_ha = expandir_a_hectarea(n_arboles_total, area_total_ha),
      vol_total_rodal_m3 = vol_ha_m3 * superficie_ha,
      dmc_cm = sqrt((ab_ha_m2 * 10000) / (pi/4) / densidad_ha)
    )
  
  # Unir todo en el orden solicitado
  tabla_5 <- por_rodal_total %>%
    left_join(quercus_data, by = "rodal") %>%
    left_join(pinus_data, by = "rodal") %>%
    mutate(UMM = as.character(rodal)) %>%
    select(
      UMM,
      `Superficie (ha)` = superficie_ha,
      # Quercus
      `N Quercus` = N_quercus,
      `D Medio Quercus (cm)` = D_medio_quercus,
      `AB Quercus (m²/ha)` = AB_quercus,
      `ER Quercus (m³/ha)` = Vol_ha_quercus,
      `ER Quercus (m³)` = Vol_rodal_quercus,
      
      # Pinus
      `N Pinus` = N_pinus,
      `D Medio Pinus (cm)` = D_medio_pinus,
      `AB Pinus (m²/ha)` = AB_pinus,
      `ER Pinus (m³/ha)` = Vol_ha_pinus,
      `ER Pinus (m³)` = Vol_rodal_pinus,
      
      # Volúmenes por rodal
      `ER Rodal (m³)` = vol_total_rodal_m3
    )
  
  # Reemplazar NA por 0 en columnas numéricas
  tabla_5 <- tabla_5 %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0)))
  
  # Si hay volumen de aprovechamiento, agregarlo
  if (!is.null(vol_aprovechamiento)) {
    tabla_5 <- tabla_5 %>%
      left_join(
        vol_aprovechamiento %>%
          select(UMM = rodal, `Vol Aprovechamiento (m³)` = vol_aprovechado),
        by = "UMM"
      ) %>%
      mutate(
        `Vol Residual (m³/ha)` = `ER Total (m³/ha)` - 
          (`Vol Aprovechamiento (m³)` / `Superficie (ha)`)
      )
  }
  
  # Agregar totales (antes del formateo para mantener precisión en cálculos)
  totales <- tabla_5 %>%
    summarise(
      UMM = "TOTAL",
      `Superficie (ha)` = sum(`Superficie (ha)`, na.rm = TRUE),
      `N Quercus` = sum(`N Quercus`, na.rm = TRUE),
      `D Medio Quercus (cm)` = mean(`D Medio Quercus (cm)`, na.rm = TRUE),
      `AB Quercus (m²/ha)` = mean(`AB Quercus (m²/ha)`, na.rm = TRUE),
      `ER Quercus (m³/ha)` = mean(`ER Quercus (m³/ha)`, na.rm = TRUE),
      `ER Quercus (m³)` = sum(`ER Quercus (m³)`, na.rm = TRUE),
      `N Pinus` = sum(`N Pinus`, na.rm = TRUE),
      `D Medio Pinus (cm)` = mean(`D Medio Pinus (cm)`, na.rm = TRUE),
      `AB Pinus (m²/ha)` = mean(`AB Pinus (m²/ha)`, na.rm = TRUE),
      `ER Pinus (m³/ha)` = mean(`ER Pinus (m³/ha)`, na.rm = TRUE),
      `ER Pinus (m³)` = sum(`ER Pinus (m³)`, na.rm = TRUE),
      `ER Rodal (m³)` = sum(`ER Rodal (m³)`, na.rm = TRUE)
    )
  
  if ("Vol Aprovechamiento (m³)" %in% names(tabla_5)) {
    totales <- totales %>%
      mutate(
        `Vol Aprovechamiento (m³)` = sum(tabla_5$`Vol Aprovechamiento (m³)`, na.rm = TRUE),
        `Vol Residual (m³/ha)` = mean(tabla_5$`Vol Residual (m³/ha)`, na.rm = TRUE)
      )
  }
  
  tabla_5 <- bind_rows(tabla_5, totales)
  
  # APLICAR FORMATEO FINAL - convertir números a character con 3 cifras significativas
  tabla_5_formateada <- formatear_3_significativas(tabla_5)
  
  return(tabla_5_formateada)
}

# ==============================================================================
# TABLA 6: EXISTENCIAS POR CATEGORÍA DIAMÉTRICA POR UMM
# ==============================================================================

tabla_6_existencias_por_categoria <- function(arboles_df, config = CONFIG) {
  
  cat("\n[TABLA 6] Existencias por categoría diamétrica...\n")
  
  vivos <- arboles_df %>% 
    filter(!dominancia %in% c(7, 8, 9)) %>%
    mutate(clase_d = asignar_clase_diametrica(
      diametro_normal, 
      breaks = config$clases_d,
      formato = "rango"
    ))
  
  # Por rodal y clase diamétrica
  por_rodal_clase <- vivos %>%
    group_by(rodal, clase_d) %>%
    summarise(
      n_arboles = n(),
      vol_muestreado_m3 = sum(volumen_m3, na.rm = TRUE),
      n_muestreos = first(num_muestreos_realizados),
      superficie_ha = first(superficie_ha),
      .groups = "drop"
    ) %>%
    mutate(
      area_total_ha = config$area_parcela_ha * n_muestreos,
      vol_ha_m3 = expandir_a_hectarea(vol_muestreado_m3, area_total_ha),
      vol_total_m3 = vol_ha_m3 * superficie_ha,
      densidad_ha = expandir_a_hectarea(n_arboles, area_total_ha)
    )
  
  # Pivotar para formato de tabla
  tabla_6 <- por_rodal_clase %>%
    select(rodal, clase_d, vol_total_m3) %>%
    pivot_wider(
      names_from = clase_d,
      values_from = vol_total_m3,
      values_fill = 0,
      names_sort = TRUE
    ) %>%
    mutate(UMM = as.character(rodal)) %>%
    select(-rodal) %>%
    select(UMM, everything())
  
  # Agregar totales por clase
  totales <- tabla_6 %>%
    summarise(
      across(where(is.numeric), \(x) sum(x, na.rm = TRUE))
    ) %>%
    mutate(UMM = "TOTAL")
  
  tabla_6 <- bind_rows(tabla_6, totales) %>%
    select(UMM, everything())
  
  # APLICAR FORMATEO FINAL con 3 cifras significativas
  tabla_6_formateada <- formatear_3_significativas(tabla_6)
  
  return(tabla_6_formateada)
}

# ==============================================================================
# TABLA 7: DENSIDADES E INCREMENTOS
# ==============================================================================

tabla_7_densidades_incrementos <- function(arboles_inicial, arboles_final, 
                                           ica_calculado, años = 10,
                                           config = CONFIG) {
  
  cat("\n[TABLA 7] Densidades e incrementos...\n")
  
  # Métricas iniciales
  inicial <- calcular_metricas_estado(arboles_inicial, config)
  
  # Métricas finales
  final <- calcular_metricas_estado(arboles_final, config)
  
  # Calcular cambios
  tabla_7 <- inicial %>%
    select(rodal, n_vivos_ini = n_vivos, vol_ha_ini = vol_ha_m3, 
           ab_ha_ini = ab_ha_m2, densidad_ha_ini = densidad_ha) %>%
    left_join(
      final %>%
        select(rodal, n_vivos_fin = n_vivos, vol_ha_fin = vol_ha_m3,
               ab_ha_fin = ab_ha_m2, densidad_ha_fin = densidad_ha),
      by = "rodal"
    ) %>%
    mutate(
      # Densidad promedio
      `N árboles/ha` = (densidad_ha_ini + densidad_ha_fin) / 2,
      
      # Área basal promedio
      `AB (m²/ha)` = (ab_ha_ini + ab_ha_fin) / 2,
      
      # ICA (del cálculo previo)
      `ICA (m³/ha/año)` = NA_real_,
      
      # IMA (volumen final / años)
      `IMA (m³/ha/año)` = vol_ha_fin / años,
      
      # Tiempo de paso (años para duplicar volumen a tasa ICA)
      `Tiempo paso (años)` = NA_real_
    )
  
  # Integrar ICA del cálculo
  if (!is.null(ica_calculado)) {
    tabla_7 <- tabla_7 %>%
      left_join(
        ica_calculado %>%
          select(rodal, ICA_m3_ha, ICA_rel_i),
        by = "rodal"
      ) %>%
      mutate(
        `ICA (m³/ha/año)` = ICA_m3_ha,
        
        # Tiempo de paso: ln(2) / ln(1 + i)
        `Tiempo paso (años)` = ifelse(ICA_rel_i > 0,
                                      log(2) / log(1 + ICA_rel_i),
                                      NA_real_)
      ) %>%
      select(-ICA_m3_ha, -ICA_rel_i)
  }
  
  tabla_7 <- tabla_7 %>%
    mutate(UMM = as.character(rodal)) %>%
    select(
      UMM,
      `N árboles/ha`,
      `AB (m²/ha)`,
      `Tiempo paso (años)`,
      `ICA (m³/ha/año)`,
      `IMA (m³/ha/año)`
    )
  
  # Agregar promedios
  promedios <- tabla_7 %>%
    summarise(
      UMM = "PROMEDIO",
      across(where(is.numeric), \(x) mean(x, na.rm = TRUE))
    )
  
  tabla_7 <- bind_rows(tabla_7, promedios)
  
  # APLICAR FORMATEO FINAL con 3 cifras significativas
  tabla_7_formateada <- formatear_3_significativas(tabla_7)
  
  return(tabla_7_formateada)
}

# ==============================================================================
# EXPORTAR TODAS LAS TABLAS EN LATEX
# ==============================================================================

exportar_todas_tablas_pmf <- function(arboles_inicial, arboles_final = NULL,
                                      ica_calculado = NULL,
                                      vol_aprovechamiento = NULL,
                                      años = 10,
                                      directorio = "tablas_latex") {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║          EXPORTANDO TABLAS PMF A LATEX                    ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
  
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
  }
  
  # Tabla 5: Existencias y aprovechamiento
  t5 <- tabla_5_existencias_aprovechamiento(arboles_inicial, CONFIG, vol_aprovechamiento)
  
  # Usar SANITIZE = FALSE porque ya están formateados como strings
  xt5 <- xtable(t5,
                caption = "Existencias reales y volumen de aprovechamiento por UMM (Sección 7.1)",
                label = "tab:existencias_aprovechamiento")
  
  print(xt5,
        file = file.path(directorio, "tabla_existencias_volumen_aprovechamiento_umm.tex"),
        include.rownames = FALSE,
        caption.placement = "top",
        booktabs = TRUE,
        sanitize.text.function = function(x) x)  # NO sanitizar, ya viene formateado
  
  cat("  ✓ tabla_existencias_volumen_aprovechamiento_umm.tex\n")
  
  # Tabla 6: Categorías diamétricas
  t6 <- tabla_6_existencias_por_categoria(arboles_inicial, CONFIG)
  
  xt6 <- xtable(t6,
                caption = "Existencias por categoría diamétrica por UMM (m³)",
                label = "tab:existencias_diametricas")
  
  print(xt6,
        file = file.path(directorio, "tabla_existencias_categoria_diametrica_umm.tex"),
        include.rownames = FALSE,
        caption.placement = "top",
        booktabs = TRUE,
        sanitize.text.function = function(x) x,
        scalebox = 0.8)
  
  cat("  ✓ tabla_existencias_categoria_diametrica_umm.tex\n")
  
  # Tabla 7: Densidades e incrementos (si hay datos finales)
  t7 <- NULL
  
  if (!is.null(arboles_final)) {
    t7 <- tabla_7_densidades_incrementos(arboles_inicial, arboles_final, 
                                         ica_calculado, años, CONFIG)
    
    xt7 <- xtable(t7,
                  caption = "Densidades e incrementos por UMM (Sección 7.3)",
                  label = "tab:densidades_incrementos")
    
    print(xt7,
          file = file.path(directorio, "tabla_densidades_incrementos_umm.tex"),
          include.rownames = FALSE,
          caption.placement = "top",
          booktabs = TRUE,
          sanitize.text.function = function(x) x)
    
    cat("  ✓ tabla_densidades_incrementos_umm.tex\n")
  }
  
  cat("\n✓ Todas las tablas PMF exportadas exitosamente\n\n")
  
  return(list(
    tabla_5 = t5,
    tabla_6 = t6,
    tabla_7 = t7
  ))
}

# ==============================================================================
# MENSAJE DE CARGA
# ==============================================================================

cat("\n✓ Módulo de tablas PMF cargado\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("Funciones disponibles:\n")
cat("  • tabla_5_existencias_aprovechamiento()\n")
cat("  • tabla_6_existencias_por_categoria()\n")
cat("  • tabla_7_densidades_incrementos()\n")
cat("  • exportar_todas_tablas_pmf()\n")
cat("══════════════════════════════════════════════════════════════\n\n")