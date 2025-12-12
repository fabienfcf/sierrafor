# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# ==============================================================================
# 08_ANALISIS_DESCRIPTIVO.R - CORRECCION n_sitios
# CAMBIO CRÃTICO: Usar TODOS los sitios muestreados, no solo donde hay Ã¡rboles
# ==============================================================================

library(tidyverse)
library(xtable)
library(patchwork)
library(viridis)

# Verificar que CONFIG estÃ© cargado
if (!exists("CONFIG")) {
  stop("âŒ CONFIG no estÃ¡ cargado. Ejecuta primero: source(file.path(PROYECTO_ROOT, 'config/01_parametros_configuracion.R'))")
}

# Verificar funciones de expansiÃ³n
if (!exists("expandir_a_hectarea")) {
  source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
}

# ==============================================================================
# VERIFICAR CONFIG
# ==============================================================================

if (is.null(CONFIG$area_parcela_regeneracion_ha)) {
  warning("âš ï¸ CONFIG$area_parcela_regeneracion_ha no existe, usando 0.0009 ha (9 mÂ²)")
  CONFIG$area_parcela_regeneracion_ha <- 0.0009
}

cat(sprintf("\n[CONFIGURACIÃ“N DE EXPANSIÃ“N]\n"))
cat(sprintf("  Arbolado: %.2f ha (factor = %.1f)\n", 
            CONFIG$area_parcela_ha, 
            calcular_factor_expansion(CONFIG$area_parcela_ha)))
cat(sprintf("  RegeneraciÃ³n: %.4f ha (9 mÂ², factor = %.1f)\n\n", 
            CONFIG$area_parcela_regeneracion_ha,
            calcular_factor_expansion(CONFIG$area_parcela_regeneracion_ha)))

# ==============================================================================
# FUNCIÃ“N AUXILIAR: EXPORTAR CSV (VERSIÃ“N ROBUSTA)
# ==============================================================================

exportar_csv <- function(df, nombre_archivo, carpeta = "resultados") {
  
  # Verificar input
  if (is.null(df) || nrow(df) == 0) {
    warning(sprintf("âš ï¸ DataFrame vacÃ­o para %s, no se exporta", nombre_archivo))
    return(invisible(FALSE))
  }
  
  # Crear carpeta si no existe
  if (!dir.exists(carpeta)) {
    dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)
    cat(sprintf("  ðŸ“ Creando carpeta: %s/\n", carpeta))
  }
  
  # Ruta completa
  ruta <- file.path(carpeta, paste0(nombre_archivo, ".csv"))
  
  # Exportar con manejo de errores
  tryCatch({
    write.csv(df, file = ruta, row.names = FALSE)
    
    # Verificar que se creÃ³
    if (file.exists(ruta)) {
      size_kb <- file.info(ruta)$size / 1024
      cat(sprintf("  âœ“ CSV: %s (%.1f KB)\n", basename(ruta), size_kb))
      return(invisible(TRUE))
    } else {
      warning(sprintf("âš ï¸ Archivo no creado: %s", ruta))
      return(invisible(FALSE))
    }
    
  }, error = function(e) {
    warning(sprintf("âŒ Error exportando %s: %s", nombre_archivo, e$message))
    return(invisible(FALSE))
  })
}

# ==============================================================================
# 1. ESTRUCTURA POBLACIONAL - CORREGIDO n_sitios
# ==============================================================================

analizar_estructura_poblacional <- function(arboles_df, config = CONFIG, 
                                            exportar_latex = TRUE,
                                            exportar_csv_flag = TRUE) {
  
  cat("\nâ•”â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•—\n")
  cat("â•‘     ANÃLISIS ESTRUCTURA POBLACIONAL (AB/ha + CSV)         â•‘\n")
  cat("â•šâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n\n")
  
  if (!exists("filtrar_arboles_vivos")) {
    source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
  }
  
  # CRÃTICO: Usar TODOS los sitios muestreados, no solo donde hay Ã¡rboles vivos
  n_sitios_total <- n_distinct(arboles_df$muestreo)
  vivos <- filtrar_arboles_vivos(arboles_df)
  n_sitios_con_vivos <- n_distinct(vivos$muestreo)
  
  cat(sprintf("Sitios muestreados TOTAL: %d\n", n_sitios_total))
  cat(sprintf("Sitios con Ã¡rboles vivos: %d\n", n_sitios_con_vivos))
  cat(sprintf("Sitios sin Ã¡rboles vivos: %d\n\n", n_sitios_total - n_sitios_con_vivos))
  
  # Usar n_sitios_total para cÃ¡lculos /ha
  n_sitios <- n_sitios_total
  
  # RESUMEN GENERAL - CON AB/HA Y DMC
  resumen_general <- vivos %>%
    summarise(
      n_arboles_total = n(),
      n_sitios = n_sitios,
      n_rodales = n_distinct(rodal),
      n_especies = n_distinct(nombre_cientifico),
      n_generos = n_distinct(genero_grupo),
      densidad_ha = expandir_a_hectarea(n_arboles_total / n_sitios, config$area_parcela_ha),
      d_medio_cm = mean(diametro_normal, na.rm = TRUE),
      dmc_cm = calcular_dmc(diametro_normal),
      d_min_cm = min(diametro_normal, na.rm = TRUE),
      d_max_cm = max(diametro_normal, na.rm = TRUE),
      h_media_m = mean(altura_total, na.rm = TRUE),
      h_min_m = min(altura_total, na.rm = TRUE),
      h_max_m = max(altura_total, na.rm = TRUE),
      ab_media_m2ha = expandir_a_hectarea(
        sum(area_basal, na.rm = TRUE) / n_sitios,
        config$area_parcela_ha
      ),
      vol_medio_m3ha = expandir_a_hectarea(
        sum(volumen_m3, na.rm = TRUE) / n_sitios, 
        config$area_parcela_ha
      )
    )
  
  cat("[RESUMEN GENERAL - VALORES POR HECTÃREA]\n")
  print(resumen_general)
  
  # POR RODAL - CON AB/HA Y DMC (CORREGIDO)
  por_rodal <- arboles_df %>%
    group_by(rodal) %>%
    summarise(
      n_sitios = n_distinct(muestreo),
      .groups = "drop"
    ) %>%
    left_join(
      vivos %>%
        group_by(rodal) %>%
        summarise(
          n_arboles_obs = n(),
          n_especies = n_distinct(nombre_cientifico),
          d_medio_cm = mean(diametro_normal, na.rm = TRUE),
          dmc_cm = calcular_dmc(diametro_normal),
          h_media_m = mean(altura_total, na.rm = TRUE),
          ab_total = sum(area_basal, na.rm = TRUE),
          vol_total = sum(volumen_m3, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "rodal"
    ) %>%
    mutate(
      # Reemplazar NA con 0 si no hay Ã¡rboles vivos
      n_arboles_obs = replace_na(n_arboles_obs, 0),
      n_especies = replace_na(n_especies, 0),
      # Calcular mÃ©tricas /ha con n_sitios del rodal
      densidad_ha = expandir_a_hectarea(n_arboles_obs / n_sitios, config$area_parcela_ha),
      ab_m2ha = expandir_a_hectarea(
        replace_na(ab_total, 0) / n_sitios,
        config$area_parcela_ha
      ),
      vol_m3ha = expandir_a_hectarea(
        replace_na(vol_total, 0) / n_sitios,
        config$area_parcela_ha
      )
    ) %>%
    select(-ab_total, -vol_total) %>%
    arrange(rodal)
  
  cat("\n[POR RODAL - VALORES POR HECTÃREA]\n")
  print(por_rodal, n = 50)
  
  # POR GÃ‰NERO - CON AB/HA Y DMC (CORREGIDO)
  por_genero <- vivos %>%
    count(genero_grupo, name = "n_arboles_obs") %>%
    mutate(
      # Usar n_sitios_total para densidad /ha
      densidad_ha = expandir_a_hectarea(n_arboles_obs / n_sitios, config$area_parcela_ha),
      proporcion_pct = (n_arboles_obs / sum(n_arboles_obs)) * 100
    ) %>%
    left_join(
      vivos %>%
        group_by(genero_grupo) %>%
        summarise(
          d_medio_cm = mean(diametro_normal, na.rm = TRUE),
          dmc_cm = calcular_dmc(diametro_normal),
          h_media_m = mean(altura_total, na.rm = TRUE),
          # Usar n_sitios_total para AB y Vol /ha
          ab_m2ha = expandir_a_hectarea(
            sum(area_basal, na.rm = TRUE) / n_sitios,
            config$area_parcela_ha
          ),
          vol_m3ha = expandir_a_hectarea(
            sum(volumen_m3, na.rm = TRUE) / n_sitios,
            config$area_parcela_ha
          ),
          .groups = "drop"
        ),
      by = "genero_grupo"
    ) %>%
    mutate(
      ab_pct = (ab_m2ha / sum(ab_m2ha)) * 100,
      vol_pct = (vol_m3ha / sum(vol_m3ha)) * 100
    ) %>%
    arrange(desc(densidad_ha))
  
  cat("\n[POR GÃ‰NERO - VALORES POR HECTÃREA]\n")
  print(por_genero)
  
  # POR ESPECIE - CON AB/HA Y DMC (TOP 10) (CORREGIDO)
  por_especie <- vivos %>%
    count(nombre_cientifico, genero_grupo, name = "n_arboles_obs") %>%
    mutate(
      # Usar n_sitios_total
      densidad_ha = expandir_a_hectarea(n_arboles_obs / n_sitios, config$area_parcela_ha),
      proporcion_pct = (n_arboles_obs / nrow(vivos)) * 100
    ) %>%
    left_join(
      vivos %>%
        group_by(nombre_cientifico) %>%
        summarise(
          d_medio_cm = mean(diametro_normal, na.rm = TRUE),
          dmc_cm = calcular_dmc(diametro_normal),
          # Usar n_sitios_total
          ab_m2ha = expandir_a_hectarea(
            sum(area_basal, na.rm = TRUE) / n_sitios,
            config$area_parcela_ha
          ),
          .groups = "drop"
        ),
      by = "nombre_cientifico"
    ) %>%
    arrange(desc(densidad_ha)) %>%
    head(10)
  
  cat("\n[TOP 10 ESPECIES - DENSIDAD POR HECTÃREA]\n")
  print(por_especie)
  
  # EXPORTAR CSV
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    exportar_csv(resumen_general, "desc_01_resumen_general")
    exportar_csv(por_rodal, "desc_02_por_rodal")
    exportar_csv(por_genero, "desc_03_por_genero")
    exportar_csv(por_especie, "desc_04_top10_especies")
  }
  
  # EXPORTAR LATEX
  if (exportar_latex) {
    dir.create("tablas_latex", showWarnings = FALSE, recursive = TRUE)
    
    # Tabla por rodal - AHORA CON AB/HA Y DMC
    xtable(por_rodal %>% select(-n_sitios, -n_arboles_obs), 
           caption = "Resumen dasomÃ©trico por rodal (valores/ha, incluye AB y DMC)",
           label = "tab:resumen_rodal",
           digits = c(0, 0, 1, 0, 2, 2, 2, 2, 2)) %>%
      print(file = "tablas_latex/desc_01_resumen_rodal.tex",
            include.rownames = FALSE, 
            floating = TRUE, 
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    # Tabla por gÃ©nero - AHORA CON AB/HA, DMC Y %
    xtable(por_genero %>% select(genero_grupo, densidad_ha, proporcion_pct, 
                                 d_medio_cm, dmc_cm, ab_m2ha, ab_pct, vol_m3ha, vol_pct),
           caption = "ComposiciÃ³n por gÃ©nero (valores/ha, incluye AB, DMC y porcentajes)",
           label = "tab:composicion_genero",
           digits = c(0, 0, 1, 2, 2, 2, 2, 2, 2, 2)) %>%
      print(file = "tablas_latex/desc_02_composicion_genero.tex",
            include.rownames = FALSE, 
            floating = TRUE, 
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    # Tabla especies - AHORA CON AB/HA Y DMC
    xtable(por_especie %>% select(-n_arboles_obs),
           caption = "Principales especies (top 10, incluye AB/ha y DMC)",
           label = "tab:top_especies",
           digits = c(0, 0, 0, 1, 2, 2, 2, 2)) %>%
      print(file = "tablas_latex/desc_03_top_especies.tex",
            include.rownames = FALSE, 
            floating = TRUE, 
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    cat("\nâœ“ Tablas LaTeX exportadas (desc_01 a desc_03)\n")
  }
  
  return(list(
    general = resumen_general,
    por_rodal = por_rodal,
    por_genero = por_genero,
    por_especie = por_especie
  ))
}

# ==============================================================================
# 1B. COMPOSICIÃ“N PINUS/QUERCUS POR RODAL - CORREGIDO
# ==============================================================================

analizar_composicion_generopq_por_rodal <- function(arboles_df, config = CONFIG,
                                                    exportar_latex = TRUE,
                                                    exportar_csv_flag = TRUE) {
  
  cat("\nâ•”â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•—\n")
  cat("â•‘  COMPOSICIÃ“N PINUS/QUERCUS POR RODAL (con AB/ha y DMC)   â•‘\n")
  cat("â•šâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n\n")
  
  vivos <- filtrar_arboles_vivos(arboles_df) %>%
    filter(genero_grupo %in% c("Pinus", "Quercus"))
  
  # CRÃTICO: Contar TODOS los sitios por rodal, no solo donde hay P/Q vivos
  sitios_por_rodal <- arboles_df %>%
    group_by(rodal) %>%
    summarise(n_sitios = n_distinct(muestreo), .groups = "drop")
  
  # Calcular por rodal y gÃ©nero CON DMC
  composicion_generopq_rodal <- vivos %>%
    group_by(rodal, genero_grupo) %>%
    summarise(
      n_arboles_obs = n(),
      d_medio_cm = mean(diametro_normal, na.rm = TRUE),
      dmc_cm = calcular_dmc(diametro_normal),
      h_media_m = mean(altura_total, na.rm = TRUE),
      ab_total = sum(area_basal, na.rm = TRUE),
      vol_total = sum(volumen_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Unir con sitios totales del rodal
    left_join(sitios_por_rodal, by = "rodal") %>%
    mutate(
      densidad_ha = expandir_a_hectarea(n_arboles_obs / n_sitios, config$area_parcela_ha),
      ab_m2ha = expandir_a_hectarea(ab_total / n_sitios, config$area_parcela_ha),
      vol_m3ha = expandir_a_hectarea(vol_total / n_sitios, config$area_parcela_ha)
    ) %>%
    select(-ab_total, -vol_total) %>%
    # Calcular porcentajes por rodal
    group_by(rodal) %>%
    mutate(
      densidad_pct = (densidad_ha / sum(densidad_ha)) * 100,
      ab_pct = (ab_m2ha / sum(ab_m2ha)) * 100,
      vol_pct = (vol_m3ha / sum(vol_m3ha)) * 100
    ) %>%
    ungroup() %>%
    arrange(rodal, desc(densidad_ha))
  
  cat("[COMPOSICIÃ“N PINUS/QUERCUS POR RODAL]\n")
  print(composicion_generopq_rodal, n = 50)
  
  # EXPORTAR CSV
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    exportar_csv(composicion_generopq_rodal, "desc_11_composicion_generopq_por_rodal")
  }
  
  # EXPORTAR LATEX
  if (exportar_latex) {
    dir.create("tablas_latex", showWarnings = FALSE, recursive = TRUE)
    
    xtable(composicion_generopq_rodal %>% 
             select(rodal, genero_grupo, densidad_ha, densidad_pct,
                    d_medio_cm, dmc_cm, h_media_m, ab_m2ha, ab_pct, vol_m3ha, vol_pct),
           caption = "ComposiciÃ³n de Pinus y Quercus por rodal (valores/ha, DMC y porcentajes)",
           label = "tab:composicion_generopq_rodal",
           digits = c(0, 0, 0, 1, 1, 2, 2, 2, 2, 1, 2, 1)) %>%
      print(file = "tablas_latex/desc_11_composicion_generopq_rodal.tex",
            include.rownames = FALSE, 
            floating = TRUE, 
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    cat("\nâœ“ Tabla LaTeX: desc_11_composicion_generopq_rodal.tex\n")
  }
  
  # GRÃFICO OPCIONAL - Volumen por rodal y gÃ©nero
  p_comp <- ggplot(composicion_generopq_rodal,
                   aes(x = factor(rodal), y = vol_m3ha, fill = genero_grupo)) +
    geom_col(position = "stack", alpha = 0.8) +
    geom_text(aes(label = sprintf("%.0f%%", vol_pct)),
              position = position_stack(vjust = 0.5),
              color = "white", fontface = "bold", size = 3.5) +
    scale_fill_manual(
      values = c("Quercus" = "#8B4513", "Pinus" = "#228B22")
    ) +
    labs(
      title = "ComposiciÃ³n de Volumen: Pinus vs Quercus por Rodal",
      subtitle = "Volumen total/ha y porcentaje de cada gÃ©nero",
      x = "Rodal",
      y = "Volumen (mÂ³/ha)",
      fill = "GÃ©nero"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold")
    )
  
  if (exportar_latex) {
    ggsave("graficos/desc_11_composicion_generopq_rodal.png", p_comp,
           width = 10, height = 6, dpi = 300)
    cat("âœ“ GrÃ¡fico: desc_11_composicion_generopq_rodal.png\n")
  }
  
  return(list(
    tabla = composicion_generopq_rodal,
    grafico = p_comp
  ))
}

# ==============================================================================
# 1C. ÃREA BASAL POR DOMINANCIA - CORREGIDO
# ==============================================================================

analizar_ab_por_dominancia <- function(arboles_df, config = CONFIG,
                                       exportar_csv_flag = TRUE) {
  
  cat("\nâ•”â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•—\n")
  cat("â•‘   AB POR DOMINANCIA: Pinus/Quercus (incluye muertos)     â•‘\n")
  cat("â•šâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n\n")
  
  if (!exists("CODIGOS_DOMINANCIA")) {
    stop("âŒ CODIGOS_DOMINANCIA no estÃ¡ cargado")
  }
  
  # TODOS LOS ÃRBOLES (incluyendo muertos) - Solo Pinus/Quercus
  pq_todos <- arboles_df %>%
    filter(genero_grupo %in% c("Pinus", "Quercus"))
  
  # CRÃTICO: Contar TODOS los sitios muestreados
  n_sitios <- n_distinct(arboles_df$muestreo)
  
  # Sitios por rodal
  sitios_por_rodal <- arboles_df %>%
    group_by(rodal) %>%
    summarise(n_sitios = n_distinct(muestreo), .groups = "drop")
  
  cat(sprintf("Ãrboles Pinus/Quercus (vivos + muertos): %d\n", nrow(pq_todos)))
  cat(sprintf("Sitios TOTAL: %d\n\n", n_sitios))
  
  # Calcular AB por rodal, gÃ©nero y dominancia
  ab_por_dominancia <- pq_todos %>%
    left_join(
      CODIGOS_DOMINANCIA %>% select(codigo, etiqueta),
      by = c("dominancia" = "codigo")
    ) %>%
    rename(etiqueta_dominancia = etiqueta) %>%
    group_by(rodal, genero_grupo, dominancia, etiqueta_dominancia) %>%
    summarise(
      n_arboles_obs = n(),
      ab_total = sum(area_basal, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Unir con sitios por rodal
    left_join(sitios_por_rodal, by = "rodal") %>%
    mutate(
      densidad_ha = expandir_a_hectarea(n_arboles_obs / n_sitios, config$area_parcela_ha),
      ab_m2ha = expandir_a_hectarea(ab_total / n_sitios, config$area_parcela_ha)
    ) %>%
    select(-ab_total) %>%
    # Calcular % de AB por rodal y gÃ©nero
    group_by(rodal, genero_grupo) %>%
    mutate(
      ab_pct_genero = (ab_m2ha / sum(ab_m2ha)) * 100
    ) %>%
    ungroup() %>%
    arrange(rodal, genero_grupo, dominancia)
  
  cat("[ÃREA BASAL POR DOMINANCIA - PINUS/QUERCUS]\n")
  print(ab_por_dominancia, n = 100)
  
  # Resumen por rodal
  resumen_rodal <- ab_por_dominancia %>%
    group_by(rodal, genero_grupo) %>%
    summarise(
      ab_total_m2ha = sum(ab_m2ha),
      ab_vivos_m2ha = sum(ab_m2ha[dominancia %in% 1:6]),
      ab_muertos_m2ha = sum(ab_m2ha[dominancia %in% 7:9]),
      pct_muertos = (ab_muertos_m2ha / ab_total_m2ha) * 100,
      .groups = "drop"
    )
  
  cat("\n[RESUMEN: AB VIVOS VS MUERTOS]\n")
  print(resumen_rodal, n = 50)
  
  # EXPORTAR CSV
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    exportar_csv(ab_por_dominancia, "desc_12_ab_por_dominancia")
    exportar_csv(resumen_rodal, "desc_13_ab_vivos_vs_muertos")
  }
  
  # GRÃFICO - AB por dominancia
  p_ab_dom <- ggplot(ab_por_dominancia,
                     aes(x = factor(rodal), y = ab_m2ha, 
                         fill = etiqueta_dominancia)) +
    geom_col(position = "stack", alpha = 0.8) +
    facet_wrap(~genero_grupo, ncol = 1) +
    scale_fill_viridis_d(option = "turbo") +
    labs(
      title = "Ãrea Basal por Clase de Dominancia",
      subtitle = "Pinus y Quercus - Incluye Ã¡rboles muertos",
      x = "Rodal",
      y = "Ãrea Basal (mÂ²/ha)",
      fill = "Dominancia"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold")
    )
  
  ggsave("graficos/desc_12_ab_por_dominancia.png", p_ab_dom,
         width = 12, height = 8, dpi = 300)
  
  cat("\nâœ“ GrÃ¡fico: desc_12_ab_por_dominancia.png\n")
  
  return(list(
    detalle = ab_por_dominancia,
    resumen = resumen_rodal,
    grafico = p_ab_dom
  ))
}

# ==============================================================================
# 2. DISTRIBUCIÃ“N DIAMÃ‰TRICA - CORREGIDO
# ==============================================================================

analizar_distribucion_diametrica <- function(arboles_df, config = CONFIG, 
                                             exportar = TRUE,
                                             exportar_csv_flag = TRUE) {
  
  cat("\nâ•”â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•—\n")
  cat("â•‘         ANÃLISIS DISTRIBUCIÃ“N DIAMÃ‰TRICA + CSV            â•‘\n")
  cat("â•šâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n\n")
  
  vivos <- filtrar_arboles_vivos(arboles_df)
  # CRÃTICO: Usar TODOS los sitios muestreados
  n_sitios <- n_distinct(arboles_df$muestreo)
  
  cat(sprintf("Sitios muestreados TOTAL: %d\n", n_sitios))
  cat(sprintf("Ãrboles vivos: %d\n\n", nrow(vivos)))
  
  dist_diametrica <- vivos %>%
    mutate(clase_d = asignar_clase_diametrica(
      diametro_normal, 
      breaks = config$clases_d,
      formato = "rango"
    )) %>%
    filter(!is.na(clase_d)) %>%
    count(clase_d, genero_grupo, name = "n_arboles_obs") %>%
    mutate(
      densidad_ha = expandir_a_hectarea(n_arboles_obs / n_sitios, config$area_parcela_ha)
    ) %>%
    group_by(genero_grupo) %>%
    mutate(
      proporcion_pct = (densidad_ha / sum(densidad_ha)) * 100,
      proporcion_acum = cumsum(proporcion_pct)
    ) %>%
    ungroup()
  
  cat("[DISTRIBUCIÃ“N DIAMÃ‰TRICA - DENSIDAD/HA]\n")
  print(dist_diametrica, n = 30)
  
  # EXPORTAR CSV
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    exportar_csv(dist_diametrica, "desc_05_distribucion_diametrica")
  }
  
  # GRÃFICOS
  p1 <- ggplot(dist_diametrica, 
               aes(x = clase_d, y = densidad_ha, fill = genero_grupo)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_fill_manual(
      values = c("Quercus" = "#8B4513", "Pinus" = "#228B22", "Otros" = "#808080")
    ) +
    labs(
      title = "DistribuciÃ³n DiamÃ©trica por GÃ©nero",
      subtitle = sprintf("Densidad/ha - Parcela: %.2f ha", config$area_parcela_ha),
      x = "Clase DiamÃ©trica (cm)",
      y = "Densidad (Ã¡rboles/ha)",
      fill = "GÃ©nero"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      plot.title = element_text(face = "bold")
    )
  
  p2 <- ggplot(dist_diametrica,
               aes(x = clase_d, y = proporcion_acum, 
                   color = genero_grupo, group = genero_grupo)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(
      values = c("Quercus" = "#8B4513", "Pinus" = "#228B22", "Otros" = "#808080")
    ) +
    labs(
      title = "ProporciÃ³n Acumulada por Clase DiamÃ©trica",
      x = "Clase DiamÃ©trica (cm)",
      y = "ProporciÃ³n Acumulada (%)",
      color = "GÃ©nero"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
  
  p_combined <- p1 / p2
  
  if (exportar) {
    dir.create("graficos", showWarnings = FALSE, recursive = TRUE)
    ggsave("graficos/desc_01_distribucion_diametrica.png", p_combined,
           width = 10, height = 8, dpi = 300)
    cat("\nâœ“ GrÃ¡fico guardado: graficos/desc_01_distribucion_diametrica.png\n")
  }
  
  return(list(
    tabla = dist_diametrica, 
    grafico_barras = p1,
    grafico_acumulado = p2,
    grafico_combinado = p_combined
  ))
}

# ==============================================================================
# 3. EROSIÃ“N - % SITIOS AFECTADOS + CSV
# ==============================================================================

analizar_erosion <- function(f01, exportar_latex = TRUE, exportar_csv_flag = TRUE) {
  
  cat("\nâ•”â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•—\n")
  cat("â•‘      ANÃLISIS DE EROSIÃ“N (% sitios afectados + CSV)       â•‘\n")
  cat("â•šâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n\n")
  
  if (!exists("CODIGOS_EROSION")) {
    stop("âŒ CODIGOS_EROSION no estÃ¡ cargado")
  }
  
  # Calcular % de sitios afectados por rodal
  erosion_por_rodal <- f01 %>%
    select(rodal, muestreo, erosion_laminar, erosion_canalillos, 
           erosion_carcavas, erosion_antropica) %>%
    pivot_longer(
      cols = starts_with("erosion_"),
      names_to = "tipo_erosion",
      values_to = "codigo"
    ) %>%
    mutate(
      tipo_erosion = recode(
        tipo_erosion,
        "erosion_laminar" = "Laminar",
        "erosion_canalillos" = "Canalillos",
        "erosion_carcavas" = "CÃ¡rcavas",
        "erosion_antropica" = "AntrÃ³pica"
      ),
      afectado = codigo > 1
    ) %>%
    group_by(rodal, tipo_erosion) %>%
    summarise(
      n_sitios_total = n(),
      n_sitios_afectados = sum(afectado),
      pct_afectado = (n_sitios_afectados / n_sitios_total) * 100,
      .groups = "drop"
    ) %>%
    arrange(rodal, tipo_erosion)
  
  cat("[EROSIÃ“N: % DE SITIOS AFECTADOS POR RODAL]\n")
  print(erosion_por_rodal, n = 50)
  
  # EXPORTAR CSV
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    exportar_csv(erosion_por_rodal, "desc_06_erosion")
  }
  
  # GrÃ¡fico
  p_erosion <- ggplot(erosion_por_rodal, 
                      aes(x = factor(rodal), y = pct_afectado, fill = tipo_erosion)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_fill_brewer(palette = "Set2") +
    geom_text(aes(label = sprintf("%.0f%%", pct_afectado)), 
              position = position_dodge(width = 0.9),
              vjust = -0.5, size = 3) +
    labs(
      title = "Porcentaje de Sitios con Evidencia de ErosiÃ³n",
      subtitle = "Por rodal y tipo de erosiÃ³n",
      x = "Rodal",
      y = "% de Sitios Afectados",
      fill = "Tipo de ErosiÃ³n"
    ) +
    ylim(0, 110) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top")
  
  if (exportar_latex) {
    # Tabla pivoteada
    tabla_latex <- erosion_por_rodal %>%
      select(rodal, tipo_erosion, pct_afectado) %>%
      pivot_wider(
        names_from = tipo_erosion,
        values_from = pct_afectado,
        names_prefix = "pct_"
      )
    
    xtable(tabla_latex,
           caption = "Porcentaje de sitios afectados por erosiÃ³n (por rodal)",
           label = "tab:erosion_pct",
           digits = c(0, 0, 1, 1, 1, 1)) %>%
      print(file = "tablas_latex/desc_04_erosion.tex",
            include.rownames = FALSE, 
            floating = TRUE, 
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    ggsave("graficos/desc_02_erosion.png", p_erosion,
           width = 10, height = 6, dpi = 300)
    
    cat("\nâœ“ Tabla y grÃ¡fico de erosiÃ³n exportados\n")
  }
  
  return(list(
    tabla = erosion_por_rodal,
    grafico = p_erosion
  ))
}

# ==============================================================================
# 4. SANIDAD - CORREGIDO
# ==============================================================================

analizar_sanidad <- function(arboles_df, config = CONFIG, 
                             exportar_latex = TRUE,
                             exportar_csv_flag = TRUE) {
  
  cat("\nâ•”â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•—\n")
  cat("â•‘      ANÃLISIS SANITARIO (paleta daltÃ³nicos + CSV)         â•‘\n")
  cat("â•šâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n\n")
  
  if (!exists("CODIGOS_SANIDAD") || !exists("CODIGOS_INTENSIDAD_INFESTACION")) {
    stop("âŒ CODIGOS_SANIDAD o CODIGOS_INTENSIDAD_INFESTACION no estÃ¡n cargados")
  }
  
  # FILTRAR SOLO VIVOS PARA ANÃLISIS SANITARIO
  vivos <- filtrar_arboles_vivos(arboles_df)
  # CRÃTICO: Usar TODOS los sitios muestreados
  n_sitios <- n_distinct(arboles_df$muestreo)
  
  cat(sprintf("Sitios muestreados TOTAL: %d\n", n_sitios))
  cat(sprintf("Ãrboles vivos evaluados: %d\n\n", nrow(vivos)))
  
  sanidad <- vivos %>%
    filter(!is.na(sanidad)) %>%
    left_join(CODIGOS_SANIDAD, by = c("sanidad" = "codigo"), suffix = c("", "_sanidad")) %>%
    left_join(CODIGOS_INTENSIDAD_INFESTACION, 
              by = c("calificacion_sanidad" = "codigo"),
              suffix = c("", "_intensidad")) %>%
    rename(
      problema = etiqueta,
      intensidad = etiqueta_intensidad
    ) %>%
    count(rodal, genero_grupo, problema, intensidad, name = "n_arboles_obs") %>%
    mutate(
      densidad_ha = expandir_a_hectarea(n_arboles_obs / n_sitios, config$area_parcela_ha)
    ) %>%
    arrange(rodal, desc(densidad_ha))
  
  resumen_sanidad <- sanidad %>%
    group_by(rodal) %>%
    summarise(
      densidad_evaluada_ha = sum(densidad_ha),
      densidad_sanos_ha = sum(densidad_ha[problema == "Sano"]),
      prop_sanos_pct = (densidad_sanos_ha / densidad_evaluada_ha) * 100,
      principal_problema = {
        problemas_rodal <- problema[problema != "Sano"]
        if (length(problemas_rodal) > 0) {
          idx_max <- which.max(densidad_ha[problema != "Sano"])
          problemas_rodal[idx_max]
        } else {
          "Ninguno"
        }
      },
      .groups = "drop"
    )
  
  cat("[SANIDAD POR RODAL - DENSIDAD/HA]\n")
  print(resumen_sanidad)
  
  # EXPORTAR CSV
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    exportar_csv(sanidad, "desc_07_sanidad_detalle")
    exportar_csv(resumen_sanidad, "desc_08_sanidad_resumen")
  }
  
  # GRÃFICO - PALETA VIRIDIS
  p_sanidad <- sanidad %>%
    filter(problema != "Sano") %>%
    ggplot(aes(x = factor(rodal), y = densidad_ha, fill = problema)) +
    geom_col(alpha = 0.9) +
    scale_fill_viridis_d(option = "plasma", begin = 0.2, end = 0.9) +
    labs(
      title = "Problemas Fitosanitarios Detectados",
      subtitle = "Densidad de Ã¡rboles afectados/ha por rodal (paleta accesible)",
      x = "Rodal",
      y = "Densidad (Ã¡rboles/ha)",
      fill = "Problema"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold")
    )
  
  if (exportar_latex) {
    xtable(resumen_sanidad,
           caption = "Estado sanitario por rodal (densidad/ha)",
           label = "tab:sanidad",
           digits = c(0, 0, 1, 1, 1, 0)) %>%
      print(file = "tablas_latex/desc_05_sanidad.tex",
            include.rownames = FALSE, 
            floating = TRUE, 
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    ggsave("graficos/desc_03_sanidad.png", p_sanidad,
           width = 10, height = 6, dpi = 300)
    
    cat("\nâœ“ Tabla y grÃ¡fico de sanidad exportados\n")
  }
  
  return(list(
    detalle = sanidad, 
    resumen = resumen_sanidad,
    grafico = p_sanidad
  ))
}

# ==============================================================================
# 5. REGENERACIÃ“N - PINUS/QUERCUS + DENSIDAD=0 + CSV
# ==============================================================================

analizar_regeneracion <- function(f05, f01, config = CONFIG, 
                                  exportar_latex = TRUE,
                                  exportar_csv_flag = TRUE) {
  
  cat("\nâ•”â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•—\n")
  cat("â•‘   REGENERACIÃ“N: Pinus/Quercus + densidad=0 + CSV          â•‘\n")
  cat("â•šâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n\n")
  
  if (!exists("ESPECIES")) {
    stop("âŒ ESPECIES no estÃ¡ cargado")
  }
  
  # 1. TODOS LOS SITIOS
  todos_sitios <- f01 %>%
    select(muestreo, rodal) %>%
    distinct()
  
  n_sitios_total <- nrow(todos_sitios)
  
  cat(sprintf("Total de sitios en inventario: %d\n", n_sitios_total))
  
  # 2. REGENERACIÃ“N OBSERVADA (solo Pinus/Quercus)
  regeneracion_obs <- f05 %>%
    left_join(ESPECIES %>% select(codigo, nombre_cientifico, genero), 
              by = c("especie" = "codigo")) %>%
    filter(genero %in% c("Pinus", "Quercus")) %>%
    mutate(
      total_individuos = frec_025_150 + frec_151_275 + frec_276_400,
      densidad_ha = expandir_a_hectarea(total_individuos, config$area_parcela_regeneracion_ha)
    ) %>%
    filter(!is.na(genero), !is.na(densidad_ha), is.finite(densidad_ha))
  
  cat(sprintf("Sitios CON regeneraciÃ³n observada (Pinus/Quercus): %d\n", 
              n_distinct(regeneracion_obs$muestreo)))
  
  # 3. COMPLETAR CON SITIOS SIN REGENERACIÃ“N (densidad = 0)
  regeneracion_completa <- expand_grid(
    muestreo = todos_sitios$muestreo,
    genero = c("Pinus", "Quercus")
  ) %>%
    left_join(todos_sitios, by = "muestreo") %>%
    left_join(
      regeneracion_obs %>% 
        group_by(muestreo, genero) %>%
        summarise(densidad_ha = sum(densidad_ha), .groups = "drop"),
      by = c("muestreo", "genero")
    ) %>%
    mutate(densidad_ha = replace_na(densidad_ha, 0))
  
  cat(sprintf("\nSitios con regeneraciÃ³n = 0: %d\n", 
              sum(regeneracion_completa$densidad_ha == 0)))
  cat(sprintf("Sitios con regeneraciÃ³n > 0: %d\n\n", 
              sum(regeneracion_completa$densidad_ha > 0)))
  
  # 4. RESUMEN POR GÃ‰NERO
  resumen_regeneracion <- regeneracion_completa %>%
    group_by(genero) %>%
    summarise(
      n_sitios_total = n(),
      n_sitios_con_regen = sum(densidad_ha > 0),
      pct_presencia = (n_sitios_con_regen / n_sitios_total) * 100,
      densidad_media_ha = mean(densidad_ha),
      densidad_mediana_ha = median(densidad_ha),
      densidad_min_ha = min(densidad_ha),
      densidad_max_ha = max(densidad_ha),
      .groups = "drop"
    )
  
  cat("[REGENERACIÃ“N - Solo Pinus/Quercus - Parcela 9 mÂ²]\n")
  print(resumen_regeneracion)
  
  # EXPORTAR CSV
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    exportar_csv(regeneracion_completa, "desc_09_regeneracion_completa")
    exportar_csv(resumen_regeneracion, "desc_10_regeneracion_resumen")
  }
  
  # 5. GRÃFICO - PALETA COLORBLIND-FRIENDLY
  p_regen <- ggplot(regeneracion_completa, 
                    aes(x = genero, y = densidad_ha, fill = genero)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
    scale_fill_manual(
      values = c("Quercus" = "#E69F00", "Pinus" = "#56B4E9")
    ) +
    labs(
      title = "Densidad de RegeneraciÃ³n Natural (Pinus y Quercus)",
      subtitle = sprintf("Parcela 9 mÂ² (0.0009 ha) - Incluye sitios sin regeneraciÃ³n (n=%d)", 
                         n_sitios_total),
      x = "GÃ©nero",
      y = "Densidad (individuos/ha)",
      fill = "GÃ©nero"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold")
    )
  
  if (exportar_latex) {
    xtable(resumen_regeneracion,
           caption = "RegeneraciÃ³n de Pinus y Quercus (parcela 9 mÂ², incluye densidad=0)",
           label = "tab:regeneracion",
           digits = c(0, 0, 0, 0, 1, 1, 1, 1, 1)) %>%
      print(file = "tablas_latex/desc_06_regeneracion.tex",
            include.rownames = FALSE, 
            floating = TRUE, 
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    ggsave("graficos/desc_04_regeneracion.png", p_regen,
           width = 8, height = 6, dpi = 300)
    
    cat("\nâœ“ Tabla y grÃ¡fico de regeneraciÃ³n exportados\n")
  }
  
  return(list(
    datos_completos = regeneracion_completa,
    resumen = resumen_regeneracion,
    grafico = p_regen
  ))
}

# ==============================================================================
# FUNCIÓN CORREGIDA: analizar_tratamientos_silvicolas (ROBUSTA)
# Detecta automáticamente los nombres de columnas
# ==============================================================================

analizar_tratamientos_silvicolas <- function(inventario_f01,
                                             exportar_latex = TRUE,
                                             exportar_csv_flag = TRUE) {
  
  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║   ANÁLISIS TRATAMIENTOS SILVÍCOLAS (TS) Y TC1-3        ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n\n")
  
  # ============================================================================
  # DETECCIÓN AUTOMÁTICA DE NOMBRES DE COLUMNAS
  # ============================================================================
  
  cols_disponibles <- names(inventario_f01)
  
  # Detectar nombres de columna TS
  col_ts <- case_when(
    "tratamiento_silvicola" %in% cols_disponibles ~ "tratamiento_silvicola",
    "35.TS" %in% cols_disponibles ~ "35.TS",
    "x35_ts" %in% cols_disponibles ~ "x35_ts",
    TRUE ~ NA_character_
  )
  
  # Detectar nombres de columnas TC
  col_tc1 <- case_when(
    "tratamiento_comp1" %in% cols_disponibles ~ "tratamiento_comp1",
    "36.TC1" %in% cols_disponibles ~ "36.TC1",
    "x36_tc1" %in% cols_disponibles ~ "x36_tc1",
    TRUE ~ NA_character_
  )
  
  col_tc2 <- case_when(
    "tratamiento_comp2" %in% cols_disponibles ~ "tratamiento_comp2",
    "36.TC2" %in% cols_disponibles ~ "36.TC2",
    "x36_tc2" %in% cols_disponibles ~ "x36_tc2",
    TRUE ~ NA_character_
  )
  
  col_tc3 <- case_when(
    "tratamiento_comp3" %in% cols_disponibles ~ "tratamiento_comp3",
    "36.TC3" %in% cols_disponibles ~ "36.TC3",
    "x36_tc3" %in% cols_disponibles ~ "x36_tc3",
    TRUE ~ NA_character_
  )
  
  # Verificar columnas encontradas
  cat("Columnas detectadas:\n")
  cat(sprintf("  TS:  %s %s\n", col_ts, ifelse(is.na(col_ts), "✖", "✔")))
  cat(sprintf("  TC1: %s %s\n", col_tc1, ifelse(is.na(col_tc1), "✖", "✔")))
  cat(sprintf("  TC2: %s %s\n", col_tc2, ifelse(is.na(col_tc2), "✖", "✔")))
  cat(sprintf("  TC3: %s %s\n\n", col_tc3, ifelse(is.na(col_tc3), "✖", "✔")))
  
  if (is.na(col_ts)) {
    stop("✖ No se encontró columna TS (tratamiento_silvicola / 35.TS)")
  }
  
  # Detectar columnas básicas
  col_rodal <- ifelse("rodal" %in% cols_disponibles, "rodal", "4.SITIO")
  col_muestreo <- ifelse("muestreo" %in% cols_disponibles, "muestreo", "5.SITIO_I")
  col_umm <- ifelse("unidad_manejo" %in% cols_disponibles, "unidad_manejo", "3.UM")
  
  # Verificar que códigos estén cargados
  if (!exists("CODIGOS_TRATAMIENTO")) {
    stop("✖ CODIGOS_TRATAMIENTO no está cargado")
  }
  if (!exists("CODIGOS_TRATAMIENTO_COMP")) {
    stop("✖ CODIGOS_TRATAMIENTO_COMP no está cargado")
  }
  
  # ============================================================================
  # PREPARAR DATOS CON NOMBRES ESTANDARIZADOS
  # ============================================================================
  
  # Seleccionar columnas que existen
  cols_select <- c(col_rodal, col_muestreo, col_umm, col_ts)
  cols_rename <- c("rodal", "muestreo", "unidad_manejo", "ts")
  
  if (!is.na(col_tc1)) {
    cols_select <- c(cols_select, col_tc1)
    cols_rename <- c(cols_rename, "tc1")
  }
  if (!is.na(col_tc2)) {
    cols_select <- c(cols_select, col_tc2)
    cols_rename <- c(cols_rename, "tc2")
  }
  if (!is.na(col_tc3)) {
    cols_select <- c(cols_select, col_tc3)
    cols_rename <- c(cols_rename, "tc3")
  }
  
  df_ts <- inventario_f01 %>%
    select(all_of(cols_select)) %>%
    setNames(cols_rename)
  
  # Agregar columnas TC faltantes como NA
  if (!"tc1" %in% names(df_ts)) df_ts$tc1 <- NA_integer_
  if (!"tc2" %in% names(df_ts)) df_ts$tc2 <- NA_integer_
  if (!"tc3" %in% names(df_ts)) df_ts$tc3 <- NA_integer_
  
  # Convertir a numérico
  df_ts <- df_ts %>%
    mutate(
      ts = as.integer(ts),
      tc1 = as.integer(tc1),
      tc2 = as.integer(tc2),
      tc3 = as.integer(tc3)
    )
  
  n_sitios_total <- nrow(df_ts)
  n_rodales <- n_distinct(df_ts$rodal)
  
  cat(sprintf("Sitios totales: %d\n", n_sitios_total))
  cat(sprintf("Rodales: %d\n\n", n_rodales))
  
  # ============================================================================
  # A. DISTRIBUCIÓN TS
  # ============================================================================
  
  cat("[A] Distribución Tratamiento Silvícola (TS)...\n")
  
  dist_ts <- df_ts %>%
    count(ts, name = "n_sitios") %>%
    filter(!is.na(ts)) %>%
    left_join(CODIGOS_TRATAMIENTO %>% select(codigo, etiqueta),
              by = c("ts" = "codigo")) %>%
    mutate(
      pct = n_sitios / sum(n_sitios) * 100
    ) %>%
    arrange(desc(n_sitios))
  
  print(dist_ts)
  
  # ============================================================================
  # B. DISTRIBUCIÓN TC1, TC2, TC3
  # ============================================================================
  
  cat("\n[B] Distribución Tratamientos Complementarios...\n")
  
  # Función helper para analizar un TC
  analizar_un_tc <- function(df, tc_col, tc_name) {
    df %>%
      count(.data[[tc_col]], name = "n_menciones") %>%
      filter(!is.na(.data[[tc_col]])) %>%
      left_join(CODIGOS_TRATAMIENTO_COMP %>% select(codigo, etiqueta),
                by = setNames("codigo", tc_col)) %>%
      mutate(
        tratamiento = tc_name,
        pct = n_menciones / sum(n_menciones) * 100
      ) %>%
      rename(codigo = !!tc_col) %>%
      select(tratamiento, codigo, etiqueta, n_menciones, pct) %>%
      arrange(desc(n_menciones))
  }
  
  dist_tc1 <- analizar_un_tc(df_ts, "tc1", "TC1")
  dist_tc2 <- analizar_un_tc(df_ts, "tc2", "TC2")
  dist_tc3 <- analizar_un_tc(df_ts, "tc3", "TC3")
  
  dist_tc_all <- bind_rows(dist_tc1, dist_tc2, dist_tc3)
  
  if (nrow(dist_tc_all) > 0) {
    cat("\n  Top 5 por cada TC:\n")
    print(dist_tc_all %>% group_by(tratamiento) %>% slice_head(n = 5))
  } else {
    cat("\n  ⚠️  No hay datos de TC disponibles\n")
  }
  
  # ============================================================================
  # C. RELACIÓN TS ↔ TC (Coherencia)
  # ============================================================================
  
  cat("\n[C] Matriz TS vs TC (principales combinaciones)...\n")
  
  # Calcular TC más frecuentes por cada TS
  relacion_ts_tc <- df_ts %>%
    pivot_longer(cols = starts_with("tc"),
                 names_to = "tipo_tc",
                 values_to = "codigo_tc") %>%
    filter(!is.na(ts), !is.na(codigo_tc)) %>%
    count(ts, codigo_tc, name = "n_menciones") %>%
    # Top 3 TC por TS
    group_by(ts) %>%
    slice_max(order_by = n_menciones, n = 3, with_ties = FALSE) %>%
    ungroup() %>%
    # Agregar etiquetas
    left_join(CODIGOS_TRATAMIENTO %>% select(codigo, ts_etiqueta = etiqueta),
              by = c("ts" = "codigo")) %>%
    left_join(CODIGOS_TRATAMIENTO_COMP %>% select(codigo, tc_etiqueta = etiqueta),
              by = c("codigo_tc" = "codigo")) %>%
    arrange(ts, desc(n_menciones))
  
  if (nrow(relacion_ts_tc) > 0) {
    print(relacion_ts_tc)
  } else {
    cat("  ⚠️  No hay combinaciones TS-TC disponibles\n")
  }
  
  # ============================================================================
  # D. COMPLETITUD TC POR TS
  # ============================================================================
  
  cat("\n[D] Completitud TC1-3 por Tratamiento Silvícola...\n")
  
  completitud_ts <- df_ts %>%
    group_by(ts) %>%
    summarise(
      n_sitios = n(),
      pct_tc1 = sum(!is.na(tc1)) / n() * 100,
      pct_tc2 = sum(!is.na(tc2)) / n() * 100,
      pct_tc3 = sum(!is.na(tc3)) / n() * 100,
      .groups = "drop"
    ) %>%
    left_join(CODIGOS_TRATAMIENTO %>% select(codigo, etiqueta),
              by = c("ts" = "codigo")) %>%
    select(ts, etiqueta, n_sitios, pct_tc1, pct_tc2, pct_tc3)
  
  print(completitud_ts)
  
  # ============================================================================
  # E. ANÁLISIS POR RODAL
  # ============================================================================
  
  cat("\n[E] Tratamientos por rodal (moda)...\n")
  
  ts_por_rodal <- df_ts %>%
    group_by(rodal) %>%
    summarise(
      n_sitios = n(),
      # TS moda
      ts_moda = as.numeric(names(sort(table(ts), decreasing = TRUE)[1])),
      ts_diversidad = n_distinct(ts, na.rm = TRUE),
      # TC1 moda
      tc1_moda = ifelse(sum(!is.na(tc1)) > 0,
                        as.numeric(names(sort(table(tc1), decreasing = TRUE)[1])),
                        NA_real_),
      # Completitud
      pct_tc1 = sum(!is.na(tc1)) / n() * 100,
      pct_tc2 = sum(!is.na(tc2)) / n() * 100,
      pct_tc3 = sum(!is.na(tc3)) / n() * 100,
      .groups = "drop"
    ) %>%
    left_join(CODIGOS_TRATAMIENTO %>% select(codigo, ts_etiqueta = etiqueta),
              by = c("ts_moda" = "codigo")) %>%
    left_join(CODIGOS_TRATAMIENTO_COMP %>% select(codigo, tc1_etiqueta = etiqueta),
              by = c("tc1_moda" = "codigo"))
  
  print(ts_por_rodal)
  
  # ============================================================================
  # F. GRÁFICOS
  # ============================================================================
  
  cat("\n[F] Generando gráficos...\n")
  
  # Gráfico 1: Distribución TS
  p_ts <- dist_ts %>%
    ggplot(aes(x = reorder(etiqueta, n_sitios), y = n_sitios)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = sprintf("%d\n(%.1f%%)", n_sitios, pct)),
              hjust = -0.1, size = 3) +
    coord_flip() +
    labs(
      title = "Distribución de Tratamientos Silvícolas (TS)",
      subtitle = sprintf("N = %d sitios", sum(dist_ts$n_sitios)),
      x = NULL,
      y = "Número de sitios"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
  
  # Gráfico 2: Comparación TC1-3 (solo si hay datos)
  p_tc_comparacion <- NULL
  if (nrow(dist_tc_all) > 0) {
    p_tc_comparacion <- dist_tc_all %>%
      group_by(tratamiento) %>%
      slice_head(n = 5) %>%
      ungroup() %>%
      ggplot(aes(x = reorder(etiqueta, n_menciones), 
                 y = n_menciones, 
                 fill = tratamiento)) +
      geom_col() +
      geom_text(aes(label = n_menciones), 
                hjust = -0.2, size = 3) +
      coord_flip() +
      facet_wrap(~tratamiento, scales = "free_y", ncol = 1) +
      scale_fill_manual(values = c("TC1" = "#1f77b4", 
                                   "TC2" = "#ff7f0e", 
                                   "TC3" = "#2ca02c")) +
      labs(
        title = "Top 5 Tratamientos Complementarios por tipo",
        x = NULL,
        y = "Número de menciones",
        fill = "Tipo"
      ) +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "none"
      )
  }
  
  # Gráfico 3: Completitud TC por TS
  p_completitud <- completitud_ts %>%
    pivot_longer(cols = starts_with("pct_"),
                 names_to = "tc_tipo",
                 values_to = "pct") %>%
    mutate(
      tc_tipo = str_replace(tc_tipo, "pct_", ""),
      tc_tipo = toupper(tc_tipo)
    ) %>%
    ggplot(aes(x = etiqueta, y = pct, fill = tc_tipo)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = sprintf("%.0f%%", pct)),
              position = position_dodge(width = 0.9),
              vjust = -0.3, size = 3) +
    scale_fill_manual(values = c("TC1" = "#1f77b4", 
                                 "TC2" = "#ff7f0e", 
                                 "TC3" = "#2ca02c")) +
    labs(
      title = "Completitud TC1-3 por Tratamiento Silvícola",
      x = "Tratamiento Silvícola",
      y = "% sitios con dato",
      fill = "TC"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
  
  # ============================================================================
  # G. EXPORTAR RESULTADOS
  # ============================================================================
  
  if (exportar_csv_flag) {
    cat("\n[G] Exportando CSV...\n")
    exportar_csv(dist_ts, "desc_14_ts_distribucion")
    if (nrow(dist_tc_all) > 0) {
      exportar_csv(dist_tc_all, "desc_15_tc_distribucion")
    }
    if (nrow(relacion_ts_tc) > 0) {
      exportar_csv(relacion_ts_tc, "desc_16_ts_tc_relacion")
    }
    exportar_csv(completitud_ts, "desc_17_ts_completitud")
    exportar_csv(ts_por_rodal, "desc_18_ts_por_rodal")
  }
  
  if (exportar_latex) {
    cat("\n[H] Exportando LaTeX y gráficos...\n")
    
    # Tabla TS
    xtable(dist_ts %>% select(-ts),
           caption = "Distribución de Tratamientos Silvícolas",
           label = "tab:ts_dist",
           align = c("l", "l", "r", "r"),
           digits = c(0, 0, 0, 1)) %>%
      print(file = "tablas_latex/desc_14_ts_distribucion.tex",
            include.rownames = FALSE,
            floating = TRUE,
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    # Tabla relación TS-TC (solo si hay datos)
    if (nrow(relacion_ts_tc) > 0) {
      xtable(relacion_ts_tc %>% 
               select(ts, ts_etiqueta, tc_etiqueta, n_menciones) %>%
               head(15),
             caption = "Principales combinaciones TS + TC",
             label = "tab:ts_tc",
             align = c("l", "r", "p{4cm}", "p{4cm}", "r")) %>%
        print(file = "tablas_latex/desc_15_ts_tc_relacion.tex",
              include.rownames = FALSE,
              floating = TRUE,
              booktabs = TRUE,
              sanitize.text.function = identity)
    }
    
    # Gráficos
    ggsave("graficos/desc_13_ts_distribucion.png", p_ts,
           width = 10, height = 6, dpi = 300)
    
    if (!is.null(p_tc_comparacion)) {
      ggsave("graficos/desc_14_tc_comparacion.png", p_tc_comparacion,
             width = 10, height = 10, dpi = 300)
    }
    
    ggsave("graficos/desc_15_completitud.png", p_completitud,
           width = 10, height = 6, dpi = 300)
    
    cat("  ✔ Tablas LaTeX y gráficos exportados\n")
  }
  
  cat("\n✔ Análisis de tratamientos completado\n\n")
  
  return(list(
    dist_ts = dist_ts,
    dist_tc = dist_tc_all,
    relacion_ts_tc = relacion_ts_tc,
    completitud_ts = completitud_ts,
    ts_por_rodal = ts_por_rodal,
    graficos = list(
      p_ts = p_ts,
      p_tc_comparacion = p_tc_comparacion,
      p_completitud = p_completitud
    )
  ))
}

cat("✔ Función analizar_tratamientos_silvicolas CORREGIDA cargada\n")
cat("  - Detecta automáticamente nombres de columnas\n")
cat("  - Maneja TC faltantes (a veces solo 1 o 2 TC)\n")
cat("  - Compatible con Excel directo o post-importación\n\n")


# ==============================================================================
# FUNCIÃ“N MAESTRA - ACTUALIZADA CON CORRECCIÃ“N n_sitios
# ==============================================================================

analisis_descriptivo_completo <- function(inventario, arboles_df, config = CONFIG,
                                          exportar_csv_flag = TRUE) {
  
  cat("\nâ•”â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•—\n")
  cat("â•‘     ANÃLISIS DESCRIPTIVO COMPLETO (AB/ha + CSV)           â•‘\n")
  cat("â•šâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n")
  
  resultados <- list()
  
  cat("\n[1/8] Estructura poblacional (con AB/ha - CORREGIDO n_sitios)...\n")
  resultados$estructura <- analizar_estructura_poblacional(arboles_df, config, 
                                                           exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[2/8] ComposiciÃ³n Pinus/Quercus por rodal (CORREGIDO)...\n")
  resultados$composicion_generopq_rodal <- analizar_composicion_generopq_por_rodal(arboles_df, config,
                                                                                   exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[3/8] AB por dominancia (CORREGIDO)...\n")
  resultados$ab_dominancia <- analizar_ab_por_dominancia(arboles_df, config,
                                                         exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[4/8] DistribuciÃ³n diamÃ©trica (CORREGIDO)...\n")
  resultados$distribucion <- analizar_distribucion_diametrica(arboles_df, config,
                                                              exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[5/8] Condiciones de erosiÃ³n (% sitios)...\n")
  resultados$erosion <- analizar_erosion(inventario$f01, exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[6/8] Condiciones sanitarias (CORREGIDO)...\n")
  resultados$sanidad <- analizar_sanidad(arboles_df, config, exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[7/8] RegeneraciÃ³n (Pinus/Quercus)...\n")
  resultados$regeneracion <- analizar_regeneracion(inventario$f05, inventario$f01, config,
                                                   exportar_csv_flag = exportar_csv_flag)
  
  
  cat("\n[8/8] Tratamientos silvícolas y complementarios...\n")
  resultados$tratamientos <- analizar_tratamientos_silvicolas(inventario$f01,
                                                               exportar_csv_flag = exportar_csv_flag)
  
  cat("\nâ•”â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•—\n")
  cat("â•‘           âœ“ ANÃLISIS DESCRIPTIVO COMPLETADO               â•‘\n")
  cat("â•šâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n\n")
  
  cat("CORRECCIÃ“N CRÃTICA APLICADA:\n")
  cat("â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n")
  cat("  âœ“ n_sitios usa TODOS los sitios muestreados\n")
  cat("  âœ“ No excluye sitios sin Ã¡rboles vivos\n")
  cat("  âœ“ MÃ©tricas /ha calculadas correctamente\n")
  cat("  âœ“ Evita sobrestimaciÃ³n de densidad/AB/volumen\n\n")
  
  cat("OTRAS CARACTERÃSTICAS:\n")
  cat("â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n")
  cat("  âœ“ TODAS las tablas usan SOLO Ã¡rboles vivos\n")
  cat("  âœ“ Todas las tablas incluyen AB/ha y DMC\n")
  cat("  âœ“ ComposiciÃ³n Pinus/Quercus por rodal\n")
  cat("  âœ“ AB por dominancia (incluye muertos, tocones)\n")
  cat("  âœ“ ExportaciÃ³n CSV en carpeta resultados/\n")
  cat("  âœ“ ErosiÃ³n: % de sitios afectados por rodal\n")
  cat("  âœ“ Sanidad: Solo Ã¡rboles vivos, paleta accesible\n")
  cat("  âœ“ RegeneraciÃ³n: Solo Pinus/Quercus + densidad=0\n\n")
  
  cat("ARCHIVOS GENERADOS:\n")
  cat("â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n")
  cat("  ðŸ“Š Tablas LaTeX (tablas_latex/): desc_01 a desc_11\n")
  cat("  ðŸ“ˆ GrÃ¡ficos PNG (graficos/): desc_01 a desc_12\n")
  cat("  ðŸ“„ CSVs (resultados/): desc_01 a desc_13\n\n")
  
  return(resultados)
}

cat("\nâœ“ MÃ³dulo de anÃ¡lisis descriptivo CORREGIDO cargado\n")
cat("  CORRECCIÃ“N CRÃTICA:\n")
cat("    âš ï¸  n_sitios ahora usa TODOS los sitios muestreados\n")
cat("    âš ï¸  Evita sobrestimaciÃ³n al excluir sitios sin Ã¡rboles\n")
cat("    âš ï¸  Aplica a: estructura, composiciÃ³n PQ, AB dominancia,\n")
cat("                 distribuciÃ³n diamÃ©trica, sanidad\n\n")
cat("  OTRAS CARACTERÃSTICAS:\n")
cat("    1. TODAS las tablas usan SOLO Ã¡rboles vivos\n")
cat("    2. Todas las tablas incluyen AB/ha y DMC\n")
cat("    3. ComposiciÃ³n Pinus/Quercus por rodal (desc_11)\n")
cat("    4. AB por dominancia - incluye muertos (desc_12, desc_13)\n")
cat("    5. ExportaciÃ³n automÃ¡tica de CSV en resultados/\n\n")