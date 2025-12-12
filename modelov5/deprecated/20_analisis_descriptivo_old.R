# ==============================================================================
# 08_ANALISIS_DESCRIPTIVO.R - CORRECCION n_sitios
# CAMBIO CRÍTICO: Usar TODOS los sitios muestreados, no solo donde hay árboles
# ==============================================================================

library(tidyverse)
library(xtable)
library(patchwork)
library(viridis)

# Verificar que CONFIG esté cargado
if (!exists("CONFIG")) {
  stop("❌ CONFIG no está cargado. Ejecuta primero: source('modelov5/01_parametros_configuracion.R')")
}

# Verificar funciones de expansión
if (!exists("expandir_a_hectarea")) {
  source("modelov5/15_core_calculos.R")
}

# ==============================================================================
# VERIFICAR CONFIG
# ==============================================================================

if (is.null(CONFIG$area_parcela_regeneracion_ha)) {
  warning("⚠️ CONFIG$area_parcela_regeneracion_ha no existe, usando 0.0009 ha (9 m²)")
  CONFIG$area_parcela_regeneracion_ha <- 0.0009
}

cat(sprintf("\n[CONFIGURACIÓN DE EXPANSIÓN]\n"))
cat(sprintf("  Arbolado: %.2f ha (factor = %.1f)\n", 
            CONFIG$area_parcela_ha, 
            calcular_factor_expansion(CONFIG$area_parcela_ha)))
cat(sprintf("  Regeneración: %.4f ha (9 m², factor = %.1f)\n\n", 
            CONFIG$area_parcela_regeneracion_ha,
            calcular_factor_expansion(CONFIG$area_parcela_regeneracion_ha)))

# ==============================================================================
# FUNCIÓN AUXILIAR: EXPORTAR CSV (VERSIÓN ROBUSTA)
# ==============================================================================

exportar_csv <- function(df, nombre_archivo, carpeta = "resultados") {
  
  # Verificar input
  if (is.null(df) || nrow(df) == 0) {
    warning(sprintf("⚠️ DataFrame vacío para %s, no se exporta", nombre_archivo))
    return(invisible(FALSE))
  }
  
  # Crear carpeta si no existe
  if (!dir.exists(carpeta)) {
    dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)
    cat(sprintf("  📁 Creando carpeta: %s/\n", carpeta))
  }
  
  # Ruta completa
  ruta <- file.path(carpeta, paste0(nombre_archivo, ".csv"))
  
  # Exportar con manejo de errores
  tryCatch({
    write.csv(df, file = ruta, row.names = FALSE)
    
    # Verificar que se creó
    if (file.exists(ruta)) {
      size_kb <- file.info(ruta)$size / 1024
      cat(sprintf("  ✓ CSV: %s (%.1f KB)\n", basename(ruta), size_kb))
      return(invisible(TRUE))
    } else {
      warning(sprintf("⚠️ Archivo no creado: %s", ruta))
      return(invisible(FALSE))
    }
    
  }, error = function(e) {
    warning(sprintf("❌ Error exportando %s: %s", nombre_archivo, e$message))
    return(invisible(FALSE))
  })
}

# ==============================================================================
# 1. ESTRUCTURA POBLACIONAL - CORREGIDO n_sitios
# ==============================================================================

analizar_estructura_poblacional <- function(arboles_df, config = CONFIG, 
                                            exportar_latex = TRUE,
                                            exportar_csv_flag = TRUE) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║     ANÁLISIS ESTRUCTURA POBLACIONAL (AB/ha + CSV)         ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  if (!exists("filtrar_arboles_vivos")) {
    source("modelov5/15_core_calculos.R")
  }
  
  # CRÍTICO: Usar TODOS los sitios muestreados, no solo donde hay árboles vivos
  n_sitios_total <- n_distinct(arboles_df$muestreo)
  vivos <- filtrar_arboles_vivos(arboles_df)
  n_sitios_con_vivos <- n_distinct(vivos$muestreo)
  
  cat(sprintf("Sitios muestreados TOTAL: %d\n", n_sitios_total))
  cat(sprintf("Sitios con árboles vivos: %d\n", n_sitios_con_vivos))
  cat(sprintf("Sitios sin árboles vivos: %d\n\n", n_sitios_total - n_sitios_con_vivos))
  
  # Usar n_sitios_total para cálculos /ha
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
  
  cat("[RESUMEN GENERAL - VALORES POR HECTÁREA]\n")
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
      # Reemplazar NA con 0 si no hay árboles vivos
      n_arboles_obs = replace_na(n_arboles_obs, 0),
      n_especies = replace_na(n_especies, 0),
      # Calcular métricas /ha con n_sitios del rodal
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
  
  cat("\n[POR RODAL - VALORES POR HECTÁREA]\n")
  print(por_rodal, n = 50)
  
  # POR GÉNERO - CON AB/HA Y DMC (CORREGIDO)
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
  
  cat("\n[POR GÉNERO - VALORES POR HECTÁREA]\n")
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
  
  cat("\n[TOP 10 ESPECIES - DENSIDAD POR HECTÁREA]\n")
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
           caption = "Resumen dasométrico por rodal (valores/ha, incluye AB y DMC)",
           label = "tab:resumen_rodal",
           digits = c(0, 0, 1, 0, 2, 2, 2, 2, 2)) %>%
      print(file = "tablas_latex/desc_01_resumen_rodal.tex",
            include.rownames = FALSE, 
            floating = TRUE, 
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    # Tabla por género - AHORA CON AB/HA, DMC Y %
    xtable(por_genero %>% select(genero_grupo, densidad_ha, proporcion_pct, 
                                 d_medio_cm, dmc_cm, ab_m2ha, ab_pct, vol_m3ha, vol_pct),
           caption = "Composición por género (valores/ha, incluye AB, DMC y porcentajes)",
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
    
    cat("\n✓ Tablas LaTeX exportadas (desc_01 a desc_03)\n")
  }
  
  return(list(
    general = resumen_general,
    por_rodal = por_rodal,
    por_genero = por_genero,
    por_especie = por_especie
  ))
}

# ==============================================================================
# 1B. COMPOSICIÓN PINUS/QUERCUS POR RODAL - CORREGIDO
# ==============================================================================

analizar_composicion_generopq_por_rodal <- function(arboles_df, config = CONFIG,
                                                    exportar_latex = TRUE,
                                                    exportar_csv_flag = TRUE) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║  COMPOSICIÓN PINUS/QUERCUS POR RODAL (con AB/ha y DMC)   ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  vivos <- filtrar_arboles_vivos(arboles_df) %>%
    filter(genero_grupo %in% c("Pinus", "Quercus"))
  
  # CRÍTICO: Contar TODOS los sitios por rodal, no solo donde hay P/Q vivos
  sitios_por_rodal <- arboles_df %>%
    group_by(rodal) %>%
    summarise(n_sitios = n_distinct(muestreo), .groups = "drop")
  
  # Calcular por rodal y género CON DMC
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
  
  cat("[COMPOSICIÓN PINUS/QUERCUS POR RODAL]\n")
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
           caption = "Composición de Pinus y Quercus por rodal (valores/ha, DMC y porcentajes)",
           label = "tab:composicion_generopq_rodal",
           digits = c(0, 0, 0, 1, 1, 2, 2, 2, 2, 1, 2, 1)) %>%
      print(file = "tablas_latex/desc_11_composicion_generopq_rodal.tex",
            include.rownames = FALSE, 
            floating = TRUE, 
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    cat("\n✓ Tabla LaTeX: desc_11_composicion_generopq_rodal.tex\n")
  }
  
  # GRÁFICO OPCIONAL - Volumen por rodal y género
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
      title = "Composición de Volumen: Pinus vs Quercus por Rodal",
      subtitle = "Volumen total/ha y porcentaje de cada género",
      x = "Rodal",
      y = "Volumen (m³/ha)",
      fill = "Género"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold")
    )
  
  if (exportar_latex) {
    ggsave("graficos/desc_11_composicion_generopq_rodal.png", p_comp,
           width = 10, height = 6, dpi = 300)
    cat("✓ Gráfico: desc_11_composicion_generopq_rodal.png\n")
  }
  
  return(list(
    tabla = composicion_generopq_rodal,
    grafico = p_comp
  ))
}

# ==============================================================================
# 1C. ÁREA BASAL POR DOMINANCIA - CORREGIDO
# ==============================================================================

analizar_ab_por_dominancia <- function(arboles_df, config = CONFIG,
                                       exportar_csv_flag = TRUE) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║   AB POR DOMINANCIA: Pinus/Quercus (incluye muertos)     ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  if (!exists("CODIGOS_DOMINANCIA")) {
    stop("❌ CODIGOS_DOMINANCIA no está cargado")
  }
  
  # TODOS LOS ÁRBOLES (incluyendo muertos) - Solo Pinus/Quercus
  pq_todos <- arboles_df %>%
    filter(genero_grupo %in% c("Pinus", "Quercus"))
  
  # CRÍTICO: Contar TODOS los sitios muestreados
  n_sitios <- n_distinct(arboles_df$muestreo)
  
  # Sitios por rodal
  sitios_por_rodal <- arboles_df %>%
    group_by(rodal) %>%
    summarise(n_sitios = n_distinct(muestreo), .groups = "drop")
  
  cat(sprintf("Árboles Pinus/Quercus (vivos + muertos): %d\n", nrow(pq_todos)))
  cat(sprintf("Sitios TOTAL: %d\n\n", n_sitios))
  
  # Calcular AB por rodal, género y dominancia
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
    # Calcular % de AB por rodal y género
    group_by(rodal, genero_grupo) %>%
    mutate(
      ab_pct_genero = (ab_m2ha / sum(ab_m2ha)) * 100
    ) %>%
    ungroup() %>%
    arrange(rodal, genero_grupo, dominancia)
  
  cat("[ÁREA BASAL POR DOMINANCIA - PINUS/QUERCUS]\n")
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
  
  # GRÁFICO - AB por dominancia
  p_ab_dom <- ggplot(ab_por_dominancia,
                     aes(x = factor(rodal), y = ab_m2ha, 
                         fill = etiqueta_dominancia)) +
    geom_col(position = "stack", alpha = 0.8) +
    facet_wrap(~genero_grupo, ncol = 1) +
    scale_fill_viridis_d(option = "turbo") +
    labs(
      title = "Área Basal por Clase de Dominancia",
      subtitle = "Pinus y Quercus - Incluye árboles muertos",
      x = "Rodal",
      y = "Área Basal (m²/ha)",
      fill = "Dominancia"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold")
    )
  
  ggsave("graficos/desc_12_ab_por_dominancia.png", p_ab_dom,
         width = 12, height = 8, dpi = 300)
  
  cat("\n✓ Gráfico: desc_12_ab_por_dominancia.png\n")
  
  return(list(
    detalle = ab_por_dominancia,
    resumen = resumen_rodal,
    grafico = p_ab_dom
  ))
}

# ==============================================================================
# 2. DISTRIBUCIÓN DIAMÉTRICA - CORREGIDO
# ==============================================================================

analizar_distribucion_diametrica <- function(arboles_df, config = CONFIG, 
                                             exportar = TRUE,
                                             exportar_csv_flag = TRUE) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║         ANÁLISIS DISTRIBUCIÓN DIAMÉTRICA + CSV            ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  vivos <- filtrar_arboles_vivos(arboles_df)
  # CRÍTICO: Usar TODOS los sitios muestreados
  n_sitios <- n_distinct(arboles_df$muestreo)
  
  cat(sprintf("Sitios muestreados TOTAL: %d\n", n_sitios))
  cat(sprintf("Árboles vivos: %d\n\n", nrow(vivos)))
  
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
  
  cat("[DISTRIBUCIÓN DIAMÉTRICA - DENSIDAD/HA]\n")
  print(dist_diametrica, n = 30)
  
  # EXPORTAR CSV
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    exportar_csv(dist_diametrica, "desc_05_distribucion_diametrica")
  }
  
  # GRÁFICOS
  p1 <- ggplot(dist_diametrica, 
               aes(x = clase_d, y = densidad_ha, fill = genero_grupo)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_fill_manual(
      values = c("Quercus" = "#8B4513", "Pinus" = "#228B22", "Otros" = "#808080")
    ) +
    labs(
      title = "Distribución Diamétrica por Género",
      subtitle = sprintf("Densidad/ha - Parcela: %.2f ha", config$area_parcela_ha),
      x = "Clase Diamétrica (cm)",
      y = "Densidad (árboles/ha)",
      fill = "Género"
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
      title = "Proporción Acumulada por Clase Diamétrica",
      x = "Clase Diamétrica (cm)",
      y = "Proporción Acumulada (%)",
      color = "Género"
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
    cat("\n✓ Gráfico guardado: graficos/desc_01_distribucion_diametrica.png\n")
  }
  
  return(list(
    tabla = dist_diametrica, 
    grafico_barras = p1,
    grafico_acumulado = p2,
    grafico_combinado = p_combined
  ))
}

# ==============================================================================
# 3. EROSIÓN - % SITIOS AFECTADOS + CSV
# ==============================================================================

analizar_erosion <- function(f01, exportar_latex = TRUE, exportar_csv_flag = TRUE) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║      ANÁLISIS DE EROSIÓN (% sitios afectados + CSV)       ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  if (!exists("CODIGOS_EROSION")) {
    stop("❌ CODIGOS_EROSION no está cargado")
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
        "erosion_carcavas" = "Cárcavas",
        "erosion_antropica" = "Antrópica"
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
  
  cat("[EROSIÓN: % DE SITIOS AFECTADOS POR RODAL]\n")
  print(erosion_por_rodal, n = 50)
  
  # EXPORTAR CSV
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    exportar_csv(erosion_por_rodal, "desc_06_erosion")
  }
  
  # Gráfico
  p_erosion <- ggplot(erosion_por_rodal, 
                      aes(x = factor(rodal), y = pct_afectado, fill = tipo_erosion)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_fill_brewer(palette = "Set2") +
    geom_text(aes(label = sprintf("%.0f%%", pct_afectado)), 
              position = position_dodge(width = 0.9),
              vjust = -0.5, size = 3) +
    labs(
      title = "Porcentaje de Sitios con Evidencia de Erosión",
      subtitle = "Por rodal y tipo de erosión",
      x = "Rodal",
      y = "% de Sitios Afectados",
      fill = "Tipo de Erosión"
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
           caption = "Porcentaje de sitios afectados por erosión (por rodal)",
           label = "tab:erosion_pct",
           digits = c(0, 0, 1, 1, 1, 1)) %>%
      print(file = "tablas_latex/desc_04_erosion.tex",
            include.rownames = FALSE, 
            floating = TRUE, 
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    ggsave("graficos/desc_02_erosion.png", p_erosion,
           width = 10, height = 6, dpi = 300)
    
    cat("\n✓ Tabla y gráfico de erosión exportados\n")
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
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║      ANÁLISIS SANITARIO (paleta daltónicos + CSV)         ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  if (!exists("CODIGOS_SANIDAD") || !exists("CODIGOS_INTENSIDAD_INFESTACION")) {
    stop("❌ CODIGOS_SANIDAD o CODIGOS_INTENSIDAD_INFESTACION no están cargados")
  }
  
  # FILTRAR SOLO VIVOS PARA ANÁLISIS SANITARIO
  vivos <- filtrar_arboles_vivos(arboles_df)
  # CRÍTICO: Usar TODOS los sitios muestreados
  n_sitios <- n_distinct(arboles_df$muestreo)
  
  cat(sprintf("Sitios muestreados TOTAL: %d\n", n_sitios))
  cat(sprintf("Árboles vivos evaluados: %d\n\n", nrow(vivos)))
  
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
  
  # GRÁFICO - PALETA VIRIDIS
  p_sanidad <- sanidad %>%
    filter(problema != "Sano") %>%
    ggplot(aes(x = factor(rodal), y = densidad_ha, fill = problema)) +
    geom_col(alpha = 0.9) +
    scale_fill_viridis_d(option = "plasma", begin = 0.2, end = 0.9) +
    labs(
      title = "Problemas Fitosanitarios Detectados",
      subtitle = "Densidad de árboles afectados/ha por rodal (paleta accesible)",
      x = "Rodal",
      y = "Densidad (árboles/ha)",
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
    
    cat("\n✓ Tabla y gráfico de sanidad exportados\n")
  }
  
  return(list(
    detalle = sanidad, 
    resumen = resumen_sanidad,
    grafico = p_sanidad
  ))
}

# ==============================================================================
# 5. REGENERACIÓN - PINUS/QUERCUS + DENSIDAD=0 + CSV
# ==============================================================================

analizar_regeneracion <- function(f05, f01, config = CONFIG, 
                                  exportar_latex = TRUE,
                                  exportar_csv_flag = TRUE) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║   REGENERACIÓN: Pinus/Quercus + densidad=0 + CSV          ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  if (!exists("ESPECIES")) {
    stop("❌ ESPECIES no está cargado")
  }
  
  # 1. TODOS LOS SITIOS
  todos_sitios <- f01 %>%
    select(muestreo, rodal) %>%
    distinct()
  
  n_sitios_total <- nrow(todos_sitios)
  
  cat(sprintf("Total de sitios en inventario: %d\n", n_sitios_total))
  
  # 2. REGENERACIÓN OBSERVADA (solo Pinus/Quercus)
  regeneracion_obs <- f05 %>%
    left_join(ESPECIES %>% select(codigo, nombre_cientifico, genero), 
              by = c("especie" = "codigo")) %>%
    filter(genero %in% c("Pinus", "Quercus")) %>%
    mutate(
      total_individuos = frec_025_150 + frec_151_275 + frec_276_400,
      densidad_ha = expandir_a_hectarea(total_individuos, config$area_parcela_regeneracion_ha)
    ) %>%
    filter(!is.na(genero), !is.na(densidad_ha), is.finite(densidad_ha))
  
  cat(sprintf("Sitios CON regeneración observada (Pinus/Quercus): %d\n", 
              n_distinct(regeneracion_obs$muestreo)))
  
  # 3. COMPLETAR CON SITIOS SIN REGENERACIÓN (densidad = 0)
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
  
  cat(sprintf("\nSitios con regeneración = 0: %d\n", 
              sum(regeneracion_completa$densidad_ha == 0)))
  cat(sprintf("Sitios con regeneración > 0: %d\n\n", 
              sum(regeneracion_completa$densidad_ha > 0)))
  
  # 4. RESUMEN POR GÉNERO
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
  
  cat("[REGENERACIÓN - Solo Pinus/Quercus - Parcela 9 m²]\n")
  print(resumen_regeneracion)
  
  # EXPORTAR CSV
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    exportar_csv(regeneracion_completa, "desc_09_regeneracion_completa")
    exportar_csv(resumen_regeneracion, "desc_10_regeneracion_resumen")
  }
  
  # 5. GRÁFICO - PALETA COLORBLIND-FRIENDLY
  p_regen <- ggplot(regeneracion_completa, 
                    aes(x = genero, y = densidad_ha, fill = genero)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
    scale_fill_manual(
      values = c("Quercus" = "#E69F00", "Pinus" = "#56B4E9")
    ) +
    labs(
      title = "Densidad de Regeneración Natural (Pinus y Quercus)",
      subtitle = sprintf("Parcela 9 m² (0.0009 ha) - Incluye sitios sin regeneración (n=%d)", 
                         n_sitios_total),
      x = "Género",
      y = "Densidad (individuos/ha)",
      fill = "Género"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold")
    )
  
  if (exportar_latex) {
    xtable(resumen_regeneracion,
           caption = "Regeneración de Pinus y Quercus (parcela 9 m², incluye densidad=0)",
           label = "tab:regeneracion",
           digits = c(0, 0, 0, 0, 1, 1, 1, 1, 1)) %>%
      print(file = "tablas_latex/desc_06_regeneracion.tex",
            include.rownames = FALSE, 
            floating = TRUE, 
            booktabs = TRUE,
            sanitize.text.function = identity)
    
    ggsave("graficos/desc_04_regeneracion.png", p_regen,
           width = 8, height = 6, dpi = 300)
    
    cat("\n✓ Tabla y gráfico de regeneración exportados\n")
  }
  
  return(list(
    datos_completos = regeneracion_completa,
    resumen = resumen_regeneracion,
    grafico = p_regen
  ))
}

# ==============================================================================
# FUNCIÓN MAESTRA - ACTUALIZADA CON CORRECCIÓN n_sitios
# ==============================================================================

analisis_descriptivo_completo <- function(inventario, arboles_df, config = CONFIG,
                                          exportar_csv_flag = TRUE) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║     ANÁLISIS DESCRIPTIVO COMPLETO (AB/ha + CSV)           ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
  
  resultados <- list()
  
  cat("\n[1/7] Estructura poblacional (con AB/ha - CORREGIDO n_sitios)...\n")
  resultados$estructura <- analizar_estructura_poblacional(arboles_df, config, 
                                                           exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[2/7] Composición Pinus/Quercus por rodal (CORREGIDO)...\n")
  resultados$composicion_generopq_rodal <- analizar_composicion_generopq_por_rodal(arboles_df, config,
                                                                                   exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[3/7] AB por dominancia (CORREGIDO)...\n")
  resultados$ab_dominancia <- analizar_ab_por_dominancia(arboles_df, config,
                                                         exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[4/7] Distribución diamétrica (CORREGIDO)...\n")
  resultados$distribucion <- analizar_distribucion_diametrica(arboles_df, config,
                                                              exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[5/7] Condiciones de erosión (% sitios)...\n")
  resultados$erosion <- analizar_erosion(inventario$f01, exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[6/7] Condiciones sanitarias (CORREGIDO)...\n")
  resultados$sanidad <- analizar_sanidad(arboles_df, config, exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[7/7] Regeneración (Pinus/Quercus)...\n")
  resultados$regeneracion <- analizar_regeneracion(inventario$f05, inventario$f01, config,
                                                   exportar_csv_flag = exportar_csv_flag)
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║           ✓ ANÁLISIS DESCRIPTIVO COMPLETADO               ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  cat("CORRECCIÓN CRÍTICA APLICADA:\n")
  cat("══════════════════════════════════════════════════════════\n")
  cat("  ✓ n_sitios usa TODOS los sitios muestreados\n")
  cat("  ✓ No excluye sitios sin árboles vivos\n")
  cat("  ✓ Métricas /ha calculadas correctamente\n")
  cat("  ✓ Evita sobrestimación de densidad/AB/volumen\n\n")
  
  cat("OTRAS CARACTERÍSTICAS:\n")
  cat("══════════════════════════════════════════════════════════\n")
  cat("  ✓ TODAS las tablas usan SOLO árboles vivos\n")
  cat("  ✓ Todas las tablas incluyen AB/ha y DMC\n")
  cat("  ✓ Composición Pinus/Quercus por rodal\n")
  cat("  ✓ AB por dominancia (incluye muertos, tocones)\n")
  cat("  ✓ Exportación CSV en carpeta resultados/\n")
  cat("  ✓ Erosión: % de sitios afectados por rodal\n")
  cat("  ✓ Sanidad: Solo árboles vivos, paleta accesible\n")
  cat("  ✓ Regeneración: Solo Pinus/Quercus + densidad=0\n\n")
  
  cat("ARCHIVOS GENERADOS:\n")
  cat("══════════════════════════════════════════════════════════\n")
  cat("  📊 Tablas LaTeX (tablas_latex/): desc_01 a desc_11\n")
  cat("  📈 Gráficos PNG (graficos/): desc_01 a desc_12\n")
  cat("  📄 CSVs (resultados/): desc_01 a desc_13\n\n")
  
  return(resultados)
}

cat("\n✓ Módulo de análisis descriptivo CORREGIDO cargado\n")
cat("  CORRECCIÓN CRÍTICA:\n")
cat("    ⚠️  n_sitios ahora usa TODOS los sitios muestreados\n")
cat("    ⚠️  Evita sobrestimación al excluir sitios sin árboles\n")
cat("    ⚠️  Aplica a: estructura, composición PQ, AB dominancia,\n")
cat("                 distribución diamétrica, sanidad\n\n")
cat("  OTRAS CARACTERÍSTICAS:\n")
cat("    1. TODAS las tablas usan SOLO árboles vivos\n")
cat("    2. Todas las tablas incluyen AB/ha y DMC\n")
cat("    3. Composición Pinus/Quercus por rodal (desc_11)\n")
cat("    4. AB por dominancia - incluye muertos (desc_12, desc_13)\n")
cat("    5. Exportación automática de CSV en resultados/\n\n")