# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# ==============================================================================
# 20_ANALISIS_DESCRIPTIVO.R - MÉTODO DE DENSIDAD GLOBAL CORREGIDO
# 
# CAMBIO CRÍTICO:
# - Densidad global por UMM: total_arboles / (n_sitios × 0.1 ha)
# - AB/ha: suma_AB_individuales / (n_sitios × 0.1 ha)
# - Vol/ha: suma_Vol_individuales / (n_sitios × 0.1 ha)
# - IC calculados correctamente para cada métrica
# ==============================================================================

library(tidyverse)
library(xtable)
library(patchwork)
library(viridis)

# Verificar que CONFIG esté cargado
if (!exists("CONFIG")) {
  stop("❌ CONFIG no está cargado. Ejecuta primero: source(file.path(PROYECTO_ROOT, 'config/01_parametros_configuracion.R'))")
}

# Verificar funciones de expansión
if (!exists("expandir_a_hectarea")) {
  source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
}

# Cargar utilidades de I/O (formateo y exportación)
if (!exists("formatear_3_cifras_sig")) {
  source(file.path(PROYECTO_ROOT, "utils/io.R"))
}

# ==============================================================================
# VERIFICAR CONFIG
# ==============================================================================

if (is.null(CONFIG$area_parcela_regeneracion_ha)) {
  warning("⚠️ CONFIG$area_parcela_regeneracion_ha no existe, usando 0.0009 ha (9 m²)")
  CONFIG$area_parcela_regeneracion_ha <- 0.0009
}

cat(sprintf("\n[CONFIGURACIÓN DE EXPANSIÓN - MÉTODO GLOBAL]\n"))
cat(sprintf("  Arbolado: %.2f ha/sitio (factor = %.1f)\n", 
            CONFIG$area_parcela_ha, 
            calcular_factor_expansion(CONFIG$area_parcela_ha)))
cat(sprintf("  Regeneración: %.4f ha (9 m², factor = %.1f)\n", 
            CONFIG$area_parcela_regeneracion_ha,
            calcular_factor_expansion(CONFIG$area_parcela_regeneracion_ha)))
cat(sprintf("\n  MÉTODO: Densidad global por UMM\n"))
cat(sprintf("  Fórmula: N_total / (n_sitios × %.2f ha)\n\n", CONFIG$area_parcela_ha))

# ==============================================================================
# FUNCIÓN CORE: CALCULAR MÉTRICAS GLOBALES POR UMM
# ==============================================================================

#' Calcular métricas dasométricas globales por UMM
#' 
#' @param arboles_df Data frame con árboles
#' @param group_vars Variables de agrupación (ej: c("rodal", "genero_grupo"))
#' @param config Configuración con area_parcela_ha
#' @return Data frame con métricas globales
#' 
#' @details
#' MÉTODO CORRECTO:
#' 1. n_sitios = TODOS los sitios del rodal (no solo donde está la especie)
#' 2. Contar TODOS los árboles del grupo
#' 3. Sumar TODAS las AB del grupo
#' 4. Sumar TODOS los volúmenes del grupo
#' 5. Área total = n_sitios_total_rodal × 0.1 ha
#' 6. Métricas/ha = valores totales / área total
#' 
#' Ejemplo UMM 1, Pinus teocote:
#' - 9 árboles (en 3 sitios donde está presente)
#' - 16 sitios TOTALES en UMM 1
#' - Área total = 16 × 0.1 = 1.6 ha
#' - Densidad = 9 / 1.6 = 5.6 árb/ha
#' 
calcular_metricas_globales <- function(arboles_df, group_vars, config = CONFIG) {
  
  # PASO 1: Calcular n_sitios TOTALES por rodal
  # (Esto es CRÍTICO: incluye todos los sitios, no solo donde hay la especie)
  sitios_por_rodal <- arboles_df %>%
    group_by(rodal) %>%
    summarise(n_sitios_total = n_distinct(muestreo), .groups = "drop")
  
  # PASO 2: Si no hay agrupación por rodal, usar total global
  if (length(group_vars) == 0) {
    # Total global: todos los sitios del inventario
    n_sitios_global <- n_distinct(arboles_df$muestreo)
    
    resultado <- arboles_df %>%
      summarise(
        n_sitios = n_sitios_global,
        n_arboles = n(),
        ab_total_m2 = sum(area_basal, na.rm = TRUE),
        vol_total_m3 = sum(volumen_m3, na.rm = TRUE),
        d_medio_cm = mean(diametro_normal, na.rm = TRUE),
        dmc_cm = calcular_dmc(diametro_normal),
        d_min_cm = min(diametro_normal, na.rm = TRUE),
        d_max_cm = max(diametro_normal, na.rm = TRUE),
        h_media_m = mean(altura_total, na.rm = TRUE),
        h_min_m = min(altura_total, na.rm = TRUE),
        h_max_m = max(altura_total, na.rm = TRUE)
      )
  } else {
    # PASO 3: Calcular métricas por grupo
    resultado <- arboles_df %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        n_arboles = n(),
        ab_total_m2 = sum(area_basal, na.rm = TRUE),
        vol_total_m3 = sum(volumen_m3, na.rm = TRUE),
        d_medio_cm = mean(diametro_normal, na.rm = TRUE),
        dmc_cm = calcular_dmc(diametro_normal),
        d_min_cm = min(diametro_normal, na.rm = TRUE),
        d_max_cm = max(diametro_normal, na.rm = TRUE),
        h_media_m = mean(altura_total, na.rm = TRUE),
        h_min_m = min(altura_total, na.rm = TRUE),
        h_max_m = max(altura_total, na.rm = TRUE),
        .groups = "drop"
      )
    
    # PASO 4: Unir con n_sitios_total del rodal
    if ("rodal" %in% group_vars) {
      resultado <- resultado %>%
        left_join(sitios_por_rodal, by = "rodal") %>%
        rename(n_sitios = n_sitios_total)
    } else {
      # Si no agrupa por rodal, usar total global
      resultado$n_sitios <- n_distinct(arboles_df$muestreo)
    }
  }
  
  # PASO 5: Calcular métricas /ha
  resultado <- resultado %>%
    mutate(
      area_total_ha = n_sitios * config$area_parcela_ha,
      densidad_ha = n_arboles / area_total_ha,
      ab_m2ha = ab_total_m2 / area_total_ha,
      vol_m3ha = vol_total_m3 / area_total_ha
    )
  
  return(resultado)
}

# ==============================================================================
# 1. ANÁLISIS DASOMÉTRICO CON IC 95% (NOM-152 4.9.2)
# ==============================================================================

analizar_dasometrico_con_ic <- function(arboles_df, 
                                        config = CONFIG,
                                        error_muestreo = 0.10,
                                        exportar_csv_flag = TRUE,
                                        exportar_latex = TRUE) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║   ANÁLISIS DASOMÉTRICO IC 95% - MÉTODO DENSIDAD GLOBAL   ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  if (!exists("filtrar_arboles_vivos")) {
    source("core/15_core_calculos.R")
  }
  
  # Separar vivos, muertos, tocones (SOLO Pinus y Quercus)
  vivos <- arboles_df %>% 
    filter(!dominancia %in% c(7, 8, 9)) %>%
    filter(genero_grupo %in% c("Pinus", "Quercus")) %>%
    mutate(rodal = as.character(rodal))
  
  muertos <- arboles_df %>% 
    filter(dominancia %in% c(7, 8)) %>%  # ← Muerto en pie (7) ET muerto caído (8)
    filter(genero_grupo %in% c("Pinus", "Quercus")) %>%
    mutate(rodal = as.character(rodal))
  
  tocones <- arboles_df %>%
    filter(dominancia == 9) %>%  # ← Tocón (9)
    filter(genero_grupo %in% c("Pinus", "Quercus")) %>%
    mutate(rodal = as.character(rodal))
  
  n_sitios_total <- n_distinct(arboles_df$muestreo)
  
  superficie_por_rodal <- arboles_df %>%
    group_by(rodal) %>%
    summarise(superficie_ha = first(superficie_ha), .groups = "drop") %>%
    mutate(rodal = as.character(rodal))
  
  cat(sprintf("Intervalo de confianza: 95%%\n"))
  cat(sprintf("Sitios totales: %d\n", n_sitios_total))
  cat(sprintf("Árboles vivos (Pinus/Quercus): %d\n", nrow(vivos)))
  cat(sprintf("Árboles muertos: %d\n", nrow(muertos)))
  cat(sprintf("Tocones: %d\n\n", nrow(tocones)))
  
  # ===========================================================================
  # PASO 1 et 2: Préparation des données (calculs d'IC faits au PASO 4)
  # ===========================================================================
  
  cat("[1-2/5] Préparation données vivos, muertos, tocones...\n")
  
  # ===========================================================================
  # PASO 3: MUERTOS Y TOCONES (PARA TABLE 3 SEPARADA)
  # ===========================================================================
  
  cat("[3/5] Calculando muertos y tocones (Tabla 3)...\n")
  
  # MUERTOS: Agregar por GÉNERO
  if (nrow(muertos) > 0) {
    tabla3_muertos <- muertos %>%
      group_by(rodal, genero_grupo) %>%
      summarise(
        n_arboles = n(),
        ab_total_m2 = sum(area_basal, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(
        arboles_df %>%
          group_by(rodal) %>%
          summarise(n_sitios = n_distinct(muestreo), .groups = "drop") %>%
          mutate(rodal = as.character(rodal)),
        by = "rodal"
      ) %>%
      mutate(
        tipo = "Muerto",
        area_total_ha = n_sitios * config$area_parcela_ha,
        densidad_ha = n_arboles / area_total_ha,
        ab_m2ha = ab_total_m2 / area_total_ha
      ) %>%
      select(rodal, genero_grupo, tipo, n_sitios, n_arboles, densidad_ha, ab_m2ha)
  } else {
    tabla3_muertos <- tibble()
  }
  
  # TOCONES: Agregar por GÉNERO
  if (nrow(tocones) > 0) {
    tabla3_tocones <- tocones %>%
      group_by(rodal, genero_grupo) %>%
      summarise(
        n_arboles = n(),
        ab_total_m2 = sum(area_basal, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(
        arboles_df %>%
          group_by(rodal) %>%
          summarise(n_sitios = n_distinct(muestreo), .groups = "drop") %>%
          mutate(rodal = as.character(rodal)),
        by = "rodal"
      ) %>%
      mutate(
        tipo = "Tocón",
        area_total_ha = n_sitios * config$area_parcela_ha,
        densidad_ha = n_arboles / area_total_ha,
        ab_m2ha = ab_total_m2 / area_total_ha
      ) %>%
      select(rodal, genero_grupo, tipo, n_sitios, n_arboles, densidad_ha, ab_m2ha)
  } else {
    tabla3_tocones <- tibble()
  }
  
  # ===========================================================================
  # PASO 4: CALCULAR IC 95% PARA MÉTRICAS (NOM-152: nivel predio)
  # ===========================================================================
  
  cat("[4/5] Calculando intervalos de confianza 95% (NOM-152: nivel predio)...\n")
  
  # NOM-152: "estimación del error de muestreo a nivel de predio"
  # → n = TODOS los sitios del predio (58), no solo donde está presente la especie
  
  # Función para calcular IC 95%
  calcular_ic95 <- function(valores) {
    n <- length(valores)
    media <- mean(valores, na.rm = TRUE)
    
    if (n <= 1) {
      return(list(media = media, ic_inf = NA_real_, ic_sup = NA_real_, n = n))
    }
    
    desv_std <- sd(valores, na.rm = TRUE)
    if (is.na(desv_std) || desv_std == 0) {
      return(list(media = media, ic_inf = media, ic_sup = media, n = n))
    }
    
    error_std <- desv_std / sqrt(n)
    t_value <- qt(0.975, df = n - 1)
    margen_error <- t_value * error_std
    ic_inf <- max(0, media - margen_error)
    ic_sup <- media + margen_error
    
    list(media = media, ic_inf = ic_inf, ic_sup = ic_sup, n = n)
  }
  
  # PASO 4.1: Crear grid completo con TODOS los sitios
  # ====================================================================
  
  cat(sprintf("  Usando n = %d sitios totales del predio (NOM-152)\n", n_sitios_total))
  
  todos_los_sitios <- arboles_df %>%
    distinct(muestreo, rodal) %>%
    mutate(rodal = as.character(rodal))
  
  # PASO 4.2: Métricas por sitio para GÉNERO
  # ====================================================================
  
  # Calcular donde están presentes
  metricas_presentes_genero <- vivos %>%
    group_by(muestreo, rodal, genero_grupo) %>%
    summarise(
      n_arboles = n(),
      ab_m2 = sum(area_basal, na.rm = TRUE),
      vol_m3 = sum(volumen_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      densidad_ha = n_arboles / config$area_parcela_ha,
      ab_m2ha = ab_m2 / config$area_parcela_ha,
      vol_m3ha = vol_m3 / config$area_parcela_ha
    )
  
  # Completar con TODOS los sitios (densidad = 0 donde no presente)
  generos_unicos <- unique(vivos$genero_grupo)
  
  metricas_completas_genero <- expand.grid(
    muestreo = unique(todos_los_sitios$muestreo),
    genero_grupo = generos_unicos,
    stringsAsFactors = FALSE
  ) %>%
    left_join(todos_los_sitios %>% select(muestreo, rodal), by = "muestreo") %>%
    left_join(
      metricas_presentes_genero %>% select(muestreo, genero_grupo, densidad_ha, ab_m2ha, vol_m3ha),
      by = c("muestreo", "genero_grupo")
    ) %>%
    mutate(
      densidad_ha = ifelse(is.na(densidad_ha), 0, densidad_ha),
      ab_m2ha = ifelse(is.na(ab_m2ha), 0, ab_m2ha),
      vol_m3ha = ifelse(is.na(vol_m3ha), 0, vol_m3ha)
    )
  
  # Calcular IC por género (n = 58 para todos)
  tabla2_con_ic <- metricas_completas_genero %>%
    group_by(genero_grupo) %>%
    summarise(
      dens_ic = list(calcular_ic95(densidad_ha)),
      ab_ic = list(calcular_ic95(ab_m2ha)),
      vol_ic = list(calcular_ic95(vol_m3ha)),
      n_sitios = n(),
      .groups = "drop"
    ) %>%
    mutate(
      tipo = "Vivo",
      dens_media = map_dbl(dens_ic, ~.x$media),
      dens_ic_inf = map_dbl(dens_ic, ~.x$ic_inf),
      dens_ic_sup = map_dbl(dens_ic, ~.x$ic_sup),
      ab_media = map_dbl(ab_ic, ~.x$media),
      ab_ic_inf = map_dbl(ab_ic, ~.x$ic_inf),
      ab_ic_sup = map_dbl(ab_ic, ~.x$ic_sup),
      vol_media = map_dbl(vol_ic, ~.x$media),
      vol_ic_inf = map_dbl(vol_ic, ~.x$ic_inf),
      vol_ic_sup = map_dbl(vol_ic, ~.x$ic_sup)
    )
  
  # PASO 4.3: Métricas por sitio para ESPECIE
  # ====================================================================
  
  # Calcular donde están presentes
  metricas_presentes_especie <- vivos %>%
    group_by(muestreo, rodal, genero_grupo, nombre_cientifico) %>%
    summarise(
      n_arboles = n(),
      ab_m2 = sum(area_basal, na.rm = TRUE),
      vol_m3 = sum(volumen_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      densidad_ha = n_arboles / config$area_parcela_ha,
      ab_m2ha = ab_m2 / config$area_parcela_ha,
      vol_m3ha = vol_m3 / config$area_parcela_ha
    )
  
  # Completar con TODOS los sitios
  especies_unicas <- vivos %>%
    distinct(genero_grupo, nombre_cientifico)
  
  metricas_completas_especie <- expand.grid(
    muestreo = unique(todos_los_sitios$muestreo),
    nombre_cientifico = unique(especies_unicas$nombre_cientifico),
    stringsAsFactors = FALSE
  ) %>%
    left_join(todos_los_sitios %>% select(muestreo, rodal), by = "muestreo") %>%
    left_join(
      especies_unicas %>% select(nombre_cientifico, genero_grupo),
      by = "nombre_cientifico"
    ) %>%
    left_join(
      metricas_presentes_especie %>% select(muestreo, nombre_cientifico, densidad_ha, ab_m2ha, vol_m3ha),
      by = c("muestreo", "nombre_cientifico")
    ) %>%
    mutate(
      densidad_ha = ifelse(is.na(densidad_ha), 0, densidad_ha),
      ab_m2ha = ifelse(is.na(ab_m2ha), 0, ab_m2ha),
      vol_m3ha = ifelse(is.na(vol_m3ha), 0, vol_m3ha)
    )
  
  # Calcular IC por especie (n = 58 para todas)
  tabla1_con_ic <- metricas_completas_especie %>%
    group_by(genero_grupo, nombre_cientifico) %>%
    summarise(
      dens_ic = list(calcular_ic95(densidad_ha)),
      ab_ic = list(calcular_ic95(ab_m2ha)),
      vol_ic = list(calcular_ic95(vol_m3ha)),
      n_sitios = n(),
      .groups = "drop"
    ) %>%
    mutate(
      tipo = "Vivo",
      dens_media = map_dbl(dens_ic, ~.x$media),
      dens_ic_inf = map_dbl(dens_ic, ~.x$ic_inf),
      dens_ic_sup = map_dbl(dens_ic, ~.x$ic_sup),
      ab_media = map_dbl(ab_ic, ~.x$media),
      ab_ic_inf = map_dbl(ab_ic, ~.x$ic_inf),
      ab_ic_sup = map_dbl(ab_ic, ~.x$ic_sup),
      vol_media = map_dbl(vol_ic, ~.x$media),
      vol_ic_inf = map_dbl(vol_ic, ~.x$ic_inf),
      vol_ic_sup = map_dbl(vol_ic, ~.x$ic_sup)
    )
  
  cat(sprintf("  ✓ IC calculados con n = %d (todos los sitios del predio)\n", n_sitios_total))
  
  # ===========================================================================
  # PASO 5: RECONSTRUIR MÉTRICAS POR UMM + SUBTOTALES
  # ===========================================================================
  
  cat("[5/5] Calculando métricas por UMM y subtotales...\n")
  
  # PASO 5.1: Métricas globales por UMM (sin IC, solo valores medios)
  # ====================================================================
  
  # Por género y UMM
  metricas_umm_genero <- calcular_metricas_globales(vivos, c("rodal", "genero_grupo"), config) %>%
    select(rodal, genero_grupo, n_sitios, densidad_ha, ab_m2ha, vol_m3ha) %>%
    rename(dens_media = densidad_ha, ab_media = ab_m2ha, vol_media = vol_m3ha)
  
  # Por especie y UMM  
  metricas_umm_especie <- calcular_metricas_globales(
    vivos, 
    c("rodal", "genero_grupo", "nombre_cientifico"), 
    config
  ) %>%
    select(rodal, genero_grupo, nombre_cientifico, n_sitios, densidad_ha, ab_m2ha, vol_m3ha) %>%
    rename(dens_media = densidad_ha, ab_media = ab_m2ha, vol_media = vol_m3ha)
  
  # PASO 5.2: Unir métricas por UMM con IC globales (nivel predio)
  # ====================================================================
  
  # Tabla 2: Por género
  tabla2_final_temp <- metricas_umm_genero %>%
    left_join(
      tabla2_con_ic %>% select(genero_grupo, dens_ic_inf, dens_ic_sup, 
                               ab_ic_inf, ab_ic_sup, vol_ic_inf, vol_ic_sup),
      by = "genero_grupo"
    ) %>%
    mutate(tipo = "Vivo")
  
  # Tabla 1: Por especie
  tabla1_final_temp <- metricas_umm_especie %>%
    left_join(
      tabla1_con_ic %>% select(genero_grupo, nombre_cientifico, 
                               dens_ic_inf, dens_ic_sup,
                               ab_ic_inf, ab_ic_sup, 
                               vol_ic_inf, vol_ic_sup),
      by = c("genero_grupo", "nombre_cientifico")
    ) %>%
    mutate(tipo = "Vivo")
  
  # PASO 5.3: Subtotales por UMM
  # ====================================================================
  
  # Subtotales por género y UMM (para tabla 1)
  subtotales_umm_genero <- tabla2_final_temp %>%
    mutate(nombre_cientifico = paste("Subtotal", genero_grupo)) %>%
    select(rodal, genero_grupo, nombre_cientifico, tipo, n_sitios,
           dens_media, dens_ic_inf, dens_ic_sup,
           ab_media, ab_ic_inf, ab_ic_sup,
           vol_media, vol_ic_inf, vol_ic_sup)
  
  # Subtotal TOTAL por UMM (suma Pinus + Quercus, SIN IC)
  subtotales_umm_total <- tabla2_final_temp %>%
    group_by(rodal) %>%
    summarise(
      genero_grupo = "TOTAL",
      nombre_cientifico = "Subtotal UMM",
      tipo = "Vivo",
      n_sitios = first(n_sitios),
      dens_media = sum(dens_media),
      dens_ic_inf = NA_real_,
      dens_ic_sup = NA_real_,
      ab_media = sum(ab_media),
      ab_ic_inf = NA_real_,
      ab_ic_sup = NA_real_,
      vol_media = sum(vol_media),
      vol_ic_inf = NA_real_,
      vol_ic_sup = NA_real_,
      .groups = "drop"
    )
  
  # Subtotal por UMM para tabla 2 (suma Pinus + Quercus)
  subtotales_umm_2 <- tabla2_final_temp %>%
    group_by(rodal) %>%
    summarise(
      genero_grupo = "Subtotal",
      tipo = "Vivo",
      n_sitios = first(n_sitios),
      dens_media = sum(dens_media),
      dens_ic_inf = NA_real_,
      dens_ic_sup = NA_real_,
      ab_media = sum(ab_media),
      ab_ic_inf = NA_real_,
      ab_ic_sup = NA_real_,
      vol_media = sum(vol_media),
      vol_ic_inf = NA_real_,
      vol_ic_sup = NA_real_,
      .groups = "drop"
    )
  
  # PASO 5.4: Total general (nivel predio, CON IC)
  # ====================================================================
  
  # Total general para tabla 2 (desde tabla2_con_ic que ya tiene IC globales)
  total_general_2 <- tabla2_con_ic %>%
    summarise(
      rodal = "TOTAL",
      genero_grupo = "Total",
      tipo = "Vivo",
      n_sitios = n_sitios_total,
      dens_media = sum(dens_media),
      dens_ic_inf = NA_real_,  # Suma no tiene IC directo
      dens_ic_sup = NA_real_,
      ab_media = sum(ab_media),
      ab_ic_inf = NA_real_,
      ab_ic_sup = NA_real_,
      vol_media = sum(vol_media),
      vol_ic_inf = NA_real_,
      vol_ic_sup = NA_real_,
      .groups = "drop"
    )
  
  # Total general para tabla 1
  total_general_1 <- total_general_2 %>%
    mutate(
      genero_grupo = "TOTAL",
      nombre_cientifico = "Total General"
    ) %>%
    select(rodal, genero_grupo, nombre_cientifico, tipo, n_sitios,
           dens_media, dens_ic_inf, dens_ic_sup,
           ab_media, ab_ic_inf, ab_ic_sup,
           vol_media, vol_ic_inf, vol_ic_sup)
  
  # PASO 5.5: Ensamblar tablas finales
  # ====================================================================
  
  tabla1_final <- bind_rows(
    tabla1_final_temp,
    subtotales_umm_genero,
    subtotales_umm_total,
    total_general_1
  ) %>%
    arrange(rodal, genero_grupo, nombre_cientifico) %>%
    left_join(superficie_por_rodal, by = "rodal")
  
  tabla2_final <- bind_rows(
    tabla2_final_temp,
    subtotales_umm_2,
    total_general_2
  ) %>%
    arrange(rodal, genero_grupo) %>%
    left_join(superficie_por_rodal, by = "rodal")
  
  # Tabla 3: muertos y tocones
  if (nrow(tabla3_muertos) > 0 || nrow(tabla3_tocones) > 0) {
    tabla3_final <- bind_rows(tabla3_muertos, tabla3_tocones) %>%
      arrange(rodal, genero_grupo, tipo) %>%
      left_join(superficie_por_rodal, by = "rodal")
  } else {
    tabla3_final <- tibble()
  }
  
  cat(sprintf("  ✓ Tablas ensambladas (IC a nivel predio, n=%d)\n", n_sitios_total))
  
  # ===========================================================================
  # Exportar CSV
  # ===========================================================================
  
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    dir.create("resultados", showWarnings = FALSE, recursive = TRUE)
    
    write.csv(tabla1_final, "resultados/analisis_dasometrico_por_especie.csv", row.names = FALSE)
    cat("  ✓ CSV: analisis_dasometrico_por_especie.csv\n")
    
    write.csv(tabla2_final, "resultados/analisis_dasometrico_por_genero.csv", row.names = FALSE)
    cat("  ✓ CSV: analisis_dasometrico_por_genero.csv\n")
    
    if (nrow(tabla3_final) > 0) {
      write.csv(tabla3_final, "resultados/analisis_muertos_tocones.csv", row.names = FALSE)
      cat("  ✓ CSV: analisis_muertos_tocones.csv\n")
    }
  }
  
  # ===========================================================================
  # Exportar LaTeX con formato 3 chiffres significatifs
  # ===========================================================================
  
  if (exportar_latex) {
    cat("\n[GENERANDO TABLAS LATEX]\n")
    dir.create("tablas_latex", showWarnings = FALSE, recursive = TRUE)
    
    formatear_con_ic <- function(media, ic_inf, ic_sup) {
      # Vectores de strings formateados
      media_str <- formatear_3_cifras_sig(media)
      ic_inf_str <- formatear_3_cifras_sig(ic_inf)
      ic_sup_str <- formatear_3_cifras_sig(ic_sup)
      
      # Detectar dónde hay IC válidos (ambos no-NA)
      tiene_ic <- !is.na(ic_inf) & !is.na(ic_sup)
      
      # Inicializar con solo media
      result <- media_str
      
      # Agregar IC donde corresponda
      result[tiene_ic] <- sprintf("%s (%s--%s)", 
                                  media_str[tiene_ic],
                                  ic_inf_str[tiene_ic],
                                  ic_sup_str[tiene_ic])
      
      return(result)
    }
    
    # -----------------------------------------------------------------------
    # TABLA 1: Por especie (SOLO VIVOS)
    # -----------------------------------------------------------------------
    
    tabla1_latex <- tabla1_final %>%
      mutate(
        UMM = rodal,
        `Sup (ha)` = formatear_3_cifras_sig(superficie_ha),
        Especie = ifelse(is.na(nombre_cientifico), genero_grupo, nombre_cientifico),
        Dens = formatear_con_ic(dens_media, dens_ic_inf, dens_ic_sup),
        AB = formatear_con_ic(ab_media, ab_ic_inf, ab_ic_sup),
        `Exist Real` = formatear_con_ic(vol_media, vol_ic_inf, vol_ic_sup)
      ) %>%
      select(UMM, `Sup (ha)`, Especie, Dens, AB, `Exist Real`)
    
    latex1_content <- c(
      "% ============================================================",
      "% TABLA 1: Análisis Dasométrico por Especie (IC 95%)",
      "% ============================================================",
      "\\begin{table}[H]",
      "\t\\centering",
      "\t\\caption{Análisis dasométrico por especie y UMM (IC 95\\%)}",
      "\t\\label{tab:dasometrico_especie}",
      "\t\\scriptsize",
      "\t\\begin{tabular}{ccp{2.8cm}ccc}",
      "\t\t\\toprule",
      "\t\t\\textbf{UMM} & \\textbf{Sup} & \\textbf{Especie} &",
      "\t\t\\textbf{Dens} & \\textbf{AB} & \\textbf{Exist. Real} \\\\",
      "\t\t & \\textbf{(ha)} & &",
      "\t\t\\textbf{(árb/ha)} & \\textbf{(m²/ha)} & \\textbf{(m³/ha)} \\\\",
      "\t\t\\midrule"
    )
    
    umm_actual <- NULL
    for (i in 1:nrow(tabla1_latex)) {
      fila <- tabla1_latex[i, ]
      
      if (!is.null(umm_actual) && fila$UMM != umm_actual && fila$UMM != "TOTAL") {
        latex1_content <- c(latex1_content, "\t\t\\midrule")
      }
      
      if (fila$UMM == "TOTAL" || grepl("Subtotal|Total", fila$Especie)) {
        linea <- sprintf("\t\t\\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} \\\\",
                         fila$UMM, fila$`Sup (ha)`, fila$Especie, fila$Dens, fila$AB, fila$`Exist Real`)
      } else {
        linea <- sprintf("\t\t%s & %s & \\textit{%s} & %s & %s & %s \\\\",
                         fila$UMM, fila$`Sup (ha)`, fila$Especie, fila$Dens, fila$AB, fila$`Exist Real`)
      }
      
      latex1_content <- c(latex1_content, linea)
      umm_actual <- fila$UMM
    }
    
    latex1_content <- c(
      latex1_content,
      "\t\t\\bottomrule",
      "\t\\end{tabular}",
      "\t\\\\[0.3cm]",
      sprintf("\t{\\footnotesize IC 95\\%% calculado a nivel predio (n=%d sitios, NOM-152). Formato: 3 cifras significativas.}", n_sitios_total),
      "\\end{table}"
    )
    
    writeLines(latex1_content, "tablas_latex/analisis_dasometrico_por_especie.tex")
    cat("  ✓ LaTeX: analisis_dasometrico_por_especie.tex\n")
    
    # -----------------------------------------------------------------------
    # TABLA 2: Por género (SOLO VIVOS)
    # -----------------------------------------------------------------------
    
    tabla2_latex <- tabla2_final %>%
      mutate(
        UMM = rodal,
        `Sup (ha)` = ifelse(is.na(superficie_ha), "--", formatear_3_cifras_sig(superficie_ha)),
        Género = genero_grupo,
        Tipo = tipo,
        Dens = formatear_con_ic(dens_media, dens_ic_inf, dens_ic_sup),
        AB = formatear_con_ic(ab_media, ab_ic_inf, ab_ic_sup),
        `Exist Real` = formatear_con_ic(vol_media, vol_ic_inf, vol_ic_sup)
      ) %>%
      select(UMM, `Sup (ha)`, Género, Tipo, Dens, AB, `Exist Real`)
    
    latex2_content <- c(
      "% ============================================================",
      "% TABLA 2: Análisis Dasométrico por Género (IC 95%)",
      "% ============================================================",
      "\\begin{table}[H]",
      "\t\\centering",
      "\t\\caption{Análisis dasométrico por género y UMM (IC 95\\%)}",
      "\t\\label{tab:dasometrico_genero}",
      "\t\\small",
      "\t\\begin{tabular}{cclcccc}",
      "\t\t\\toprule",
      "\t\t\\textbf{UMM} & \\textbf{Sup} & \\textbf{Género} & \\textbf{Tipo} &",
      "\t\t\\textbf{Dens} & \\textbf{AB} & \\textbf{Exist. Real} \\\\",
      "\t\t & \\textbf{(ha)} & & &",
      "\t\t\\textbf{(árb/ha)} & \\textbf{(m²/ha)} & \\textbf{(m³/ha)} \\\\",
      "\t\t\\midrule"
    )
    
    umm_actual <- NULL
    for (i in 1:nrow(tabla2_latex)) {
      fila <- tabla2_latex[i, ]
      
      if (!is.null(umm_actual) && fila$UMM != umm_actual && fila$UMM != "TOTAL") {
        latex2_content <- c(latex2_content, "\t\t\\midrule")
      }
      
      if (fila$UMM == "TOTAL" || fila$Género %in% c("Subtotal", "Total")) {
        linea <- sprintf("\t\t\\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} \\\\",
                         fila$UMM, fila$`Sup (ha)`, fila$Género, fila$Tipo, fila$Dens, fila$AB, fila$`Exist Real`)
      } else {
        linea <- sprintf("\t\t%s & %s & \\textit{%s} & %s & %s & %s & %s \\\\",
                         fila$UMM, fila$`Sup (ha)`, fila$Género, fila$Tipo, fila$Dens, fila$AB, fila$`Exist Real`)
      }
      
      latex2_content <- c(latex2_content, linea)
      umm_actual <- fila$UMM
    }
    
    latex2_content <- c(
      latex2_content,
      "\t\t\\bottomrule",
      "\t\\end{tabular}",
      "\t\\\\[0.3cm]",
      sprintf("\t{\\footnotesize IC 95\\%% a nivel predio (n=%d, NOM-152). Formato: 3 cifras significativas.}", n_sitios_total),
      "\\end{table}"
    )
    
    writeLines(latex2_content, "tablas_latex/analisis_dasometrico_por_genero.tex")
    cat("  ✓ LaTeX: analisis_dasometrico_por_genero.tex\n")
    
    # -----------------------------------------------------------------------
    # TABLA 3: Muertos y Tocones
    # -----------------------------------------------------------------------
    
    if (nrow(tabla3_final) > 0) {
      tabla3_latex <- tabla3_final %>%
        mutate(
          UMM = rodal,
          `Sup (ha)` = ifelse(is.na(superficie_ha), "--", formatear_3_cifras_sig(superficie_ha)),
          Género = genero_grupo,
          Tipo = tipo,
          Dens = formatear_3_cifras_sig(densidad_ha),
          AB = formatear_3_cifras_sig(ab_m2ha)
        ) %>%
        select(UMM, `Sup (ha)`, Género, Tipo, Dens, AB)
      
      latex3_content <- c(
        "% ============================================================",
        "% TABLA 3: Árboles Muertos y Tocones",
        "% ============================================================",
        "\\begin{table}[H]",
        "\t\\centering",
        "\t\\caption{Árboles muertos y tocones por género y UMM}",
        "\t\\label{tab:muertos_tocones}",
        "\t\\small",
        "\t\\begin{tabular}{cclccc}",
        "\t\t\\toprule",
        "\t\t\\textbf{UMM} & \\textbf{Sup} & \\textbf{Género} & \\textbf{Tipo} &",
        "\t\t\\textbf{Dens} & \\textbf{AB} \\\\",
        "\t\t & \\textbf{(ha)} & & &",
        "\t\t\\textbf{(árb/ha)} & \\textbf{(m²/ha)} \\\\",
        "\t\t\\midrule"
      )
      
      umm_actual <- NULL
      for (i in 1:nrow(tabla3_latex)) {
        fila <- tabla3_latex[i, ]
        
        if (!is.null(umm_actual) && fila$UMM != umm_actual) {
          latex3_content <- c(latex3_content, "\t\t\\midrule")
        }
        
        linea <- sprintf("\t\t%s & %s & \\textit{%s} & %s & %s & %s \\\\",
                         fila$UMM, fila$`Sup (ha)`, fila$Género, fila$Tipo, fila$Dens, fila$AB)
        
        latex3_content <- c(latex3_content, linea)
        umm_actual <- fila$UMM
      }
      
      latex3_content <- c(
        latex3_content,
        "\t\t\\bottomrule",
        "\t\\end{tabular}",
        "\t\\\\[0.3cm]",
        sprintf("\t{\\footnotesize Muerto = dominancia 7-8. Tocón = dominancia 9. Formato: 3 cifras significativas. n=%d sitios.}", n_sitios_total),
        "\\end{table}"
      )
      
      writeLines(latex3_content, "tablas_latex/analisis_muertos_tocones.tex")
      cat("  ✓ LaTeX: analisis_muertos_tocones.tex\n")
    }
  }
  
  cat("\n✓ Análisis dasométrico completado\n")
  cat(sprintf("  MÉTODO: Densidad global + IC 95%% a nivel predio (n=%d, NOM-152)\n\n", n_sitios_total))
  
  return(list(
    tabla1_especie = tabla1_final,
    tabla2_genero = tabla2_final,
    tabla3_muertos_tocones = tabla3_final,
    parametros = list(
      metodo = "densidad_global",
      area_parcela_ha = config$area_parcela_ha,
      n_sitios_total = n_sitios_total,
      n_vivos = nrow(vivos),
      n_muertos = nrow(muertos),
      n_tocones = nrow(tocones),
      ic_nivel = 0.95
    )
  ))
}

cat("\n✓ Módulo de análisis descriptivo cargado (MÉTODO DENSIDAD GLOBAL)\n")
cat("  ✓ Densidad = N_total / (n_sitios × 0.1 ha)\n")
cat("  ✓ AB/ha = Σ(AB_individuales) / (n_sitios × 0.1 ha)\n")
cat("  ✓ Vol/ha = Σ(Vol_individuales) / (n_sitios × 0.1 ha)\n")
cat(sprintf("  ✓ IC 95%% a nivel predio (NOM-152): n = todos los sitios\n"))
cat("  ✓ Formato LaTeX: 3 cifras significativas\n")
cat("  ✓ Utilidades cargadas: formatear_3_cifras_sig(), exportar_csv()\n\n")

# ==============================================================================
# 2. ESTRUCTURA POBLACIONAL - MÉTODO GLOBAL
# ==============================================================================

analizar_estructura_poblacional <- function(arboles_df, config = CONFIG,
                                            exportar_latex = TRUE,
                                            exportar_csv_flag = TRUE) {
  
  cat("\n╔═══════════════════════════════════════════════════════════╗\n")
  cat("║     ANÁLISIS ESTRUCTURA POBLACIONAL (MÉTODO GLOBAL)      ║\n")
  cat("╚═══════════════════════════════════════════════════════════╝\n\n")
  
  if (!exists("filtrar_arboles_vivos")) {
    source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
  }
  
  vivos <- filtrar_arboles_vivos(arboles_df)
  
  # RESUMEN GENERAL
  resumen_general <- calcular_metricas_globales(vivos, character(0), config) %>%
    select(n_sitios, n_arboles, densidad_ha, d_medio_cm, dmc_cm, 
           h_media_m, ab_m2ha, vol_m3ha)
  
  cat("[RESUMEN GENERAL]\n")
  print(resumen_general)
  
  # POR RODAL
  por_rodal <- calcular_metricas_globales(vivos, "rodal", config) %>%
    select(rodal, n_sitios, n_arboles, densidad_ha, d_medio_cm, dmc_cm,
           h_media_m, ab_m2ha, vol_m3ha)
  
  cat("\n[POR RODAL]\n")
  print(por_rodal, n = 50)
  
  # POR GÉNERO
  por_genero <- calcular_metricas_globales(vivos, "genero_grupo", config) %>%
    mutate(
      proporcion_pct = (n_arboles / sum(n_arboles)) * 100,
      ab_pct = (ab_m2ha / sum(ab_m2ha)) * 100,
      vol_pct = (vol_m3ha / sum(vol_m3ha)) * 100
    ) %>%
    arrange(desc(densidad_ha))
  
  cat("\n[POR GÉNERO]\n")
  print(por_genero)
  
  # POR ESPECIE (TOP 10)
  por_especie <- calcular_metricas_globales(vivos, c("nombre_cientifico", "genero_grupo"), config) %>%
    mutate(proporcion_pct = (n_arboles / sum(n_arboles)) * 100) %>%
    arrange(desc(densidad_ha)) %>%
    head(10)
  
  cat("\n[TOP 10 ESPECIES]\n")
  print(por_especie)
  
  # EXPORTAR CSV
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    exportar_csv(resumen_general, "desc_01_resumen_general")
    exportar_csv(por_rodal, "desc_02_por_rodal")
    exportar_csv(por_genero, "desc_03_por_genero")
    exportar_csv(por_especie, "desc_04_top10_especies")
  }
  
  return(list(
    general = resumen_general,
    por_rodal = por_rodal,
    por_genero = por_genero,
    por_especie = por_especie
  ))
}

# ==============================================================================
# 3. COMPOSICIÓN PINUS/QUERCUS POR RODAL
# ==============================================================================

analizar_composicion_generopq_por_rodal <- function(arboles_df, config = CONFIG,
                                                    exportar_latex = TRUE,
                                                    exportar_csv_flag = TRUE) {
  
  cat("\n╔═══════════════════════════════════════════════════════════╗\n")
  cat("║  COMPOSICIÓN PINUS/QUERCUS POR RODAL (MÉTODO GLOBAL)    ║\n")
  cat("╚═══════════════════════════════════════════════════════════╝\n\n")
  
  vivos_pq <- filtrar_arboles_vivos(arboles_df) %>%
    filter(genero_grupo %in% c("Pinus", "Quercus"))
  
  composicion <- calcular_metricas_globales(vivos_pq, c("rodal", "genero_grupo"), config) %>%
    group_by(rodal) %>%
    mutate(
      densidad_pct = (densidad_ha / sum(densidad_ha)) * 100,
      ab_pct = (ab_m2ha / sum(ab_m2ha)) * 100,
      vol_pct = (vol_m3ha / sum(vol_m3ha)) * 100
    ) %>%
    ungroup() %>%
    arrange(rodal, desc(densidad_ha))
  
  cat("[COMPOSICIÓN PINUS/QUERCUS POR RODAL]\n")
  print(composicion, n = 50)
  
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    exportar_csv(composicion, "desc_11_composicion_generopq_rodal")
  }
  
  return(list(tabla = composicion))
}

# ==============================================================================
# 4. DISTRIBUCIÓN DIAMÉTRICA
# ==============================================================================

analizar_distribucion_diametrica <- function(arboles_df, config = CONFIG,
                                             exportar = TRUE,
                                             exportar_csv_flag = TRUE) {
  
  cat("\n╔═══════════════════════════════════════════════════════════╗\n")
  cat("║         ANÁLISIS DISTRIBUCIÓN DIAMÉTRICA                 ║\n")
  cat("╚═══════════════════════════════════════════════════════════╝\n\n")
  
  vivos <- filtrar_arboles_vivos(arboles_df)
  
  # Calcular por clase diamétrica y género
  dist_diametrica <- vivos %>%
    mutate(clase_d = asignar_clase_diametrica(diametro_normal, 
                                              breaks = config$clases_d,
                                              formato = "rango")) %>%
    filter(!is.na(clase_d)) %>%
    group_by(clase_d, genero_grupo) %>%
    summarise(
      n_arboles = n(),
      .groups = "drop"
    ) %>%
    # Calcular densidad global
    mutate(
      n_sitios_total = n_distinct(arboles_df$muestreo),
      area_total_ha = n_sitios_total * config$area_parcela_ha,
      densidad_ha = n_arboles / area_total_ha
    ) %>%
    group_by(genero_grupo) %>%
    mutate(
      proporcion_pct = (densidad_ha / sum(densidad_ha)) * 100,
      proporcion_acum = cumsum(proporcion_pct)
    ) %>%
    ungroup()
  
  cat("[DISTRIBUCIÓN DIAMÉTRICA]\n")
  print(dist_diametrica, n = 30)
  
  if (exportar_csv_flag) {
    cat("\n[EXPORTANDO CSV]\n")
    exportar_csv(dist_diametrica, "desc_05_distribucion_diametrica")
  }
  
  return(list(tabla = dist_diametrica))
}

# ==============================================================================
# FUNCIÓN MAESTRA - ANÁLISIS DESCRIPTIVO COMPLETO
# ==============================================================================

analisis_descriptivo_completo <- function(inventario, arboles_df, config = CONFIG,
                                          exportar_csv_flag = TRUE) {
  
  cat("\n╔═══════════════════════════════════════════════════════════╗\n")
  cat("║     ANÁLISIS DESCRIPTIVO COMPLETO (MÉTODO GLOBAL)        ║\n")
  cat("╚═══════════════════════════════════════════════════════════╝\n")
  
  resultados <- list()
  
  cat("\n[1/4] Estructura poblacional...\n")
  resultados$estructura <- analizar_estructura_poblacional(arboles_df, config,
                                                           exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[2/4] Composición Pinus/Quercus por rodal...\n")
  resultados$composicion_pq <- analizar_composicion_generopq_por_rodal(arboles_df, config,
                                                                       exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[3/4] Distribución diamétrica...\n")
  resultados$distribucion <- analizar_distribucion_diametrica(arboles_df, config,
                                                              exportar_csv_flag = exportar_csv_flag)
  
  cat("\n[4/4] Análisis dasométrico con IC 95%...\n")
  resultados$dasometrico <- analizar_dasometrico_con_ic(arboles_df, config,
                                                        exportar_csv_flag = exportar_csv_flag)
  
  cat("\n╔═══════════════════════════════════════════════════════════╗\n")
  cat("║           ✓ ANÁLISIS DESCRIPTIVO COMPLETADO              ║\n")
  cat("╚═══════════════════════════════════════════════════════════╝\n\n")
  
  cat("MÉTODO APLICADO:\n")
  cat("═══════════════════════════════════════════════════════════\n")
  cat("  ✓ Densidad global por UMM: N_total / (n_sitios_UMM × 0.1 ha)\n")
  cat("  ✓ AB/ha: Σ(AB_individuales) / (n_sitios_UMM × 0.1 ha)\n")
  cat("  ✓ Vol/ha: Σ(Vol_individuales) / (n_sitios_UMM × 0.1 ha)\n")
  cat("  ✓ IC 95%: Calculado a nivel predio (NOM-152)\n")
  cat(sprintf("  ✓ n = %d sitios totales del predio para todos los IC\n", 
              resultados$dasometrico$parametros$n_sitios_total))
  cat("  ✓ Especies ausentes: densidad = 0 en sitios sin presencia\n\n")
  
  cat("ARCHIVOS GENERADOS:\n")
  cat("═══════════════════════════════════════════════════════════\n")
  cat("  📊 Tablas LaTeX: tablas_latex/\n")
  cat("     - analisis_dasometrico_por_especie.tex (Tabla 1)\n")
  cat("     - analisis_dasometrico_por_genero.tex (Tabla 2)\n")
  cat("     - analisis_muertos_tocones.tex (Tabla 3)\n")
  cat("  📄 CSVs: resultados/desc_*.csv\n")
  cat("  📐 Formato: 3 cifras significativas (99.6, 1150, 9.93)\n\n")
  
  return(resultados)
}

cat("\n✓ Función analisis_descriptivo_completo() cargada\n")
cat("  Ejecutar: resultados <- analisis_descriptivo_completo(inventario, arboles_df, CONFIG)\n\n")
