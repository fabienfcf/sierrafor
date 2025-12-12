# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# ==============================================================================
# CÁLCULO DE ICA Y VARIABLES SECCIÓN 11.1.4 DEL MANUAL PMF
# ==============================================================================
# 
# Este módulo calcula el ICA sobre 10 años SIN operación forestal para obtener
# valores de crecimiento reales derivados del modelo poblacional.
#
# Calcula todas las variables mencionadas en la sección 11.1.4 del Manual PMF:
#   - Sup (ha): Superficie por rodal
#   - IS (m): Índice de Sitio  
#   - ER (m³/ha): Existencias Reales al inicio
#   - ICA (m³/ha): Incremento Corriente Anual
#   - ICA Rel (i): Incremento relativo = ICA/ER
#   - IntCor Rel (IC): Intensidad de corta relativa = 1 - 1/(1+i)^cc
#   - VC/ha (m³): Volumen de corta por ha = IC * ER
#   - ER/rodal (m³): Existencias reales por rodal
#   - VC/rodal (m³): Volumen de corta por rodal
#
# Estos cálculos se realizan por:
#   - Especie (géneros Pinus y Quercus)
#   - Género 
#   - Rodal (UMM)
#   - Total del predio
#
# ==============================================================================

library(tidyverse)
library(xtable)

# Cargar utilidades compartidas si no están disponibles
if (!exists("calcular_metricas_estado")) {
  source(file.path(PROYECTO_ROOT, "utils/utils_metricas.R"))
}

# ==============================================================================
# FUNCIÓN PRINCIPAL: SIMULAR SIN CORTES Y CALCULAR ICA
# ==============================================================================

calcular_ica_sin_cortes <- function(arboles_inicial, config = CONFIG, años = 10) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║         CÁLCULO DE ICA - SIMULACIÓN SIN CORTES           ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  cat(sprintf("• Período de simulación: %d años\n", años))
  cat(sprintf("• Población inicial: %d árboles\n", nrow(arboles_inicial)))
  cat("• Métodos: crecimiento + mortalidad + reclutamiento\n")
  cat("• Sin operaciones forestales\n\n")
  
  # Verificar que los módulos estén cargados
  if (!exists("aplicar_crecimiento_poblacion")) {
    stop("❌ Módulos de simulación no cargados. Ejecuta 40_WORKFLOW_COMPLETO.R primero")
  }
  
  # ===========================================================================
  # 1. SIMULACIÓN SIN CORTES
  # ===========================================================================
  
  cat("[PASO 1/4] Simulando crecimiento sin cortes...\n\n")
  
  arboles_actual <- arboles_inicial
  historial_completo <- list()
  
  # Guardar estado inicial
  historial_completo[[1]] <- arboles_actual %>% mutate(año_simulacion = 0)
  
  # Simular año por año (solo procesos naturales)
  for (año in 1:años) {
    
    cat(sprintf("  └─ Año %d/%d\n", año, años))
    
    # Crecimiento
    arboles_actual <- aplicar_crecimiento_poblacion(arboles_actual, config, año)
    
    # Mortalidad natural
    arboles_actual <- aplicar_mortalidad_poblacion(arboles_actual, config, año)
    
    # Reclutamiento
    arboles_actual <- aplicar_reclutamiento(arboles_actual, config, año)
    
    # Guardar estado
    historial_completo[[año + 1]] <- arboles_actual %>% 
      mutate(año_simulacion = año)
  }
  
  cat("\n✓ Simulación completada\n\n")
  
  # ===========================================================================
  # 2. CALCULAR MÉTRICAS INICIALES Y FINALES
  # ===========================================================================
  
  cat("[PASO 2/4] Calculando métricas por rodal, género y especie...\n\n")
  
  # Métricas iniciales (año 0)
  metricas_inicial <- calcular_metricas_detalladas(arboles_inicial, config)
  
  # Métricas finales (año 10)
  metricas_final <- calcular_metricas_detalladas(arboles_actual, config)
  
  # ===========================================================================
  # 3. CALCULAR VARIABLES DEL 11.1.4
  # ===========================================================================
  
  cat("[PASO 3/4] Calculando variables según Manual PMF 11.1.4...\n\n")
  
  # Calcular por rodal
  ica_por_rodal <- calcular_variables_114_rodal(
    metricas_inicial, 
    metricas_final, 
    años, 
    config,
    arboles_inicial  # Para obtener superficies reales
  )
  
  # Calcular por género y rodal
  ica_por_genero_rodal <- calcular_variables_114_genero_rodal(
    metricas_inicial,
    metricas_final,
    años,
    config,
    arboles_inicial  # Para obtener superficies reales
  )
  
  # Calcular por especie y rodal (solo Pinus y Quercus)
  ica_por_especie_rodal <- calcular_variables_114_especie_rodal(
    arboles_inicial,
    arboles_actual,
    años,
    config,
    metricas_inicial,  # Agregar métricas
    metricas_final     # Agregar métricas
  )
  
  # Resumen general del predio
  resumen_predio <- calcular_resumen_predio(
    ica_por_rodal,
    años,
    config
  )
  
  cat("✓ Cálculos completados\n\n")
  
  # ===========================================================================
  # 4. RETORNAR RESULTADOS
  # ===========================================================================
  
  return(list(
    # Datos de simulación
    poblacion_inicial = arboles_inicial,
    poblacion_final = arboles_actual,
    historial = bind_rows(historial_completo),
    años_simulados = años,
    
    # Tablas ICA calculadas
    ica_por_rodal = ica_por_rodal,
    ica_por_genero_rodal = ica_por_genero_rodal,
    ica_por_especie_rodal = ica_por_especie_rodal,
    resumen_predio = resumen_predio
  ))
}

# ==============================================================================
# MÉTRICAS DETALLADAS POR RODAL, GÉNERO Y ESPECIE
# ==============================================================================

calcular_metricas_detalladas <- function(arboles_df, config) {

  # Usar funciones compartidas de utils_metricas.R (evita duplicación)
  por_rodal <- calcular_metricas_estado(arboles_df, config) %>%
    select(rodal, n_arboles = n_vivos, vol_muestreado_m3, vol_ha_m3,
           area_basal_m2 = ab_muestreada_m2, d_medio_cm, h_media_m)

  por_genero <- calcular_metricas_por_genero(arboles_df, config) %>%
    select(rodal, genero_grupo, n_arboles = n_vivos, vol_muestreado_m3,
           vol_ha_m3, area_basal_m2 = ab_muestreada_m2)

  # Por especie (filtrar solo Pinus y Quercus)
  por_especie <- calcular_metricas_por_especie(arboles_df, config) %>%
    filter(genero_grupo %in% c("Pinus", "Quercus")) %>%
    select(rodal, genero_grupo, nombre_cientifico, n_arboles = n_vivos,
           vol_muestreado_m3, vol_ha_m3, area_basal_m2 = ab_muestreada_m2)

  return(list(
    por_rodal = por_rodal,
    por_genero = por_genero,
    por_especie = por_especie
  ))
}

# ==============================================================================
# VARIABLES 11.1.4 POR RODAL (UMM)
# ==============================================================================

calcular_variables_114_rodal <- function(inicial, final, años, config, arboles_inicial) {
  
  # La superficie ya está en arboles_inicial (viene de arboles_analisis.rds)
  # Simplemente extraerla
  superficie_por_rodal <- arboles_inicial %>%
    group_by(rodal) %>%
    summarise(
      superficie_ha = first(superficie_ha),
      .groups = "drop"
    )
  
  # Verificar si hay NAs o valores inválidos
  if (any(is.na(superficie_por_rodal$superficie_ha))) {
    warning("⚠️ Algunos rodales tienen superficie NA. Usando promedio para esos casos.")
    sup_promedio <- mean(superficie_por_rodal$superficie_ha, na.rm = TRUE)
    superficie_por_rodal <- superficie_por_rodal %>%
      mutate(superficie_ha = ifelse(is.na(superficie_ha), sup_promedio, superficie_ha))
  }
  
  # Unir métricas inicial y final con superficies reales
  comparacion <- inicial$por_rodal %>%
    select(rodal, 
           vol_muestreado_ini = vol_muestreado_m3,
           vol_ha_ini = vol_ha_m3,
           n_arboles_ini = n_arboles) %>%
    left_join(
      final$por_rodal %>%
        select(rodal,
               vol_muestreado_fin = vol_muestreado_m3,
               vol_ha_fin = vol_ha_m3,
               n_arboles_fin = n_arboles),
      by = "rodal"
    ) %>%
    left_join(superficie_por_rodal, by = "rodal") %>%
    mutate(
      # Existencias Reales (ER) al inicio en m³/ha (ya calculado correctamente)
      ER_m3_ha = vol_ha_ini,
      
      # Volumen final en m³/ha (ya calculado correctamente)
      vol_fin_m3_ha = vol_ha_fin,
      
      # ICA en m³/ha (incremento anual)
      ICA_m3_ha = (vol_fin_m3_ha - ER_m3_ha) / años,
      
      # ICA Relativo (i) = ICA/ER
      ICA_rel_i = ifelse(ER_m3_ha > 0, ICA_m3_ha / ER_m3_ha, 0),
      
      # Ciclo de corta
      ciclo_corta = config$periodo,
      
      # Intensidad de corta relativa (IC): fórmula interés compuesto
      # IC = 1 - 1/(1+i)^cc
      IntCor_rel_IC = ifelse(ICA_rel_i > -1, 
                             1 - 1/((1 + ICA_rel_i)^ciclo_corta),
                             0),
      
      # Volumen de corta por hectárea: VC = IC * ER
      VC_ha_m3 = IntCor_rel_IC * ER_m3_ha,
      
      # Existencias reales por rodal (usar superficie_ha que viene de la tabla)
      ER_rodal_m3 = ER_m3_ha * superficie_ha,
      
      # Volumen de corta por rodal
      VC_rodal_m3 = VC_ha_m3 * superficie_ha
    ) %>%
    select(rodal, Sup_ha = superficie_ha, ER_m3_ha, ICA_m3_ha, ICA_rel_i, 
           ciclo_corta, IntCor_rel_IC, VC_ha_m3, 
           ER_rodal_m3, VC_rodal_m3)
  
  return(comparacion)
}

# ==============================================================================
# VARIABLES 11.1.4 POR GÉNERO Y RODAL
# ==============================================================================

calcular_variables_114_genero_rodal <- function(inicial, final, años, config, arboles_inicial) {
  
  # La superficie ya está en arboles_inicial
  superficie_por_rodal <- arboles_inicial %>%
    group_by(rodal) %>%
    summarise(
      superficie_ha = first(superficie_ha),
      .groups = "drop"
    )
  
  # Verificar NAs
  if (any(is.na(superficie_por_rodal$superficie_ha))) {
    sup_promedio <- mean(superficie_por_rodal$superficie_ha, na.rm = TRUE)
    superficie_por_rodal <- superficie_por_rodal %>%
      mutate(superficie_ha = ifelse(is.na(superficie_ha), sup_promedio, superficie_ha))
  }
  
  comparacion <- inicial$por_genero %>%
    select(rodal, genero = genero_grupo,
           vol_muestreado_ini = vol_muestreado_m3,
           vol_ha_ini = vol_ha_m3,
           n_arboles_ini = n_arboles) %>%
    left_join(
      final$por_genero %>%
        select(rodal, genero = genero_grupo,
               vol_muestreado_fin = vol_muestreado_m3,
               vol_ha_fin = vol_ha_m3,
               n_arboles_fin = n_arboles),
      by = c("rodal", "genero")
    ) %>%
    left_join(superficie_por_rodal, by = "rodal") %>%
    mutate(
      # ER y volumen final ya están calculados correctamente como m³/ha
      ER_m3_ha = vol_ha_ini,
      vol_fin_m3_ha = vol_ha_fin,
      ICA_m3_ha = (vol_fin_m3_ha - ER_m3_ha) / años,
      ICA_rel_i = ifelse(ER_m3_ha > 0, ICA_m3_ha / ER_m3_ha, 0),
      ciclo_corta = config$periodo,
      IntCor_rel_IC = ifelse(ICA_rel_i > -1, 
                             1 - 1/((1 + ICA_rel_i)^ciclo_corta),
                             0),
      VC_ha_m3 = IntCor_rel_IC * ER_m3_ha,
      ER_rodal_m3 = ER_m3_ha * superficie_ha,
      VC_rodal_m3 = VC_ha_m3 * superficie_ha
    ) %>%
    select(rodal, genero, Sup_ha = superficie_ha, ER_m3_ha, ICA_m3_ha, ICA_rel_i,
           ciclo_corta, IntCor_rel_IC, VC_ha_m3,
           ER_rodal_m3, VC_rodal_m3)
  
  return(comparacion)
}

# ==============================================================================
# VARIABLES 11.1.4 POR ESPECIE Y RODAL (PINUS Y QUERCUS)
# ==============================================================================

calcular_variables_114_especie_rodal <- function(arboles_inicial, arboles_final, 
                                                 años, config, 
                                                 metricas_inicial, metricas_final) {
  
  # Usar las métricas ya calculadas que incluyen vol_ha_m3
  inicial_especies <- metricas_inicial$por_especie %>%
    rename(genero = genero_grupo, especie = nombre_cientifico)
  
  final_especies <- metricas_final$por_especie %>%
    rename(genero = genero_grupo, especie = nombre_cientifico)
  
  # Obtener superficie REAL por rodal
  tiene_superficie <- "superficie_ha" %in% names(arboles_inicial)
  
  if (tiene_superficie) {
    superficie_por_rodal <- arboles_inicial %>%
      group_by(rodal) %>%
      summarise(
        superficie_ha = first(na.omit(superficie_ha)),
        .groups = "drop"
      )
    
    if (all(is.na(superficie_por_rodal$superficie_ha)) || 
        nrow(superficie_por_rodal) == 0 ||
        any(superficie_por_rodal$superficie_ha <= 0, na.rm = TRUE)) {
      tiene_superficie <- FALSE
    }
  }
  
  # Fallback si no hay datos
  if (!tiene_superficie) {
    warning("⚠️ No se encontraron superficies por rodal. Usando distribución equitativa.")
    superficie_total_ha <- 100
    n_rodales <- n_distinct(inicial_especies$rodal)
    superficie_por_rodal <- tibble(
      rodal = unique(inicial_especies$rodal),
      superficie_ha = superficie_total_ha / n_rodales
    )
  }
  
  # Asegurar que todos los rodales tengan superficie
  rodales_sin_superficie <- inicial_especies %>%
    distinct(rodal) %>%
    anti_join(superficie_por_rodal, by = "rodal")
  
  if (nrow(rodales_sin_superficie) > 0) {
    sup_promedio <- mean(superficie_por_rodal$superficie_ha, na.rm = TRUE)
    superficie_faltante <- tibble(
      rodal = rodales_sin_superficie$rodal,
      superficie_ha = sup_promedio
    )
    superficie_por_rodal <- bind_rows(superficie_por_rodal, superficie_faltante)
  }
  
  comparacion <- inicial_especies %>%
    rename(vol_muestreado_ini = vol_muestreado_m3,
           vol_ha_ini = vol_ha_m3,
           n_arboles_ini = n_arboles) %>%
    left_join(
      final_especies %>%
        rename(vol_muestreado_fin = vol_muestreado_m3,
               vol_ha_fin = vol_ha_m3,
               n_arboles_fin = n_arboles),
      by = c("rodal", "genero", "especie")
    ) %>%
    left_join(superficie_por_rodal, by = "rodal") %>%
    mutate(
      # ER y volumen final ya están calculados correctamente como m³/ha
      ER_m3_ha = vol_ha_ini,
      vol_fin_m3_ha = vol_ha_fin,
      ICA_m3_ha = (vol_fin_m3_ha - ER_m3_ha) / años,
      ICA_rel_i = ifelse(ER_m3_ha > 0, ICA_m3_ha / ER_m3_ha, 0),
      ciclo_corta = config$periodo,
      IntCor_rel_IC = ifelse(ICA_rel_i > -1,
                             1 - 1/((1 + ICA_rel_i)^ciclo_corta),
                             0),
      VC_ha_m3 = IntCor_rel_IC * ER_m3_ha,
      ER_rodal_m3 = ER_m3_ha * superficie_ha,
      VC_rodal_m3 = VC_ha_m3 * superficie_ha
    ) %>%
    select(rodal, genero, especie, Sup_ha = superficie_ha, ER_m3_ha, ICA_m3_ha, ICA_rel_i,
           ciclo_corta, IntCor_rel_IC, VC_ha_m3,
           ER_rodal_m3, VC_rodal_m3)
  
  return(comparacion)
}

# ==============================================================================
# RESUMEN GENERAL DEL PREDIO
# ==============================================================================

calcular_resumen_predio <- function(ica_por_rodal, años, config) {
  
  resumen <- ica_por_rodal %>%
    summarise(
      superficie_total_ha = sum(Sup_ha),
      ER_total_m3 = sum(ER_rodal_m3),
      ciclo_corta = first(ciclo_corta),
      VC_total_ciclo_m3 = sum(VC_rodal_m3),
      VC_anual_m3 = VC_total_ciclo_m3 / ciclo_corta,
      area_corta_anual_ha = superficie_total_ha / ciclo_corta,
      .groups = "drop"
    )
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║            RESUMEN GENERAL DEL PREDIO                     ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  cat(sprintf("Superficie de la UM:                  %.3f hectáreas\n", 
              resumen$superficie_total_ha))
  cat(sprintf("Existencias reales en la UM:          %.3f m³\n", 
              resumen$ER_total_m3))
  cat(sprintf("Ciclo de corta:                       %d años\n", 
              resumen$ciclo_corta))
  cat(sprintf("Vol. de corta durante el ciclo:       %.3f m³\n", 
              resumen$VC_total_ciclo_m3))
  cat(sprintf("Posibilidad anual:                    %.3f m³\n", 
              resumen$VC_anual_m3))
  cat(sprintf("Área de corta anual:                  %.3f ha\n\n", 
              resumen$area_corta_anual_ha))
  
  return(resumen)
}

# ==============================================================================
# EXPORTAR TABLAS LaTeX
# ==============================================================================

exportar_tablas_latex_ica <- function(resultados_ica, directorio = "tablas_latex") {
  
  cat("\n[PASO 4/4] Exportando tablas LaTeX...\n\n")
  
  # Crear directorio si no existe
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
  }
  
  # --------------------------------------------------
  # TABLA 1: ICA POR RODAL
  # --------------------------------------------------
  
  tabla1 <- resultados_ica$ica_por_rodal %>%
    mutate(across(where(is.numeric), ~round(., 3)))
  
  xtab1 <- xtable(tabla1,
                  caption = "Variables de manejo forestal por Rodal (UMM) según sección 11.1.4 del Manual PMF",
                  label = "tab:ica_rodal",
                  digits = 3)
  
  print(xtab1,
        file = file.path(directorio, "31_ica_por_rodal.tex"),
        include.rownames = FALSE,
        caption.placement = "top",
        booktabs = TRUE,
        sanitize.text.function = identity)
  
  cat(sprintf("  ✓ %s\n", "31_ica_por_rodal.tex"))
  
  # --------------------------------------------------
  # TABLA 2: ICA POR GÉNERO Y RODAL
  # --------------------------------------------------
  
  tabla2 <- resultados_ica$ica_por_genero_rodal %>%
    mutate(across(where(is.numeric), ~round(., 3)))
  
  xtab2 <- xtable(tabla2,
                  caption = "Variables de manejo forestal por Género y Rodal",
                  label = "tab:ica_genero_rodal",
                  digits = 3)
  
  print(xtab2,
        file = file.path(directorio, "31_ica_por_genero_rodal.tex"),
        include.rownames = FALSE,
        caption.placement = "top",
        booktabs = TRUE,
        sanitize.text.function = identity)
  
  cat(sprintf("  ✓ %s\n", "31_ica_por_genero_rodal.tex"))
  
  # --------------------------------------------------
  # TABLA 3: ICA POR ESPECIE Y RODAL (PINUS/QUERCUS)
  # --------------------------------------------------
  
  tabla3 <- resultados_ica$ica_por_especie_rodal %>%
    mutate(across(where(is.numeric), ~round(., 3)))
  
  xtab3 <- xtable(tabla3,
                  caption = "Variables de manejo forestal por Especie (Pinus y Quercus) y Rodal",
                  label = "tab:ica_especie_rodal",
                  digits = 3)
  
  print(xtab3,
        file = file.path(directorio, "31_ica_por_especie_rodal.tex"),
        include.rownames = FALSE,
        caption.placement = "top",
        booktabs = TRUE,
        sanitize.text.function = identity)
  
  cat(sprintf("  ✓ %s\n", "31_ica_por_especie_rodal.tex"))
  
  # --------------------------------------------------
  # TABLA 4: RESUMEN PREDIO
  # --------------------------------------------------
  
  tabla4 <- resultados_ica$resumen_predio %>%
    mutate(across(where(is.numeric), ~round(., 3))) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Valor")
  
  xtab4 <- xtable(tabla4,
                  caption = "Resumen general del predio - Posibilidad de corta",
                  label = "tab:resumen_predio",
                  digits = 3)
  
  print(xtab4,
        file = file.path(directorio, "31_resumen_predio.tex"),
        include.rownames = FALSE,
        caption.placement = "top",
        booktabs = TRUE,
        sanitize.text.function = identity)
  
  cat(sprintf("  ✓ %s\n", "31_resumen_predio.tex"))
  
  cat("\n✓ Tablas LaTeX exportadas exitosamente\n\n")
}

# ==============================================================================
# GUARDAR RESULTADOS
# ==============================================================================

guardar_resultados_ica <- function(resultados_ica, directorio = "resultados") {
  
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
  }
  
  # Guardar RDS completo
  saveRDS(resultados_ica, file.path(directorio, "31_resultados_ica.rds"))
  
  # Guardar CSVs individuales para uso en optimizador
  write.csv(resultados_ica$ica_por_rodal,
            file.path(directorio, "31_ica_por_rodal.csv"),
            row.names = FALSE)
  
  write.csv(resultados_ica$ica_por_genero_rodal,
            file.path(directorio, "31_ica_por_genero_rodal.csv"),
            row.names = FALSE)
  
  write.csv(resultados_ica$ica_por_especie_rodal,
            file.path(directorio, "31_ica_por_especie_rodal.csv"),
            row.names = FALSE)
  
  write.csv(resultados_ica$resumen_predio,
            file.path(directorio, "31_resumen_predio.csv"),
            row.names = FALSE)
  
  cat("\n✓ Resultados guardados en directorio resultados/\n")
  cat("  • 31_resultados_ica.rds (objeto completo)\n")
  cat("  • 31_ica_por_rodal.csv\n")
  cat("  • 31_ica_por_genero_rodal.csv\n")
  cat("  • 31_ica_por_especie_rodal.csv\n")
  cat("  • 31_resumen_predio.csv\n\n")
}

# ==============================================================================
# MENSAJE DE CARGA
# ==============================================================================

cat("\n✓ Módulo de cálculo de ICA cargado (sin cortes)\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("Funciones disponibles:\n")
cat("  • calcular_ica_sin_cortes(arboles, config, años=10)\n")
cat("  • exportar_tablas_latex_ica(resultados)\n")
cat("  • guardar_resultados_ica(resultados)\n\n")
cat("Este módulo calcula ICA derivado del modelo poblacional,\n")
cat("más preciso que la fórmula tradicional de interés compuesto.\n")
cat("══════════════════════════════════════════════════════════════\n\n")