# ==============================================================================
# 16_DIAGNOSTICO_AUTOMATIZADO.R
# Sistema de diagnóstico de estructura y recomendación de estrategias de corta
# Genera reportes técnicos para PMF con visión a 30 años (3 ciclos)
# ==============================================================================

library(tidyverse)

# Verificar dependencias
if (!exists("CONFIG")) {
  stop("❌ CONFIG no está cargado. Ejecuta: source('modelov5/01_parametros_configuracion.R')")
}

# ==============================================================================
# 1. CALCULAR MÉTRICAS DE DIAGNÓSTICO
# ==============================================================================

calcular_metricas_diagnostico <- function(vivos, config = CONFIG) {
  
  if (nrow(vivos) < 20) {
    warning("Muy pocos árboles para diagnóstico confiable")
    return(NULL)
  }
  
  # Área basimétrica actual
  n_muestreos <- n_distinct(vivos$muestreo)
  area_total_ha <- config$area_parcela_ha * n_muestreos
  
  G_actual <- sum(vivos$area_basal, na.rm = TRUE) / area_total_ha
  
  # G potencial (usar del config o calibrar según calidad sitio)
  G_potencial <- ifelse(is.null(config$g_objetivo), 25, config$g_objetivo)
  
  # Distribución por clases de 5 cm
  dist <- vivos %>%
    mutate(
      clase = cut(
        diametro_normal,
        breaks = seq(5, max(diametro_normal) + 5, by = 5),
        include.lowest = TRUE,
        right = FALSE
      )
    ) %>%
    filter(!is.na(clase)) %>%
    count(clase, .drop = FALSE) %>%
    mutate(pct = n / sum(n) * 100)
  
  # Índice de regeneración (5-15 cm)
  clases_regen <- c("[5,10)", "[10,15)")
  IR <- sum(dist$pct[dist$clase %in% clases_regen], na.rm = TRUE)
  
  # Índice de reserva (> 30 cm, DMC promedio)
  dmc_prom <- mean(unlist(config$dmc))
  idx_reserva <- which(as.numeric(gsub("\\[|\\(|,.*", "", dist$clase)) >= dmc_prom)
  IRe <- sum(dist$pct[idx_reserva], na.rm = TRUE)
  
  # Índice de concentración (clase con mayor %)
  IC <- max(dist$pct, na.rm = TRUE)
  clase_dominante <- dist$clase[which.max(dist$pct)]
  
  # Coeficiente de forma (diferencia con Liocourt ideal)
  dist_ideal <- generar_distribucion_liocourt_ideal(config$q_factor, nrow(dist))
  CF <- sum(abs(dist$pct - dist_ideal$pct)) / 2
  
  # Ratio G
  ratio_G <- G_actual / G_potencial
  
  # Densidad
  densidad_ha <- nrow(vivos) / area_total_ha
  
  return(list(
    G_actual = G_actual,
    G_potencial = G_potencial,
    ratio_G = ratio_G,
    IR = IR,
    IRe = IRe,
    IC = IC,
    CF = CF,
    clase_dominante = clase_dominante,
    distribucion = dist,
    n_arboles = nrow(vivos),
    densidad_ha = densidad_ha,
    area_muestreada_ha = area_total_ha
  ))
}

# ==============================================================================
# 2. GENERAR DISTRIBUCIÓN LIOCOURT IDEAL (REFERENCIA)
# ==============================================================================

generar_distribucion_liocourt_ideal <- function(q_factor = 1.7, n_clases = 10) {
  
  # J invertida teórica: N_i = N_0 / q^i
  N_base <- 100  # Valor arbitrario para calcular proporciones
  
  idx <- 0:(n_clases - 1)
  n_esperado <- N_base / (q_factor ^ idx)
  pct <- (n_esperado / sum(n_esperado)) * 100
  
  return(data.frame(
    clase_idx = idx,
    pct = pct
  ))
}

# ==============================================================================
# 3. CLASIFICAR ESTRUCTURA DEL RODAL
# ==============================================================================

clasificar_estructura <- function(metricas, verbose = TRUE) {
  
  if (is.null(metricas)) {
    return(list(
      clase = "INSUFICIENTE",
      descripcion = "Datos insuficientes para clasificar"
    ))
  }
  
  # Lógica de clasificación según árbol de decisión
  if (metricas$ratio_G < 0.5) {
    clase <- "D_EMPOBRECIDA"
    descripcion <- "Rodal degradado con baja densidad. Requiere regeneración"
    
  } else if (metricas$IR > 30 && metricas$CF < 20) {
    clase <- "A_J_INVERTIDA"
    descripcion <- "Estructura irregular balanceada (J invertida). Apta para Liocourt"
    
  } else if (metricas$IC > 50 && metricas$IRe < 10) {
    clase <- "B_CAMPANA"
    descripcion <- "Estructura en campana o bimodal. Requiere transformación"
    
  } else if (metricas$IC > 60) {
    clase <- "C_UNIFORME"
    descripcion <- "Estructura uniforme/coetánea. Requiere diversificación"
    
  } else {
    clase <- "INDETERMINADA"
    descripcion <- "Estructura irregular no clasificable. Evaluar caso por caso"
  }
  
  if (verbose) {
    cat("\n[CLASIFICACIÓN ESTRUCTURAL]\n")
    cat("═══════════════════════════════════════════════════════════\n")
    cat(sprintf("Clase:       %s\n", clase))
    cat(sprintf("Descripción: %s\n", descripcion))
  }
  
  return(list(
    clase = clase,
    descripcion = descripcion
  ))
}

# ==============================================================================
# 4. GENERAR ESTRATEGIA A 30 AÑOS (3 CICLOS × 10 AÑOS)
# ==============================================================================

generar_estrategia_30_anos <- function(clase, metricas, rodal_id, config = CONFIG) {
  
  # Ajustar intensidad según ratio_G
  intensidad_base <- case_when(
    metricas$ratio_G > 1.0 ~ 25,
    metricas$ratio_G > 0.9 ~ 20,
    metricas$ratio_G > 0.7 ~ 15,
    TRUE ~ 10
  )
  
  # Estrategia según clase estructural
  estrategia <- switch(
    clase,
    
    # ========================================================================
    # CLASE A: J INVERTIDA → MANTENIMIENTO
    # ========================================================================
    "A_J_INVERTIDA" = {
      list(
        objetivo = "MANTENIMIENTO de estructura J invertida",
        fase = "Mantenimiento continuo",
        
        ciclo1 = list(
          año = 2,
          metodo = "LIOCOURT",
          intensidad = min(intensidad_base, 20),
          d_min = NULL,  # Usar DMC del config
          d_max = NULL,
          prioridad = "suprimidos",
          excluir_semilleros = TRUE,
          descripcion = "Corta selectiva en clases sobrepobladas"
        ),
        
        ciclo2 = list(
          año = 12,
          metodo = "LIOCOURT",
          intensidad = min(intensidad_base - 2, 18),
          d_min = NULL,
          d_max = NULL,
          prioridad = "suprimidos",
          excluir_semilleros = TRUE,
          descripcion = "Continuar mantenimiento con intensidad moderada"
        ),
        
        ciclo3 = list(
          año = 22,
          metodo = "LIOCOURT",
          intensidad = min(intensidad_base - 5, 15),
          d_min = NULL,
          d_max = NULL,
          prioridad = "suprimidos",
          excluir_semilleros = TRUE,
          descripcion = "Consolidar estructura equilibrada"
        )
      )
    },
    
    # ========================================================================
    # CLASE B: CAMPANA → TRANSFORMACIÓN
    # ========================================================================
    "B_CAMPANA" = {
      # Identificar rango de clases sobrerrepresentadas
      clase_dom_num <- as.numeric(gsub("\\[|\\(|,.*", "", metricas$clase_dominante))
      
      list(
        objetivo = "TRANSFORMACIÓN a estructura J invertida",
        fase = "Transformación progresiva",
        
        ciclo1 = list(
          año = 2,
          metodo = "EXISTENCIAS",
          intensidad = intensidad_base,
          d_min = 15,
          d_max = 35,
          prioridad = "intermedios",
          excluir_semilleros = TRUE,
          descripcion = "Reducir clases intermedias sobrerrepresentadas (15-35cm)"
        ),
        
        ciclo2 = list(
          año = 12,
          metodo = "ICA",
          intensidad = round(intensidad_base * 3),  # 60% aprox
          d_min = 18,
          d_max = NULL,
          prioridad = "suprimidos",
          excluir_semilleros = TRUE,
          descripcion = "Aprovechar ICA acumulado, favoreciendo suprimidos"
        ),
        
        ciclo3 = list(
          año = 22,
          metodo = "LIOCOURT",
          intensidad = 15,
          d_min = NULL,
          d_max = NULL,
          prioridad = "suprimidos",
          excluir_semilleros = TRUE,
          descripcion = "Transición a Liocourt si estructura mejoró"
        )
      )
    },
    
    # ========================================================================
    # CLASE C: UNIFORME → DIVERSIFICACIÓN
    # ========================================================================
    "C_UNIFORME" = {
      clase_dom_num <- as.numeric(gsub("\\[|\\(|,.*", "", metricas$clase_dominante))
      
      list(
        objetivo = "DIVERSIFICACIÓN estructural progresiva",
        fase = "Transición a irregular",
        
        ciclo1 = list(
          año = 2,
          metodo = "ICA",
          intensidad = round(intensidad_base * 0.6),  # Conservador
          d_min = max(clase_dom_num - 5, 15),
          d_max = clase_dom_num + 10,
          prioridad = "suprimidos",
          excluir_semilleros = TRUE,
          descripcion = "Iniciar diversificación en clase dominante"
        ),
        
        ciclo2 = list(
          año = 12,
          metodo = "EXISTENCIAS",
          intensidad = round(intensidad_base * 0.8),
          d_min = clase_dom_num,
          d_max = clase_dom_num + 10,
          prioridad = "intermedios",
          excluir_semilleros = FALSE,
          descripcion = "Reducir concentración en clase dominante"
        ),
        
        ciclo3 = list(
          año = 22,
          metodo = "ICA",
          intensidad = round(intensidad_base * 3),
          d_min = NULL,
          d_max = NULL,
          prioridad = "suprimidos",
          excluir_semilleros = TRUE,
          descripcion = "Continuar diversificación estructural"
        )
      )
    },
    
    # ========================================================================
    # CLASE D: EMPOBRECIDA → REGENERACIÓN
    # ========================================================================
    "D_EMPOBRECIDA" = {
      list(
        objetivo = "REGENERACIÓN antes de cosecha",
        fase = "Protección y recuperación",
        
        ciclo1 = list(
          año = NULL,
          metodo = "SIN_CORTA",
          intensidad = 0,
          d_min = NULL,
          d_max = NULL,
          prioridad = NULL,
          excluir_semilleros = TRUE,
          descripcion = "Permitir regeneración natural sin intervención"
        ),
        
        ciclo2 = list(
          año = 12,
          metodo = "SANITARIO",
          intensidad = 5,
          d_min = NULL,
          d_max = NULL,
          prioridad = "muertos",
          excluir_semilleros = TRUE,
          descripcion = "Corta sanitaria mínima (muertos/enfermos)"
        ),
        
        ciclo3 = list(
          año = 22,
          metodo = "ICA",
          intensidad = 10,
          d_min = NULL,
          d_max = NULL,
          prioridad = "suprimidos",
          excluir_semilleros = TRUE,
          descripcion = "Iniciar aprovechamiento si estructura mejoró"
        )
      )
    },
    
    # ========================================================================
    # DEFAULT: INDETERMINADA
    # ========================================================================
    {
      list(
        objetivo = "Evaluación manual requerida",
        fase = "Estrategia conservadora por defecto",
        
        ciclo1 = list(
          año = 2,
          metodo = "ICA",
          intensidad = round(intensidad_base * 0.6),
          d_min = NULL,
          d_max = NULL,
          prioridad = "suprimidos",
          excluir_semilleros = TRUE,
          descripcion = "Aprovechamiento conservador del ICA"
        ),
        
        ciclo2 = list(
          año = 12,
          metodo = "ICA",
          intensidad = round(intensidad_base * 0.6),
          d_min = NULL,
          d_max = NULL,
          prioridad = "suprimidos",
          excluir_semilleros = TRUE,
          descripcion = "Continuar aprovechamiento conservador"
        ),
        
        ciclo3 = list(
          año = 22,
          metodo = "ICA",
          intensidad = round(intensidad_base * 0.6),
          d_min = NULL,
          d_max = NULL,
          prioridad = "suprimidos",
          excluir_semilleros = TRUE,
          descripcion = "Mantener estrategia conservadora"
        )
      )
    }
  )
  
  # Convertir a formato PROGRAMA_CORTAS
  programa <- generar_programa_cortas_from_estrategia(rodal_id, estrategia)
  estrategia$programa_cortas <- programa
  
  return(estrategia)
}

# ==============================================================================
# 5. CONVERTIR ESTRATEGIA A FORMATO PROGRAMA_CORTAS
# ==============================================================================

generar_programa_cortas_from_estrategia <- function(rodal_id, estrategia) {
  
  filas <- list()
  
  for (ciclo_nombre in c("ciclo1", "ciclo2", "ciclo3")) {
    ciclo <- estrategia[[ciclo_nombre]]
    
    # Solo agregar si hay año de corta (no NULL)
    if (!is.null(ciclo$año)) {
      fila <- tibble(
        rodal = rodal_id,
        año_corta = ciclo$año,
        metodo = ciclo$metodo,
        intensidad_pct = ciclo$intensidad,
        d_min = ifelse(is.null(ciclo$d_min), NA_real_, ciclo$d_min),
        d_max = ifelse(is.null(ciclo$d_max), NA_real_, ciclo$d_max),
        prioridad = ifelse(is.null(ciclo$prioridad), "suprimidos", ciclo$prioridad),
        excluir_semilleros = ciclo$excluir_semilleros
      )
      filas[[length(filas) + 1]] <- fila
    }
  }
  
  if (length(filas) > 0) {
    return(bind_rows(filas))
  } else {
    return(tibble())  # Rodal sin cortas programadas
  }
}

# ==============================================================================
# 6. GENERAR REPORTE TÉCNICO
# ==============================================================================

generar_reporte_tecnico <- function(rodal_id, metricas, clasificacion, 
                                     estrategia, arboles_vivos) {
  
  # Evaluación general
  eval_general <- case_when(
    metricas$ratio_G < 0.5 ~ "DEFICIENTE - Requiere regeneración prioritaria",
    metricas$ratio_G < 0.7 || metricas$CF > 40 ~ "REGULAR - Requiere transformación estructural",
    metricas$CF < 20 && metricas$IR > 30 ~ "BUENO - Estructura equilibrada",
    TRUE ~ "ACEPTABLE - Con potencial de mejora"
  )
  
  # Recomendaciones adicionales
  recos <- generar_recomendaciones_adicionales(clasificacion$clase, metricas)
  
  # Texto del reporte
  texto <- sprintf("
═══════════════════════════════════════════════════════════
REPORTE DE DIAGNÓSTICO Y ESTRATEGIA DE MANEJO
═══════════════════════════════════════════════════════════
Rodal:          %d
Fecha:          %s
Área muestreada: %.2f ha
Árboles:        %d (%.0f árb/ha)
═══════════════════════════════════════════════════════════

1. DIAGNÓSTICO ACTUAL
───────────────────────────────────────────────────────────
Clasificación estructural: %s
%s

Métricas dasométricas:
  • Área basimétrica:    %.2f m²/ha (objetivo: %.2f m²/ha)
  • Ratio G:             %.2f (%.0f%% del objetivo)
  • Densidad:            %.0f árb/ha
  
Índices estructurales:
  • Regeneración (IR):   %.1f%% (objetivo: >30%%)
  • Reserva (IRe):       %.1f%% (objetivo: >15%%)
  • Concentración (IC):  %.1f%% en %s (objetivo: <30%%)
  • Forma (CF):          %.1f%% (objetivo: <20%%)

Evaluación general: %s

2. ESTRATEGIA RECOMENDADA A 30 AÑOS
───────────────────────────────────────────────────────────
Objetivo a largo plazo: %s
Fase de manejo:         %s

CICLO 1 (Años 1-10):
  Año de intervención:  %s
  Método:               %s
  Intensidad:           %d%%
  Filtros diamétricos:  %s
  Prioridad de corta:   %s
  Proteger semilleros:  %s
  
  Justificación:
  %s

CICLO 2 (Años 11-20):
  Año de intervención:  %s
  Método:               %s
  Intensidad:           %d%%
  Filtros diamétricos:  %s
  
  Justificación:
  %s

CICLO 3 (Años 21-30):
  Año de intervención:  %s
  Método:               %s
  Intensidad:           %d%%
  
  Justificación:
  %s

3. RECOMENDACIONES COMPLEMENTARIAS
───────────────────────────────────────────────────────────
%s

4. MONITOREO Y REEVALUACIÓN
───────────────────────────────────────────────────────────
□ Realizar inventario intermedio en año 5 y año 15
□ Evaluar éxito de regeneración natural post-corta
□ Ajustar estrategia según evolución de estructura
□ Considerar tratamientos silvícolas complementarios
□ Documentar respuesta del rodal a intervenciones

NOTA: Este diagnóstico es automatizado. Se recomienda validación
      por técnico forestal antes de implementar.

═══════════════════════════════════════════════════════════
Generado por: SIERRAFOR v2.0 - Módulo de diagnóstico
═══════════════════════════════════════════════════════════
",
    # Header
    rodal_id,
    format(Sys.Date(), "%d/%m/%Y"),
    metricas$area_muestreada_ha,
    metricas$n_arboles,
    metricas$densidad_ha,
    
    # Diagnóstico
    clasificacion$clase,
    clasificacion$descripcion,
    metricas$G_actual,
    metricas$G_potencial,
    metricas$ratio_G,
    (metricas$ratio_G * 100),
    metricas$densidad_ha,
    metricas$IR,
    metricas$IRe,
    metricas$IC,
    as.character(metricas$clase_dominante),
    metricas$CF,
    eval_general,
    
    # Estrategia
    estrategia$objetivo,
    estrategia$fase,
    
    # Ciclo 1
    ifelse(is.null(estrategia$ciclo1$año), "SIN CORTA", 
           sprintf("Año %d (%d)", estrategia$ciclo1$año, 
                   CONFIG$inicio + estrategia$ciclo1$año - 1)),
    estrategia$ciclo1$metodo,
    estrategia$ciclo1$intensidad,
    generar_texto_filtros(estrategia$ciclo1),
    estrategia$ciclo1$prioridad,
    ifelse(estrategia$ciclo1$excluir_semilleros, "Sí", "No"),
    estrategia$ciclo1$descripcion,
    
    # Ciclo 2
    ifelse(is.null(estrategia$ciclo2$año), "SIN CORTA", 
           sprintf("Año %d (%d)", estrategia$ciclo2$año,
                   CONFIG$inicio + estrategia$ciclo2$año - 1)),
    estrategia$ciclo2$metodo,
    estrategia$ciclo2$intensidad,
    generar_texto_filtros(estrategia$ciclo2),
    estrategia$ciclo2$descripcion,
    
    # Ciclo 3
    ifelse(is.null(estrategia$ciclo3$año), "SIN CORTA", 
           sprintf("Año %d (%d)", estrategia$ciclo3$año,
                   CONFIG$inicio + estrategia$ciclo3$año - 1)),
    estrategia$ciclo3$metodo,
    estrategia$ciclo3$intensidad,
    estrategia$ciclo3$descripcion,
    
    # Recomendaciones
    recos
  )
  
  return(list(
    texto = texto
  ))
}

# ==============================================================================
# 7. FUNCIONES AUXILIARES
# ==============================================================================

generar_texto_filtros <- function(ciclo) {
  filtros <- c()
  
  if (!is.null(ciclo$d_min) && !is.na(ciclo$d_min)) {
    filtros <- c(filtros, sprintf("d_min=%dcm", ciclo$d_min))
  }
  if (!is.null(ciclo$d_max) && !is.na(ciclo$d_max)) {
    filtros <- c(filtros, sprintf("d_max=%dcm", ciclo$d_max))
  }
  
  if (length(filtros) == 0) {
    return("Según DMC por género"
  } else {
    return(paste(filtros, collapse=", "))
  }
}

generar_recomendaciones_adicionales <- function(clase, metricas) {
  recos <- c()
  
  # Regeneración crítica
  if (metricas$IR < 15) {
    recos <- c(recos, "• CRÍTICO: Promover regeneración natural mediante:")
    recos <- c(recos, "  - Aperturas selectivas del dosel")
    recos <- c(recos, "  - Control de vegetación competidora")
    recos <- c(recos, "  - Protección contra ganado en áreas críticas")
  } else if (metricas$IR < 25) {
    recos <- c(recos, "• Monitorear regeneración y considerar aperturas de dosel")
  }
  
  # Reserva de semilleros
  if (metricas$IRe < 10) {
    recos <- c(recos, "• IMPORTANTE: Proteger estrictamente árboles semilleros:")
    recos <- c(recos, "  - Árboles >DMC con dominancia 1-3")
    recos <- c(recos, "  - Distribuidos espacialmente en todo el rodal")
  }
  
  # Alta concentración
  if (metricas$IC > 50) {
    recos <- c(recos, "• Priorizar diversificación estructural en primeros ciclos")
  }
  
  # Sobrepoblación
  if (metricas$ratio_G > 1.0) {
    recos <- c(recos, "• Evaluar necesidad de cortas de liberación adicionales")
    recos <- c(recos, "• Reducir competencia para favorecer crecimiento")
  }
  
  # Recomendaciones específicas por clase
  if (clase == "B_CAMPANA") {
    recos <- c(recos, "• Evaluar tratamientos de liberación de regeneración")
    recos <- c(recos, "• Monitorear respuesta de clases pequeñas post-corta")
    recos <- c(recos, "• Considerar plantación enriquecimiento si regeneración insuficiente")
  }
  
  if (clase == "D_EMPOBRECIDA") {
    recos <- c(recos, "• PRIORITARIO: Restaurar densidad antes de cosechar")
    recos <- c(recos, "• Considerar vedas temporales o protección estricta")
    recos <- c(recos, "• Evaluar necesidad de plantación de enriquecimiento")
  }
  
  # Si no hay recomendaciones críticas
  if (length(recos) == 0) {
    return("• Seguir estrategia propuesta y monitorear evolución\n• Ajustar según respuesta del rodal a intervenciones")
  } else {
    return(paste(recos, collapse = "\n"))
  }
}

# ==============================================================================
# 8. FUNCIÓN MAESTRA: DIAGNOSTICAR RODAL
# ==============================================================================

diagnosticar_rodal_automatico <- function(arboles_rodal, 
                                          rodal_id,
                                          config = CONFIG,
                                          verbose = TRUE) {
  
  if (verbose) {
    cat(sprintf("\n╔════════════════════════════════════════════════════════════╗\n"))
    cat(sprintf("║         DIAGNÓSTICO AUTOMATIZADO - RODAL %02d                ║\n", rodal_id))
    cat(sprintf("╚════════════════════════════════════════════════════════════╝\n"))
  }
  
  # Filtrar vivos
  vivos <- arboles_rodal %>% filtrar_arboles_vivos()
  
  if (nrow(vivos) < 20) {
    cat("⚠️  Muy pocos árboles. Diagnóstico no confiable.\n")
    return(list(
      rodal_id = rodal_id,
      clasificacion = list(
        clase = "INSUFICIENTE",
        descripcion = "Menos de 20 árboles. Evaluar manualmente"
      ),
      reporte = list(texto = "RODAL CON DATOS INSUFICIENTES")
    ))
  }
  
  # PASO 1: Calcular métricas
  if (verbose) cat("\n[PASO 1] Calculando métricas...\n")
  metricas <- calcular_metricas_diagnostico(vivos, config)
  
  if (verbose) {
    cat(sprintf("  G actual:      %.2f m²/ha\n", metricas$G_actual))
    cat(sprintf("  G objetivo:    %.2f m²/ha\n", metricas$G_potencial))
    cat(sprintf("  Ratio G:       %.2f (%.0f%%)\n", metricas$ratio_G, metricas$ratio_G*100))
    cat(sprintf("  Densidad:      %.0f árb/ha\n", metricas$densidad_ha))
    cat(sprintf("  Regeneración:  %.1f%%\n", metricas$IR))
    cat(sprintf("  Reserva:       %.1f%%\n", metricas$IRe))
    cat(sprintf("  Concentración: %.1f%%\n", metricas$IC))
    cat(sprintf("  Forma:         %.1f%%\n", metricas$CF))
  }
  
  # PASO 2: Clasificar estructura
  if (verbose) cat("\n[PASO 2] Clasificando estructura...\n")
  clasificacion <- clasificar_estructura(metricas, verbose)
  
  # PASO 3: Generar estrategia 30 años
  if (verbose) cat("\n[PASO 3] Generando estrategia 30 años...\n")
  estrategia <- generar_estrategia_30_anos(
    clasificacion$clase,
    metricas,
    rodal_id,
    config
  )
  
  if (verbose) {
    cat(sprintf("  Objetivo:  %s\n", estrategia$objetivo))
    cat(sprintf("  Ciclo 1:   Año %s - %s (%d%%)\n",
                ifelse(is.null(estrategia$ciclo1$año), "N/A", estrategia$ciclo1$año),
                estrategia$ciclo1$metodo,
                estrategia$ciclo1$intensidad))
    cat(sprintf("  Ciclo 2:   Año %s - %s (%d%%)\n",
                ifelse(is.null(estrategia$ciclo2$año), "N/A", estrategia$ciclo2$año),
                estrategia$ciclo2$metodo,
                estrategia$ciclo2$intensidad))
    cat(sprintf("  Ciclo 3:   Año %s - %s (%d%%)\n",
                ifelse(is.null(estrategia$ciclo3$año), "N/A", estrategia$ciclo3$año),
                estrategia$ciclo3$metodo,
                estrategia$ciclo3$intensidad))
  }
  
  # PASO 4: Generar reporte
  if (verbose) cat("\n[PASO 4] Generando reporte técnico...\n")
  reporte <- generar_reporte_tecnico(
    rodal_id,
    metricas,
    clasificacion,
    estrategia,
    vivos
  )
  
  if (verbose) {
    cat("\n✓ Diagnóstico completado\n")
  }
  
  # Retornar resultado completo
  return(list(
    rodal_id = rodal_id,
    metricas = metricas,
    clasificacion = clasificacion,
    estrategia = estrategia,
    reporte = reporte,
    programa_cortas = estrategia$programa_cortas
  ))
}

# ==============================================================================
# 9. DIAGNOSTICAR TODOS LOS RODALES
# ==============================================================================

diagnosticar_todos_rodales <- function(arboles_df, config = CONFIG, 
                                        guardar = TRUE, verbose = TRUE) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║     DIAGNÓSTICO AUTOMATIZADO - TODOS LOS RODALES          ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  rodales <- sort(unique(arboles_df$rodal))
  cat(sprintf("Rodales a diagnosticar: %s\n", paste(rodales, collapse=", ")))
  
  diagnosticos <- list()
  programas <- list()
  
  for (r in rodales) {
    arboles_r <- arboles_df %>% filter(rodal == r)
    
    diag <- diagnosticar_rodal_automatico(arboles_r, r, config, verbose = verbose)
    
    diagnosticos[[as.character(r)]] <- diag
    programas[[length(programas) + 1]] <- diag$programa_cortas
    
    # Guardar reporte individual si solicitado
    if (guardar && nrow(diag$programa_cortas) > 0) {
      dir.create("reportes", showWarnings = FALSE)
      writeLines(
        diag$reporte$texto,
        sprintf("reportes/diagnostico_rodal_%02d.txt", r)
      )
    }
  }
  
  # Consolidar programa de cortas
  programa_consolidado <- bind_rows(programas)
  
  # Guardar resultados
  if (guardar) {
    saveRDS(diagnosticos, "resultados/diagnosticos_automatizados.rds")
    write.csv(programa_consolidado, 
              "resultados/programa_cortas_recomendado.csv",
              row.names = FALSE)
    
    cat("\n✓ Resultados guardados en:\n")
    cat("  • resultados/diagnosticos_automatizados.rds\n")
    cat("  • resultados/programa_cortas_recomendado.csv\n")
    cat("  • reportes/diagnostico_rodal_XX.txt (uno por rodal)\n\n")
  }
  
  # Resumen por clase
  cat("\n[RESUMEN POR CLASIFICACIÓN]\n")
  cat("═══════════════════════════════════════════════════════════\n")
  
  resumen_clases <- tibble(
    rodal = rodales,
    clase = sapply(diagnosticos, function(d) d$clasificacion$clase),
    G_actual = sapply(diagnosticos, function(d) d$metricas$G_actual),
    metodo_ciclo1 = sapply(diagnosticos, function(d) d$estrategia$ciclo1$metodo)
  )
  
  print(resumen_clases)
  
  cat("\n")
  tabla_clases <- table(resumen_clases$clase)
  print(tabla_clases)
  
  return(list(
    diagnosticos = diagnosticos,
    programa_cortas = programa_consolidado,
    resumen = resumen_clases
  ))
}

# ==============================================================================
# MENSAJE DE CARGA
# ==============================================================================

cat("\n✓ Módulo de diagnóstico automatizado cargado\n")
cat("  Funciones principales:\n")
cat("    - diagnosticar_rodal_automatico(arboles, rodal_id, config)\n")
cat("    - diagnosticar_todos_rodales(arboles_df, config)\n\n")
cat("  Sistema de clasificación:\n")
cat("    A = J invertida (Liocourt directo)\n")
cat("    B = Campana (Existencias → ICA → Liocourt)\n")
cat("    C = Uniforme (ICA conservador)\n")
cat("    D = Empobrecida (Sin corta → Regeneración)\n\n")
