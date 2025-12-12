# ==============================================================================
# 14_OPTIMIZADOR_CORTAS.R - MÉTODO ICA-LIOCOURT FINAL
# Versión correcta: ICA define CUÁNTO, Liocourt define DÓNDE
# ==============================================================================

library(tidyverse)

if (!exists("CONFIG")) {
  stop("❌ CONFIG no está cargado. Ejecuta: source('modelov5/01_parametros_configuracion.R')")
}

# ==============================================================================
# CALCULAR DISTRIBUCIÓN LIOCOURT COMO GUÍA
# ==============================================================================

#' @title Calcular distribución Liocourt como guía de estructura
#' @description NO ajusta N_ref, solo identifica clases sobrepobladas
#' 
#' @param arboles_vivos Data frame con árboles vivos
#' @param q Cociente de Liocourt (define FORMA, no magnitud)
#' @param tolerancia Tolerancia % para considerar "equilibrado"
#' @param amplitud Amplitud de clase diamétrica (cm)
#' 
#' @return Data frame con diagnóstico por clase

calcular_distribucion_liocourt_guia <- function(arboles_vivos, 
                                                q = 1.7,
                                                tolerancia = 20,
                                                amplitud = 5) {
  
  # Clasificar árboles
  arboles_clasificados <- arboles_vivos %>%
    mutate(clase_d = floor(diametro_normal / amplitud) * amplitud) %>%
    filter(!is.na(clase_d))
  
  if (nrow(arboles_clasificados) == 0) {
    warning("No hay árboles para clasificar")
    return(NULL)
  }
  
  # Distribución actual
  dist_actual <- arboles_clasificados %>%
    group_by(clase_d) %>%
    summarise(
      n_actual = n(),
      vol_actual = sum(volumen_m3, na.rm = TRUE),
      vol_medio = mean(volumen_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(clase_d)
  
  if (nrow(dist_actual) == 0) return(NULL)
  
  # ============================================================================
  # CLAVE: Usar distribución ACTUAL como base (no ajustar)
  # ============================================================================
  
  # Clase de referencia = clase con más árboles
  clase_ref <- dist_actual %>%
    arrange(desc(n_actual)) %>%
    slice(1) %>%
    pull(clase_d)
  
  n_ref <- dist_actual %>%
    filter(clase_d == clase_ref) %>%
    pull(n_actual)
  
  cat("\n[DISTRIBUCIÓN LIOCOURT - GUÍA DE ESTRUCTURA]\n")
  cat("═══════════════════════════════════════════════════════════\n")
  cat(sprintf("  Clase referencia: %d cm (n=%d)\n", clase_ref, n_ref))
  cat(sprintf("  Q-factor: %.2f\n", q))
  cat(sprintf("  Tolerancia: ±%d%%\n\n", tolerancia))
  
  # Parámetros
  d_min <- min(dist_actual$clase_d)
  d_max <- max(dist_actual$clase_d)
  clases <- seq(d_min, d_max, by = amplitud)
  
  # Distribución objetivo según Liocourt
  dist_objetivo <- data.frame(clase_d = clases) %>%
    mutate(
      # Índice desde clase de referencia
      idx = (clase_ref - clase_d) / amplitud,
      
      # N_objetivo = N_ref × q^idx
      n_objetivo = round(n_ref * q^idx)
    ) %>%
    left_join(dist_actual, by = "clase_d") %>%
    mutate(
      n_actual = replace_na(n_actual, 0),
      vol_actual = replace_na(vol_actual, 0),
      vol_medio = replace_na(vol_medio, 0),
      
      # Ratio y diferencia
      ratio = ifelse(n_objetivo > 0, n_actual / n_objetivo, NA),
      diferencia_pct = ifelse(n_objetivo > 0, 
                              ((n_actual - n_objetivo) / n_objetivo) * 100, 
                              NA_real_),
      
      # Diagnóstico
      diagnostico = case_when(
        n_objetivo == 0 ~ "Sin objetivo",
        abs(diferencia_pct) <= tolerancia ~ "Equilibrado",
        diferencia_pct > tolerancia ~ "Sobrepoblado",
        diferencia_pct < -tolerancia ~ "Subpoblado",
        TRUE ~ "Equilibrado"
      ),
      
      # Prioridad de corta: sobrepoblación × volumen medio
      prioridad_corta = ifelse(
        diagnostico == "Sobrepoblado" & n_actual > 0,
        (diferencia_pct / 100) * vol_medio * n_actual,  # Prioridad total de la clase
        0
      )
    ) %>%
    arrange(desc(prioridad_corta))
  
  # Resumen diagnóstico
  sobrepobladas <- dist_objetivo %>% filter(diagnostico == "Sobrepoblado")
  equilibradas <- dist_objetivo %>% filter(diagnostico == "Equilibrado")
  subpobladas <- dist_objetivo %>% filter(diagnostico == "Subpoblado")
  
  cat("[DIAGNÓSTICO]\n")
  cat(sprintf("  Sobrepobladas:  %d clases\n", nrow(sobrepobladas)))
  cat(sprintf("  Equilibradas:   %d clases\n", nrow(equilibradas)))
  cat(sprintf("  Subpobladas:    %d clases\n\n", nrow(subpobladas)))
  
  if (nrow(sobrepobladas) > 0) {
    cat("  Clases prioritarias para corta:\n")
    for (i in 1:min(5, nrow(sobrepobladas))) {
      clase <- sobrepobladas[i, ]
      cat(sprintf("    %d. Clase %d cm: %+.0f%% (n=%d, vol_medio=%.2f m³)\n",
                  i, clase$clase_d, clase$diferencia_pct, 
                  clase$n_actual, clase$vol_medio))
    }
    cat("\n")
  }
  
  return(dist_objetivo)
}

# ==============================================================================
# CALCULAR VOLUMEN OBJETIVO
# ==============================================================================

calcular_volumen_objetivo <- function(arboles_vivos, 
                                      config = CONFIG, 
                                      corte_config,
                                      arboles_inicial = NULL,
                                      arboles_año_anterior = NULL,
                                      año_actual = NULL) {
  
  vol_actual <- sum(arboles_vivos$volumen_m3, na.rm = TRUE)
  
  # ============================================================================
  # 1. CALCULAR ICA SI HAY DATOS HISTÓRICOS
  # ============================================================================
  
  ica_info <- NULL
  
  if (!is.null(arboles_inicial) && !is.null(año_actual) && año_actual > 0) {
    vol_inicial <- sum(arboles_inicial$volumen_m3, na.rm = TRUE)
    años_transcurridos <- año_actual
    
    ica_anual <- (vol_actual - vol_inicial) / años_transcurridos
    ica_total_pmf <- ica_anual * config$periodo
    
    ica_info <- list(
      ica_anual = ica_anual,
      ica_total_pmf = ica_total_pmf,
      vol_inicial = vol_inicial,
      vol_actual = vol_actual,
      años = años_transcurridos
    )
  }
  
  # ============================================================================
  # 2. CALCULAR VOLUMEN OBJETIVO SEGÚN MÉTODO
  # ============================================================================
  
  if (corte_config$metodo == "ICA") {
    
    if (is.null(ica_info)) {
      cat("\n[VOLUMEN OBJETIVO - MÉTODO ICA]\n")
      cat("  ⚠️ No hay datos históricos para calcular ICA\n")
      cat("  💡 Usa método EXISTENCIAS para el primer año\n\n")
      return(list(
        vol_objetivo = 0, 
        metodo_usado = "ICA", 
        dist_obj = NULL,
        ica_info = NULL
      ))
    }
    
    vol_disponible <- ica_info$ica_total_pmf
    vol_objetivo <- vol_disponible * (corte_config$intensidad_pct / 100)
    
    cat("\n[VOLUMEN OBJETIVO - MÉTODO ICA]\n")
    cat("══════════════════════════════════════════════════════════\n")
    cat(sprintf("  ICA anual: %.2f m³/año\n", ica_info$ica_anual))
    cat(sprintf("  ICA total PMF (%d años): %.2f m³\n", 
                config$periodo, ica_info$ica_total_pmf))
    cat(sprintf("  Intensidad: %d%%\n", corte_config$intensidad_pct))
    cat(sprintf("  ✓ Vol objetivo: %.2f m³ (FIJO)\n", vol_objetivo))
    
    # Calcular distribución Liocourt como GUÍA (sin ajustar)
    dist_obj <- calcular_distribucion_liocourt_guia(
      arboles_vivos, 
      q = corte_config$q_factor,
      tolerancia = corte_config$tolerancia,
      amplitud = 5
    )
    
    return(list(
      vol_objetivo = vol_objetivo,
      vol_actual = vol_actual,
      metodo_usado = "ICA",
      dist_obj = dist_obj,
      ica_info = ica_info
    ))
    
  } else if (corte_config$metodo == "EXISTENCIAS") {
    
    vol_objetivo <- vol_actual * (corte_config$intensidad_pct / 100)
    
    cat("\n[VOLUMEN OBJETIVO - MÉTODO EXISTENCIAS]\n")
    cat("══════════════════════════════════════════════════════════\n")
    cat(sprintf("  Vol actual: %.2f m³\n", vol_actual))
    cat(sprintf("  Intensidad: %d%%\n", corte_config$intensidad_pct))
    cat(sprintf("  ✓ Vol objetivo: %.2f m³ (FIJO)\n", vol_objetivo))
    
    dist_obj <- calcular_distribucion_liocourt_guia(
      arboles_vivos, 
      q = corte_config$q_factor,
      tolerancia = corte_config$tolerancia,
      amplitud = 5
    )
    
    return(list(
      vol_objetivo = vol_objetivo,
      vol_actual = vol_actual,
      metodo_usado = "EXISTENCIAS",
      dist_obj = dist_obj,
      ica_info = ica_info
    ))
  }
}

# ==============================================================================
# CALCULAR PLAN DE CORTAS
# ==============================================================================

calcular_plan_cortas <- function(arboles_df, 
                                 config = CONFIG,
                                 arboles_inicial = NULL,
                                 arboles_año_anterior = NULL,
                                 corte_config = NULL,
                                 año_actual = NULL) {
  
  if (is.null(corte_config)) {
    corte_config <- configurar_corte(
      metodo = "ICA",
      intensidad_pct = 80,
      q_factor = config$q_factor,
      tolerancia_equilibrio = config$tolerancia
    )
  }
  
  cat(sprintf("\n╔═══════════════════════════════════════════════════════════╗\n"))
  cat(sprintf("║  CORTE - %s", corte_config$metodo))
  cat(sprintf("%*s║\n", 53 - nchar(corte_config$metodo), ""))
  cat(sprintf("╚═══════════════════════════════════════════════════════════╝\n"))
  
  vivos <- arboles_df %>% filtrar_arboles_vivos()
  
  if (nrow(vivos) < 10) {
    cat("\n  ⚠️ Muy pocos árboles vivos\n")
    return(list(
      arboles_marcados = tibble(), 
      resumen = tibble(),
      vol_info = list(vol_objetivo = 0)
    ))
  }
  
  # Calcular volumen objetivo y distribución Liocourt
  vol_info <- calcular_volumen_objetivo(
    vivos, 
    config, 
    corte_config,
    arboles_inicial,
    arboles_año_anterior,
    año_actual = año_actual
  )
  
  # Marcar árboles
  marcados <- marcar_arboles(
    vivos,
    vol_info$vol_objetivo,
    corte_config,
    config,
    vol_info$dist_obj
  )
  
  # Resumen
  if (nrow(marcados) > 0) {
    vol_marcado <- sum(marcados$volumen_m3, na.rm = TRUE)
    
    resumen <- tibble(
      metodo = corte_config$metodo,
      n_arboles = nrow(marcados),
      vol_total_m3 = vol_marcado,
      vol_objetivo_m3 = vol_info$vol_objetivo,
      vol_actual_m3 = vol_info$vol_actual,
      pct_vol_actual = (vol_marcado / vol_info$vol_actual) * 100,
      diferencia_m3 = vol_marcado - vol_info$vol_objetivo,
      diferencia_pct = ((vol_marcado - vol_info$vol_objetivo) / vol_info$vol_objetivo) * 100
    )
  } else {
    resumen <- tibble()
  }
  
  return(list(
    arboles_marcados = marcados,
    resumen = resumen,
    vol_info = vol_info
  ))
}

# ==============================================================================
# MARCAR ÁRBOLES
# ==============================================================================

marcar_arboles <- function(arboles_vivos, 
                           vol_objetivo,
                           corte_config,
                           config = CONFIG,
                           dist_obj = NULL) {
  
  if (vol_objetivo <= 0) {
    cat("\n  ⚠️ Vol objetivo = 0, no se marca ningún árbol\n")
    return(tibble())
  }
  
  if (is.null(dist_obj)) {
    cat("\n  ⚠️ dist_obj es NULL, no se puede marcar\n")
    return(tibble())
  }
  
  cat("\n[MARCADO DE ÁRBOLES]\n")
  cat("═══════════════════════════════════════════════════════════\n")
  cat(sprintf("  Vol objetivo: %.2f m³\n", vol_objetivo))
  cat(sprintf("  Estrategia: %s\n\n", corte_config$prioridad))
  
  # Verificar columnas
  if (!"genero_grupo" %in% names(arboles_vivos)) {
    stop("❌ ERROR: Columna 'genero_grupo' no existe")
  }
  
  # ============================================================================
  # 1. IDENTIFICAR CLASES PRIORITARIAS
  # ============================================================================
  
  clases_prioritarias <- dist_obj %>%
    filter(diagnostico == "Sobrepoblado", prioridad_corta > 0) %>%
    arrange(desc(prioridad_corta)) %>%
    select(clase_d, diferencia_pct, prioridad_corta, n_actual, vol_medio)
  
  if (nrow(clases_prioritarias) == 0) {
    cat("  ℹ️ No hay clases sobrepobladas según Liocourt\n")
    cat("  → Marcando de todas las clases >= DMC\n\n")
    
    # Si no hay sobrepoblación, usar todas las clases
    clases_prioritarias <- arboles_vivos %>%
      mutate(clase_d = floor(diametro_normal / 5) * 5) %>%
      group_by(clase_d) %>%
      summarise(
        n_actual = n(),
        vol_medio = mean(volumen_m3, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        diferencia_pct = 0,
        prioridad_corta = vol_medio * n_actual
      ) %>%
      arrange(desc(prioridad_corta))
  }
  
  cat(sprintf("  Clases con prioridad: %d\n", nrow(clases_prioritarias)))
  for (i in 1:min(3, nrow(clases_prioritarias))) {
    clase <- clases_prioritarias[i, ]
    cat(sprintf("    %d. Clase %d cm (%+.0f%%, n=%d)\n",
                i, clase$clase_d, clase$diferencia_pct, clase$n_actual))
  }
  cat("\n")
  
  # ============================================================================
  # 2. PREPARAR CANDIDATOS
  # ============================================================================
  
  cat("  Preparando candidatos...\n")
  
  # Paso 1: Calcular clase diamétrica
  candidatos <- arboles_vivos %>%
    mutate(clase_d = floor(diametro_normal / 5) * 5)
  
  # Paso 2: Asignar DMC según género
  candidatos <- candidatos %>%
    mutate(
      dmc_aplicable = case_when(
        genero_grupo == "Pinus" ~ config$dmc$Pinus,
        genero_grupo == "Quercus" ~ config$dmc$Quercus,
        TRUE ~ 25
      )
    )
  
  # Paso 3: Sobrescribir con d_min si existe (manejar lista correctamente)
  d_min_raw <- corte_config$d_min
  
  # Si d_min es una lista, extraer el primer elemento
  if (is.list(d_min_raw)) {
    d_min_raw <- d_min_raw[[1]]
  }
  
  # Solo aplicar si no es NULL y no es NA
  if (!is.null(d_min_raw) && !is.na(d_min_raw)) {
    d_min_valor <- as.numeric(d_min_raw)
    candidatos <- candidatos %>%
      mutate(dmc_aplicable = d_min_valor)
  }
  
  # Paso 4: Filtrar por diámetro mínimo
  candidatos <- candidatos %>%
    filter(diametro_normal >= dmc_aplicable)
  
  # Paso 5: Aplicar d_max si existe (manejar lista correctamente)
  d_max_raw <- corte_config$d_max
  
  # Si d_max es una lista, extraer el primer elemento
  if (is.list(d_max_raw)) {
    d_max_raw <- d_max_raw[[1]]
  }
  
  # Solo aplicar si no es NULL y no es NA
  if (!is.null(d_max_raw) && !is.na(d_max_raw)) {
    d_max_valor <- as.numeric(d_max_raw)
    candidatos <- candidatos %>% 
      filter(diametro_normal <= d_max_valor)
  }
  
  # Paso 6: Excluir semilleros si configurado
  if (!is.null(corte_config$excluir_semilleros) && corte_config$excluir_semilleros) {
    n_antes <- nrow(candidatos)
    candidatos <- candidatos %>%
      mutate(
        es_semillero = dominancia %in% c(1, 2, 3) & 
          diametro_normal >= quantile(diametro_normal, 0.75, na.rm = TRUE)
      ) %>%
      filter(!es_semillero)
    
    n_excluidos <- n_antes - nrow(candidatos)
    if (n_excluidos > 0) {
      cat(sprintf("  Semilleros excluidos: %d árboles\n", n_excluidos))
    }
  }
  
  if (nrow(candidatos) == 0) {
    cat("  ⚠️ No hay candidatos disponibles\n")
    return(tibble())
  }
  
  cat(sprintf("  Candidatos disponibles: %d árboles\n\n", nrow(candidatos)))
  
  # ============================================================================
  # 3. UNIR CON PRIORIDADES Y ORDENAR
  # ============================================================================
  
  candidatos <- candidatos %>%
    left_join(
      clases_prioritarias %>% select(clase_d, prioridad_corta),
      by = "clase_d"
    ) %>%
    mutate(prioridad_corta = replace_na(prioridad_corta, 0))
  
  # Ordenar: prioridad Liocourt → dominancia → diámetro
  if (corte_config$prioridad == "suprimidos") {
    candidatos <- candidatos %>%
      arrange(desc(prioridad_corta), desc(dominancia), diametro_normal)
  } else if (corte_config$prioridad == "dominantes") {
    candidatos <- candidatos %>%
      arrange(desc(prioridad_corta), dominancia, desc(diametro_normal))
  } else {
    candidatos <- candidatos %>%
      arrange(desc(prioridad_corta), runif(n()))
  }
  
  # ============================================================================
  # 4. SELECCIONAR HASTA ALCANZAR OBJETIVO
  # ============================================================================
  
  vol_acum <- 0
  seleccionados <- tibble()
  
  cat("[SELECCIÓN]\n")
  cat("─────────────────────────────────────────────────────────\n")
  
  for (i in 1:nrow(candidatos)) {
    arbol <- candidatos[i, ]
    seleccionados <- bind_rows(seleccionados, arbol)
    vol_acum <- vol_acum + arbol$volumen_m3
    
    if (i %% 10 == 0 || vol_acum >= vol_objetivo) {
      cat(sprintf("  %3d árboles: %.2f m³ (%.1f%%)\n",
                  i, vol_acum, (vol_acum/vol_objetivo)*100))
    }
    
    if (vol_acum >= vol_objetivo) {
      cat(sprintf("\n  ✓ Objetivo alcanzado\n"))
      break
    }
  }
  
  # ============================================================================
  # 4. SELECCIONAR HASTA ALCANZAR OBJETIVO
  # ============================================================================
  
  vol_acum <- 0
  seleccionados <- tibble()
  
  cat("[SELECCIÓN]\n")
  cat("─────────────────────────────────────────────────────────\n")
  
  for (i in 1:nrow(candidatos)) {
    arbol <- candidatos[i, ]
    seleccionados <- bind_rows(seleccionados, arbol)
    vol_acum <- vol_acum + arbol$volumen_m3
    
    if (i %% 10 == 0 || vol_acum >= vol_objetivo) {
      cat(sprintf("  %3d árboles: %.2f m³ (%.1f%%)\n",
                  i, vol_acum, (vol_acum/vol_objetivo)*100))
    }
    
    if (vol_acum >= vol_objetivo) {
      cat(sprintf("\n  ✓ Objetivo alcanzado\n"))
      break
    }
  }
  # ============================================================================
  # 4. SELECCIONAR HASTA ALCANZAR OBJETIVO
  # ============================================================================
  
  vol_acum <- 0
  seleccionados <- tibble()
  
  cat("[SELECCIÓN]\n")
  cat("─────────────────────────────────────────────────────────\n")
  
  for (i in 1:nrow(candidatos)) {
    arbol <- candidatos[i, ]
    seleccionados <- bind_rows(seleccionados, arbol)
    vol_acum <- vol_acum + arbol$volumen_m3
    
    if (i %% 10 == 0 || vol_acum >= vol_objetivo) {
      cat(sprintf("  %3d árboles: %.2f m³ (%.1f%%)\n",
                  i, vol_acum, (vol_acum/vol_objetivo)*100))
    }
    
    if (vol_acum >= vol_objetivo) {
      cat(sprintf("\n  ✓ Objetivo alcanzado\n"))
      break
    }
  }
  
  # ============================================================================
  # 5. RESUMEN
  # ============================================================================
  
  cat("\n[RESUMEN]\n")
  cat("─────────────────────────────────────────────────────────\n")
  
  resumen <- seleccionados %>%
    group_by(clase_d) %>%
    summarise(
      n = n(),
      vol = sum(volumen_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(vol))
  
  for (i in 1:nrow(resumen)) {
    cat(sprintf("  Clase %2d cm: %2d árboles, %.2f m³\n",
                resumen$clase_d[i], resumen$n[i], resumen$vol[i]))
  }
  
  cat("─────────────────────────────────────────────────────────\n")
  cat(sprintf("  TOTAL: %d árboles, %.2f m³ (%.1f%%)\n",
              nrow(seleccionados), vol_acum, (vol_acum/vol_objetivo)*100))
  cat("═══════════════════════════════════════════════════════════\n\n")
  
  return(seleccionados)
}

# ==============================================================================
# APLICAR CORTAS
# ==============================================================================

aplicar_cortas <- function(arboles_df, plan_cortas, año_corta) {
  
  if (nrow(plan_cortas$arboles_marcados) == 0) {
    return(arboles_df)
  }
  
  ids_marcados <- plan_cortas$arboles_marcados$arbol_id
  
  arboles_df %>%
    mutate(
      fue_cortado = arbol_id %in% ids_marcados,
      dominancia = if_else(fue_cortado, 8, dominancia),
      año_corta = if_else(fue_cortado, año_corta, NA_real_)
    )
}

# ==============================================================================
# MENSAJE DE CARGA
# ==============================================================================

cat("\n✓ Optimizador de cortas (ICA-LIOCOURT v5 FINAL)\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("  MÉTODO CORRECTO:\n")
cat("    1. ICA define CUÁNTO cortar (vol_objetivo FIJO)\n")
cat("    2. Liocourt identifica clases sobrepobladas\n")
cat("    3. Marcar árboles hasta alcanzar vol_objetivo\n\n")
cat("  SIN método iterativo (era ilógico)\n")
cat("  SIN ajuste de N_ref\n")
cat("  Liocourt solo da prioridades por clase\n\n")