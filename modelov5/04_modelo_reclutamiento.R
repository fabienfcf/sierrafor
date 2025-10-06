# ==============================================================================
# MÓDULO 4: MODELO DE RECLUTAMIENTO
# Ingreso de nuevos árboles que alcanzan el diámetro mínimo de inventario
# ==============================================================================

library(tidyverse)

# ==============================================================================
# 1. CALCULAR NÚMERO DE RECLUTAS POR RODAL
# ==============================================================================

calcular_n_reclutas <- function(arboles_rodal, config) {
  
  # Contar árboles vivos actuales
  n_vivos <- sum(!arboles_rodal$dominancia %in% c(7, 8, 9))
  
  # Calcular número de reclutas según tasa
  n_reclutas <- round(n_vivos * config$tasa_reclutamiento)
  
  # Mínimo 0, máximo razonable (evitar explosión demográfica)
  n_reclutas <- max(0, min(n_reclutas, n_vivos * 0.1))  # Máx 10% del rodal
  
  return(n_reclutas)
}

# ==============================================================================
# 2. DETERMINAR COMPOSICIÓN DE ESPECIES DE RECLUTAS
# ==============================================================================

calcular_composicion_reclutas <- function(arboles_rodal, n_reclutas, config) {
  
  # Obtener composición actual de especies (solo vivos)
  composicion_actual <- arboles_rodal %>%
    filtrar_arboles_vivos() %>%
    filter(genero_grupo %in% config$generos) %>%
    count(nombre_cientifico, genero_grupo, name = "n_actual") %>%
    mutate(proporcion = n_actual / sum(n_actual))
  
  # Si no hay árboles vivos, distribución equitativa por género
  if (nrow(composicion_actual) == 0) {
    warning("No hay árboles vivos para determinar composición. Usando distribución por defecto.")
    
    # Asignar especies por género de manera equitativa
    especies_disponibles <- config$especies %>%
      filter(genero %in% config$generos)
    
    composicion_actual <- especies_disponibles %>%
      group_by(genero) %>%
      slice(1) %>%  # Una especie por género
      ungroup() %>%
      mutate(
        proporcion = 1 / n(),
        n_actual = 1
      ) %>%
      rename(nombre_cientifico = especie, genero_grupo = genero)
  }
  
  # Asignar número de reclutas por especie
  reclutas_por_especie <- composicion_actual %>%
    mutate(
      n_reclutas_esperado = n_reclutas * proporcion,
      n_reclutas = round(n_reclutas_esperado)
    )
  
  # Ajustar para que sume exactamente n_reclutas
  diferencia <- n_reclutas - sum(reclutas_por_especie$n_reclutas)
  
  if (diferencia != 0) {
    # Agregar/quitar la diferencia a la especie más abundante
    idx_max <- which.max(reclutas_por_especie$n_reclutas)
    reclutas_por_especie$n_reclutas[idx_max] <- 
      reclutas_por_especie$n_reclutas[idx_max] + diferencia
  }
  
  return(reclutas_por_especie)
}

# ==============================================================================
# 3. GENERAR ÁRBOLES RECLUTAS
# ==============================================================================

generar_reclutas <- function(rodal_id, composicion_reclutas, config, año_actual) {
  
  if (sum(composicion_reclutas$n_reclutas) == 0) {
    return(tibble())  # Sin reclutas
  }
  
  # Generar árboles por especie
  reclutas_list <- list()
  id_counter <- 1
  
  for (i in 1:nrow(composicion_reclutas)) {
    
    especie_info <- composicion_reclutas[i, ]
    n <- especie_info$n_reclutas
    
    if (n > 0) {
      
      # Generar diámetros aleatorios en rango de ingreso
      diametros <- runif(n, 
                         min = config$reclut_d_min, 
                         max = config$reclut_d_max)
      
      # Altura inicial según género
      altura_base <- config$reclut_altura[[especie_info$genero_grupo]]
      
      if (is.null(altura_base)) {
        altura_base <- 3.0  # Default
      }
      
      # Altura con variación ±20%
      alturas <- altura_base * runif(n, min = 0.8, max = 1.2)
      
      # Crear tibble de reclutas
      reclutas_especie <- tibble(
        arbol_id = paste0("RECLUTA_R", rodal_id, "_A", año_actual, "_", 
                          especie_info$nombre_cientifico, "_", 
                          sprintf("%03d", 1:n)),
        rodal = rodal_id,
        nombre_cientifico = especie_info$nombre_cientifico,
        genero_grupo = especie_info$genero_grupo,
        dominancia = config$reclut_dominancia,  # Suprimidos
        diametro_normal = diametros,
        altura_total = alturas,
        area_basal = pi * (diametros / 200)^2,
        año_reclutamiento = año_actual,
        es_recluta = TRUE
      )
      
      reclutas_list[[i]] <- reclutas_especie
    }
  }
  
  # Combinar todos los reclutas
  if (length(reclutas_list) > 0) {
    reclutas_df <- bind_rows(reclutas_list)
    return(reclutas_df)
  } else {
    return(tibble())
  }
}

# ==============================================================================
# 4. APLICAR RECLUTAMIENTO A TODA LA POBLACIÓN
# ==============================================================================

aplicar_reclutamiento <- function(arboles_df, config, año_actual) {
  
  cat(sprintf("\n[AÑO %d] Aplicando reclutamiento...\n", año_actual))
  
  # Contar árboles vivos totales antes
  n_vivos_inicial <- nrow(filtrar_arboles_vivos(arboles_df))  
  
  cat(sprintf("  Árboles vivos antes: %d\n", n_vivos_inicial))
  
  # Generar reclutas por rodal
  reclutas_total <- tibble()
  
  rodales_unicos <- unique(arboles_df$rodal)
  
  for (rodal_id in rodales_unicos) {
    
    # Filtrar árboles del rodal
    arboles_rodal <- arboles_df %>% filter(rodal == rodal_id)
    
    # Calcular número de reclutas
    n_reclutas <- calcular_n_reclutas(arboles_rodal, config)
    
    if (n_reclutas > 0) {
      
      # Determinar composición
      composicion <- calcular_composicion_reclutas(arboles_rodal, n_reclutas, config)
      
      # Generar reclutas
      reclutas_rodal <- generar_reclutas(rodal_id, composicion, config, año_actual)
      
      if (nrow(reclutas_rodal) > 0) {
        reclutas_total <- bind_rows(reclutas_total, reclutas_rodal)
      }
    }
  }
  
  # Reportar
  n_reclutas_total <- nrow(reclutas_total)
  
  if (n_reclutas_total > 0) {
    cat(sprintf("  Reclutas generados:  %d (%.2f%% de vivos)\n", 
                n_reclutas_total, 
                (n_reclutas_total / n_vivos_inicial) * 100))
    
    # Distribución por género
    dist_genero <- reclutas_total %>%
      count(genero_grupo, name = "n_reclutas")
    
    cat("\n  Distribución por género:\n")
    print(dist_genero)
    
    # Agregar columnas faltantes para compatibilidad
    reclutas_total <- reclutas_total %>%
      mutate(
        # Inicializar columnas que otros módulos esperan
        volumen_m3 = NA_real_,
        incremento_d_cm = 0,
        incremento_h_m = 0,
        incremento_vol_m3 = 0,
        murio_este_año = FALSE,
        dominancia_original = NA_real_,
        año_muerte = NA_real_,
        probabilidad_muerte = NA_real_
      )
    
    # Unir con población existente
    arboles_con_reclutas <- bind_rows(arboles_df, reclutas_total)
    
  } else {
    cat("  Reclutas generados:  0\n")
    arboles_con_reclutas <- arboles_df
  }
  
  # Contar total después
  n_vivos_final <- sum(!arboles_con_reclutas$dominancia %in% c(7, 8, 9))
  cat(sprintf("  Árboles vivos después: %d\n", n_vivos_final))
  
  return(arboles_con_reclutas)
}

# ==============================================================================
# 5. VALIDACIÓN DE RECLUTAMIENTO
# ==============================================================================

validar_reclutamiento <- function(arboles_antes, arboles_despues, config) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║         VALIDACIÓN DE RECLUTAMIENTO APLICADO              ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # Identificar reclutas
  if ("es_recluta" %in% names(arboles_despues)) {
    reclutas <- arboles_despues %>% filter(es_recluta == TRUE)
  } else {
    reclutas <- tibble()
  }
  
  n_reclutas <- nrow(reclutas)
  
  if (n_reclutas == 0) {
    cat("✓ No hubo reclutamiento este año\n")
    return(TRUE)
  }
  
  cat(sprintf("✓ Total reclutas: %d\n", n_reclutas))
  
  # Validar diámetros
  d_min <- min(reclutas$diametro_normal)
  d_max <- max(reclutas$diametro_normal)
  
  cat(sprintf("✓ Rango diámetros: [%.2f - %.2f] cm\n", d_min, d_max))
  
  if (d_min < config$reclut_d_min | d_max > config$reclut_d_max) {
    warning("⚠️  Algunos diámetros fuera del rango esperado")
  }
  
  # Validar dominancia
  dominancias_unicas <- unique(reclutas$dominancia)
  if (length(dominancias_unicas) == 1 & dominancias_unicas[1] == config$reclut_dominancia) {
    cat(sprintf("✓ Todos los reclutas son suprimidos (dom = %d)\n", 
                config$reclut_dominancia))
  } else {
    warning("⚠️  Algunos reclutas tienen dominancia incorrecta")
  }
  
  # Validar géneros
  generos_reclutas <- unique(reclutas$genero_grupo)
  if (all(generos_reclutas %in% config$generos)) {
    cat(sprintf("✓ Géneros válidos: %s\n", paste(generos_reclutas, collapse = ", ")))
  } else {
    warning("⚠️  Algunos reclutas tienen género no objetivo")
  }
  
  cat("\n✓ Validación completada\n\n")
  
  return(TRUE)
}

# ==============================================================================
# 6. REPORTE DE RECLUTAMIENTO ACUMULADO
# ==============================================================================

reporte_reclutamiento_acumulado <- function(arboles_df) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║        REPORTE DE RECLUTAMIENTO ACUMULADO                 ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  if (!"es_recluta" %in% names(arboles_df)) {
    cat("No hay información de reclutamiento disponible.\n\n")
    return(NULL)
  }
  
  reclutas <- arboles_df %>% filter(es_recluta == TRUE)
  
  if (nrow(reclutas) == 0) {
    cat("No ha habido reclutamiento hasta el momento.\n\n")
    return(NULL)
  }
  
  # Por género
  cat("[RECLUTAMIENTO POR GÉNERO]\n")
  por_genero <- reclutas %>%
    group_by(genero_grupo) %>%
    summarise(
      n_reclutas = n(),
      d_medio = mean(diametro_normal, na.rm = TRUE),
      h_media = mean(altura_total, na.rm = TRUE),
      .groups = "drop"
    )
  print(por_genero)
  
  # Por año
  if ("año_reclutamiento" %in% names(reclutas)) {
    cat("\n[RECLUTAMIENTO POR AÑO]\n")
    por_año <- reclutas %>%
      count(año_reclutamiento, name = "n_reclutas") %>%
      arrange(año_reclutamiento)
    print(por_año)
  }
  
  # Por rodal
  cat("\n[RECLUTAMIENTO POR RODAL]\n")
  por_rodal <- reclutas %>%
    count(rodal, name = "n_reclutas") %>%
    arrange(desc(n_reclutas))
  print(por_rodal)
  
  cat("\n")
  
  return(por_genero)
}

# ==============================================================================
# 7. FUNCIÓN DE TEST UNITARIO
# ==============================================================================

test_reclutamiento <- function() {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║            TEST UNITARIO - MÓDULO RECLUTAMIENTO           ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # Cargar configuración
  if (!exists("CONFIG")) {
    source("R/01_parametros_configuracion.R")
  }
  
  # Crear población de prueba
  set.seed(456)
  
  arboles_test <- tibble(
    arbol_id = paste0("TEST_", 1:500),
    rodal = sample(1:3, 500, replace = TRUE),
    genero_grupo = sample(c("Pinus", "Quercus"), 500, replace = TRUE, prob = c(0.6, 0.4)),
    nombre_cientifico = if_else(
      genero_grupo == "Pinus",
      "Pinus pseudostrobus",
      "Quercus rysophylla"
    ),
    dominancia = sample(1:6, 500, replace = TRUE),
    diametro_normal = runif(500, 15, 60),
    altura_total = runif(500, 8, 20),
    area_basal = pi * (diametro_normal / 200)^2
  )
  
  cat(sprintf("[TEST] Población inicial: %d árboles\n", nrow(arboles_test)))
  cat(sprintf("       Distribución: %.0f%% Pinus, %.0f%% Quercus\n",
              mean(arboles_test$genero_grupo == "Pinus") * 100,
              mean(arboles_test$genero_grupo == "Quercus") * 100))
  
  # Aplicar reclutamiento
  arboles_con_reclutas <- aplicar_reclutamiento(arboles_test, CONFIG, año_actual = 1)
  
  # Validar
  validar_reclutamiento(arboles_test, arboles_con_reclutas, CONFIG)
  
  # Verificar proporción
  if ("es_recluta" %in% names(arboles_con_reclutas)) {
    reclutas <- arboles_con_reclutas %>% filter(es_recluta == TRUE)
    
    if (nrow(reclutas) > 0) {
      cat("\n[VERIFICACIÓN DE PROPORCIÓN]\n")
      
      # Proporción Pinus/Quercus en originales vs reclutas
      prop_original <- arboles_test %>%
        count(genero_grupo) %>%
        mutate(prop_original = n / sum(n))
      
      prop_reclutas <- reclutas %>%
        count(genero_grupo) %>%
        mutate(prop_reclutas = n / sum(n))
      
      comparacion <- prop_original %>%
        left_join(prop_reclutas, by = "genero_grupo") %>%
        select(genero_grupo, prop_original, prop_reclutas)
      
      print(comparacion)
      
      cat("\n✓ Test completado\n")
      cat("  La proporción de géneros en reclutas debe ser similar a la original\n\n")
    }
  }
  
  return(arboles_con_reclutas)
}

cat("\n✓ Módulo de reclutamiento cargado exitosamente\n")
cat("  Funciones disponibles:\n")
cat("    - aplicar_reclutamiento(arboles_df, config, año)\n")
cat("    - validar_reclutamiento(antes, despues, config)\n")
cat("    - reporte_reclutamiento_acumulado(arboles_df)\n")
cat("    - test_reclutamiento()  [ejecutar para probar]\n\n")