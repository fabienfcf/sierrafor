# ==============================================================================
# MÓDULO 3: MODELO DE MORTALIDAD
# Aplicación de tasas de mortalidad diferencial por dominancia
# ==============================================================================

library(tidyverse)

# ==============================================================================
# 1. CALCULAR PROBABILIDAD DE MUERTE INDIVIDUAL
# ==============================================================================

calcular_probabilidad_muerte <- function(arbol, config) {
  
  # Árboles ya muertos permanecen muertos
  if (arbol$dominancia %in% c(7, 8, 9)) {
    return(1.0)  # 100% muerto (ya está muerto)
  }
  
  # Obtener tasa base de mortalidad
  tasa_base <- config$mortalidad_base
  
  # Obtener modificador por dominancia
  modificador_mort <- config$modificadores_dominancia %>%
    filter(codigo_dom == arbol$dominancia) %>%
    pull(factor_mortalidad)
  
  if (length(modificador_mort) == 0) {
    warning(sprintf("Dominancia %d no encontrada. Usando factor 1.0 por defecto.", 
                    arbol$dominancia))
    modificador_mort <- 1.0
  }
  
  # Probabilidad anual de muerte
  prob_muerte <- tasa_base * modificador_mort
  
  # Limitar a rango válido [0, 1]
  prob_muerte <- max(0, min(prob_muerte, 1))
  
  return(prob_muerte)
}

# ==============================================================================
# 2. APLICAR MORTALIDAD A UN ÁRBOL
# ==============================================================================

aplicar_mortalidad_arbol <- function(arbol, config, valor_aleatorio) {
  
  # Si ya está muerto, no hacer nada
  if (arbol$dominancia %in% c(7, 8, 9)) {
    arbol$murio_este_año <- FALSE
    arbol$dominancia_original <- NA_real_  # Asegurar que existe
    return(arbol)
  }
  
  # Calcular probabilidad de muerte
  prob_muerte <- calcular_probabilidad_muerte(arbol, config)
  
  # Decidir si muere (comparar con valor aleatorio)
  muere <- valor_aleatorio < prob_muerte
  
  if (muere) {
    # Marcar como muerto en pie
    arbol$dominancia_original <- arbol$dominancia
    arbol$dominancia <- 7  # Muerto en pie
    arbol$murio_este_año <- TRUE
    arbol$año_muerte <- NA  # Se asignará en la función principal
  } else {
    # NO murió - asegurar que dominancia_original sea NA
    arbol$dominancia_original <- NA_real_
    arbol$murio_este_año <- FALSE
  }
  
  return(arbol)
}

# ==============================================================================
# 3. APLICAR MORTALIDAD A TODA LA POBLACIÓN
# ==============================================================================

aplicar_mortalidad_poblacion <- function(arboles_df, config, año_actual) {
  
  cat(sprintf("\n[AÑO %d] Aplicando mortalidad...\n", año_actual))
  
  # Contar vivos iniciales
  n_vivos_inicial <- sum(!arboles_df$dominancia %in% c(7, 8, 9))
  
  cat(sprintf("  Árboles vivos al inicio: %d\n", n_vivos_inicial))
  
  # Establecer semilla para reproducibilidad
  set.seed(config$semilla_mortalidad + año_actual)  # Semilla única por año
  
  # Generar valores aleatorios para todos los árboles
  n_arboles <- nrow(arboles_df)
  valores_aleatorios <- runif(n_arboles)
  
  # Aplicar mortalidad a cada árbol
  arboles_procesados <- arboles_df %>%
    mutate(
      valor_aleatorio = valores_aleatorios,
      probabilidad_muerte = map_dbl(
        1:n(),
        ~calcular_probabilidad_muerte(arboles_df[.x, ], config)
      )
    ) %>%
    rowwise() %>%
    mutate(
      arbol_actualizado = list(aplicar_mortalidad_arbol(
        arbol = pick(everything()),  # Reemplazar cur_data() por pick()
        config = config,
        valor_aleatorio = valor_aleatorio
      ))
    ) %>%
    ungroup() %>%
    # Desempaquetar resultados
    mutate(
      dominancia_original = map_dbl(arbol_actualizado, ~.x$dominancia_original),
      dominancia = map_dbl(arbol_actualizado, ~.x$dominancia),
      murio_este_año = map_lgl(arbol_actualizado, ~.x$murio_este_año),
      año_muerte = if_else(murio_este_año, año_actual, NA_real_)
    ) %>%
    select(-arbol_actualizado, -valor_aleatorio)
  
  # Contar muertos este año
  n_muertos_año <- sum(arboles_procesados$murio_este_año, na.rm = TRUE)
  n_vivos_final <- sum(!arboles_procesados$dominancia %in% c(7, 8, 9))
  
  cat(sprintf("  Árboles muertos este año: %d (%.2f%%)\n", 
              n_muertos_año, 
              (n_muertos_año / n_vivos_inicial) * 100))
  cat(sprintf("  Árboles vivos al final:   %d\n", n_vivos_final))
  
  # Estadísticas por dominancia
  if (n_muertos_año > 0) {
    cat("\n  Mortalidad por clase:\n")
    
    # Filtrar solo árboles que murieron este año y tienen dominancia_original
    muertos_este_año <- arboles_procesados %>%
      filter(murio_este_año, !is.na(dominancia_original))
    
    if (nrow(muertos_este_año) > 0) {
      resumen_mort <- muertos_este_año %>%
        left_join(
          config$modificadores_dominancia %>% select(codigo_dom, nombre_dom),
          by = c("dominancia_original" = "codigo_dom")
        ) %>%
        count(nombre_dom, name = "n_muertos") %>%
        arrange(desc(n_muertos))
      
      print(resumen_mort)
    }
  }
  
  return(arboles_procesados)
}

# ==============================================================================
# 4. VALIDACIÓN DE MORTALIDAD
# ==============================================================================

validar_mortalidad <- function(arboles_antes, arboles_despues) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║           VALIDACIÓN DE MORTALIDAD APLICADA               ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # Verificar que no se perdieron árboles
  if (nrow(arboles_antes) != nrow(arboles_despues)) {
    warning("⚠️  Número de árboles cambió durante aplicación de mortalidad!")
  }
  
  # Contar cambios
  vivos_antes <- sum(!arboles_antes$dominancia %in% c(7, 8, 9))
  vivos_despues <- sum(!arboles_despues$dominancia %in% c(7, 8, 9))
  muertos_nuevos <- vivos_antes - vivos_despues
  
  cat(sprintf("✓ Árboles vivos antes:    %d\n", vivos_antes))
  cat(sprintf("✓ Árboles vivos después:  %d\n", vivos_despues))
  cat(sprintf("✓ Mortalidad aplicada:    %d árboles (%.2f%%)\n", 
              muertos_nuevos, (muertos_nuevos / vivos_antes) * 100))
  
  # Verificar que árboles muertos no crecieron
  arboles_verificar <- arboles_despues %>%
    filter(dominancia == 7, murio_este_año == TRUE)
  
  if (nrow(arboles_verificar) > 0) {
    if ("incremento_d_cm" %in% names(arboles_verificar)) {
      incrementos_invalidos <- sum(arboles_verificar$incremento_d_cm > 0, na.rm = TRUE)
      if (incrementos_invalidos > 0) {
        warning(sprintf("⚠️  %d árboles muertos tienen incremento > 0!", 
                        incrementos_invalidos))
      } else {
        cat("✓ Árboles muertos no tienen crecimiento\n")
      }
    }
  }
  
  cat("\n✓ Validación completada\n\n")
  
  return(TRUE)
}

# ==============================================================================
# 5. REPORTE DE MORTALIDAD ACUMULADA
# ==============================================================================

reporte_mortalidad_acumulada <- function(arboles_df, config) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║        REPORTE DE MORTALIDAD ACUMULADA                    ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # Mortalidad por género
  mort_genero <- arboles_df %>%
    group_by(genero_grupo) %>%
    summarise(
      n_total = n(),
      n_muertos = sum(dominancia %in% c(7, 8, 9)),
      n_vivos = n_total - n_muertos,
      tasa_mort = (n_muertos / n_total) * 100,
      .groups = "drop"
    )
  
  cat("[MORTALIDAD POR GÉNERO]\n")
  print(mort_genero)
  
  # Mortalidad por dominancia original
  if ("dominancia_original" %in% names(arboles_df)) {
    mort_dominancia <- arboles_df %>%
      filter(dominancia == 7) %>%
      left_join(
        config$modificadores_dominancia %>% select(codigo_dom, nombre_dom),
        by = c("dominancia_original" = "codigo_dom")
      ) %>%
      count(nombre_dom, name = "n_muertos") %>%
      arrange(desc(n_muertos))
    
    if (nrow(mort_dominancia) > 0) {
      cat("\n[MORTALIDAD POR CLASE DE DOMINANCIA]\n")
      print(mort_dominancia)
    }
  }
  
  # Distribución temporal de mortalidad
  if ("año_muerte" %in% names(arboles_df)) {
    mort_temporal <- arboles_df %>%
      filter(!is.na(año_muerte)) %>%
      count(año_muerte, name = "n_muertos") %>%
      arrange(año_muerte)
    
    if (nrow(mort_temporal) > 0) {
      cat("\n[MORTALIDAD POR AÑO]\n")
      print(mort_temporal)
    }
  }
  
  cat("\n")
  
  return(mort_genero)
}

# ==============================================================================
# 6. FUNCIÓN DE TEST UNITARIO
# ==============================================================================

test_mortalidad <- function() {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║              TEST UNITARIO - MÓDULO MORTALIDAD            ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # Cargar configuración
  if (!exists("CONFIG")) {
    source("R/01_parametros_configuracion.R")
  }
  
  # Crear población de prueba con diferentes dominancias
  set.seed(123)
  
  arboles_test <- tibble(
    arbol_id = paste0("TEST_", 1:1000),
    genero_grupo = sample(c("Pinus", "Quercus"), 1000, replace = TRUE),
    dominancia = sample(c(1, 3, 6), 1000, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
    diametro_normal = runif(1000, 10, 50),
    altura_total = runif(1000, 5, 20)
  )
  
  cat(sprintf("[TEST] Población inicial: %d árboles\n", nrow(arboles_test)))
  
  # Mostrar distribución inicial
  dist_inicial <- arboles_test %>%
    left_join(
      CONFIG$modificadores_dominancia %>% select(codigo_dom, nombre_dom),
      by = c("dominancia" = "codigo_dom")
    ) %>%
    count(nombre_dom, name = "n_inicial")
  
  cat("\nDistribución inicial:\n")
  print(dist_inicial)
  
  # Aplicar mortalidad
  arboles_test_mort <- aplicar_mortalidad_poblacion(arboles_test, CONFIG, año_actual = 1)
  
  # Calcular tasas observadas vs esperadas
  cat("\n[COMPARACIÓN TASAS ESPERADAS VS OBSERVADAS]\n")
  
  # Obtener muertos con dominancia_original válida
  muertos_analisis <- arboles_test_mort %>%
    filter(murio_este_año, !is.na(dominancia_original))
  
  comparacion <- arboles_test %>%
    left_join(
      CONFIG$modificadores_dominancia %>% select(codigo_dom, nombre_dom, factor_mortalidad),
      by = c("dominancia" = "codigo_dom")
    ) %>%
    group_by(nombre_dom) %>%
    summarise(
      n_total = n(),
      tasa_esperada = first(factor_mortalidad) * CONFIG$mortalidad_base * 100,
      .groups = "drop"
    ) %>%
    left_join(
      muertos_analisis %>%
        left_join(
          CONFIG$modificadores_dominancia %>% select(codigo_dom, nombre_dom),
          by = c("dominancia_original" = "codigo_dom")
        ) %>%
        count(nombre_dom, name = "n_muertos"),
      by = "nombre_dom"
    ) %>%
    mutate(
      n_muertos = replace_na(n_muertos, 0),
      tasa_observada = (n_muertos / n_total) * 100,
      diferencia = abs(tasa_observada - tasa_esperada)
    )
  
  print(comparacion)
  
  cat("\n✓ Test completado\n")
  cat("  Nota: Con n=1000, las tasas observadas deberían estar cerca de las esperadas\n")
  cat("        (diferencias <1% son normales por variación aleatoria)\n\n")
  
  return(arboles_test_mort)
}

cat("\n✓ Módulo de mortalidad cargado exitosamente\n")
cat("  Funciones disponibles:\n")
cat("    - aplicar_mortalidad_poblacion(arboles_df, config, año)\n")
cat("    - validar_mortalidad(antes, despues)\n")
cat("    - reporte_mortalidad_acumulada(arboles_df, config)\n")
cat("    - test_mortalidad()  [ejecutar para probar]\n\n")