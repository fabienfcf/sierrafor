# ==============================================================================
# MÓDULO 5: DISTRIBUCIÓN OBJETIVO (LIOCOURT)
# Cálculo de la distribución diamétrica ideal en "J invertida"
# ==============================================================================

library(tidyverse)

# ==============================================================================
# 1. CALCULAR DISTRIBUCIÓN IDEAL DE LIOCOURT
# ==============================================================================

calcular_distribucion_liocourt <- function(arboles_rodal, config) {
  
  # Filtrar solo árboles vivos del rodal
  arboles_vivos <- arboles_rodal %>%
    filtrar_arboles_vivos()
  
  if (nrow(arboles_vivos) == 0) {
    warning("No hay árboles vivos en el rodal para calcular distribución.")
    return(tibble())
  }
  
  # Crear clases diamétricas
  arboles_con_clase <- arboles_vivos %>%
    mutate(
      clase_diametrica = cut(
        diametro_normal,
        breaks = config$clases_d,
        labels = paste0(config$clases_d[-length(config$clases_d)], "-", 
                        config$clases_d[-1]),
        include.lowest = TRUE,
        right = FALSE
      )
    ) %>%
    filter(!is.na(clase_diametrica))
  
  # Contar árboles por clase (distribución actual)
  dist_actual <- arboles_con_clase %>%
    count(clase_diametrica, name = "n_actual") %>%
    arrange(clase_diametrica)
  
  # Obtener q factor
  q <- config$q_factor
  
  # Calcular distribución ideal usando De Liocourt
  # N_i = N_1 / q^(i-1)
  # donde N_1 es el número de árboles en la clase más pequeña
  
  n_clases <- nrow(dist_actual)
  
  if (n_clases == 0) {
    return(tibble())
  }
  
  # Usar el número actual de la clase más pequeña como referencia
  n_clase_menor <- dist_actual$n_actual[1]
  
  # Generar distribución ideal
  dist_ideal <- dist_actual %>%
    mutate(
      clase_num = 1:n(),
      n_ideal = n_clase_menor / (q ^ (clase_num - 1))
    )
  
  # Calcular diferencias y diagnóstico
  distribucion_completa <- dist_ideal %>%
    mutate(
      diferencia = n_actual - n_ideal,
      diferencia_pct = (diferencia / n_ideal) * 100,
      ratio_actual_ideal = n_actual / n_ideal,
      
      # Clasificar según tolerancia
      diagnostico = case_when(
        abs(diferencia_pct) <= config$tolerancia ~ "Equilibrado",
        diferencia_pct > config$tolerancia ~ "Sobrepoblado",
        diferencia_pct < -config$tolerancia ~ "Subpoblado",
        TRUE ~ "Equilibrado"
      )
    )
  
  return(distribucion_completa)
}

# ==============================================================================
# 2. CALCULAR DISTRIBUCIÓN POR GÉNERO
# ==============================================================================

calcular_distribucion_por_genero <- function(arboles_rodal, config) {
  
  # Filtrar géneros objetivo
  arboles_filtrados <- arboles_rodal %>%
    filter(genero_grupo %in% config$generos) %>%
    filter(!dominancia %in% c(7, 8, 9))
  
  if (nrow(arboles_filtrados) == 0) {
    return(tibble())
  }
  
  # Calcular distribución para cada género
  generos <- unique(arboles_filtrados$genero_grupo)
  
  dist_por_genero <- map_df(generos, function(gen) {
    
    arboles_gen <- arboles_filtrados %>% filter(genero_grupo == gen)
    
    dist <- calcular_distribucion_liocourt(arboles_gen, config)
    
    if (nrow(dist) > 0) {
      dist <- dist %>% mutate(genero_grupo = gen)
    }
    
    return(dist)
  })
  
  return(dist_por_genero)
}

# ==============================================================================
# 3. VALIDAR CONVERGENCIA HACIA J INVERTIDA
# ==============================================================================

evaluar_forma_j_invertida <- function(distribucion) {
  
  if (nrow(distribucion) < 3) {
    return(list(
      es_j_invertida = FALSE,
      puntuacion = 0,
      mensaje = "Pocas clases para evaluar"
    ))
  }
  
  # Criterios de J invertida:
  # 1. Clases pequeñas tienen MÁS árboles que clases grandes
  # 2. Tendencia decreciente monotónica (o casi)
  
  # Calcular ratios consecutivos
  ratios <- numeric(nrow(distribucion) - 1)
  for (i in 1:(nrow(distribucion) - 1)) {
    ratios[i] <- distribucion$n_actual[i] / distribucion$n_actual[i + 1]
  }
  
  # Criterio 1: ¿La primera clase tiene más que la última?
  criterio_extremos <- distribucion$n_actual[1] > distribucion$n_actual[nrow(distribucion)]
  
  # Criterio 2: ¿La mayoría de ratios son > 1 (decreciente)?
  criterio_monotonia <- mean(ratios > 0.9) > 0.7  # 70% de ratios decrecientes
  
  # Criterio 3: ¿Los ratios están cerca del q factor ideal?
  q_ideal <- first(distribucion$clase_num)  # Extraer q si está disponible
  if (!"clase_num" %in% names(distribucion)) {
    q_ideal <- 1.5  # Por defecto
  }
  
  desviacion_q <- mean(abs(ratios - q_ideal), na.rm = TRUE)
  criterio_q <- desviacion_q < 1.0  # Desviación razonable del q
  
  # Puntuación total (0-100)
  puntuacion <- 
    (criterio_extremos * 40) +
    (criterio_monotonia * 40) +
    (criterio_q * 20)
  
  # Clasificación
  es_j_invertida <- puntuacion >= 60
  
  mensaje <- case_when(
    puntuacion >= 80 ~ "Excelente forma J invertida",
    puntuacion >= 60 ~ "Buena aproximación a J invertida",
    puntuacion >= 40 ~ "Forma parcial J invertida",
    TRUE ~ "No sigue patrón J invertida"
  )
  
  return(list(
    es_j_invertida = es_j_invertida,
    puntuacion = puntuacion,
    mensaje = mensaje,
    ratios_observados = ratios,
    criterio_extremos = criterio_extremos,
    criterio_monotonia = criterio_monotonia,
    criterio_q = criterio_q
  ))
}

# ==============================================================================
# 4. REPORTE DE DISTRIBUCIÓN
# ==============================================================================

reporte_distribucion <- function(distribucion, genero = NULL) {
  
  titulo <- if (!is.null(genero)) {
    sprintf("DISTRIBUCIÓN DIAMÉTRICA - %s", genero)
  } else {
    "DISTRIBUCIÓN DIAMÉTRICA"
  }
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat(sprintf("║  %s\n", titulo))
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  if (nrow(distribucion) == 0) {
    cat("Sin datos para reportar.\n\n")
    return(NULL)
  }
  
  # Tabla resumida
  cat("[COMPARACIÓN ACTUAL vs IDEAL]\n")
  tabla_reporte <- distribucion %>%
    select(clase_diametrica, n_actual, n_ideal, diferencia, 
           diferencia_pct, diagnostico) %>%
    mutate(
      n_ideal = round(n_ideal, 1),
      diferencia = round(diferencia, 1),
      diferencia_pct = round(diferencia_pct, 1)
    )
  
  print(tabla_reporte, n = Inf)
  
  # Resumen por diagnóstico
  cat("\n[RESUMEN POR DIAGNÓSTICO]\n")
  resumen_diag <- distribucion %>%
    group_by(diagnostico) %>%
    summarise(
      n_clases = n(),
      arboles_totales = sum(n_actual),
      .groups = "drop"
    )
  print(resumen_diag)
  
  # Evaluación de J invertida
  cat("\n[EVALUACIÓN FORMA J INVERTIDA]\n")
  eval <- evaluar_forma_j_invertida(distribucion)
  cat(sprintf("  Puntuación: %.0f/100\n", eval$puntuacion))
  cat(sprintf("  Estado: %s\n", eval$mensaje))
  cat(sprintf("  Extremos decrecientes: %s\n", 
              if_else(eval$criterio_extremos, "✓ Sí", "✗ No")))
  cat(sprintf("  Tendencia decreciente: %s\n", 
              if_else(eval$criterio_monotonia, "✓ Sí", "✗ No")))
  
  cat("\n")
  
  return(tabla_reporte)
}

# ==============================================================================
# 5. VISUALIZACIÓN DE LA DISTRIBUCIÓN
# ==============================================================================

graficar_distribucion <- function(distribucion, genero = NULL, rodal = NULL) {
  
  if (nrow(distribucion) == 0) {
    warning("No hay datos para graficar")
    return(NULL)
  }
  
  # Preparar datos para gráfico
  datos_grafico <- distribucion %>%
    select(clase_diametrica, n_actual, n_ideal) %>%
    pivot_longer(cols = c(n_actual, n_ideal),
                 names_to = "tipo",
                 values_to = "n_arboles") %>%
    mutate(
      tipo = recode(tipo,
                    "n_actual" = "Distribución Actual",
                    "n_ideal" = "J Invertida Ideal")
    )
  
  # Título dinámico
  titulo <- "Distribución Diamétrica"
  if (!is.null(genero)) titulo <- paste(titulo, "-", genero)
  if (!is.null(rodal)) titulo <- paste(titulo, "- Rodal", rodal)
  
  # Crear gráfico
  p <- ggplot(datos_grafico, aes(x = clase_diametrica, y = n_arboles, 
                                 fill = tipo, group = tipo)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_fill_manual(
      values = c("Distribución Actual" = "#2E86AB", 
                 "J Invertida Ideal" = "#A23B72"),
      name = NULL
    ) +
    labs(
      title = titulo,
      subtitle = "Comparación con distribución objetivo de Liocourt",
      x = "Clase Diamétrica (cm)",
      y = "Número de Árboles"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# ==============================================================================
# 6. FUNCIÓN DE TEST UNITARIO
# ==============================================================================

test_distribucion_objetivo <- function() {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║         TEST UNITARIO - MÓDULO DISTRIBUCIÓN OBJETIVO      ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # Cargar configuración
  if (!exists("CONFIG")) {
    source("R/01_parametros_configuracion.R")
  }
  
  # Crear población de prueba con distribución NO ideal
  set.seed(789)
  
  # Simular distribución "real" desbalanceada
  diametros_test <- c(
    runif(150, 10, 15),   # Muchos en clase pequeña
    runif(120, 15, 20),   # Muchos
    runif(80, 20, 25),    # Moderado
    runif(90, 25, 30),    # ¡Más de lo esperado! (sobrepoblado)
    runif(40, 30, 35),    # Razonable
    runif(15, 35, 40),    # Poco
    runif(8, 40, 45),     # Muy poco
    runif(3, 45, 50)      # Casi nada
  )
  
  arboles_test <- tibble(
    arbol_id = paste0("TEST_", 1:length(diametros_test)),
    rodal = 1,
    genero_grupo = sample(c("Pinus", "Quercus"), length(diametros_test), 
                          replace = TRUE, prob = c(0.7, 0.3)),
    dominancia = sample(1:6, length(diametros_test), replace = TRUE),
    diametro_normal = diametros_test,
    altura_total = 5 + diametros_test * 0.3
  )
  
  cat(sprintf("[TEST] Población: %d árboles\n", nrow(arboles_test)))
  cat(sprintf("       Rango diámetros: %.1f - %.1f cm\n\n",
              min(diametros_test), max(diametros_test)))
  
  # Calcular distribución
  dist <- calcular_distribucion_liocourt(arboles_test, CONFIG)
  
  # Mostrar reporte
  reporte_distribucion(dist)
  
  # Graficar
  cat("Generando gráfico...\n")
  p <- graficar_distribucion(dist, rodal = 1)
  print(p)
  
  # Prueba por género
  cat("\n" , rep("=", 60), "\n")
  cat("DISTRIBUCIÓN POR GÉNERO\n")
  cat(rep("=", 60), "\n\n")
  
  dist_por_genero <- calcular_distribucion_por_genero(arboles_test, CONFIG)
  
  # Reportar cada género
  for (gen in unique(dist_por_genero$genero_grupo)) {
    dist_gen <- dist_por_genero %>% filter(genero_grupo == gen)
    reporte_distribucion(dist_gen, genero = gen)
  }
  
  cat("\n✓ Test completado\n")
  cat("  Nota: La clase 25-30 debería aparecer como 'Sobrepoblado'\n")
  cat("        (diseñado intencionalmente en el test)\n\n")
  
  return(list(
    distribucion_total = dist,
    distribucion_por_genero = dist_por_genero,
    grafico = p
  ))
}

cat("\n✓ Módulo de distribución objetivo cargado exitosamente\n")
cat("  Funciones disponibles:\n")
cat("    - calcular_distribucion_liocourt(arboles_rodal, config)\n")
cat("    - calcular_distribucion_por_genero(arboles_rodal, config)\n")
cat("    - evaluar_forma_j_invertida(distribucion)\n")
cat("    - reporte_distribucion(distribucion, genero)\n")
cat("    - graficar_distribucion(distribucion, genero, rodal)\n")
cat("    - test_distribucion_objetivo()  [ejecutar para probar]\n\n")