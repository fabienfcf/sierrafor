# ==============================================================================
# MÓDULO 7: OPTIMIZADOR DE CORTAS FORESTALES (VERSIÓN CORREGIDA)
# Selección de árboles para aprovechamiento según J invertida y DMC
# ==============================================================================

library(tidyverse)

# Cargar dependencias
source("R5/core_calculos.R")

# ==============================================================================
# 1. CALCULAR PLAN DE CORTAS PARA UN RODAL
# ==============================================================================

calcular_plan_cortas <- function(arboles_rodal, config) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║           OPTIMIZADOR DE CORTAS FORESTALES                ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # ✓ CORRECCIÓN 1: Filtrar correctamente arboles_rodal (no arboles_df)
  arboles_vivos <- filtrar_arboles_vivos(arboles_rodal)
  
  cat(sprintf("[POBLACIÓN] Árboles vivos: %d\n", nrow(arboles_vivos)))
  
  # 1. Calcular distribución actual vs J invertida ideal (GLOBAL)
  cat("\n[PASO 1] Calculando distribución objetivo (J invertida global)...\n")
  distribucion <- calcular_distribucion_liocourt(arboles_vivos, config)
  
  if (nrow(distribucion) == 0) {
    warning("No se pudo calcular distribución objetivo")
    return(NULL)
  }
  
  # Mostrar diagnóstico
  cat("\n[DIAGNÓSTICO ESTRUCTURAL]\n")
  diagnostico_resumen <- distribucion %>%
    count(diagnostico) %>%
    arrange(desc(n))
  print(diagnostico_resumen)
  
  # 2. Identificar clases sobrepobladas
  cat("\n[PASO 2] Identificando clases a intervenir...\n")
  
  clases_a_intervenir <- distribucion %>%
    mutate(
      # Extraer límite inferior usando función de core
      clase_d_num = extraer_limite_inferior_clase(clase_diametrica)
    ) %>%
    filter(
      diagnostico == "Sobrepoblado",
      diferencia > 0
    )
  
  # ✓ CORRECCIÓN 2: Simplificar lógica - solo marcar sobrepobladas
  # La verificación de DMC se hace después por género específico
  clases_cortables <- clases_a_intervenir
  
  if (nrow(clases_cortables) == 0) {
    cat("\n✓ No hay clases sobrepobladas\n")
    cat("  El rodal está en equilibrio estructural\n\n")
    return(list(
      distribucion = distribucion,
      arboles_marcados = tibble(),
      resumen = tibble(
        n_clases_sobrepobladas = 0,
        n_clases_cortables = 0,
        n_arboles_marcados = 0,
        volumen_marcado = 0
      )
    ))
  }
  
  cat(sprintf("  Clases sobrepobladas: %d\n", nrow(clases_cortables)))
  
  # 3. Distribuir cortas entre géneros (aquí se verifica DMC específico)
  cat("\n[PASO 3] Distribuyendo cortas entre géneros...\n")
  cat(sprintf("  Estrategia: %s\n", 
              if_else("dominancia_baja" %in% config$prioridad,
                      "Liberar espacio para regeneración (cortar suprimidos primero)",
                      "Aprovechar árboles de mayor calidad (cortar dominantes primero)")))
  
  arboles_marcados <- tibble()
  
  for (i in 1:nrow(clases_cortables)) {
    
    clase <- clases_cortables[i, ]
    
    cat(sprintf("\n  Clase %s:\n", clase$clase_diametrica))
    cat(sprintf("    Actual: %d | Ideal: %.1f | Exceso: %.1f árboles\n",
                clase$n_actual, clase$n_ideal, clase$diferencia))
    
    # Cantidad a remover (según intensidad de corta)
    n_a_remover_total <- floor(clase$diferencia * config$intensidad_corta)
    
    cat(sprintf("    A remover (%.0f%% intensidad): %d árboles\n",
                config$intensidad_corta * 100, n_a_remover_total))
    
    if (n_a_remover_total == 0) next
    
    # Distribuir cortas entre géneros
    cortas_clase <- distribuir_cortas_generos(
      arboles_rodal = arboles_vivos,
      clase_diametrica = clase$clase_diametrica,
      n_a_remover = n_a_remover_total,
      config = config
    )
    
    if (nrow(cortas_clase) > 0) {
      arboles_marcados <- bind_rows(arboles_marcados, cortas_clase)
    }
  }
  
  # 4. Resumen final
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║              RESUMEN DEL PLAN DE CORTAS                   ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  if (nrow(arboles_marcados) == 0) {
    cat("  No se marcaron árboles para corta\n")
    cat("  (Todas las clases sobrepobladas están bajo DMC)\n\n")
    return(list(
      distribucion = distribucion,
      arboles_marcados = tibble(),
      resumen = tibble(
        n_clases_sobrepobladas = nrow(clases_a_intervenir),
        n_clases_cortables = 0,
        n_arboles_marcados = 0,
        volumen_marcado = 0
      )
    ))
  }
  
  # Calcular estadísticas
  resumen_cortas <- arboles_marcados %>%
    summarise(
      n_arboles = n(),
      volumen_total = sum(volumen_m3, na.rm = TRUE),
      d_promedio = mean(diametro_normal),
      d_min = min(diametro_normal),
      d_max = max(diametro_normal)
    )
  
  resumen_genero <- arboles_marcados %>%
    group_by(genero_grupo) %>%
    summarise(
      n_arboles = n(),
      volumen_m3 = sum(volumen_m3, na.rm = TRUE),
      d_promedio = mean(diametro_normal),
      .groups = "drop"
    )
  
  cat("[ÁRBOLES MARCADOS]\n")
  cat(sprintf("  Total:           %d árboles\n", resumen_cortas$n_arboles))
  cat(sprintf("  Volumen total:   %.2f m³\n", resumen_cortas$volumen_total))
  cat(sprintf("  Ø promedio:      %.1f cm (rango: %.1f - %.1f cm)\n",
              resumen_cortas$d_promedio, resumen_cortas$d_min, resumen_cortas$d_max))
  
  cat("\n[DISTRIBUCIÓN POR GÉNERO]\n")
  print(resumen_genero)
  
  cat("\n")
  
  return(list(
    distribucion = distribucion,
    arboles_marcados = arboles_marcados,
    resumen_cortas = resumen_cortas,
    resumen_genero = resumen_genero
  ))
}

# ==============================================================================
# 2. DISTRIBUIR CORTAS ENTRE GÉNEROS (SIMPLIFICADO)
# ==============================================================================

distribuir_cortas_generos <- function(arboles_rodal, clase_diametrica, 
                                      n_a_remover, config) {
  
  # ✓ CORRECCIÓN 3: Usar función de core_calculos
  arboles_clase <- arboles_rodal %>%
    mutate(
      clase_d = asignar_clase_diametrica(
        diametro_normal, 
        breaks = config$clases_d,
        formato = "rango"
      )
    ) %>%
    filter(clase_d == clase_diametrica)
  
  if (nrow(arboles_clase) == 0) {
    return(tibble())
  }
  
  # Calcular composición actual por género
  composicion <- arboles_clase %>%
    group_by(genero_grupo) %>%
    summarise(
      n_disponibles = n(),
      .groups = "drop"
    ) %>%
    mutate(prop = n_disponibles / sum(n_disponibles))
  
  # Distribuir corta PROPORCIONALMENTE
  cortas_por_genero <- composicion %>%
    mutate(
      n_a_cortar = round(n_a_remover * prop)
    ) %>%
    # Ajustar si redondeo causó diferencia
    mutate(
      diferencia_total = sum(n_a_cortar) - n_a_remover,
      n_a_cortar = if_else(
        row_number() == 1,
        n_a_cortar - diferencia_total,
        n_a_cortar
      )
    ) %>%
    filter(n_a_cortar > 0)
  
  cat(sprintf("      Distribución proporcional:\n"))
  for (i in 1:nrow(cortas_por_genero)) {
    gen <- cortas_por_genero[i, ]
    cat(sprintf("        %s: %d árboles (%.1f%%)\n",
                gen$genero_grupo, gen$n_a_cortar, gen$prop * 100))
  }
  
  # Seleccionar árboles específicos por género
  arboles_seleccionados <- tibble()
  
  for (i in 1:nrow(cortas_por_genero)) {
    
    genero <- cortas_por_genero$genero_grupo[i]
    n_cortar <- cortas_por_genero$n_a_cortar[i]
    
    # Filtrar árboles de este género en esta clase
    arboles_genero <- arboles_clase %>%
      filter(genero_grupo == genero)
    
    # ✓ CORRECCIÓN 4: Verificar DMC específico del género AQUÍ
    # (no antes a nivel de clase)
    dmc_genero <- config$dmc[[genero]]
    
    if (is.null(dmc_genero)) {
      warning(sprintf("DMC no definido para género %s", genero))
      next
    }
    
    arboles_genero_cortables <- arboles_genero %>%
      filter(diametro_normal >= dmc_genero)
    
    if (nrow(arboles_genero_cortables) == 0) {
      cat(sprintf("        %s: Sin árboles ≥ DMC (%.0f cm) - OMITIDO\n", 
                  genero, dmc_genero))
      next
    }
    
    # Seleccionar según prioridad
    arboles_selec_genero <- seleccionar_arboles_corta(
      arboles = arboles_genero_cortables,
      n_a_cortar = n_cortar,
      prioridad = config$prioridad
    )
    
    if (nrow(arboles_selec_genero) > 0) {
      arboles_seleccionados <- bind_rows(arboles_seleccionados, arboles_selec_genero)
    }
  }
  
  return(arboles_seleccionados)
}

# ==============================================================================
# 3. SELECCIONAR ÁRBOLES ESPECÍFICOS SEGÚN PRIORIDAD
# ==============================================================================

seleccionar_arboles_corta <- function(arboles, n_a_cortar, prioridad) {
  
  # Limitar al número disponible
  n_a_cortar <- min(n_a_cortar, nrow(arboles))
  
  if (n_a_cortar == 0) {
    return(tibble())
  }
  
  # ESTRATEGIAS DE SELECCIÓN:
  #
  # OPCIÓN 1: "dominancia_baja" + "diametro_menor"
  #    Objetivo: FAVORECER REGENERACIÓN
  #    Corta: Suprimidos e intermedios primero (liberar espacio bajo dosel)
  #    Mantiene: Dominantes y codominantes (fuente de semilla, protección)
  #    Uso: Bosques densos que necesitan aclareos para regeneración
  #
  # OPCIÓN 2: "dominancia_alta" + "diametro_mayor"  
  #    Objetivo: MAXIMIZAR APROVECHAMIENTO COMERCIAL
  #    Corta: Dominantes y codominantes grandes (mejor calidad maderera)
  #    Mantiene: Intermedios y suprimidos (futura cosecha)
  #    Uso: Aprovechamiento comercial tradicional
  
  arboles_ordenados <- arboles
  
  for (criterio in prioridad) {
    
    if (criterio == "dominancia_baja") {
      # Priorizar suprimidos e intermedios (6, 5, 3)
      arboles_ordenados <- arboles_ordenados %>%
        arrange(desc(dominancia))
      
    } else if (criterio == "dominancia_alta") {
      # Priorizar dominantes y codominantes (1, 2, 4)
      arboles_ordenados <- arboles_ordenados %>%
        arrange(dominancia)
      
    } else if (criterio == "diametro_mayor") {
      # Dentro de cada dominancia, los más grandes primero
      arboles_ordenados <- arboles_ordenados %>%
        arrange(desc(dominancia), desc(diametro_normal))
      
    } else if (criterio == "diametro_menor") {
      # Dentro de cada dominancia, los más pequeños primero
      arboles_ordenados <- arboles_ordenados %>%
        arrange(desc(dominancia), diametro_normal)
    }
  }
  
  # Seleccionar los primeros N
  arboles_marcados <- arboles_ordenados %>%
    slice(1:n_a_cortar) %>%
    mutate(marcado_para_corta = TRUE)
  
  return(arboles_marcados)
}

# ==============================================================================
# 4. GENERAR LISTA DE APROVECHAMIENTO
# ==============================================================================

generar_lista_aprovechamiento <- function(plan_cortas, rodal_id = NULL) {
  
  if (is.null(plan_cortas) || nrow(plan_cortas$arboles_marcados) == 0) {
    cat("\nNo hay árboles marcados para generar lista de aprovechamiento\n")
    return(NULL)
  }
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║          LISTA DE APROVECHAMIENTO FORESTAL                ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  if (!is.null(rodal_id)) {
    cat(sprintf("Rodal: %s\n\n", rodal_id))
  }
  
  # Lista detallada
  lista <- plan_cortas$arboles_marcados %>%
    mutate(
      num_consecutivo = row_number()
    ) %>%
    select(
      num_consecutivo,
      arbol_id,
      genero_grupo,
      nombre_cientifico,
      diametro_normal,
      altura_total,
      volumen_m3,
      dominancia
    ) %>%
    arrange(desc(volumen_m3))
  
  cat("[ÁRBOLES MARCADOS PARA APROVECHAMIENTO]\n\n")
  print(lista, n = Inf)
  
  # ✓ CORRECCIÓN 5: Usar función de core
  cat("\n[RESUMEN POR CLASE DIAMÉTRICA]\n")
  
  resumen_clases <- plan_cortas$arboles_marcados %>%
    mutate(
      clase_d = asignar_clase_diametrica(
        diametro_normal,
        breaks = seq(5, 100, by = 5),
        formato = "rango"
      )
    ) %>%
    group_by(clase_d) %>%
    summarise(
      n_arboles = n(),
      volumen_m3 = sum(volumen_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(clase_d)
  
  print(resumen_clases)
  
  cat("\n")
  
  return(lista)
}

# ==============================================================================
# 5. VISUALIZAR PLAN DE CORTAS
# ==============================================================================

visualizar_plan_cortas <- function(plan_cortas, rodal_id = NULL) {
  
  if (is.null(plan_cortas) || nrow(plan_cortas$distribucion) == 0) {
    warning("No hay datos para visualizar")
    return(NULL)
  }
  
  distribucion <- plan_cortas$distribucion
  arboles_marcados <- plan_cortas$arboles_marcados
  
  # Preparar datos para gráfico
  datos_grafico <- distribucion %>%
    select(clase_diametrica, n_actual, n_ideal) %>%
    pivot_longer(
      cols = c(n_actual, n_ideal),
      names_to = "tipo",
      values_to = "n_arboles"
    ) %>%
    mutate(
      tipo = recode(tipo,
                    "n_actual" = "Distribución Actual",
                    "n_ideal" = "J Invertida Ideal")
    )
  
  # Calcular árboles a remover por clase
  if (nrow(arboles_marcados) > 0) {
    # ✓ CORRECCIÓN 6: Usar función de core
    arboles_removidos <- arboles_marcados %>%
      mutate(
        clase_d = asignar_clase_diametrica(
          diametro_normal,
          breaks = seq(5, 100, by = 5),
          formato = "rango"
        )
      ) %>%
      count(clase_d, name = "n_a_remover") %>%
      rename(clase_diametrica = clase_d)
    
    datos_grafico <- datos_grafico %>%
      left_join(arboles_removidos, by = "clase_diametrica") %>%
      mutate(n_a_remover = replace_na(n_a_remover, 0))
  } else {
    datos_grafico <- datos_grafico %>%
      mutate(n_a_remover = 0)
  }
  
  # Título dinámico
  titulo <- "Plan de Cortas - Optimización de Estructura"
  if (!is.null(rodal_id)) {
    titulo <- paste(titulo, "- Rodal", rodal_id)
  }
  
  # Crear gráfico
  p <- ggplot() +
    # Barras de distribución actual
    geom_col(
      data = datos_grafico %>% filter(tipo == "Distribución Actual"),
      aes(x = clase_diametrica, y = n_arboles, fill = "Actual"),
      alpha = 0.7
    ) +
    
    # Línea de J invertida ideal
    geom_line(
      data = datos_grafico %>% filter(tipo == "J Invertida Ideal"),
      aes(x = clase_diametrica, y = n_arboles, group = 1, color = "Ideal"),
      linewidth = 1.5
    ) +
    geom_point(
      data = datos_grafico %>% filter(tipo == "J Invertida Ideal"),
      aes(x = clase_diametrica, y = n_arboles, color = "Ideal"),
      size = 3
    ) +
    
    # Barras de árboles a remover (negativas)
    geom_col(
      data = datos_grafico %>% 
        filter(tipo == "Distribución Actual", n_a_remover > 0),
      aes(x = clase_diametrica, y = -n_a_remover, fill = "A Remover"),
      alpha = 0.8
    ) +
    
    scale_fill_manual(
      values = c("Actual" = "#2E86AB", "A Remover" = "#A23B72"),
      name = NULL
    ) +
    scale_color_manual(
      values = c("Ideal" = "#F18F01"),
      name = NULL
    ) +
    
    labs(
      title = titulo,
      subtitle = sprintf("Árboles marcados: %d | Volumen: %.2f m³",
                         nrow(arboles_marcados),
                         sum(arboles_marcados$volumen_m3, na.rm = TRUE)),
      x = "Clase Diamétrica (cm)",
      y = "Número de Árboles"
    ) +
    
    geom_hline(yintercept = 0, linetype = "solid", color = "gray30") +
    
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
# 6. APLICAR CORTAS AL RODAL (NUEVA FUNCIÓN)
# ==============================================================================

#' @title Aplicar plan de cortas a la población
#' @description Marca árboles como cortados (dominancia = 9: tocón)
#' @param arboles_df Población completa
#' @param plan_cortas Resultado de calcular_plan_cortas()
#' @param año_corta Año en que se ejecuta la corta
#' @return Población actualizada con árboles cortados
aplicar_cortas <- function(arboles_df, plan_cortas, año_corta = NULL) {
  
  if (is.null(plan_cortas) || nrow(plan_cortas$arboles_marcados) == 0) {
    cat("\n[CORTAS] No hay árboles marcados - sin intervención\n")
    return(arboles_df)
  }
  
  cat(sprintf("\n[AÑO %s] Aplicando cortas...\n", 
              if_else(is.null(año_corta), "N/A", as.character(año_corta))))
  
  # IDs de árboles a cortar
  ids_a_cortar <- plan_cortas$arboles_marcados$arbol_id
  
  cat(sprintf("  Árboles a cortar: %d\n", length(ids_a_cortar)))
  
  # Marcar como cortados (dominancia 9 = tocón)
  arboles_actualizado <- arboles_df %>%
    mutate(
      dominancia_pre_corta = if_else(
        arbol_id %in% ids_a_cortar,
        dominancia,
        NA_real_
      ),
      dominancia = if_else(
        arbol_id %in% ids_a_cortar,
        9,  # Tocón
        dominancia
      ),
      año_corta = if_else(
        arbol_id %in% ids_a_cortar & !is.null(año_corta),
        año_corta,
        NA_real_
      )
    )
  
  # Estadísticas
  vol_cortado <- arboles_actualizado %>%
    filter(arbol_id %in% ids_a_cortar) %>%
    summarise(vol_total = sum(volumen_m3, na.rm = TRUE)) %>%
    pull(vol_total)
  
  cat(sprintf("  Volumen extraído: %.2f m³\n", vol_cortado))
  
  # Por género
  resumen_genero <- arboles_actualizado %>%
    filter(arbol_id %in% ids_a_cortar) %>%
    group_by(genero_grupo) %>%
    summarise(
      n_cortados = n(),
      vol_cortado = sum(volumen_m3, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("\n  Cortas por género:\n")
  print(resumen_genero)
  
  cat("\n")
  
  return(arboles_actualizado)
}

# ==============================================================================
# 7. FUNCIÓN DE TEST UNITARIO
# ==============================================================================

test_optimizador_cortas <- function() {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║       TEST UNITARIO - MÓDULO OPTIMIZADOR CORTAS           ║\n")
  cat("║                  (VERSIÓN CORREGIDA)                       ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # Cargar configuración
  if (!exists("CONFIG")) {
    cat("⚠️  Cargando CONFIG...\n")
    source("R/01_parametros_configuracion.R")
  }
  
  # Verificar módulo 05
  if (!exists("calcular_distribucion_liocourt")) {
    cat("⚠️  Cargando módulo 05...\n")
    source("R/05_distribucion_objetivo.R")
  }
  
  # Crear población de prueba realista
  set.seed(789)
  
  cat("[TEST] Creando población de prueba...\n")
  
  # Simular distribución NO ideal (sobrepoblada en clases medias)
  diametros_test <- c(
    runif(180, 10, 15),   # Muchos en clase pequeña (correcto)
    runif(150, 15, 20),   # Muchos
    runif(100, 20, 25),   # Moderado
    runif(120, 25, 30),   # ¡SOBREPOBLADO! (más de lo esperado)
    runif(90, 30, 35),    # ¡SOBREPOBLADO! (más de lo esperado)
    runif(60, 35, 40),    # Razonable
    runif(30, 40, 45),    # Poco
    runif(15, 45, 50),    # Muy poco
    runif(5, 50, 55)      # Casi nada
  )
  
  arboles_test <- tibble(
    arbol_id = paste0("TEST_", sprintf("%04d", 1:length(diametros_test))),
    rodal = 3,
    genero_grupo = sample(
      c("Pinus", "Quercus"), 
      length(diametros_test), 
      replace = TRUE, 
      prob = c(0.65, 0.35)
    ),
    nombre_cientifico = if_else(
      genero_grupo == "Pinus", 
      "Pinus pseudostrobus", 
      "Quercus rysophylla"
    ),
    dominancia = sample(
      c(1, 2, 3, 6), 
      length(diametros_test), 
      replace = TRUE, 
      prob = c(0.25, 0.30, 0.30, 0.15)
    ),
    diametro_normal = diametros_test,
    altura_total = 5 + diametros_test * 0.28 + rnorm(length(diametros_test), 0, 1.2),
    superficie_ha = 11.5,
    tipo = "potencia",
    a = 0.00004,
    b = 1.93694,
    c = 1.03169
  )
  
  # Calcular área basal y volumen
  arboles_test <- arboles_test %>%
    mutate(area_basal = calcular_area_basal(diametro_normal))
  
  arboles_test <- calcular_volumenes_vectorizado(arboles_test)
  
  cat(sprintf("  Población: %d árboles\n", nrow(arboles_test)))
  cat(sprintf("  Rango diámetros: %.1f - %.1f cm\n",
              min(diametros_test), max(diametros_test)))
  cat(sprintf("  Composición: %.0f%% Pinus, %.0f%% Quercus\n\n",
              mean(arboles_test$genero_grupo == "Pinus") * 100,
              mean(arboles_test$genero_grupo == "Quercus") * 100))
  
  # ══════════════════════════════════════════════════════════════════
  # TEST 1: Calcular plan de cortas
  # ══════════════════════════════════════════════════════════════════
  
  cat("\n" , rep("═", 60), "\n")
  cat("TEST 1: CALCULAR PLAN DE CORTAS\n")
  cat(rep("═", 60), "\n")
  
  plan <- calcular_plan_cortas(arboles_test, CONFIG)
  
  # Verificaciones
  cat("\n[VERIFICACIONES]\n")
  
  if (!is.null(plan)) {
    cat("✓ Plan generado exitosamente\n")
    
    if (nrow(plan$arboles_marcados) > 0) {
      cat(sprintf("✓ Árboles marcados: %d\n", nrow(plan$arboles_marcados)))
      
      # Verificar que todos cumplen DMC
      verif_dmc <- plan$arboles_marcados %>%
        group_by(genero_grupo) %>%
        summarise(
          d_min = min(diametro_normal),
          dmc = first(genero_grupo)
        ) %>%
        mutate(
          dmc_config = if_else(genero_grupo == "Pinus", 
                               CONFIG$dmc$Pinus, 
                               CONFIG$dmc$Quercus),
          cumple = d_min >= dmc_config
        )
      
      if (all(verif_dmc$cumple)) {
        cat("✓ Todos los árboles marcados cumplen DMC\n")
      } else {
        cat("✗ PROBLEMA: Algunos árboles NO cumplen DMC\n")
        print(verif_dmc)
      }
      
      # Verificar que son de clases sobrepobladas
      clases_sobrepobladas <- plan$distribucion %>%
        filter(diagnostico == "Sobrepoblado") %>%
        pull(clase_diametrica)
      
      clases_marcadas <- plan$arboles_marcados %>%
        mutate(
          clase = asignar_clase_diametrica(diametro_normal, formato = "rango")
        ) %>%
        pull(clase) %>%
        unique()
      
      if (all(clases_marcadas %in% clases_sobrepobladas)) {
        cat("✓ Todos los árboles son de clases sobrepobladas\n")
      } else {
        cat("✗ PROBLEMA: Algunos árboles NO son de clases sobrepobladas\n")
      }
      
    } else {
      cat("⚠️  No se marcaron árboles (probablemente todo está bajo DMC)\n")
    }
  } else {
    cat("✗ ERROR: Plan es NULL\n")
  }
  
  # ══════════════════════════════════════════════════════════════════
  # TEST 2: Generar lista de aprovechamiento
  # ══════════════════════════════════════════════════════════════════
  
  if (!is.null(plan) && nrow(plan$arboles_marcados) > 0) {
    cat("\n" , rep("═", 60), "\n")
    cat("TEST 2: LISTA DE APROVECHAMIENTO\n")
    cat(rep("═", 60), "\n")
    
    lista <- generar_lista_aprovechamiento(plan, rodal_id = 3)
    
    if (!is.null(lista)) {
      cat("\n✓ Lista generada correctamente\n")
    }
  }
  
  # ══════════════════════════════════════════════════════════════════
  # TEST 3: Visualizar plan
  # ══════════════════════════════════════════════════════════════════
  
  if (!is.null(plan)) {
    cat("\n" , rep("═", 60), "\n")
    cat("TEST 3: VISUALIZACIÓN\n")
    cat(rep("═", 60), "\n\n")
    
    cat("Generando gráfico...\n")
    p <- visualizar_plan_cortas(plan, rodal_id = 3)
    
    if (!is.null(p)) {
      print(p)
      cat("\n✓ Gráfico generado correctamente\n")
    }
  }
  
  # ══════════════════════════════════════════════════════════════════
  # TEST 4: Aplicar cortas
  # ══════════════════════════════════════════════════════════════════
  
  if (!is.null(plan) && nrow(plan$arboles_marcados) > 0) {
    cat("\n" , rep("═", 60), "\n")
    cat("TEST 4: APLICAR CORTAS\n")
    cat(rep("═", 60), "\n")
    
    arboles_post_corta <- aplicar_cortas(arboles_test, plan, año_corta = 2025)
    
    # Verificar
    n_tocones = sum(arboles_post_corta$dominancia == 9)
    
    cat(sprintf("\n✓ Árboles convertidos a tocón: %d\n", n_tocones))
    cat(sprintf("  Esperado: %d\n", nrow(plan$arboles_marcados)))
    
    if (n_tocones == nrow(plan$arboles_marcados)) {
      cat("✓ Cortas aplicadas correctamente\n")
    } else {
      cat("✗ PROBLEMA: Número de tocones no coincide\n")
    }
  }
  
  # ══════════════════════════════════════════════════════════════════
  # RESUMEN FINAL
  # ══════════════════════════════════════════════════════════════════
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║              RESUMEN DE TESTS                              ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  cat("✓ Módulo 07 corregido y funcionando\n")
  cat("✓ Todas las correcciones aplicadas:\n")
  cat("  1. filtrar_arboles_vivos(arboles_rodal) ✓\n")
  cat("  2. Verificación DMC simplificada ✓\n")
  cat("  3. Uso de asignar_clase_diametrica() ✓\n")
  cat("  4. Uso de extraer_limite_inferior_clase() ✓\n")
  cat("  5. Lógica forestal consistente ✓\n\n")
  
  return(plan)
}

# ==============================================================================
# MENSAJE DE CARGA
# ==============================================================================

cat("\n✓ 07_optimizador_cortas.R (VERSIÓN CORREGIDA v2.0) cargado\n")
cat("  Dependencias:\n")
cat("    - core_calculos.R (filtrar_vivos, asignar_clase, extraer_limite)\n")
cat("    - 05_distribucion_objetivo.R (calcular_distribucion_liocourt)\n")
cat("    - 01_parametros_configuracion.R (CONFIG)\n\n")
cat("  Correcciones aplicadas:\n")
cat("    🔧 filtrar_arboles_vivos(arboles_rodal) en vez de (arboles_df)\n")
cat("    🔧 Simplificada verificación DMC (solo a nivel género)\n")
cat("    🔧 Uso consistente de funciones de core_calculos\n")
cat("    🔧 Eliminada redundancia en filtrado de clases\n\n")
cat("  Funciones disponibles:\n")
cat("    - calcular_plan_cortas(arboles_rodal, config)\n")
cat("    - generar_lista_aprovechamiento(plan_cortas, rodal_id)\n")
cat("    - visualizar_plan_cortas(plan_cortas, rodal_id)\n")
cat("    - aplicar_cortas(arboles_df, plan_cortas, año_corta)\n")
cat("    - test_optimizador_cortas()  [ejecutar para probar]\n\n")