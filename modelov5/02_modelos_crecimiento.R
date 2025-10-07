# ==============================================================================
# MÓDULO 2: MODELOS DE CRECIMIENTO INDIVIDUAL
# Cálculo de incrementos anuales en diámetro, altura y volumen
# ==============================================================================

library(tidyverse)

source("modelov5/core_calculos.R")

# ==============================================================================
# 1. CRECIMIENTO DIAMÉTRICO
# ==============================================================================

calcular_incremento_diametro <- function(arbol, config) {
  # Extraer atributos del árbol
  genero <- arbol$genero_grupo
  dominancia <- arbol$dominancia
  
  # Validar que el árbol esté vivo
  if (dominancia %in% c(7, 8, 9)) {
    return(0)  # Árboles muertos no crecen
  }
  
  # 1. Obtener tasa base de crecimiento por género
  tasa_base <- config$crecimiento_base[[genero]]
  
  if (is.null(tasa_base)) {
    warning(sprintf("Género '%s' no encontrado en config. Usando 0.30 cm/año por defecto.", genero))
    tasa_base <- 0.30
  }
  
  # 2. Aplicar modificador por dominancia
  modificador_dom <- config$modificadores_dominancia %>%
    filter(codigo_dom == dominancia) %>%
    pull(factor_crecimiento)
  
  if (length(modificador_dom) == 0) {
    warning(sprintf("Dominancia %d no encontrada. Usando factor 0.70 por defecto.", dominancia))
    modificador_dom <- 0.70
  }
  
  # 3. Cálculo final del incremento
  incremento_d <- tasa_base * modificador_dom
  
  return(incremento_d)
}

# ==============================================================================
# 2. CRECIMIENTO EN ALTURA
# ==============================================================================

calcular_incremento_altura <- function(arbol, incremento_d, config) {
  # Validar que el árbol esté vivo
  if (arbol$dominancia %in% c(7, 8, 9)) {
    return(0)
  }
  
  # Si no hubo incremento en diámetro, tampoco en altura
  if (incremento_d <= 0) {
    return(0)
  }
  
  # Obtener dh/dd calculado desde Chapman-Richards
  dhdd <- config$interpolar_dhdd(
    especie = arbol$nombre_cientifico,
    diametro = arbol$diametro_normal,
    dominancia = arbol$dominancia,
    parametros_cr = config$parametros_cr
  )
  
  # Calcular incremento en altura: Δh = dh/dd × Δd
  incremento_h <- dhdd * incremento_d
  
  # Limitar incremento máximo razonable (seguridad)
  incremento_h <- min(incremento_h, 0.5)  # Máximo 50 cm/año
  
  return(incremento_h)
}

# ==============================================================================
# 3. RECALCULAR VOLUMEN
# ==============================================================================

calcular_volumen_arbol <- function(diametro, altura, tipo, a, b, c) {
  # Validar inputs
  if (is.na(diametro) | is.na(altura) | is.na(tipo) | 
      is.na(a) | is.na(b) | is.na(c) |
      diametro <= 0 | altura <= 0) {
    return(NA_real_)
  }
  
  # Aplicar ecuación alométrica
  if (tipo == "potencia") {
    # V = a × d^b × h^c
    volumen <- a * (diametro ^ b) * (altura ^ c)
  } else if (tipo == "exp") {
    # V = exp(a + b×ln(d) + c×ln(h))
    volumen <- exp(a + b * log(diametro) + c * log(altura))
  } else {
    return(NA_real_)
  }
  
  return(volumen)
}

# ==============================================================================
# 4. FUNCIÓN PRINCIPAL: APLICAR CRECIMIENTO ANUAL A UN ÁRBOL
# ==============================================================================

aplicar_crecimiento_anual <- function(arbol, config) {
  # Solo procesar árboles vivos
  if (!es_arbol_vivo(arbol$dominancia)) {  # ← usar función de core
    return(arbol)
  }
  
  # 1. Calcular incrementos
  delta_d <- calcular_incremento_diametro(arbol, config)
  delta_h <- calcular_incremento_altura(arbol, delta_d, config)
  
  # 2. Nuevas dimensiones
  diametro_nuevo <- arbol$diametro_normal + delta_d
  altura_nueva <- arbol$altura_total + delta_h
  
  # 3. Recalcular métricas usando core_calculos
  volumen_nuevo <- calcular_volumen_arbol(
    d_cm = diametro_nuevo,
    h_m = altura_nueva,
    tipo = arbol$tipo,
    a = arbol$a,
    b = arbol$b,
    c = arbol$c
  )
  
  area_basal_nueva <- calcular_area_basal(diametro_nuevo)
  
  # 4. Guardar incrementos
  arbol$incremento_d_cm <- delta_d
  arbol$incremento_h_m <- delta_h
  arbol$incremento_vol_m3 <- if_else(
    is.na(volumen_nuevo) | is.na(arbol$volumen_m3),
    NA_real_,
    volumen_nuevo - arbol$volumen_m3
  )
  
  # 5. Actualizar valores
  arbol$diametro_normal <- diametro_nuevo
  arbol$altura_total <- altura_nueva
  arbol$volumen_m3 <- volumen_nuevo
  arbol$area_basal <- area_basal_nueva
  
  return(arbol)
}

# ==============================================================================
# 5. APLICAR CRECIMIENTO A TODA UNA POBLACIÓN
# ==============================================================================

aplicar_crecimiento_poblacion <- function(arboles_df, config, año_actual = NULL) {
  
  if (!is.null(año_actual)) {
    cat(sprintf("\n[AÑO %d] Aplicando crecimiento...\n", año_actual))
  } else {
    cat("\n[CRECIMIENTO] Aplicando incrementos...\n")
  }
  
  # Contar árboles vivos - USAR FUNCIÓN
  vivos_inicial <- filtrar_arboles_vivos(arboles_df)
  n_vivos_inicial <- nrow(vivos_inicial)
  
  cat(sprintf("  Árboles vivos: %d\n", n_vivos_inicial))
  
  # Aplicar crecimiento a cada árbol
  arboles_crecidos <- arboles_df %>%
    rowwise() %>%
    mutate(
      arbol_actualizado = list(aplicar_crecimiento_anual(
        arbol = pick(everything()),
        config = config
      ))
    ) %>%
    ungroup() %>%
    # Desempaquetar resultados
    mutate(
      diametro_normal = map_dbl(arbol_actualizado, ~.x$diametro_normal),
      altura_total = map_dbl(arbol_actualizado, ~.x$altura_total),
      volumen_m3 = map_dbl(arbol_actualizado, ~.x$volumen_m3),
      area_basal = map_dbl(arbol_actualizado, ~.x$area_basal),
      incremento_d_cm = map_dbl(arbol_actualizado, ~.x$incremento_d_cm),
      incremento_h_m = map_dbl(arbol_actualizado, ~.x$incremento_h_m),
      incremento_vol_m3 = map_dbl(arbol_actualizado, ~.x$incremento_vol_m3)
    ) %>%
    select(-arbol_actualizado)
  
  # Estadísticas de crecimiento - USAR FUNCIÓN
  stats <- filtrar_arboles_vivos(arboles_crecidos) %>%
    summarise(
      delta_d_medio = mean(incremento_d_cm, na.rm = TRUE),
      delta_h_medio = mean(incremento_h_m, na.rm = TRUE),
      delta_vol_total = sum(incremento_vol_m3, na.rm = TRUE)
    )
  
  cat(sprintf("  Incremento medio diámetro: %.3f cm\n", stats$delta_d_medio))
  cat(sprintf("  Incremento medio altura:   %.3f m\n", stats$delta_h_medio))
  cat(sprintf("  Incremento total volumen:  %.2f m³\n", stats$delta_vol_total))
  
  return(arboles_crecidos)
}

# ==============================================================================
# 6. VALIDACIÓN Y DIAGNÓSTICO
# ==============================================================================

validar_crecimiento <- function(arboles_antes, arboles_despues) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║           VALIDACIÓN DE CRECIMIENTO APLICADO              ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # Verificar que no se perdieron árboles
  if (nrow(arboles_antes) != nrow(arboles_despues)) {
    warning("⚠️  Número de árboles cambió durante crecimiento!")
  }
  
  # Verificar que los diámetros aumentaron (solo vivos)
  vivos_antes <- filtrar_arboles_vivos(arboles_antes)
  vivos_despues <- filtrar_arboles_vivos(arboles_despues)
  
  diametros_decrecieron <- sum(
    vivos_despues$diametro_normal < vivos_antes$diametro_normal,
    na.rm = TRUE
  )
  
  if (diametros_decrecieron > 0) {
    warning(sprintf("⚠️  %d árboles tuvieron diámetro decreciente!", 
                    diametros_decrecieron))
  } else {
    cat("✓ Todos los diámetros aumentaron correctamente\n")
  }
  
  # Verificar rangos razonables de incremento
  incrementos <- vivos_despues %>%
    summarise(
      delta_d_min = min(incremento_d_cm, na.rm = TRUE),
      delta_d_max = max(incremento_d_cm, na.rm = TRUE),
      delta_h_min = min(incremento_h_m, na.rm = TRUE),
      delta_h_max = max(incremento_h_m, na.rm = TRUE)
    )
  
  cat(sprintf("✓ Rango incremento diámetro: [%.3f - %.3f] cm\n", 
              incrementos$delta_d_min, incrementos$delta_d_max))
  cat(sprintf("✓ Rango incremento altura:   [%.3f - %.3f] m\n", 
              incrementos$delta_h_min, incrementos$delta_h_max))
  
  # Advertencias si valores fuera de rango esperado
  if (incrementos$delta_d_max > 1.0) {
    warning("⚠️  Incremento diamétrico muy alto (>1 cm/año)")
  }
  
  if (incrementos$delta_h_max > 0.8) {
    warning("⚠️  Incremento altura muy alto (>0.8 m/año)")
  }
  
  cat("\n✓ Validación completada\n\n")
  
  return(TRUE)
}

# ==============================================================================
# 7. REPORTE DE CRECIMIENTO POR GÉNERO Y DOMINANCIA
# ==============================================================================

reporte_crecimiento <- function(arboles_df) {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║        REPORTE DE CRECIMIENTO POR GÉNERO Y DOMINANCIA     ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  reporte <- arboles_df %>%
    filtrar_arboles_vivos() %>%
    left_join(
      CONFIG$modificadores_dominancia %>% select(codigo_dom, nombre_dom),
      by = c("dominancia" = "codigo_dom")
    ) %>%
    group_by(genero_grupo, nombre_dom) %>%
    summarise(
      n_arboles = n(),
      d_medio = mean(diametro_normal, na.rm = TRUE),
      inc_d_medio = mean(incremento_d_cm, na.rm = TRUE),
      inc_h_medio = mean(incremento_h_m, na.rm = TRUE),
      inc_vol_total = sum(incremento_vol_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(genero_grupo, desc(inc_d_medio))
  
  print(reporte, n = 50)
  
  cat("\n")
  
  return(reporte)
}

# ==============================================================================
# 8. FUNCIÓN DE TEST UNITARIO
# ==============================================================================

test_crecimiento_individual <- function() {
  
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║              TEST UNITARIO - MÓDULO CRECIMIENTO           ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # Cargar configuración
  if (!exists("CONFIG")) {
    source("R/01_parametros_configuracion.R")
  }
  
  # Crear árbol de prueba
  arbol_test <- tibble(
    rodal = 1,
    arbol_id = "TEST_001",
    especie = 11,
    nombre_cientifico = "Pinus pseudostrobus",
    genero_grupo = "Pinus",
    dominancia = 1,  # Dominante
    diametro_normal = 30.0,
    altura_total = 12.5,
    area_basal = pi * (30/200)^2,
    volumen_m3 = 0.35,
    tipo = "potencia",
    a = 0.00004,
    b = 1.93694,
    c = 1.03169
  )
  
  cat("[TEST 1] Árbol dominante Pinus 30 cm\n")
  cat(sprintf("  D inicial: %.1f cm | H inicial: %.1f m\n", 
              arbol_test$diametro_normal, arbol_test$altura_total))
  
  # Aplicar crecimiento
  arbol_crecido <- aplicar_crecimiento_anual(arbol_test, CONFIG)
  
  cat(sprintf("  D final:   %.2f cm | H final:   %.2f m\n", 
              arbol_crecido$diametro_normal, arbol_crecido$altura_total))
  cat(sprintf("  Δd = %.3f cm | Δh = %.3f m | ΔV = %.4f m³\n",
              arbol_crecido$incremento_d_cm,
              arbol_crecido$incremento_h_m,
              arbol_crecido$incremento_vol_m3))
  
  # Test con suprimido
  arbol_test2 <- arbol_test
  arbol_test2$dominancia <- 6  # Suprimido
  arbol_test2$diametro_normal <- 15.0
  arbol_test2$altura_total <- 6.0
  
  cat("\n[TEST 2] Árbol suprimido Pinus 15 cm\n")
  cat(sprintf("  D inicial: %.1f cm | H inicial: %.1f m\n", 
              arbol_test2$diametro_normal, arbol_test2$altura_total))
  
  arbol_crecido2 <- aplicar_crecimiento_anual(arbol_test2, CONFIG)
  
  cat(sprintf("  D final:   %.2f cm | H final:   %.2f m\n", 
              arbol_crecido2$diametro_normal, arbol_crecido2$altura_total))
  cat(sprintf("  Δd = %.3f cm | Δh = %.3f m\n",
              arbol_crecido2$incremento_d_cm,
              arbol_crecido2$incremento_h_m))
  
  cat("\n✓ Tests completados\n")
  cat(sprintf("  Dominante creció ~%.0f%% más que suprimido en diámetro\n",
              (arbol_crecido$incremento_d_cm / arbol_crecido2$incremento_d_cm - 1) * 100))
  
  return(list(dominante = arbol_crecido, suprimido = arbol_crecido2))
}

cat("\n✓ Módulo de crecimiento cargado exitosamente\n")
cat("  Funciones disponibles:\n")
cat("    - aplicar_crecimiento_anual(arbol, config)\n")
cat("    - aplicar_crecimiento_poblacion(arboles_df, config, año)\n")
cat("    - validar_crecimiento(antes, despues)\n")
cat("    - reporte_crecimiento(arboles_df)\n")
cat("    - test_crecimiento_individual()  [ejecutar para probar]\n\n")