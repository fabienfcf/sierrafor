# ==============================================================================
# REPORTE_CORTAS_DETALLADO.R
# Reporte completo: Inicial → Antes corta → Cortado → Después corta
# ==============================================================================

setwd("/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5")

# CARGAR MÓDULOS
source("modelov5/core_calculos.R")
source("modelov5/01_parametros_configuracion.R")
source("modelov5/05_distribucion_objetivo.R")
source("modelov5/07_optimizador_cortas.R")

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║         REPORTE DETALLADO DE CORTAS POR RODAL             ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# 1. CARGAR DATOS - 3 MOMENTOS
# ==============================================================================

cat("[1/4] Cargando datos de diferentes momentos...\n\n")

# MOMENTO 1: Inicial (año 0)
arboles_inicial <- readRDS("datos_intermedios/arboles_analisis.rds") %>%
  filter(genero_grupo %in% c("Pinus", "Quercus"))

cat(sprintf("✓ Población INICIAL (año 0): %d árboles\n", nrow(arboles_inicial)))

# MOMENTO 2: Antes de corta (año 6)
if (file.exists("datos_intermedios/arboles_año6.rds")) {
  arboles_antes_corta <- readRDS("datos_intermedios/arboles_año6.rds") %>%
    filter(genero_grupo %in% c("Pinus", "Quercus"))
  cat(sprintf("✓ Población ANTES CORTA (año 6): %d árboles\n", nrow(arboles_antes_corta)))
  tiene_año6 <- TRUE
} else {
  cat("⚠️  No existe arboles_año6.rds - usando inicial como antes_corta\n")
  arboles_antes_corta <- arboles_inicial
  tiene_año6 <- FALSE
}

cat("\n")

# ==============================================================================
# 2. SELECCIONAR RODAL
# ==============================================================================

rodales_disponibles <- unique(arboles_antes_corta$rodal)
cat(sprintf("Rodales disponibles: %s\n", paste(rodales_disponibles, collapse = ", ")))

rodal_test <- rodales_disponibles[1]
cat(sprintf("→ Analizando RODAL %d\n\n", rodal_test))

# ==============================================================================
# 3. CALCULAR VOLÚMENES POR CLASE Y GÉNERO - INICIAL
# ==============================================================================

cat("[2/4] Calculando volúmenes INICIALES (año 0)...\n\n")

# ✓ OBTENER FACTOR DE EXPANSIÓN
arboles_rodal_inicial <- arboles_inicial %>% filter(rodal == rodal_test)
n_sitios <- first(arboles_rodal_inicial$num_muestreos_realizados)
area_muestreada_ha <- n_sitios * CONFIG$area_parcela_ha
factor_expansion <- 1 / area_muestreada_ha

cat(sprintf("Factor de expansión rodal %d:\n", rodal_test))
cat(sprintf("  Sitios muestreados:    %d\n", n_sitios))
cat(sprintf("  Área muestreada:       %.2f ha\n", area_muestreada_ha))
cat(sprintf("  Factor de expansión:   %.2f\n\n", factor_expansion))

vol_inicial_detalle <- arboles_inicial %>%
  filter(rodal == rodal_test, !dominancia %in% c(7, 8, 9)) %>%
  mutate(
    clase_d = asignar_clase_diametrica(diametro_normal, formato = "rango")
  ) %>%
  group_by(genero_grupo, clase_d) %>%
  summarise(
    n_arboles_muestreado = n(),
    vol_muestreado_m3 = sum(volumen_m3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # EXPANDIR
    n_arboles_ha = n_arboles_muestreado * factor_expansion,
    vol_ha_m3 = vol_muestreado_m3 * factor_expansion
  ) %>%
  arrange(genero_grupo, clase_d)

vol_inicial_total <- arboles_inicial %>%
  filter(rodal == rodal_test, !dominancia %in% c(7, 8, 9)) %>%
  group_by(genero_grupo) %>%
  summarise(
    n_arboles_muestreado = n(),
    vol_muestreado_m3 = sum(volumen_m3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    n_arboles_ha = n_arboles_muestreado * factor_expansion,
    vol_ha_m3 = vol_muestreado_m3 * factor_expansion
  )

cat("VOLUMEN INICIAL por género:\n")
print(vol_inicial_total %>% 
        select(genero_grupo, n_arboles_muestreado, vol_muestreado_m3, n_arboles_ha, vol_ha_m3) %>%
        mutate(across(where(is.numeric), ~round(.x, 2))))
cat("\n")

# ==============================================================================
# 4. CALCULAR VOLÚMENES POR CLASE Y GÉNERO - ANTES CORTA
# ==============================================================================

cat("[3/4] Calculando volúmenes ANTES DE CORTA (año 6)...\n\n")

vol_antes_detalle <- arboles_antes_corta %>%
  filter(rodal == rodal_test, !dominancia %in% c(7, 8, 9)) %>%
  mutate(
    clase_d = asignar_clase_diametrica(diametro_normal, formato = "rango")
  ) %>%
  group_by(genero_grupo, clase_d) %>%
  summarise(
    n_arboles_muestreado = n(),
    vol_muestreado_m3 = sum(volumen_m3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    n_arboles_ha = n_arboles_muestreado * factor_expansion,
    vol_ha_m3 = vol_muestreado_m3 * factor_expansion
  ) %>%
  arrange(genero_grupo, clase_d)

vol_antes_total <- arboles_antes_corta %>%
  filter(rodal == rodal_test, !dominancia %in% c(7, 8, 9)) %>%
  group_by(genero_grupo) %>%
  summarise(
    n_arboles_muestreado = n(),
    vol_muestreado_m3 = sum(volumen_m3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    n_arboles_ha = n_arboles_muestreado * factor_expansion,
    vol_ha_m3 = vol_muestreado_m3 * factor_expansion
  )

cat("VOLUMEN ANTES DE CORTA por género:\n")
print(vol_antes_total %>%
        select(genero_grupo, n_arboles_muestreado, vol_muestreado_m3, n_arboles_ha, vol_ha_m3) %>%
        mutate(across(where(is.numeric), ~round(.x, 2))))
cat("\n")

# ==============================================================================
# 5. CALCULAR PLAN DE CORTAS
# ==============================================================================

cat("[4/4] Calculando plan de cortas...\n\n")

arboles_rodal <- arboles_antes_corta %>%
  filter(rodal == rodal_test)

plan_cortas <- calcular_plan_cortas(arboles_rodal, CONFIG)

# Después de calcular el plan
cat("\n=== TEST DE DUPLICADOS ===\n")

ids_plan <- plan_cortas$arboles_marcados$arbol_id

cat(sprintf("IDs en plan: %d\n", length(ids_plan)))
cat(sprintf("IDs únicos:  %d\n", n_distinct(ids_plan)))

if (length(ids_plan) != n_distinct(ids_plan)) {
  cat("\n❌ HAY IDs DUPLICADOS EN EL PLAN\n")
  
  # Ver cuáles están duplicados
  duplicados <- ids_plan[duplicated(ids_plan)]
  cat(sprintf("IDs duplicados (%d):\n", length(duplicados)))
  print(duplicados)
  
  # Ver tabla completa de duplicados
  plan_cortas$arboles_marcados %>%
    group_by(arbol_id) %>%
    filter(n() > 1) %>%
    arrange(arbol_id) %>%
    select(arbol_id, rodal, muestreo, arbol, genero_grupo, 
           diametro_normal, dominancia) %>%
    print(n = Inf)
}

# Test en rodal también
cat(sprintf("\nIDs en rodal: %d\n", length(arboles_rodal$arbol_id)))
cat(sprintf("IDs únicos:   %d\n", n_distinct(arboles_rodal$arbol_id)))

# Justo después de: plan_cortas <- calcular_plan_cortas(arboles_rodal, CONFIG)

cat("\n=== DIAGNÓSTICO CRÍTICO ===\n")

# 1. Tamaños
cat(sprintf("Población rodal: %d árboles\n", nrow(arboles_rodal)))
cat(sprintf("Marcados plan: %d árboles\n", nrow(plan_cortas$arboles_marcados)))

# 2. IDs del plan existen en rodal?
ids_plan <- plan_cortas$arboles_marcados$arbol_id
ids_rodal <- arboles_rodal$arbol_id

match_exacto <- sum(ids_plan %in% ids_rodal)
cat(sprintf("IDs que coinciden: %d de %d\n", match_exacto, length(ids_plan)))

# 3. Si NO coinciden todos, hay un bug en calcular_plan_cortas()
if (match_exacto != length(ids_plan)) {
  cat("\n❌ ERROR DETECTADO: calcular_plan_cortas() devuelve IDs que no existen\n")
  cat("Esto es imposible - revisar 07_optimizador_cortas.R línea 180-220\n")
}

# Después de: plan_cortas <- calcular_plan_cortas(...)
# Pero ANTES de: aplicar_cortas(...)

cat("\n=== VERIFICACIÓN DE VOLÚMENES ===\n")

# Volumen JUSTO antes de cortar
vol_inmediato_antes <- sum(arboles_rodal$volumen_m3[
  !arboles_rodal$dominancia %in% c(7,8,9)
], na.rm = TRUE)

# Volumen marcado
vol_plan <- sum(plan_cortas$arboles_marcados$volumen_m3, na.rm = TRUE)

cat(sprintf("Vol INMEDIATO antes de cortar: %.2f m³\n", vol_inmediato_antes))
cat(sprintf("Vol en plan de cortas:         %.2f m³\n", vol_plan))
cat(sprintf("Vol esperado después:          %.2f m³\n", 
            vol_inmediato_antes - vol_plan))

# Aplicar cortas
arboles_despues <- aplicar_cortas(arboles_rodal, plan_cortas, 6)

# Volumen real después
vol_real_despues <- sum(arboles_despues$volumen_m3[
  !arboles_despues$dominancia %in% c(7,8,9)
], na.rm = TRUE)

cat(sprintf("Vol REAL después:              %.2f m³\n", vol_real_despues))
cat(sprintf("\nDiscrepancia: %.2f m³ (%.1f%%)\n", 
            vol_real_despues - (vol_inmediato_antes - vol_plan),
            ((vol_real_despues - (vol_inmediato_antes - vol_plan)) / 
               (vol_inmediato_antes - vol_plan)) * 100))


# 4. Aplicar cortas
arboles_despues <- aplicar_cortas(arboles_rodal, plan_cortas, 6)

# Después de: arboles_despues <- aplicar_cortas(arboles_rodal, plan_cortas, 6)

cat("\n=== DIAGNÓSTICO DE CORTAS FALLIDAS ===\n")

# 1. IDs marcados vs IDs cortados
ids_marcados <- plan_cortas$arboles_marcados$arbol_id
ids_cortados <- arboles_despues$arbol_id[
  arboles_despues$dominancia == 9 & 
  !is.na(arboles_despues$año_corta)
]

cat(sprintf("IDs marcados: %d\n", length(ids_marcados)))
cat(sprintf("IDs cortados: %d\n", length(ids_cortados)))

# 2. ¿Cuáles NO se cortaron?
ids_no_cortados <- setdiff(ids_marcados, ids_cortados)

if (length(ids_no_cortados) > 0) {
  cat(sprintf("\n❌ %d árboles NO se cortaron:\n", length(ids_no_cortados)))
  
  # Ver info de los no cortados
  arboles_problema <- arboles_rodal %>%
    filter(arbol_id %in% ids_no_cortados) %>%
    select(arbol_id, rodal, muestreo, arbol, 
           genero_grupo, diametro_normal, dominancia)
  
  print(arboles_problema)
  
  # Después de generar ids_no_cortados
  
  cat("\n=== BÚSQUEDA EXHAUSTIVA ===\n")
  
  # Buscar por otros métodos
  for (id_problema in ids_no_cortados) {
    cat(sprintf("\nBuscando: '%s'\n", id_problema))
    
    # ¿Existe en plan_cortas?
    existe_plan <- id_problema %in% plan_cortas$arboles_marcados$arbol_id
    cat(sprintf("  En plan: %s\n", existe_plan))
    
    # ¿Existe en arboles_rodal?
    existe_rodal <- id_problema %in% arboles_rodal$arbol_id
    cat(sprintf("  En rodal: %s\n", existe_rodal))
    
    # Si está en plan pero NO en rodal, ver sus datos originales
    if (existe_plan && !existe_rodal) {
      arbol_plan <- plan_cortas$arboles_marcados %>%
        filter(arbol_id == id_problema) %>%
        select(arbol_id, rodal, muestreo, arbol, genero_grupo, 
               diametro_normal, es_recluta)
      
      cat("  Datos del plan:\n")
      print(arbol_plan)
      
      # Buscar en rodal por rodal+muestreo+arbol
      if (nrow(arbol_plan) > 0) {
        arbol_rodal_equiv <- arboles_rodal %>%
          filter(
            rodal == arbol_plan$rodal[1],
            muestreo == arbol_plan$muestreo[1],
            arbol == arbol_plan$arbol[1]
          ) %>%
          select(arbol_id, rodal, muestreo, arbol, genero_grupo)
        
        cat("  Equivalente en rodal (por rodal+muestreo+arbol):\n")
        print(arbol_rodal_equiv)
        
        if (nrow(arbol_rodal_equiv) > 0) {
          cat("\n  ❌ PROBLEMA ENCONTRADO:\n")
          cat(sprintf("     ID del plan:  '%s'\n", id_problema))
          cat(sprintf("     ID del rodal: '%s'\n", arbol_rodal_equiv$arbol_id[1]))
          cat("     → Los IDs no coinciden pero es el MISMO árbol\n")
        }
      }
    }
  }
  
  # 3. Verificar tipos de datos
  cat("\nTIPOS DE DATOS:\n")
  cat(sprintf("  Tipo arbol_id marcados: %s\n", class(ids_marcados)))
  cat(sprintf("  Tipo arbol_id rodal:    %s\n", class(arboles_rodal$arbol_id)))
  
  # 4. Test exacto del %in%
  cat("\nTEST MANUAL DEL %in%:\n")
  for (id_prob in head(ids_no_cortados, 3)) {
    existe <- id_prob %in% arboles_rodal$arbol_id
    idx <- which(arboles_rodal$arbol_id == id_prob)
    
    cat(sprintf("  ID '%s':\n", id_prob))
    cat(sprintf("    %in% devuelve: %s\n", existe))
    cat(sprintf("    which() devuelve: %s\n", 
                ifelse(length(idx) > 0, paste(idx, collapse=","), "NADA")))
  }
  
  # 5. Comparación carácter por carácter
  cat("\nCOMPARACIÓN DETALLADA:\n")
  id_test <- ids_no_cortados[1]
  id_rodal_match <- arboles_rodal$arbol_id[
    arboles_rodal$rodal == arboles_problema$rodal[1] &
    arboles_rodal$muestreo == arboles_problema$muestreo[1] &
    arboles_rodal$arbol == arboles_problema$arbol[1]
  ]
  
  cat(sprintf("  ID del plan:  '%s' (length: %d)\n", 
              id_test, nchar(id_test)))
  cat(sprintf("  ID del rodal: '%s' (length: %d)\n", 
              id_rodal_match, nchar(id_rodal_match)))
  cat(sprintf("  ¿Son idénticos?: %s\n", 
              identical(id_test, id_rodal_match)))
  
  # Bytes
  cat("\n  Bytes del plan:\n")
  print(charToRaw(id_test))
  cat("  Bytes del rodal:\n")
  print(charToRaw(id_rodal_match))
}

# 5. Verificar resultado
n_tocones_creados <- sum(arboles_despues$dominancia == 9 & 
                           !is.na(arboles_despues$año_corta))

cat(sprintf("\nTocones creados: %d\n", n_tocones_creados))
cat(sprintf("Esperado: %d\n", length(ids_plan)))

# 6. Volumen después
vol_antes <- sum(arboles_rodal$volumen_m3[!arboles_rodal$dominancia %in% c(7,8,9)], na.rm=TRUE)
vol_cortado <- sum(plan_cortas$arboles_marcados$volumen_m3, na.rm=TRUE)
vol_despues <- sum(arboles_despues$volumen_m3[!arboles_despues$dominancia %in% c(7,8,9)], na.rm=TRUE)

cat(sprintf("\nVolumen ANTES: %.2f m³\n", vol_antes))
cat(sprintf("Volumen CORTADO: %.2f m³\n", vol_cortado))
cat(sprintf("Volumen DESPUÉS: %.2f m³\n", vol_despues))
cat(sprintf("Diferencia: %.2f m³ (esperado: %.2f)\n", 
            vol_antes - vol_despues, vol_cortado))

# ==============================================================================
# 6. CALCULAR VOLÚMENES CORTADOS POR CLASE Y GÉNERO
# ==============================================================================

if (!is.null(plan_cortas) && nrow(plan_cortas$arboles_marcados) > 0) {
  
  vol_cortado_detalle <- plan_cortas$arboles_marcados %>%
    mutate(
      clase_d = asignar_clase_diametrica(diametro_normal, formato = "rango")
    ) %>%
    group_by(genero_grupo, clase_d) %>%
    summarise(
      n_arboles_muestreado = n(),
      vol_muestreado_m3 = sum(volumen_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      n_arboles_ha = n_arboles_muestreado * factor_expansion,
      vol_ha_m3 = vol_muestreado_m3 * factor_expansion
    ) %>%
    arrange(genero_grupo, clase_d)
  
  vol_cortado_total <- plan_cortas$arboles_marcados %>%
    group_by(genero_grupo) %>%
    summarise(
      n_arboles_muestreado = n(),
      vol_muestreado_m3 = sum(volumen_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      n_arboles_ha = n_arboles_muestreado * factor_expansion,
      vol_ha_m3 = vol_muestreado_m3 * factor_expansion
    )
  
  cat("\n═══════════════════════════════════════════════════════════\n")
  cat("DIAGNÓSTICO DETALLADO PRE-CORTA\n")
  cat("═══════════════════════════════════════════════════════════\n\n")
  
  # 1. Población del rodal
  cat("[1] POBLACIÓN DEL RODAL\n")
  cat(sprintf("  Total árboles: %d\n", nrow(arboles_rodal)))
  cat(sprintf("  Vivos: %d\n", sum(!arboles_rodal$dominancia %in% c(7,8,9))))
  cat(sprintf("  Tocones previos: %d\n", sum(arboles_rodal$dominancia == 9, na.rm = TRUE)))
  
  # 2. Plan de cortas
  cat("\n[2] PLAN DE CORTAS\n")
  cat(sprintf("  Árboles marcados: %d\n", nrow(plan_cortas$arboles_marcados)))
  
  # 3. Tipos de arbol_id
  cat("\n[3] TIPOS DE DATOS\n")
  cat(sprintf("  Tipo arbol_id rodal: %s\n", class(arboles_rodal$arbol_id)))
  cat(sprintf("  Tipo arbol_id plan: %s\n", class(plan_cortas$arboles_marcados$arbol_id)))
  
  # 4. Muestras de IDs
  cat("\n[4] MUESTRAS DE IDs\n")
  cat("  Rodal (primeros 5):\n")
  print(head(arboles_rodal$arbol_id, 5))
  cat("\n  Plan (primeros 5):\n")
  print(head(plan_cortas$arboles_marcados$arbol_id, 5))
  
  # 5. Verificar existencia de IDs
  ids_plan <- plan_cortas$arboles_marcados$arbol_id
  ids_rodal <- arboles_rodal$arbol_id
  
  ids_no_encontrados <- setdiff(ids_plan, ids_rodal)
  ids_encontrados <- intersect(ids_plan, ids_rodal)
  
  cat("\n[5] VERIFICACIÓN DE IDs\n")
  cat(sprintf("  IDs en plan: %d\n", length(ids_plan)))
  cat(sprintf("  IDs encontrados en rodal: %d\n", length(ids_encontrados)))
  cat(sprintf("  IDs NO encontrados: %d\n", length(ids_no_encontrados)))
  
  if (length(ids_no_encontrados) > 0) {
    cat("\n  ❌ PROBLEMA: IDs del plan no existen en rodal\n")
    cat("  Primeros 5 IDs no encontrados:\n")
    print(head(ids_no_encontrados, 5))
  }
  
  # 6. Test del operador %in%
  cat("\n[6] TEST DEL OPERADOR %in%\n")
  test_id <- ids_plan[1]
  cat(sprintf("  Probando ID: %s (tipo: %s)\n", test_id, class(test_id)))
  cat(sprintf("  ¿Está en rodal?: %s\n", test_id %in% ids_rodal))
  
  # 7. Comparación exacta de primeros IDs
  cat("\n[7] COMPARACIÓN EXACTA\n")
  for (i in 1:min(3, length(ids_plan))) {
    id_plan <- ids_plan[i]
    match <- any(ids_rodal == id_plan)
    cat(sprintf("  Plan[%d]: '%s' -> Match: %s\n", i, id_plan, match))
  }
  
  cat("\n")
  
  # Aplicar cortas para calcular "después"
  arboles_despues_corta <- aplicar_cortas(arboles_rodal, plan_cortas, año_corta = 6)
  
} else {
  vol_cortado_detalle <- tibble()
  vol_cortado_total <- tibble(
    genero_grupo = character(), 
    n_arboles_muestreado = integer(), 
    vol_muestreado_m3 = numeric(),
    n_arboles_ha = numeric(),
    vol_ha_m3 = numeric()
  )
  arboles_despues_corta <- arboles_rodal
}

# ==============================================================================
# 7. CALCULAR VOLÚMENES DESPUÉS DE CORTA
# ==============================================================================

vol_despues_detalle <- arboles_despues_corta %>%
  filter(!dominancia %in% c(7, 8, 9)) %>%
  mutate(
    clase_d = asignar_clase_diametrica(diametro_normal, formato = "rango")
  ) %>%
  group_by(genero_grupo, clase_d) %>%
  summarise(
    n_arboles_muestreado = n(),
    vol_muestreado_m3 = sum(volumen_m3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    n_arboles_ha = n_arboles_muestreado * factor_expansion,
    vol_ha_m3 = vol_muestreado_m3 * factor_expansion
  ) %>%
  arrange(genero_grupo, clase_d)

vol_despues_total <- arboles_despues_corta %>%
  filter(!dominancia %in% c(7, 8, 9)) %>%
  group_by(genero_grupo) %>%
  summarise(
    n_arboles_muestreado = n(),
    vol_muestreado_m3 = sum(volumen_m3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    n_arboles_ha = n_arboles_muestreado * factor_expansion,
    vol_ha_m3 = vol_muestreado_m3 * factor_expansion
  )

# ==============================================================================
# 8. REPORTE CONSOLIDADO
# ==============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat(sprintf("║         REPORTE CONSOLIDADO - RODAL %d                     ║\n", rodal_test))
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# RESUMEN GENERAL POR GÉNERO
# ─────────────────────────────────────────────────────────────────────────────

cat("═══════════════════════════════════════════════════════════\n")
cat("RESUMEN GENERAL POR GÉNERO\n")
cat("═══════════════════════════════════════════════════════════\n\n")

resumen_general <- vol_inicial_total %>%
  select(genero_grupo, 
         n_ini_muestreado = n_arboles_muestreado, 
         vol_ini_muestreado = vol_muestreado_m3,
         n_ini_ha = n_arboles_ha,
         vol_ini_ha = vol_ha_m3) %>%
  left_join(
    vol_antes_total %>% 
      select(genero_grupo,
             n_antes_muestreado = n_arboles_muestreado,
             vol_antes_muestreado = vol_muestreado_m3,
             n_antes_ha = n_arboles_ha,
             vol_antes_ha = vol_ha_m3),
    by = "genero_grupo"
  ) %>%
  left_join(
    vol_cortado_total %>% 
      select(genero_grupo,
             n_cortado_muestreado = n_arboles_muestreado,
             vol_cortado_muestreado = vol_muestreado_m3,
             n_cortado_ha = n_arboles_ha,
             vol_cortado_ha = vol_ha_m3),
    by = "genero_grupo"
  ) %>%
  left_join(
    vol_despues_total %>% 
      select(genero_grupo,
             n_despues_muestreado = n_arboles_muestreado,
             vol_despues_muestreado = vol_muestreado_m3,
             n_despues_ha = n_arboles_ha,
             vol_despues_ha = vol_ha_m3),
    by = "genero_grupo"
  ) %>%
  mutate(
    across(starts_with("n_cortado") | starts_with("vol_cortado"), ~replace_na(.x, 0)),
    
    # Cambios (trabajamos con vol_ha)
    delta_inicial_antes_ha = vol_antes_ha - vol_ini_ha,
    pct_crecimiento = (delta_inicial_antes_ha / vol_ini_ha) * 100,
    pct_cortado = (vol_cortado_ha / vol_antes_ha) * 100,
    delta_antes_despues_ha = vol_despues_ha - vol_antes_ha,
    pct_remanente = (vol_despues_ha / vol_antes_ha) * 100
  )

print(resumen_general %>%
        select(genero_grupo, 
               vol_ini_ha, vol_antes_ha, vol_cortado_ha, vol_despues_ha,
               pct_crecimiento, pct_cortado, pct_remanente) %>%
        mutate(across(where(is.numeric), ~round(.x, 2))))

cat("\n")
cat("NOTA: vol_*_ha = Volumen expandido por hectárea\n")
cat(sprintf("      Factor de expansión: %.2f (basado en %d sitios de 0.05 ha)\n\n", 
            factor_expansion, n_sitios))

# ─────────────────────────────────────────────────────────────────────────────
# TABLA DETALLADA POR CLASE DIAMÉTRICA Y GÉNERO
# ─────────────────────────────────────────────────────────────────────────────

cat("═══════════════════════════════════════════════════════════\n")
cat("DETALLE POR CLASE DIAMÉTRICA Y GÉNERO\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Crear tabla completa combinando todos los momentos
todas_clases <- bind_rows(
  vol_inicial_detalle %>% select(genero_grupo, clase_d),
  vol_antes_detalle %>% select(genero_grupo, clase_d),
  vol_cortado_detalle %>% select(genero_grupo, clase_d),
  vol_despues_detalle %>% select(genero_grupo, clase_d)
) %>%
  distinct()

tabla_completa <- todas_clases %>%
  left_join(
    vol_inicial_detalle %>% 
      rename(n_ini = n_arboles_ha, vol_ini = vol_ha_m3),
    by = c("genero_grupo", "clase_d")
  ) %>%
  left_join(
    vol_antes_detalle %>% 
      rename(n_antes = n_arboles_ha, vol_antes = vol_ha_m3),
    by = c("genero_grupo", "clase_d")
  ) %>%
  left_join(
    vol_cortado_detalle %>% 
      rename(n_cortado = n_arboles_ha, vol_cortado = vol_ha_m3),
    by = c("genero_grupo", "clase_d")
  ) %>%
  left_join(
    vol_despues_detalle %>% 
      rename(n_despues = n_arboles_ha, vol_despues = vol_ha_m3),
    by = c("genero_grupo", "clase_d")
  ) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  arrange(genero_grupo, clase_d)

# Mostrar por género
for (gen in unique(tabla_completa$genero_grupo)) {
  
  cat(sprintf("\n─── %s ───\n\n", gen))
  
  tabla_gen <- tabla_completa %>%
    filter(genero_grupo == gen) %>%
    select(-genero_grupo)
  
  print(tabla_gen %>%
          mutate(across(where(is.numeric), ~round(.x, 2))))
  
  cat("\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# RESUMEN EJECUTIVO
# ─────────────────────────────────────────────────────────────────────────────

cat("═══════════════════════════════════════════════════════════\n")
cat("RESUMEN EJECUTIVO\n")
cat("═══════════════════════════════════════════════════════════\n\n")

total_inicial_ha = sum(vol_inicial_total$vol_ha_m3)
total_antes_ha = sum(vol_antes_total$vol_ha_m3)
total_cortado_ha = sum(vol_cortado_total$vol_ha_m3)
total_despues_ha = sum(vol_despues_total$vol_ha_m3)

cat(sprintf("VOLUMEN POR HECTÁREA - RODAL %d:\n\n", rodal_test))
cat(sprintf("  1. Inicial (año 0):        %.2f m³/ha\n", total_inicial_ha))
cat(sprintf("  2. Antes corta (año 6):    %.2f m³/ha  (%+.2f m³/ha, %+.1f%%)\n", 
            total_antes_ha, 
            total_antes_ha - total_inicial_ha,
            ((total_antes_ha - total_inicial_ha) / total_inicial_ha) * 100))
cat(sprintf("  3. Volumen cortado:        %.2f m³/ha  (%.1f%% del disponible)\n", 
            total_cortado_ha,
            (total_cortado_ha / total_antes_ha) * 100))
cat(sprintf("  4. Después corta:          %.2f m³/ha  (%.1f%% remanente)\n", 
            total_despues_ha,
            (total_despues_ha / total_antes_ha) * 100))
cat("\n")

cat("INTERPRETACIÓN:\n")
cat(sprintf("  • Crecimiento año 0→6:     %+.2f m³/ha (%+.1f%%)\n",
            total_antes_ha - total_inicial_ha,
            ((total_antes_ha - total_inicial_ha) / total_inicial_ha) * 100))
cat(sprintf("  • Extracción en año 6:     %.2f m³/ha (%.1f%%)\n",
            total_cortado_ha,
            (total_cortado_ha / total_antes_ha) * 100))
cat(sprintf("  • Balance neto año 0→6:    %+.2f m³/ha (%+.1f%%)\n",
            total_despues_ha - total_inicial_ha,
            ((total_despues_ha - total_inicial_ha) / total_inicial_ha) * 100))
cat("\n")

# Mostrar también valores absolutos del rodal
if ("superficie_ha" %in% names(arboles_rodal)) {
  sup_rodal <- first(arboles_rodal$superficie_ha)
  cat(sprintf("VOLUMEN TOTAL DEL RODAL (%.1f ha):\n\n", sup_rodal))
  cat(sprintf("  1. Inicial:             %.2f m³\n", total_inicial_ha * sup_rodal))
  cat(sprintf("  2. Antes corta:         %.2f m³\n", total_antes_ha * sup_rodal))
  cat(sprintf("  3. Cortado:             %.2f m³\n", total_cortado_ha * sup_rodal))
  cat(sprintf("  4. Después corta:       %.2f m³\n\n", total_despues_ha * sup_rodal))
}

cat(sprintf("NOTA: Valores expandidos desde %d sitios de 0.05 ha (factor: %.2f)\n\n",
            n_sitios, factor_expansion))

# ─────────────────────────────────────────────────────────────────────────────
# EXPORTAR A CSV
# ─────────────────────────────────────────────────────────────────────────────

cat("═══════════════════════════════════════════════════════════\n")
cat("EXPORTANDO RESULTADOS\n")
cat("═══════════════════════════════════════════════════════════\n\n")

if (!dir.exists("resultados")) dir.create("resultados", recursive = TRUE)

# Resumen general
write_csv(resumen_general, 
          sprintf("resultados/reporte_cortas_rodal%d_resumen.csv", rodal_test))
cat(sprintf("✓ resultados/reporte_cortas_rodal%d_resumen.csv\n", rodal_test))

# Detalle por clase
write_csv(tabla_completa, 
          sprintf("resultados/reporte_cortas_rodal%d_detalle.csv", rodal_test))
cat(sprintf("✓ resultados/reporte_cortas_rodal%d_detalle.csv\n", rodal_test))

cat("\n✓ Reporte completado\n\n")