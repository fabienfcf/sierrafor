# ==============================================================================
# SCRIPT DE DIAGNOSTIC COMPLET DU MODÈLE DE CROISSANCE
# ==============================================================================

library(tidyverse)

# ==============================================================================
# 1. VALIDATION DES DONNÉES D'ENTRÉE
# ==============================================================================

validar_estructura_datos <- function(arboles_df) {
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║         VALIDATION DE LA STRUCTURE DES DONNÉES            ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  errores <- c()
  advertencias <- c()
  
  # Colonnes obligatoires
  cols_obligatorias <- c(
    "rodal", "arbol_id", "especie", "nombre_cientifico",
    "genero_grupo", "dominancia", "diametro_normal", 
    "altura_total", "volumen_m3", "area_basal",
    "tipo", "a", "b", "c"
  )
  
  for (col in cols_obligatorias) {
    if (!col %in% names(arboles_df)) {
      errores <- c(errores, sprintf("✗ Colonne manquante: %s", col))
    }
  }
  
  if (length(errores) > 0) {
    cat("ERREURS CRITIQUES:\n")
    for (e in errores) cat(paste0("  ", e, "\n"))
    stop("Données invalides - correction nécessaire")
  }
  
  # Validations de cohérence
  cat("VALIDATION DES DONNÉES:\n")
  
  # 1. Codes de dominance valides
  dominancia_invalida <- arboles_df %>%
    filter(!dominancia %in% 1:9) %>%
    nrow()
  
  if (dominancia_invalida > 0) {
    advertencias <- c(advertencias, 
                      sprintf("⚠ %d arbres avec dominance invalide", dominancia_invalida))
  } else {
    cat("  ✓ Tous les codes de dominance sont valides (1-9)\n")
  }
  
  # 2. Diamètres et hauteurs positifs (sauf morts)
  vivos <- arboles_df %>% filter(!dominancia %in% c(7, 8, 9))
  
  diametros_invalidos <- sum(vivos$diametro_normal <= 0, na.rm = TRUE)
  alturas_invalidas <- sum(vivos$altura_total <= 0, na.rm = TRUE)
  
  if (diametros_invalidos > 0) {
    advertencias <- c(advertencias,
                      sprintf("⚠ %d arbres vivants avec diamètre ≤ 0", diametros_invalidos))
  } else {
    cat("  ✓ Tous les diamètres sont positifs\n")
  }
  
  if (alturas_invalidas > 0) {
    advertencias <- c(advertencias,
                      sprintf("⚠ %d arbres vivants avec hauteur ≤ 0", alturas_invalidas))
  } else {
    cat("  ✓ Toutes les hauteurs sont positives\n")
  }
  
  # 3. Équations alométriques complètes
  ecuaciones_incompletas <- arboles_df %>%
    filter(is.na(tipo) | is.na(a) | is.na(b) | is.na(c)) %>%
    nrow()
  
  if (ecuaciones_incompletas > 0) {
    advertencias <- c(advertencias,
                      sprintf("⚠ %d arbres sans équation alométrique complète", 
                              ecuaciones_incompletas))
  } else {
    cat("  ✓ Toutes les équations alométriques sont complètes\n")
  }
  
  # 4. Distribution par genre
  cat("\nDISTRIBUTION PAR GENRE:\n")
  dist_genero <- arboles_df %>%
    count(genero_grupo, name = "n") %>%
    mutate(pct = round(n / sum(n) * 100, 1))
  print(dist_genero)
  
  # 5. Distribution par dominance
  cat("\nDISTRIBUTION PAR DOMINANCE:\n")
  dist_dominancia <- arboles_df %>%
    count(dominancia, name = "n") %>%
    mutate(
      status = if_else(dominancia %in% c(7, 8, 9), "Mort", "Vivant"),
      pct = round(n / sum(n) * 100, 1)
    )
  print(dist_dominancia)
  
  # Résumé
  if (length(advertencias) > 0) {
    cat("\nAVERTISSEMENTS:\n")
    for (a in advertencias) cat(paste0("  ", a, "\n"))
  }
  
  n_vivos <- sum(!arboles_df$dominancia %in% c(7, 8, 9))
  n_muertos <- sum(arboles_df$dominancia %in% c(7, 8, 9))
  
  cat(sprintf("\n✓ Total arbres: %d (Vivants: %d, Morts: %d)\n",
              nrow(arboles_df), n_vivos, n_muertos))
  
  return(TRUE)
}

# ==============================================================================
# 2. DIAGNOSTIC DES PARAMÈTRES DE CROISSANCE
# ==============================================================================

diagnosticar_parametros_crecimiento <- function(config) {
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║       DIAGNOSTIC DES PARAMÈTRES DE CROISSANCE             ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # 1. Taux de croissance par genre
  cat("TAUX DE CROISSANCE BASE (cm/an):\n")
  for (gen in names(config$crecimiento_base)) {
    cat(sprintf("  %s: %.2f cm/an\n", gen, config$crecimiento_base[[gen]]))
  }
  
  # 2. Modificateurs de dominance
  cat("\nMODIFICATEURS PAR DOMINANCE:\n")
  print(config$modificadores_dominancia %>% 
          select(codigo, etiqueta, factor_crecimiento, factor_mortalidad))
  
  # 3. Simulation de croissance théorique
  cat("\nCROISSANCE THÉORIQUE ANNUELLE:\n")
  
  resultados_teoricos <- expand_grid(
    genero = names(config$crecimiento_base),
    dominancia = 1:6
  ) %>%
    mutate(
      tasa_base = map_dbl(genero, ~config$crecimiento_base[[.x]]),
      factor_dom = map_dbl(dominancia, function(d) {
        config$modificadores_dominancia %>%
          filter(codigo == d) %>%
          pull(factor_crecimiento)
      }),
      incremento_d_cm = tasa_base * factor_dom
    )
  
  # Tableau récapitulatif
  tabla_resumen <- resultados_teoricos %>%
    pivot_wider(
      names_from = dominancia,
      values_from = incremento_d_cm,
      names_prefix = "Dom_"
    )
  
  print(tabla_resumen, n = 20)
  
  # Vérifier cohérence
  cat("\nVÉRIFICATIONS:\n")
  
  # Les dominants doivent croître plus
  dominantes <- resultados_teoricos %>% filter(dominancia == 1)
  suprimidos <- resultados_teoricos %>% filter(dominancia == 6)
  
  for (gen in unique(resultados_teoricos$genero)) {
    inc_dom <- dominantes %>% filter(genero == gen) %>% pull(incremento_d_cm)
    inc_sup <- suprimidos %>% filter(genero == gen) %>% pull(incremento_d_cm)
    
    ratio <- inc_dom / inc_sup
    if (ratio > 1.5) {
      cat(sprintf("  ✓ %s: Dominants croissent %.1fx plus que supprimés\n", 
                  gen, ratio))
    } else {
      cat(sprintf("  ⚠ %s: Ratio dominants/supprimés faible (%.1fx)\n", 
                  gen, ratio))
    }
  }
  
  return(resultados_teoricos)
}

# ==============================================================================
# 3. ANALYSE DE SENSIBILITÉ
# ==============================================================================

analisis_sensibilidad <- function(arboles_df, config, n_simulaciones = 5) {
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║             ANALYSE DE SENSIBILITÉ                        ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # Préparer données
  muestra <- arboles_df %>%
    filter(!dominancia %in% c(7, 8, 9)) %>%
    sample_n(min(50, n()))
  
  resultados <- list()
  
  # Scénarios de variation
  escenarios <- list(
    "Base" = 1.0,
    "Croissance_-20%" = 0.8,
    "Croissance_+20%" = 1.2,
    "Mortalité_x2" = 1.0,
    "Mortalité_x0.5" = 1.0
  )
  
  for (escenario in names(escenarios)) {
    cat(sprintf("\nScénario: %s\n", escenario))
    
    # Modifier configuration temporairement
    config_temp <- config
    
    if (grepl("Croissance", escenario)) {
      factor <- escenarios[[escenario]]
      for (gen in names(config_temp$crecimiento_base)) {
        config_temp$crecimiento_base[[gen]] <- 
          config_temp$crecimiento_base[[gen]] * factor
      }
    }
    
    if (grepl("Mortalité", escenario)) {
      if (escenario == "Mortalité_x2") {
        config_temp$mortalidad_base <- config_temp$mortalidad_base * 2
      } else if (escenario == "Mortalité_x0.5") {
        config_temp$mortalidad_base <- config_temp$mortalidad_base * 0.5
      }
    }
    
    # Simuler 1 an
    source("modelov5/02_modelos_crecimiento_CORREGIDO.R")
    
    muestra_sim <- aplicar_crecimiento_poblacion(muestra, config_temp)
    
    # Calculer métriques
    metricas <- muestra_sim %>%
      summarise(
        inc_d_medio = mean(incremento_d_cm, na.rm = TRUE),
        inc_h_medio = mean(incremento_h_m, na.rm = TRUE),
        inc_vol_total = sum(incremento_vol_m3, na.rm = TRUE)
      )
    
    resultados[[escenario]] <- metricas
    
    cat(sprintf("  Δd moyen: %.3f cm\n", metricas$inc_d_medio))
    cat(sprintf("  Δh moyen: %.3f m\n", metricas$inc_h_medio))
    cat(sprintf("  ΔV total: %.2f m³\n", metricas$inc_vol_total))
  }
  
  # Résumé comparatif
  cat("\nRÉSUMÉ COMPARATIF:\n")
  resumen <- bind_rows(resultados, .id = "Scenario")
  print(resumen)
  
  return(resumen)
}

# ==============================================================================
# 4. DIAGNOSTIC COMPLET
# ==============================================================================

ejecutar_diagnostico_completo <- function(arboles_df, config) {
  cat("\n")
  cat("══════════════════════════════════════════════════════════════\n")
  cat("         DIAGNOSTIC COMPLET DU MODÈLE DE CROISSANCE          \n")
  cat("══════════════════════════════════════════════════════════════\n")
  
  # 1. Validation structure
  validar_estructura_datos(arboles_df)
  
  # 2. Diagnostic paramètres
  params_teoricos <- diagnosticar_parametros_crecimiento(config)
  
  # 3. Test sur échantillon
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║              TEST SUR ÉCHANTILLON                         ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  # Prendre un échantillon
  muestra <- arboles_df %>%
    filter(!dominancia %in% c(7, 8, 9)) %>%
    sample_n(min(100, n()))
  
  cat(sprintf("Test avec %d arbres vivants...\n", nrow(muestra)))
  
  # Charger le module corrigé
  source("modelov5/02_modelos_crecimiento.R")
  
  # Appliquer croissance
  muestra_crecida <- aplicar_crecimiento_poblacion(muestra, config)
  
  # Valider
  validar_crecimiento(muestra, muestra_crecida)
  
  # 4. Analyse par genre et dominance
  cat("\nANALYSE DES RÉSULTATS PAR GENRE ET DOMINANCE:\n")
  reporte <- reporte_crecimiento(muestra_crecida)
  
  # 5. Détection d'anomalies
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║             DÉTECTION D'ANOMALIES                         ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  anomalias <- muestra_crecida %>%
    filter(
      incremento_d_cm > 1.0 |    # Croissance excessive
        incremento_d_cm < 0 |       # Croissance négative
        incremento_h_m > 0.8 |      # Hauteur excessive
        incremento_h_m < 0          # Hauteur négative
    )
  
  if (nrow(anomalias) > 0) {
    cat(sprintf("⚠ %d arbres avec valeurs anomales détectés\n", nrow(anomalias)))
    print(anomalias %>% 
            select(arbol_id, genero_grupo, dominancia, 
                   incremento_d_cm, incremento_h_m))
  } else {
    cat("✓ Aucune anomalie détectée\n")
  }
  
  # Résumé final
  cat("\n══════════════════════════════════════════════════════════════\n")
  cat("                    DIAGNOSTIC TERMINÉ                        \n")
  cat("══════════════════════════════════════════════════════════════\n\n")
  
  return(list(
    muestra_original = muestra,
    muestra_crecida = muestra_crecida,
    reporte = reporte,
    anomalias = anomalias
  ))
}

# ==============================================================================
# EXEMPLE D'UTILISATION
# ==============================================================================

# Pour exécuter le diagnostic complet:
# resultado_diagnostico <- ejecutar_diagnostico_completo(arboles_inicial, CONFIG)

cat("\n✓ Script de diagnostic chargé\n")
cat("  Pour exécuter:\n")
cat("    diagnostico <- ejecutar_diagnostico_completo(arboles_inicial, CONFIG)\n\n")