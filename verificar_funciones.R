# ==============================================================================
# SCRIPT DE VÉRIFICATION DES FONCTIONS
# Identifie les conflits et signatures de fonctions
# ==============================================================================

cat("\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("         VÉRIFICATION DES FONCTIONS DU MODÈLE                 \n")
cat("══════════════════════════════════════════════════════════════\n\n")

# 1. Vérifier quelle version de calcular_volumen_arbol existe
cat("1. RECHERCHE DE calcular_volumen_arbol:\n")
if (exists("calcular_volumen_arbol")) {
  cat("  ✓ Fonction trouvée\n")
  
  # Obtenir les arguments
  args_func <- formals(calcular_volumen_arbol)
  cat("  Arguments:", paste(names(args_func), collapse = ", "), "\n")
  
  # Tester avec différentes signatures
  cat("\n2. TESTS DE COMPATIBILITÉ:\n")
  
  # Test 1: Signature core_calculos (d_cm, h_m, tipo, a, b, c)
  test1 <- tryCatch({
    calcular_volumen_arbol(d_cm = 30, h_m = 15, tipo = "potencia", 
                           a = 0.00004, b = 1.93694, c = 1.03169)
    cat("  ✓ Compatible avec signature core_calculos (d_cm, h_m, ...)\n")
    TRUE
  }, error = function(e) {
    cat("  ✗ Incompatible avec signature core_calculos\n")
    cat("    Erreur:", e$message, "\n")
    FALSE
  })
  
  # Test 2: Signature alternative (diametro, altura, tipo, a, b, c)
  test2 <- tryCatch({
    calcular_volumen_arbol(diametro = 30, altura = 15, tipo = "potencia",
                           a = 0.00004, b = 1.93694, c = 1.03169)
    cat("  ✓ Compatible avec signature alternative (diametro, altura, ...)\n")
    TRUE
  }, error = function(e) {
    cat("  ✗ Incompatible avec signature alternative\n")
    cat("    Erreur:", e$message, "\n")
    FALSE
  })
  
  # Test 3: Signature simple (d, h, tipo, a, b, c)
  test3 <- tryCatch({
    calcular_volumen_arbol(d = 30, h = 15, tipo = "potencia",
                           a = 0.00004, b = 1.93694, c = 1.03169)
    cat("  ✓ Compatible avec signature simple (d, h, ...)\n")
    TRUE
  }, error = function(e) {
    cat("  ✗ Incompatible avec signature simple\n")
    cat("    Erreur:", e$message, "\n")
    FALSE
  })
  
} else {
  cat("  ✗ Fonction NON trouvée - Charger core_calculos.R\n")
}

# 2. Vérifier autres fonctions clés
cat("\n3. VÉRIFICATION DES AUTRES FONCTIONS CLÉS:\n")

funciones_requeridas <- c(
  "filtrar_arboles_vivos",
  "filtrar_arboles_muertos", 
  "es_arbol_vivo",
  "calcular_area_basal",
  "calcular_incremento_diametro",
  "calcular_incremento_altura",
  "aplicar_crecimiento_anual"
)

for (func in funciones_requeridas) {
  if (exists(func)) {
    cat(sprintf("  ✓ %s existe\n", func))
  } else {
    cat(sprintf("  ✗ %s MANQUANTE\n", func))
  }
}

# 3. Vérifier les sources chargées
cat("\n4. FICHIERS SOURCES RECOMMANDÉS:\n")
archivos_necesarios <- c(
  "modelov5/core_calculos.R",
  "modelov5/01_parametros_configuracion.R"
)

for (archivo in archivos_necesarios) {
  if (file.exists(archivo)) {
    cat(sprintf("  ✓ %s trouvé\n", archivo))
  } else {
    cat(sprintf("  ✗ %s NON trouvé\n", archivo))
  }
}

# 4. Solution adaptative
cat("\n5. FONCTION ADAPTATIVE RECOMMANDÉE:\n")
cat("Créer une fonction wrapper qui s'adapte à la signature disponible:\n\n")

cat('calcular_volumen_arbol_wrapper <- function(d, h, tipo, a, b, c) {
  # Essayer différentes signatures
  resultado <- tryCatch({
    # Signature 1: core_calculos
    calcular_volumen_arbol(d_cm = d, h_m = h, tipo = tipo, a = a, b = b, c = c)
  }, error = function(e) {
    tryCatch({
      # Signature 2: alternative
      calcular_volumen_arbol(diametro = d, altura = h, tipo = tipo, a = a, b = b, c = c)
    }, error = function(e2) {
      tryCatch({
        # Signature 3: simple
        calcular_volumen_arbol(d = d, h = h, tipo = tipo, a = a, b = b, c = c)
      }, error = function(e3) {
        NA_real_  # Si aucune ne fonctionne
      })
    })
  })
  return(resultado)
}\n')

cat("\n══════════════════════════════════════════════════════════════\n")
cat("                    VÉRIFICATION TERMINÉE                      \n")
cat("══════════════════════════════════════════════════════════════\n\n")