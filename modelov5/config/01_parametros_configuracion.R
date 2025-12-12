# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# ==============================================================================
# 01_PARAMETROS_CONFIGURACION.R
# Carga centralizada de toda la configuración del sistema
# VERSIÓN ACTUALIZADA: Método ICA-Liocourt (sin G_OBJETIVO)
# ==============================================================================

library(tidyverse)

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║       SISTEMA DE GESTIÓN FORESTAL DINÁMICA v2.0           ║\n")
cat("║         Cargando configuración modular...                 ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")

# ==============================================================================
# CARGAR MÓDULOS DE CONFIGURACIÓN (EN ORDEN)
# ==============================================================================

source(file.path(PROYECTO_ROOT, "config/02_config_especies.R"))
source(file.path(PROYECTO_ROOT, "config/03_config_codigos.R"))
source(file.path(PROYECTO_ROOT, "config/04_config_simulacion.R"))
source(file.path(PROYECTO_ROOT, "config/05_config_programa_cortas.R"))

# ==============================================================================
# CREAR CONFIGURACIÓN GLOBAL
# ==============================================================================

cat("\n[5/5] Integrando configuración global...\n")

CONFIG <- crear_configuracion_simulacion()

# Agregar parámetros de corta al CONFIG
CONFIG$dmc <- DMC
CONFIG$programa_cortas <- PROGRAMA_CORTAS
CONFIG$q_factor <- Q_FACTOR
CONFIG$tolerancia <- TOLERANCIA_EQUILIBRIO
# NOTA: G_OBJETIVO fue eliminado en la refactorización ICA-Liocourt

# ==============================================================================
# VALIDAR TODO EL SISTEMA
# ==============================================================================

validar_configuracion(CONFIG)
validar_programa_cortas(PROGRAMA_CORTAS, CONFIG)

# ==============================================================================
# MENSAJE FINAL
# ==============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║           ✓ SISTEMA LISTO PARA SIMULACIÓN                 ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("CONFIGURACIÓN DISPONIBLE:\n")
cat("══════════════════════════════════════════════════════════\n\n")

cat("📦 Objeto principal:\n")
cat("   CONFIG    - Lista maestra con toda la configuración\n\n")

cat("🌳 Especies y modelos:\n")
cat("   ESPECIES                    - Catálogo completo\n")
cat("   ECUACIONES_VOLUMEN          - Alometrías disponibles\n")
cat("   PARAMETROS_ALTURA_DIAMETRO  - Chapman-Richards\n")
cat("   CRECIMIENTO_DIAMETRICO      - Tasas por género\n\n")

cat("🔢 Códigos SIPLAFOR:\n")
cat("   CODIGOS_DOMINANCIA          - Con factores crecimiento/mortalidad\n")
cat("   CODIGOS_EROSION             - Clasificación por nivel\n")
cat("   CODIGOS_SANIDAD             - Problemas fitosanitarios\n")
cat("   ... y 15 tablas más\n\n")

cat("⚙️  Parámetros simulación:\n")
cat(sprintf("   CONFIG$periodo              - %d años\n", CONFIG$periodo))
cat(sprintf("   CONFIG$mortalidad_base      - %.2f%%\n", CONFIG$mortalidad_base * 100))
cat(sprintf("   CONFIG$tasa_reclutamiento   - %.1f%%\n\n", CONFIG$tasa_reclutamiento * 100))

cat("🪓 Programa de cortas:\n")
cat("   DMC                         - Diámetros mínimos por género\n")
cat("   PROGRAMA_CORTAS             - Calendario de intervenciones\n")
cat(sprintf("   Q_FACTOR                    - %.1f (guía de estructura)\n", Q_FACTOR))
cat(sprintf("   TOLERANCIA_EQUILIBRIO       - ±%d%%\n\n", TOLERANCIA_EQUILIBRIO))

cat("🔧 Funciones helper:\n")
cat("   obtener_ecuacion_volumen(especie)\n")
cat("   obtener_parametros_altura(especie, dominancia)\n")
cat("   obtener_tasa_crecimiento(genero)\n")
cat("   traducir_codigo(codigo, tipo)\n")
cat("   configurar_corte(...)\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("CAMBIO METODOLÓGICO IMPLEMENTADO:\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("  ✓ G_OBJETIVO eliminado (era arbitrario)\n")
cat("  ✓ Q-factor define solo la FORMA de distribución (ratio relativo)\n")
cat("  ✓ Volumen objetivo basado en ICA (crecimiento real)\n")
cat("  ✓ Método Liocourt como GUÍA de dónde cortar\n")
cat("  ✓ Sostenibilidad garantizada: nunca cortar más del crecimiento\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("Para comenzar la simulación, ejecuta:\n")
cat("  source(file.path(PROYECTO_ROOT, 'workflows/40_WORKFLOW_COMPLETO.R'))\n\n")