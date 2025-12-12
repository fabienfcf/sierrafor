# ==============================================================================
# WORKFLOW COMPLETO: ANÁLISIS E INVENTARIO FORESTAL
# PMF Las Alazanas 2026-2036
# Actualizado para estructura de configuración modular
# ==============================================================================

# Limpiar entorno
rm(list = ls())
gc()

# Configurar directorio
setwd("/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5")

PROYECTO_ROOT <- getwd()
cat(sprintf("\n✓ Directorio base: %s\n", PROYECTO_ROOT))

# Librerías
library(tidyverse)
library(readxl)
library(janitor)
library(xtable)
library(patchwork)

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║         WORKFLOW COMPLETO - PMF LAS ALAZANAS              ║\n")
cat("║              Sistema de Gestión Forestal v2.0             ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")

# ==============================================================================
# FASE 0: CARGAR CONFIGURACIÓN
# ==============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║                FASE 0: CARGAR CONFIGURACIÓN               ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")

# Esto carga automáticamente:
# - 01_config_especies.R
# - 02_config_codigos.R
# - 03_config_simulacion.R
# - 04_config_programa_cortas.R
# Y valida todo el sistema

source(file.path(PROYECTO_ROOT, "config/01_parametros_configuracion.R"))

cat("\n✓ Configuración global disponible en: CONFIG\n")
cat("✓ Parámetros de corta: Q_FACTOR, TOLERANCIA_EQUILIBRIO, DMC\n")
cat("✓ Especies, códigos y ecuaciones cargadas\n")

# ==============================================================================
# FASE 1: IMPORTACIÓN
# ==============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║                   FASE 1: IMPORTACIÓN                     ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")

source(file.path(PROYECTO_ROOT, "config/00_importar_inventario.R"))

# Importar inventario completo
inventario <- importar_inventario_completo(
  ruta_archivo = "inventario_forestal.xlsx",
  ruta_umm = "UMM_stats.csv"
)

cat("\n✓ Inventario importado:\n")
cat(sprintf("  • F01 (sitios):         %d registros\n", nrow(inventario$f01)))
cat(sprintf("  • F02 (regeneración):   %d árboles\n", nrow(inventario$f02)))
cat(sprintf("  • F03 (árboles):        %d árboles\n", nrow(inventario$f03)))
cat(sprintf("  • F04 (virutas):        %d registros\n", nrow(inventario$f04)))
cat(sprintf("  • F05 (regeneración):   %d registros\n", nrow(inventario$f05)))
cat(sprintf("  • F06 (combustibles):   %d sitios\n", nrow(inventario$f06)))

# ==============================================================================
# FASE 2: CONSTRUCCIÓN DEL DATASET
# ==============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║              FASE 2: CONSTRUCCIÓN DEL DATASET             ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")

source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))

# Construir dataset de árboles
arboles_analisis <- construir_arboles_analisis(inventario, CONFIG)

cat(sprintf("\n✓ Dataset construido:\n"))
cat(sprintf("  • Árboles totales:   %d\n", nrow(arboles_analisis)))
cat(sprintf("  • Árboles vivos:     %d\n", 
            sum(!arboles_analisis$dominancia %in% c(7, 8, 9))))
cat(sprintf("  • Rodales:           %d\n", n_distinct(arboles_analisis$rodal)))
cat(sprintf("  • Géneros:           %s\n", 
            paste(unique(arboles_analisis$genero_grupo), collapse = ", ")))

# Guardar datos intermedios
dir.create("datos_intermedios", showWarnings = FALSE)
saveRDS(arboles_analisis, "datos_intermedios/arboles_analisis.rds")
saveRDS(inventario, "datos_intermedios/inventario_completo.rds")

write.csv(arboles_analisis, file="arboles_analisis.csv")

cat("\n✓ Datos guardados en datos_intermedios/\n")

# ==============================================================================
# FASE 3: ANÁLISIS DESCRIPTIVO
# ==============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║              FASE 3: ANÁLISIS DESCRIPTIVO                 ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")

source(file.path(PROYECTO_ROOT, "analisis/20_analisis_descriptivo.R"))

# Ejecutar análisis completo
resultados_descriptivos <- analisis_descriptivo_completo(
  inventario = inventario,
  arboles_df = arboles_analisis,
  config = CONFIG
)

# Guardar resultados
dir.create("resultados", showWarnings = FALSE)
saveRDS(resultados_descriptivos, "resultados/analisis_descriptivo.rds")

cat("\n✓ Análisis descriptivo guardado en resultados/\n")

# ==============================================================================
# FASE 4: ANÁLISIS DE RIESGO DE INCENDIO
# ==============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║           FASE 4: ANÁLISIS DE RIESGO DE INCENDIO         ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")

source(file.path(PROYECTO_ROOT, "opcional/23_Main_incendio.R"))
 

# ==============================================================================
# FASE 5: SIMULACIÓN 10 AÑOS
# ==============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║              FASE 5: SIMULACIÓN 10 AÑOS                   ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")

# Cargar módulos de simulación
cat("\n[5.1] Cargando módulos de simulación...\n")
source(file.path(PROYECTO_ROOT, "core/10_modelos_crecimiento.R"))
source(file.path(PROYECTO_ROOT, "core/11_modelo_mortalidad.R"))
source(file.path(PROYECTO_ROOT, "core/12_modelo_reclutamiento.R"))
source(file.path(PROYECTO_ROOT, "core/13_simulador_crecimiento.R"))
source(file.path(PROYECTO_ROOT, "core/14_optimizador_cortas.R"))

cat("\n[5.2] Ejecutando simulación completa...\n")

# Ejecutar simulación con programa de cortas
source(file.path(PROYECTO_ROOT, "simulaciones/30_SIMULACION_10AÑOS_COMPLETA.R"))

cat("\n✓ Simulación 10 años completada\n")

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║                  ✓ WORKFLOW COMPLETADO                    ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("ESTRUCTURA DE ARCHIVOS GENERADA:\n")
cat("══════════════════════════════════════════════════════════\n\n")

cat("📁 datos_intermedios/\n")
cat("   ├── arboles_analisis.rds\n")
cat("   └── inventario_completo.rds\n\n")

cat("📁 resultados/\n")
cat("   ├── analisis_descriptivo.rds\n")
cat("   ├── historial_completo_10años.rds\n")
cat("   ├── metricas_10años.rds\n")
cat("   └── registro_cortas.rds\n\n")

cat("📁 tablas_latex/\n")
cat("   ├── desc_01_resumen_rodal.tex\n")
cat("   ├── desc_02_composicion_genero.tex\n")
cat("   ├── desc_03_top_especies.tex\n")
cat("   ├── desc_04_erosion.tex\n")
cat("   ├── desc_05_sanidad.tex\n")
cat("   ├── desc_06_regeneracion.tex\n")
cat("   ├── 01_inventario_inicial.tex\n")
cat("   ├── 02_comparacion_inicial_final.tex\n")
cat("   ├── 03_intensidad_corte_rodal.tex\n")
cat("   └── ...\n\n")

cat("📁 graficos/\n")
cat("   ├── desc_01_distribucion_diametrica.png\n")
cat("   ├── desc_02_erosion.png\n")
cat("   ├── desc_03_sanidad.png\n")
cat("   ├── desc_04_regeneracion.png\n")
cat("   ├── evolucion_10años_rodales.png\n")
cat("   └── ...\n\n")

cat("══════════════════════════════════════════════════════════\n")
cat("PARÁMETROS UTILIZADOS:\n")
cat("══════════════════════════════════════════════════════════\n\n")

cat(sprintf("Periodo simulación:      %d años\n", CONFIG$periodo))
cat(sprintf("Q-factor (Liocourt):     %.2f\n", CONFIG$q_factor))
cat(sprintf("Tolerancia equilibrio:   ±%d%%\n", CONFIG$tolerancia))
cat(sprintf("Mortalidad base:         %.2f%%\n", CONFIG$mortalidad_base * 100))
cat(sprintf("Reclutamiento:           %.1f%%\n", CONFIG$tasa_reclutamiento * 100))
cat(sprintf("Rodales con corta:       %d\n", nrow(CONFIG$programa_cortas)))

cat("\n══════════════════════════════════════════════════════════\n")
cat("Para incluir las tablas en tu PMF (LaTeX):\n")
cat("  \\input{tablas_latex/desc_01_resumen_rodal.tex}\n")
cat("  \\input{tablas_latex/01_inventario_inicial.tex}\n")
cat("  ... etc.\n\n")

cat("Para visualizar los gráficos:\n")
cat("  \\includegraphics{graficos/desc_01_distribucion_diametrica.png}\n\n")

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║          Sistema listo para generar PMF completo          ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")