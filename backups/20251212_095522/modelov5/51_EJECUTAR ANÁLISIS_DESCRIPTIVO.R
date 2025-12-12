# ==============================================================================
# SCRIPT EJECUTAR ANÁLISIS DESCRIPTIVO CON CSV
# ==============================================================================

library(tidyverse)
library(xtable)
library(patchwork)
library(viridis)

# ==============================================================================
# 1. CARGAR CONFIG Y FUNCIONES
# ==============================================================================

cat("\n[1/4] Cargando configuración y funciones...\n")

source("modelov5/01_parametros_configuracion.R")
source("modelov5/15_core_calculos.R")

# Cargar el módulo actualizado (versión con CSV y AB/ha)
source("modelov5/20_analisis_descriptivo.R")

cat("✓ Módulos cargados\n")


# ==============================================================================
# 2. CARGAR DATOS
# ==============================================================================

cat("\n[2/4] Cargando datos...\n")

# Verificar si existen datos procesados
if (file.exists("datos_intermedios/arboles_analisis.rds") && 
    file.exists("datos_intermedios/inventario_completo.rds")) {
  
  # OPCIÓN A: Cargar datos ya procesados
  arboles_analisis <- readRDS("datos_intermedios/arboles_analisis.rds")
  inventario <- readRDS("datos_intermedios/inventario_completo.rds")
  
  cat("✓ Datos cargados desde datos_intermedios/\n")
  
} else {
  
  # OPCIÓN B: Importar y procesar desde Excel
  cat("  No se encontraron datos procesados. Importando desde Excel...\n")
  
  source("modelov5/00_importar_inventario.R")
  
  inventario <- importar_inventario_completo(
    ruta_archivo = "inventario_forestal.xlsx",
    ruta_umm = "UMM_stats.csv"
  )
  
  arboles_analisis <- construir_arboles_analisis(inventario, CONFIG)
  
  # Guardar para próximas ejecuciones
  dir.create("datos_intermedios", showWarnings = FALSE)
  saveRDS(arboles_analisis, "datos_intermedios/arboles_analisis.rds")
  saveRDS(inventario, "datos_intermedios/inventario_completo.rds")
  
  cat("✓ Datos importados y guardados en datos_intermedios/\n")
}

cat(sprintf("\n  Árboles: %d\n", nrow(arboles_analisis)))
cat(sprintf("  Sitios:  %d\n", nrow(inventario$f01)))

# ==============================================================================
# 3. EJECUTAR ANÁLISIS DESCRIPTIVO COMPLETO
# ==============================================================================

cat("\n[3/4] Ejecutando análisis descriptivo...\n\n")

# ESTA ES LA FUNCIÓN QUE EXPORTA TODO
resultados_descriptivos <- analisis_descriptivo_completo(
  inventario = inventario,
  arboles_df = arboles_analisis,
  config = CONFIG
  # exportar_csv_flag = TRUE por defecto
)

# ==============================================================================
# 4. GUARDAR RESULTADOS
# ==============================================================================

cat("\n[4/4] Guardando resultados consolidados...\n")

# Guardar objeto R con todos los resultados
saveRDS(resultados_descriptivos, "resultados/analisis_descriptivo.rds")

cat("\n✓ Resultados guardados en resultados/analisis_descriptivo.rds\n")

# ==============================================================================
# RESUMEN
# ==============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║                  ✓ ANÁLISIS COMPLETADO                    ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("ARCHIVOS GENERADOS:\n")
cat("══════════════════════════════════════════════════════════\n\n")

cat("📂 resultados/ (CSV)\n")
cat("   ├── desc_01_resumen_general.csv\n")
cat("   ├── desc_02_por_rodal.csv\n")
cat("   ├── desc_03_por_genero.csv\n")
cat("   ├── desc_04_top10_especies.csv\n")
cat("   ├── desc_05_distribucion_diametrica.csv\n")
cat("   ├── desc_06_erosion.csv\n")
cat("   ├── desc_07_sanidad_detalle.csv\n")
cat("   ├── desc_08_sanidad_resumen.csv\n")
cat("   ├── desc_09_regeneracion_completa.csv\n")
cat("   ├── desc_10_regeneracion_resumen.csv\n")
cat("   ├── desc_11_composicion_generopq_por_rodal.csv ⭐ NUEVO\n")
cat("   └── analisis_descriptivo.rds\n\n")

cat("📂 tablas_latex/\n")
cat("   ├── desc_01_resumen_rodal.tex\n")
cat("   ├── desc_02_composicion_genero.tex\n")
cat("   ├── desc_03_top_especies.tex\n")
cat("   ├── desc_04_erosion.tex\n")
cat("   ├── desc_05_sanidad.tex\n")
cat("   ├── desc_06_regeneracion.tex\n")
cat("   └── desc_11_composicion_generopq_rodal.tex ⭐ NUEVO\n\n")

cat("📂 graficos/\n")
cat("   ├── desc_01_distribucion_diametrica.png\n")
cat("   ├── desc_02_erosion.png\n")
cat("   ├── desc_03_sanidad.png\n")
cat("   ├── desc_04_regeneracion.png\n")
cat("   └── desc_11_composicion_generopq_rodal.png ⭐ NUEVO\n\n")

cat("══════════════════════════════════════════════════════════\n\n")

# Verificar que realmente se crearon los archivos
archivos_csv <- list.files("resultados", pattern = "^desc_.*\\.csv$", full.names = TRUE)
cat(sprintf("✓ %d archivos CSV generados\n", length(archivos_csv)))

if (length(archivos_csv) > 0) {
  cat("\nPrimeros archivos CSV:\n")
  for (archivo in head(archivos_csv, 3)) {
    info <- file.info(archivo)
    cat(sprintf("  %s (%.1f KB)\n", basename(archivo), info$size / 1024))
  }
} else {
  cat("\n⚠️ No se generaron archivos CSV. Revisa los mensajes de error arriba.\n")
}

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║              Para revisar resultados:                     ║\n")
cat("║  res <- readRDS('resultados/analisis_descriptivo.rds')   ║\n")
cat("║  View(res$estructura$por_rodal)                          ║\n")
cat("║  View(res$composicion_generopq_rodal$tabla)  ⭐ NUEVO          ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")