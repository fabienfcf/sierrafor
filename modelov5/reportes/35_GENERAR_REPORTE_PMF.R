# ==============================================================================
# WORKFLOW: GENERAR REPORTE COMPLETO PMF
# ==============================================================================
#
# Este workflow ejecuta secuencialmente:
#   1. Cálculo de ICAs (31_calcular_ica.R)
#   2. Generación de tablas PMF (32_tablas_pmf.R)
#   3. Generación de gráficos PMF (33_graficos_pmf.R)
#
# Genera todos los insumos necesarios para el PMF:
#   - Tablas LaTeX listas para incluir
#   - Gráficos en alta resolución
#   - Datos intermedios en CSV/RDS
#
# ==============================================================================

# Limpiar entorno
rm(list = ls())

# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)
gc()

# Configurar directorio
setwd("/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5")

# Librerías
library(tidyverse)
library(xtable)
library(patchwork)

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║         WORKFLOW: GENERACIÓN REPORTE PMF COMPLETO         ║\n")
cat("║              Tablas + Gráficos + Cálculos ICA            ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# FASE 0: CARGAR MÓDULOS
# ==============================================================================

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║                FASE 0: CARGAR MÓDULOS                     ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

source(file.path(PROYECTO_ROOT, "config/01_parametros_configuracion.R"))
source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
source(file.path(PROYECTO_ROOT, "core/10_modelos_crecimiento.R"))
source(file.path(PROYECTO_ROOT, "core/11_modelo_mortalidad.R"))
source(file.path(PROYECTO_ROOT, "core/12_modelo_reclutamiento.R"))
source(file.path(PROYECTO_ROOT, "core/13_simulador_crecimiento.R"))
source(file.path(PROYECTO_ROOT, "core/16_calcular_ica.R"))
source(file.path(PROYECTO_ROOT, "generadores/32_tablas_pmf.R"))
source(file.path(PROYECTO_ROOT, "generadores/33_graficos_pmf.R"))

cat("✓ Todos los módulos cargados\n\n")

# ==============================================================================
# FASE 1: CARGAR DATOS
# ==============================================================================

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║                 FASE 1: CARGAR DATOS                      ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# Verificar que existen los datos
if (!file.exists("datos_intermedios/arboles_analisis.rds")) {
  cat("❌ ERROR: No se encontró arboles_analisis.rds\n")
  cat("   Ejecuta primero: source(file.path(PROYECTO_ROOT, 'workflows/40_WORKFLOW_COMPLETO.R'))\n\n")
  stop("Datos no encontrados")
}

arboles_inicial <- readRDS("datos_intermedios/arboles_analisis.rds") %>%
  filter(genero_grupo %in% c("Pinus", "Quercus"))

cat(sprintf("✓ Población cargada: %d árboles\n", nrow(arboles_inicial)))
cat(sprintf("  • Pinus:   %d árboles\n", 
            sum(arboles_inicial$genero_grupo == "Pinus")))
cat(sprintf("  • Quercus: %d árboles\n", 
            sum(arboles_inicial$genero_grupo == "Quercus")))
cat(sprintf("  • Rodales: %d\n\n", n_distinct(arboles_inicial$rodal)))

# ==============================================================================
# FASE 2: CALCULAR ICAs (SI NO EXISTEN)
# ==============================================================================

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║              FASE 2: CALCULAR ICAs                        ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

if (!file.exists("resultados/31_ica_por_rodal.csv")) {
  cat("ℹ️ ICAs no encontrados. Calculando...\n\n")
  
  resultado_ica <- calcular_ica_sin_cortes(
    arboles_inicial = arboles_inicial,
    config = CONFIG,
    años = 10
  )
  
  exportar_tablas_latex_ica(resultado_ica, directorio = "tablas_latex")
  guardar_resultados_ica(resultado_ica, directorio = "resultados")
  
} else {
  cat("✓ ICAs ya calculados. Cargando desde archivo...\n\n")
  resultado_ica <- readRDS("resultados/31_resultados_ica.rds")
}

# Extraer datos necesarios
ica_por_rodal <- resultado_ica$ica_por_rodal
arboles_final <- resultado_ica$poblacion_final

cat("✓ ICAs disponibles\n\n")

# ==============================================================================
# FASE 3: GENERAR TODAS LAS TABLAS PMF
# ==============================================================================

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║            FASE 3: GENERAR TABLAS PMF                     ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

todas_tablas <- exportar_todas_tablas_pmf(
  arboles_inicial = arboles_inicial,
  arboles_final = arboles_final,
  ica_calculado = ica_por_rodal,
  vol_aprovechamiento = NULL,  # Agregar si hay datos de cortas
  años = 10,
  directorio = "tablas_latex"
)

cat("✓ Tablas PMF generadas\n\n")

# ==============================================================================
# FASE 4: GENERAR TODOS LOS GRÁFICOS
# ==============================================================================

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║           FASE 4: GENERAR GRÁFICOS PMF                    ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

todos_graficos <- generar_todos_graficos_pmf(
  arboles_df = arboles_inicial,
  config = CONFIG,
  directorio = "graficos"
)

cat("✓ Gráficos PMF generados\n\n")

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║          ✓ REPORTE PMF GENERADO EXITOSAMENTE              ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("ARCHIVOS GENERADOS:\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("📁 tablas_latex/\n")
cat("   SECCIÓN 7.1 - Existencias:\n")
cat("   ├── 31_ica_por_rodal.tex\n")
cat("   ├── 31_ica_por_genero_rodal.tex\n")
cat("   ├── 31_ica_por_especie_rodal.tex\n")
cat("   ├── 31_resumen_predio.tex\n")
cat("   ├── 32_tabla5_existencias.tex\n")
cat("   └── 32_tabla6_categorias.tex\n\n")

cat("   SECCIÓN 7.2 - Ecuaciones:\n")
cat("   └── 32_tabla72_ecuaciones.tex\n\n")

cat("   SECCIÓN 7.3 - Incrementos:\n")
cat("   └── 32_tabla7_incrementos.tex\n\n")

cat("📁 graficos/\n")
cat("   ├── 33_01_frecuencia_especies.png\n")
cat("   ├── 33_02-04_frecuencia_ab_*.png (Pinus, Quercus, Total)\n")
cat("   ├── 33_05-07_frecuencia_vol_*.png (Pinus, Quercus, Total)\n")
cat("   ├── 33_08-10_dist_diametro_*.png (Pinus, Quercus, Total)\n")
cat("   └── 33_11-13_dist_altura_*.png (Pinus, Quercus, Total)\n\n")

cat("📁 resultados/\n")
cat("   ├── 31_resultados_ica.rds\n")
cat("   ├── 31_ica_por_rodal.csv\n")
cat("   ├── 31_ica_por_genero_rodal.csv\n")
cat("   └── 31_ica_por_especie_rodal.csv\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("USO EN LATEX (PMF):\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("\\section{Existencias y Aprovechamiento}\n")
cat("\\input{tablas_latex/32_tabla5_existencias.tex}\n\n")

cat("\\section{Distribución Diamétrica}\n")
cat("\\input{tablas_latex/32_tabla6_categorias.tex}\n\n")

cat("\\section{Ecuaciones Alométricas}\n")
cat("\\input{tablas_latex/32_tabla72_ecuaciones.tex}\n\n")

cat("\\section{Densidades e Incrementos}\n")
cat("\\input{tablas_latex/32_tabla7_incrementos.tex}\n\n")

cat("\\section{Cálculo de Posibilidad (ICA)}\n")
cat("\\input{tablas_latex/31_ica_por_rodal.tex}\n")
cat("\\input{tablas_latex/31_resumen_predio.tex}\n\n")

cat("\\section{Análisis Gráfico}\n")
cat("\\includegraphics[width=0.8\\textwidth]{graficos/33_01_frecuencia_especies.png}\n")
cat("\\includegraphics[width=0.8\\textwidth]{graficos/33_08_dist_diametro_pinus.png}\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("PRÓXIMOS PASOS:\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("1. Revisar tablas generadas en tablas_latex/\n")
cat("2. Revisar gráficos generados en graficos/\n")
cat("3. Integrar en documento LaTeX del PMF\n")
cat("4. Ejecutar simulación con cortas (opcional):\n")
cat("   source(file.path(PROYECTO_ROOT, 'simulaciones/30_SIMULACION_10AÑOS_COMPLETA.R'))\n\n")

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║              Sistema PMF listo para uso                   ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")