# ==============================================================================
# SCRIPT: SIMULACIÓN DE CRECIMIENTO 6 AÑOS
# ==============================================================================
setwd("/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/")

# 1. CARGAR MÓDULOS (en orden correcto)
source("R5/core_calculos.R")
source("R5/01_parametros_configuracion.R")
source("R5/02_modelos_crecimiento.R")
source("R5/03_modelo_mortalidad.R")
source("R5/04_modelo_reclutamiento.R")
source("R5/06_simulador_crecimiento.R")

# 2. CARGAR DATOS
arboles_inicial <- readRDS("datos_intermedios/arboles_analisis.rds") %>%
  filter(genero_grupo %in% c("Pinus", "Quercus"))

cat("\n╔═══════════════════════════════════════════════════════════╗\n")
cat("║     SIMULACIÓN DE CRECIMIENTO - 6 AÑOS                   ║\n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

cat(sprintf("⚠ Árboles 'Otros' excluidos del análisis\n"))
cat(sprintf("  Población después de filtrado: %d árboles\n", nrow(arboles_inicial)))
cat(sprintf("Población inicial: %d árboles\n", nrow(arboles_inicial)))
cat(sprintf("Período: 6 años (2025-2030)\n\n"))

# 3. EJECUTAR SIMULACIÓN
resultado_6años <- simular_crecimiento_rodal(
  arboles_inicial = arboles_inicial,
  config = CONFIG,
  años = 6
)

# 4. EXTRAER POBLACIÓN FINAL
arboles_año6 <- resultado_6años$poblacion_final

# 5. GUARDAR RESULTADOS
saveRDS(arboles_año6, "datos_intermedios/arboles_año6.rds")
saveRDS(resultado_6años, "resultados/simulacion_6años_completa.rds")

cat("\n✓ Población año 6 guardada en: datos_intermedios/arboles_año6.rds\n")
cat("✓ Simulación completa guardada en: resultados/simulacion_6años_completa.rds\n\n")

# 6. ANÁLISIS DE RESULTADOS
comparacion <- comparar_estados(resultado_6años)
comparacion_genero <- comparar_estados_por_genero(resultado_6años)

# 7. VISUALIZACIÓN
library(patchwork)
grafico_evolucion <- graficar_evolucion_temporal(resultado_6años)
ggsave("graficos/evolucion_6años.png", grafico_evolucion, 
       width = 10, height = 12, dpi = 300)

cat("✓ Gráfico guardado en: graficos/evolucion_6años.png\n\n")

# 8. REPORTE DETALLADO POR RODAL
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("║           RESUMEN POR RODAL - AÑO 6                      ║\n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

resumen_rodales <- arboles_año6 %>%
  filter(!dominancia %in% c(7, 8, 9)) %>%
  group_by(rodal) %>%
  summarise(
    n_vivos = n(),
    d_medio = mean(diametro_normal, na.rm = TRUE),
    h_media = mean(altura_total, na.rm = TRUE),
    vol_total = sum(volumen_m3, na.rm = TRUE),
    ab_total = sum(area_basal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    arboles_año6 %>% 
      group_by(rodal) %>% 
      summarise(superficie_ha = first(superficie_ha), .groups = "drop"),
    by = "rodal"
  ) %>%
  mutate(
    vol_ha = vol_total / superficie_ha * (1/0.05),
    ab_ha = ab_total / superficie_ha * (1/0.05)
  )

print(resumen_rodales)

cat("\n✓ Simulación completada exitosamente\n\n")