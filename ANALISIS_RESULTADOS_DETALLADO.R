# ==============================================================================
# ANÁLISIS DETALLADO DE RESULTADOS - SIMULACIÓN 10 AÑOS
# ==============================================================================

setwd("/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5")

library(tidyverse)
library(patchwork)
library(gridExtra)

# Cargar CONFIG si no está cargado
if (!exists("CONFIG")) {
  source("modelov5/core_calculos.R")
  source("modelov5/01_parametros_configuracion.R")
}

cat("\n")
cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║       ANÁLISIS DETALLADO DE RESULTADOS                    ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# CARGAR DATOS
# ==============================================================================

cat("[1] Cargando datos de simulación...\n")

df_historial <- readRDS("resultados/historial_completo_10años.rds")
df_metricas <- readRDS("resultados/metricas_10años.rds")
df_cortas <- readRDS("resultados/registro_cortas.rds")

cat(sprintf("  ✓ Historial: %d registros\n", nrow(df_historial)))
cat(sprintf("  ✓ Métricas: %d registros\n", nrow(df_metricas)))
cat(sprintf("  ✓ Cortas: %d árboles\n", nrow(df_cortas)))

# ==============================================================================
# ANÁLISIS 1: EVOLUCIÓN POR GÉNERO
# ==============================================================================

cat("\n[2] Analizando evolución por género...\n")

evolucion_genero <- df_metricas %>%
  group_by(año_simulacion, genero_grupo) %>%
  summarise(
    vol_total_ha = sum(vol_ha_m3, na.rm = TRUE),
    densidad_total_ha = sum(densidad_ha, na.rm = TRUE),
    n_rodales = n_distinct(rodal),
    .groups = "drop"
  )

# Gráfico evolución por género
p_genero <- ggplot(evolucion_genero, 
                   aes(x = año_simulacion, y = vol_total_ha, 
                       color = genero_grupo, group = genero_grupo)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Evolución del volumen por género",
    x = "Año",
    y = "Volumen total (m³/ha)",
    color = "Género"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("graficos/evolucion_genero.png", p_genero, 
       width = 10, height = 6, dpi = 300)

cat("  ✓ Gráfico guardado: graficos/evolucion_genero.png\n")

# ==============================================================================
# ANÁLISIS 2: IMPACTO DE LAS CORTAS
# ==============================================================================

cat("\n[3] Analizando impacto de cortas...\n")

if (nrow(df_cortas) > 0) {
  
  # Métricas antes y después de cada corta
  impacto_cortas <- df_cortas %>%
    group_by(rodal_cortado, año_corta) %>%
    summarise(
      n_arboles_cortados = n(),
      vol_cortado_m3 = sum(volumen_m3, na.rm = TRUE),
      ab_cortada_m2 = sum(area_basal, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      df_metricas %>%
        filter(año_simulacion %in% c(0:10)) %>%
        group_by(rodal, año_simulacion) %>%
        summarise(
          vol_ha_antes = sum(vol_ha_m3, na.rm = TRUE),
          .groups = "drop"
        ),
      by = c("rodal_cortado" = "rodal", "año_corta" = "año_simulacion")
    ) %>%
    mutate(
      porcentaje_cortado = (vol_cortado_m3 / vol_ha_antes) * 100
    )
  
  cat("\n  Resumen de impacto:\n")
  print(impacto_cortas %>% select(rodal_cortado, año_corta, n_arboles_cortados, 
                                  vol_cortado_m3, porcentaje_cortado))
  
  # Gráfico de barras - volumen cortado por rodal
  p_cortas <- ggplot(impacto_cortas, 
                     aes(x = factor(rodal_cortado), y = vol_cortado_m3, 
                         fill = factor(año_corta))) +
    geom_col(position = "dodge") +
    labs(
      title = "Volumen cortado por rodal y año",
      x = "Rodal",
      y = "Volumen cortado (m³)",
      fill = "Año de corta"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave("graficos/volumen_cortado_rodal.png", p_cortas, 
         width = 10, height = 6, dpi = 300)
  
  cat("\n  ✓ Gráfico guardado: graficos/volumen_cortado_rodal.png\n")
  
} else {
  cat("  ⚠ No hay datos de cortas para analizar\n")
}

# ==============================================================================
# ANÁLISIS 3: DISTRIBUCIÓN DIAMÉTRICA FINAL
# ==============================================================================

cat("\n[4] Analizando distribución diamétrica final...\n")

# Estado final (año 10)
arboles_finales <- df_historial %>%
  filter(año_simulacion == 10, !dominancia %in% c(7, 8, 9))

dist_diametrica <- arboles_finales %>%
  mutate(clase_d = asignar_clase_diametrica(diametro_normal, formato = "centro")) %>%
  group_by(rodal, genero_grupo, clase_d) %>%
  summarise(
    n_arboles = n(),
    .groups = "drop"
  )

# Gráfico de distribución diamétrica por género (todos los rodales)
p_dist <- ggplot(dist_diametrica, 
                 aes(x = clase_d, y = n_arboles, fill = genero_grupo)) +
  geom_col(position = "dodge") +
  facet_wrap(~rodal, scales = "free_y") +
  labs(
    title = "Distribución diamétrica final (año 10) por rodal",
    x = "Clase diamétrica (cm)",
    y = "Número de árboles",
    fill = "Género"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("graficos/distribucion_diametrica_final.png", p_dist, 
       width = 14, height = 10, dpi = 300)

cat("  ✓ Gráfico guardado: graficos/distribucion_diametrica_final.png\n")

# ==============================================================================
# ANÁLISIS 4: TASA DE CRECIMIENTO Y MORTALIDAD
# ==============================================================================

cat("\n[5] Calculando tasas de crecimiento y mortalidad...\n")

# Calcular incrementos anuales
incrementos_anuales <- df_metricas %>%
  arrange(rodal, genero_grupo, año_simulacion) %>%
  group_by(rodal, genero_grupo) %>%
  mutate(
    incremento_vol = vol_ha_m3 - lag(vol_ha_m3),
    incremento_dens = densidad_ha - lag(densidad_ha),
    tasa_incremento_vol = (incremento_vol / lag(vol_ha_m3)) * 100
  ) %>%
  filter(!is.na(incremento_vol))

resumen_incrementos <- incrementos_anuales %>%
  group_by(genero_grupo) %>%
  summarise(
    incremento_vol_medio = mean(incremento_vol, na.rm = TRUE),
    incremento_vol_sd = sd(incremento_vol, na.rm = TRUE),
    tasa_media = mean(tasa_incremento_vol, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n  Tasas de crecimiento promedio:\n")
print(resumen_incrementos)

# Gráfico de tasas de incremento
p_tasas <- ggplot(incrementos_anuales, 
                  aes(x = año_simulacion, y = tasa_incremento_vol, 
                      color = genero_grupo)) +
  geom_line(alpha = 0.5, aes(group = interaction(rodal, genero_grupo))) +
  geom_smooth(method = "loess", se = TRUE, linewidth = 1.5) +
  labs(
    title = "Tasa de incremento volumétrico",
    subtitle = "Líneas individuales por rodal, curva suavizada por género",
    x = "Año",
    y = "Tasa de incremento (%)",
    color = "Género"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("graficos/tasas_crecimiento.png", p_tasas, 
       width = 10, height = 6, dpi = 300)

cat("  ✓ Gráfico guardado: graficos/tasas_crecimiento.png\n")

# ==============================================================================
# ANÁLISIS 5: COMPARACIÓN INICIAL VS FINAL
# ==============================================================================

cat("\n[6] Generando comparación inicial vs final...\n")

comparacion <- df_metricas %>%
  filter(año_simulacion %in% c(0, 10)) %>%
  select(rodal, genero_grupo, año_simulacion, vol_ha_m3, densidad_ha) %>%
  pivot_wider(
    names_from = año_simulacion,
    values_from = c(vol_ha_m3, densidad_ha),
    names_prefix = "año_"
  ) %>%
  mutate(
    cambio_vol = vol_ha_m3_año_10 - vol_ha_m3_año_0,
    cambio_pct = (cambio_vol / vol_ha_m3_año_0) * 100,
    cambio_dens = densidad_ha_año_10 - densidad_ha_año_0
  )

cat("\n  Cambios netos (año 0 → año 10):\n")
print(comparacion %>% 
        select(rodal, genero_grupo, vol_ha_m3_año_0, vol_ha_m3_año_10, 
               cambio_vol, cambio_pct))

# Exportar comparación a CSV
write_csv(comparacion, "resultados/comparacion_inicial_final_detallada.csv")
cat("\n  ✓ Exportado: resultados/comparacion_inicial_final_detallada.csv\n")

# ==============================================================================
# RESUMEN EJECUTIVO
# ==============================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║              RESUMEN EJECUTIVO                            ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# Población total
pob_inicial <- df_historial %>% 
  filter(año_simulacion == 0, !dominancia %in% c(7, 8, 9)) %>% 
  nrow()

pob_final <- df_historial %>% 
  filter(año_simulacion == 10, !dominancia %in% c(7, 8, 9)) %>% 
  nrow()

# Volumen total
vol_inicial <- df_metricas %>% 
  filter(año_simulacion == 0) %>% 
  summarise(vol = sum(vol_ha_m3, na.rm = TRUE)) %>% 
  pull(vol)

vol_final <- df_metricas %>% 
  filter(año_simulacion == 10) %>% 
  summarise(vol = sum(vol_ha_m3, na.rm = TRUE)) %>% 
  pull(vol)

cat("POBLACIÓN:\n")
cat(sprintf("  Año 0:  %d árboles\n", pob_inicial))
cat(sprintf("  Año 10: %d árboles\n", pob_final))
cat(sprintf("  Cambio: %+d árboles (%+.1f%%)\n\n", 
            pob_final - pob_inicial,
            ((pob_final - pob_inicial) / pob_inicial) * 100))

cat("VOLUMEN (m³/ha):\n")
cat(sprintf("  Año 0:  %.2f m³/ha\n", vol_inicial))
cat(sprintf("  Año 10: %.2f m³/ha\n", vol_final))
cat(sprintf("  Cambio: %+.2f m³/ha (%+.1f%%)\n\n", 
            vol_final - vol_inicial,
            ((vol_final - vol_inicial) / vol_inicial) * 100))

if (nrow(df_cortas) > 0) {
  cat("CORTAS:\n")
  cat(sprintf("  Árboles cortados:  %d\n", nrow(df_cortas)))
  cat(sprintf("  Volumen extraído:  %.2f m³\n", 
              sum(df_cortas$volumen_m3, na.rm = TRUE)))
  cat(sprintf("  Rodales cortados:  %d\n", 
              n_distinct(df_cortas$rodal_cortado)))
  cat("\n")
}

cat("ARCHIVOS GENERADOS:\n")
cat("  Gráficos:\n")
cat("    - graficos/evolucion_genero.png\n")
cat("    - graficos/volumen_cortado_rodal.png\n")
cat("    - graficos/distribucion_diametrica_final.png\n")
cat("    - graficos/tasas_crecimiento.png\n\n")
cat("  Datos:\n")
cat("    - resultados/comparacion_inicial_final_detallada.csv\n\n")

cat("✓ Análisis completado\n\n")