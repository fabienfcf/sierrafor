# ==============================================================================
# SIMULACIÓN 10 AÑOS CON CORTAS PROGRAMADAS + EXPORTACIÓN LATEX
# ==============================================================================

setwd("/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5")

# Verificar/crear directorios
dirs <- c("datos_intermedios", "resultados", "graficos", 
          "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5/tablas_latex")
for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat(sprintf("✓ Creado: %s/\n", dir))
  }
}

# Cargar módulos
cat("\n[1/6] Cargando módulos...\n")
source("modelov5/core_calculos.R")
source("modelov5/01_parametros_configuracion.R")
source("modelov5/02_modelos_crecimiento.R")
source("modelov5/03_modelo_mortalidad.R")
source("modelov5/04_modelo_reclutamiento.R")
source("modelov5/06_simulador_crecimiento.R")
source("modelov5/07_optimizador_cortas.R")

library(xtable)
library(gridExtra)

# ==============================================================================
# CARGAR DATOS INICIALES
# ==============================================================================

cat("\n[2/6] Cargando datos iniciales...\n")
arboles_inicial <- readRDS("datos_intermedios/arboles_analisis.rds") %>%
  filter(genero_grupo %in% c("Pinus", "Quercus"))

cat(sprintf("  Población inicial: %d árboles\n", nrow(arboles_inicial)))
cat(sprintf("  Rodales: %s\n", paste(unique(arboles_inicial$rodal), collapse = ", ")))

# ==============================================================================
# SIMULACIÓN 10 AÑOS CON CORTAS PROGRAMADAS
# ==============================================================================

cat("\n[3/6] Simulando 10 años con cortas programadas...\n\n")

# Inicializar
arboles_actual <- arboles_inicial
historial_completo <- list()
historial_metricas <- list()
registro_cortas <- list()

# Estado inicial (año 0)
historial_completo[[1]] <- arboles_actual %>% mutate(año_simulacion = 0)
historial_metricas[[1]] <- calcular_metricas_estado(arboles_actual, CONFIG) %>%
  mutate(año_simulacion = 0)

# Simulación año por año
# Simulación año por año
for (año in 1:PERIODO_SIMULACION) {
  
  cat(sprintf("═══ AÑO %d ═══\n", año))
  
  # 1. CRECIMIENTO
  cat(sprintf("\n[AÑO %d] Crecimiento...\n", año))
  arboles_actual <- aplicar_crecimiento_poblacion(arboles_actual, CONFIG, año)
  arboles_actual <- actualizar_volumenes(arboles_actual)
  
  # 2. MORTALIDAD
  cat(sprintf("\n[AÑO %d] Mortalidad...\n", año))
  arboles_actual <- aplicar_mortalidad_poblacion(arboles_actual, CONFIG, año)
  
  # 3. RECLUTAMIENTO
  cat(sprintf("\n[AÑO %d] Reclutamiento...\n", año))
  arboles_actual <- aplicar_reclutamiento(arboles_actual, CONFIG, año)

  # 4. CORTAS según PROGRAMA_CORTAS
  rodales_cortar <- PROGRAMA_CORTAS %>%
    filter(año_corta == año) %>%
    pull(rodal)
  
  if (length(rodales_cortar) > 0) {
    
    cat(sprintf("\n[AÑO %d] 🪓 CORTAS PROGRAMADAS\n", año))
    cat(sprintf("  Rodales: %s\n\n", paste(rodales_cortar, collapse = ", ")))
    
    for (rodal_id in rodales_cortar) {
      
      # Obtener configuración del rodal
      config_rodal <- PROGRAMA_CORTAS %>% 
        filter(rodal == rodal_id, año_corta == año)
      
      # Crear configuración de corte
      corte_config <- configurar_corte(
        metodo = config_rodal$metodo,
        intensidad_pct = config_rodal$intensidad_pct,
        años_ica = PERIODO_SIMULACION,
        d_min = config_rodal$d_min,
        d_max = config_rodal$d_max,
        prioridad = config_rodal$prioridad,
        excluir_semilleros = config_rodal$excluir_semilleros
        )
      
      # Filtrar árboles del rodal
      arboles_rodal <- arboles_actual %>% filter(rodal == rodal_id)
      arboles_rodal_inicial <- arboles_inicial %>% filter(rodal == rodal_id)
      
      cat(sprintf("  ──── Rodal %d ────\n", rodal_id))
      cat(sprintf("  Método: %s\n", config_rodal$metodo))
      cat(sprintf("  Árboles actuales: %d\n", nrow(arboles_rodal)))
      
      # Calcular plan de cortas
      plan_cortas <- tryCatch({
        calcular_plan_cortas(
          arboles_rodal,
          CONFIG,
          arboles_rodal_inicial,
          corte_config,
          año_actual = año  # ← AGREGAR esta línea
        )
      }, error = function(e) {
        cat(sprintf("  ❌ Error: %s\n", e$message))
        list(arboles_marcados = tibble(), resumen = tibble())
      })
      
      # Registrar corta
      if (nrow(plan_cortas$arboles_marcados) > 0) {
        registro_cortas[[length(registro_cortas) + 1]] <- 
          plan_cortas$arboles_marcados %>%
          mutate(
            año_corta = año,
            rodal_cortado = rodal_id,
            metodo = config_rodal$metodo
          )
      }
      
      # Aplicar corta
      arboles_actual <- aplicar_cortas(arboles_actual, plan_cortas, año_corta = año)
      
      cat("\n")
    }
  }
  
  # 5. GUARDAR ESTADO DEL AÑO
  historial_completo[[año + 1]] <- arboles_actual %>% mutate(año_simulacion = año)
  historial_metricas[[año + 1]] <- calcular_metricas_estado(arboles_actual, CONFIG) %>%
    mutate(año_simulacion = año)
}

# Consolidar historial
df_historial <- bind_rows(historial_completo)
df_metricas <- bind_rows(historial_metricas)
df_cortas <- bind_rows(registro_cortas)

cat("\n✓ Simulación completada\n")

# ==============================================================================
# GRÁFICOS: EVOLUCIÓN VOLUMEN Y DENSIDAD POR RODAL
# ==============================================================================
library(patchwork)

cat("\n[4/6] Generando gráficos...\n")

# Preparar datos para gráficos
evolucion_rodal <- df_metricas %>%
  group_by(rodal, año_simulacion) %>%
  summarise(
    vol_ha = sum(vol_ha_m3, na.rm = TRUE),
    densidad_ha = sum(densidad_ha, na.rm = TRUE),
    .groups = "drop"
  )

# Gráfico 1: Evolución del volumen
p_volumen <- ggplot(evolucion_rodal, aes(x = año_simulacion, y = vol_ha, 
                                         color = factor(rodal), 
                                         group = rodal)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  # Marcar años de corta
  geom_vline(data = CONFIG$programa_cortas, 
             aes(xintercept = año_corta), 
             linetype = "dashed", alpha = 0.3) +
  labs(
    title = "Evolución del volumen por rodal (10 años)",
    subtitle = "Líneas verticales = años de corta programada",
    x = "Año",
    y = "Volumen (m³/ha)",
    color = "Rodal"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Gráfico 2: Evolución de la densidad
p_densidad <- ggplot(evolucion_rodal, aes(x = año_simulacion, y = densidad_ha, 
                                          color = factor(rodal), 
                                          group = rodal)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(data = CONFIG$programa_cortas, 
             aes(xintercept = año_corta), 
             linetype = "dashed", alpha = 0.3) +
  labs(
    title = "Evolución de la densidad por rodal (10 años)",
    subtitle = "Líneas verticales = años de corta programada",
    x = "Año",
    y = "Densidad (árboles/ha)",
    color = "Rodal"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Combinar y guardar
p_combined <- p_volumen / p_densidad
ggsave("graficos/evolucion_10años_rodales.png", p_combined, 
       width = 12, height = 10, dpi = 300)

cat("  ✓ Gráfico guardado: graficos/evolucion_10años_rodales.png\n")

# ==============================================================================
# TABLAS DE INTENSIDAD DE CORTE
# ==============================================================================

cat("\n[5/6] Calculando intensidad de corte...\n")

if (nrow(df_cortas) > 0) {
  
  # Tabla 1: Intensidad por rodal, año, género y clase diamétrica
  intensidad_corte <- df_cortas %>%
    mutate(
      clase_d = asignar_clase_diametrica(diametro_normal, formato = "rango")
    ) %>%
    group_by(rodal_cortado, año_corta, genero_grupo, clase_d) %>%
    summarise(
      n_arboles = n(),
      vol_cortado_m3 = sum(volumen_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(rodal_cortado, año_corta, genero_grupo, clase_d)
  
  # Tabla 2: Resumen por rodal y género
  resumen_corte_rodal <- df_cortas %>%
    group_by(rodal_cortado, año_corta, genero_grupo) %>%
    summarise(
      n_arboles = n(),
      vol_cortado_m3 = sum(volumen_m3, na.rm = TRUE),
      d_medio = mean(diametro_normal, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat(sprintf("  Total de árboles cortados: %d\n", nrow(df_cortas)))
  cat(sprintf("  Volumen total cortado: %.2f m³\n", 
              sum(df_cortas$volumen_m3, na.rm = TRUE)))
  
} else {
  cat("  ⚠ No se registraron cortas en la simulación\n")
  intensidad_corte <- tibble()
  resumen_corte_rodal <- tibble()
}

# ==============================================================================
# EXPORTACIÓN A LATEX - NORMA 152
# ==============================================================================

cat("\n[6/6] Exportando tablas a LaTeX...\n")

ruta_latex <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5/tablas_latex"

# -------------------------
# TABLA 1: Estado inicial del inventario
# -------------------------
tabla_inicial <- arboles_inicial %>%
  filter(!dominancia %in% c(7, 8, 9)) %>%
  group_by(rodal, genero_grupo) %>%
  summarise(
    n_arboles = n(),
    AB_m2 = sum(area_basal, na.rm = TRUE),
    Vol_m3 = sum(volumen_m3, na.rm = TRUE),
    D_medio_cm = mean(diametro_normal, na.rm = TRUE),
    H_media_m = mean(altura_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(rodal, genero_grupo)

xtable_inicial <- xtable(tabla_inicial,
                         caption = "Inventario inicial por rodal y género",
                         label = "tab:inventario_inicial")

print(xtable_inicial,
      file = file.path(ruta_latex, "01_inventario_inicial.tex"),
      include.rownames = FALSE,
      floating = TRUE,
      booktabs = TRUE)

cat("  ✓ 01_inventario_inicial.tex\n")

# -------------------------
# TABLA 2: Resumen de crecimiento (año 0 vs año 10)
# -------------------------
comparacion_inicial_final <- df_metricas %>%
  filter(año_simulacion %in% c(0, PERIODO_SIMULACION)) %>%
  group_by(rodal, año_simulacion) %>%
  summarise(
    vol_ha = sum(vol_ha_m3, na.rm = TRUE),
    densidad_ha = sum(densidad_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = año_simulacion,
    values_from = c(vol_ha, densidad_ha),
    names_prefix = "año_"
  ) %>%
  mutate(
    incremento_vol = vol_ha_año_10 - vol_ha_año_0,
    incremento_dens = densidad_ha_año_10 - densidad_ha_año_0
  )

xtable_comparacion <- xtable(comparacion_inicial_final,
                             caption = "Comparación estado inicial vs final (10 años)",
                             label = "tab:comparacion_10años")

print(xtable_comparacion,
      file = file.path(ruta_latex, "02_comparacion_inicial_final.tex"),
      include.rownames = FALSE,
      floating = TRUE,
      booktabs = TRUE)

cat("  ✓ 02_comparacion_inicial_final.tex\n")

# -------------------------
# TABLA 3: Intensidad de corte por rodal
# -------------------------
if (nrow(resumen_corte_rodal) > 0) {
  xtable_corte <- xtable(resumen_corte_rodal,
                         caption = "Intensidad de corte por rodal y género",
                         label = "tab:intensidad_corte")
  
  print(xtable_corte,
        file = file.path(ruta_latex, "03_intensidad_corte_rodal.tex"),
        include.rownames = FALSE,
        floating = TRUE,
        booktabs = TRUE)
  
  cat("  ✓ 03_intensidad_corte_rodal.tex\n")
}

# -------------------------
# TABLA 4: Detalle de corte por clase diamétrica
# -------------------------
if (nrow(intensidad_corte) > 0) {
  xtable_corte_detalle <- xtable(intensidad_corte,
                                 caption = "Detalle de corte por género y clase diamétrica",
                                 label = "tab:corte_detalle")
  
  print(xtable_corte_detalle,
        file = file.path(ruta_latex, "04_corte_por_clase_diametrica.tex"),
        include.rownames = FALSE,
        floating = TRUE,
        booktabs = TRUE)
  
  cat("  ✓ 04_corte_por_clase_diametrica.tex\n")
}

# -------------------------
# TABLAS DE CORTA POR RODAL (una tabla por cada rodal cortado)
# -------------------------
if (nrow(df_cortas) > 0) {
  rodales_cortados <- unique(df_cortas$rodal_cortado)
  
  for (rodal_id in rodales_cortados) {
    
    tabla_corta_rodal <- df_cortas %>%
      filter(rodal_cortado == rodal_id) %>%
      mutate(clase_d = asignar_clase_diametrica(diametro_normal, formato = "rango")) %>%
      group_by(año_corta, genero_grupo, clase_d) %>%
      summarise(
        n_individuos = n(),
        vol_m3 = sum(volumen_m3, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(año_corta, genero_grupo, clase_d)
    
    xtable_rodal <- xtable(tabla_corta_rodal,
                           caption = sprintf("Programa de corta - Rodal %d", rodal_id),
                           label = sprintf("tab:corta_rodal_%d", rodal_id))
    
    print(xtable_rodal,
          file = file.path(ruta_latex, sprintf("05_corta_rodal_%02d.tex", rodal_id)),
          include.rownames = FALSE,
          floating = TRUE,
          booktabs = TRUE)
    
    cat(sprintf("  ✓ 05_corta_rodal_%02d.tex\n", rodal_id))
  }
}

# ==============================================================================
# GUARDAR DATOS FINALES
# ==============================================================================

saveRDS(df_historial, "resultados/historial_completo_10años.rds")
saveRDS(df_metricas, "resultados/metricas_10años.rds")
saveRDS(df_cortas, "resultados/registro_cortas.rds")
saveRDS(list(
  volumen = evolucion_rodal,
  densidad = evolucion_rodal,
  cortas = df_cortas
), "resultados/datos_graficos.rds")

# Exportar a CSV también
write_csv(evolucion_rodal, "resultados/evolucion_rodal_10años.csv")
write_csv(intensidad_corte, "resultados/intensidad_corte_detalle.csv")
write_csv(resumen_corte_rodal, "resultados/resumen_corte_rodal.csv")

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

cat("\n")
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("║         ✓ SIMULACIÓN 10 AÑOS COMPLETADA                  ║\n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

cat("RESULTADOS:\n")
cat(sprintf("  • Años simulados:        10\n"))
cat(sprintf("  • Rodales procesados:    %d\n", n_distinct(arboles_inicial$rodal)))
cat(sprintf("  • Cortas realizadas:     %d rodales\n", n_distinct(df_cortas$rodal_cortado)))
cat(sprintf("  • Árboles cortados:      %d\n", nrow(df_cortas)))
cat(sprintf("  • Volumen cortado:       %.2f m³\n\n", 
            sum(df_cortas$volumen_m3, na.rm = TRUE)))

cat("ARCHIVOS GENERADOS:\n")
cat("  Gráficos:\n")
cat("    - graficos/evolucion_10años_rodales.png\n\n")
cat("  Datos RDS:\n")
cat("    - resultados/historial_completo_10años.rds\n")
cat("    - resultados/metricas_10años.rds\n")
cat("    - resultados/registro_cortas.rds\n\n")
cat("  Tablas LaTeX (en tablas_latex/):\n")
cat("    - 01_inventario_inicial.tex\n")
cat("    - 02_comparacion_inicial_final.tex\n")
cat("    - 03_intensidad_corte_rodal.tex\n")
cat("    - 04_corte_por_clase_diametrica.tex\n")
cat("    - 05_corta_rodal_XX.tex (una por cada rodal cortado)\n\n")

cat("Las tablas están listas para incluir en el PMF usando \\input{}\n\n")