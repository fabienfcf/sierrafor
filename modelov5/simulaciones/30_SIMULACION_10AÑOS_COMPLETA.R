# ==============================================================================
# SIMULACIÓN 10 AÑOS - CORREGIDA
# ==============================================================================

# Al inicio de 30_SIMULACION_10AÑOS_COMPLETA.R
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)


# Verificar/crear directorios
dirs <- c("datos_intermedios", "resultados", "graficos", "tablas_latex")
for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}

# Cargar módulos
cat("\n[1/6] Cargando módulos...\n")
source(file.path(PROYECTO_ROOT, "core/15_core_calculos.R"))
source(file.path(PROYECTO_ROOT, "config/01_parametros_configuracion.R"))
source(file.path(PROYECTO_ROOT, "core/10_modelos_crecimiento.R"))
source(file.path(PROYECTO_ROOT, "core/11_modelo_mortalidad.R"))
source(file.path(PROYECTO_ROOT, "core/12_modelo_reclutamiento.R"))
source(file.path(PROYECTO_ROOT, "core/13_simulador_crecimiento.R"))
source(file.path(PROYECTO_ROOT, "core/14_optimizador_cortas.R"))  # ← Versión corregida

library(xtable)
library(gridExtra)
library(patchwork)

# ==============================================================================
# CARGAR DATOS
# ==============================================================================

cat("\n[2/6] Cargando datos iniciales...\n")
arboles_inicial <- readRDS("datos_intermedios/arboles_analisis.rds") %>%
  filter(genero_grupo %in% c("Pinus", "Quercus"))

cat(sprintf("  Población inicial: %d árboles\n", nrow(arboles_inicial)))

# ==============================================================================
# SIMULACIÓN
# ==============================================================================

cat("\n[3/6] Simulando 10 años...\n\n")

arboles_actual <- arboles_inicial
historial_completo <- list()
historial_metricas <- list()
registro_cortas <- list()

# Estado inicial
historial_completo[[1]] <- arboles_actual %>% mutate(año_simulacion = 0)
historial_metricas[[1]] <- calcular_metricas_estado(arboles_actual, CONFIG) %>%
  mutate(año_simulacion = 0)

# Simulación año por año
for (año in 1:PERIODO_SIMULACION) {
  
  cat(sprintf("═══ AÑO %d ═══\n", año))
  
  # 1. CRECIMIENTO
  cat(sprintf("\n[AÑO %d] Crecimiento...\n", año))
  arboles_actual <- aplicar_crecimiento_poblacion(arboles_actual, CONFIG, año)
  
  # 2. MORTALIDAD
  cat(sprintf("\n[AÑO %d] Mortalidad...\n", año))
  arboles_actual <- aplicar_mortalidad_poblacion(arboles_actual, CONFIG, año)
  
  # 3. RECLUTAMIENTO
  cat(sprintf("\n[AÑO %d] Reclutamiento...\n", año))
  arboles_actual <- aplicar_reclutamiento(arboles_actual, CONFIG, año)
  
  # 4. CORTAS
  rodales_cortar <- PROGRAMA_CORTAS %>%
    filter(año_corta == año) %>%
    pull(rodal)
  
  if (length(rodales_cortar) > 0) {
    
    cat(sprintf("\n[AÑO %d] 🪓 CORTAS PROGRAMADAS\n", año))
    cat(sprintf("  Rodales: %s\n\n", paste(rodales_cortar, collapse = ", ")))
    
    for (rodal_id in rodales_cortar) {
      
      # ✅ Obtener configuración completa del rodal
      config_rodal <- PROGRAMA_CORTAS %>% 
        filter(rodal == rodal_id, año_corta == año) %>%
        slice(1)  # Por si hay duplicados
      
      # ✅ Crear configuración de corte con TODOS los parámetros
      corte_config <- configurar_corte(
        metodo = config_rodal$metodo,
        intensidad_pct = config_rodal$intensidad_pct,
        años_ica = PERIODO_SIMULACION,
        d_min = config_rodal$d_min,
        d_max = config_rodal$d_max,
        prioridad = config_rodal$prioridad,
        excluir_semilleros = config_rodal$excluir_semilleros  # ✅ AHORA EXISTE
      )
      
      # Filtrar árboles del rodal
      arboles_rodal <- arboles_actual %>% filter(rodal == rodal_id)
      arboles_rodal_inicial <- arboles_inicial %>% filter(rodal == rodal_id)
      
      arboles_rodal_año_anterior <- NULL
      if (año > 1 && length(historial_completo) >= año) {
        arboles_rodal_año_anterior <- historial_completo[[año]] %>%
          filter(rodal == rodal_id)
      }
      
      cat(sprintf("  ──── Rodal %d ────\n", rodal_id))
      
      # Calcular plan de cortas
      plan_cortas <- tryCatch({
        calcular_plan_cortas(
          arboles_rodal,
          CONFIG,
          arboles_rodal_inicial,
          arboles_rodal_año_anterior,
          corte_config,
          año_actual = año
        )
      }, error = function(e) {
        cat(sprintf("  ❌ Error: %s\n", e$message))
        list(arboles_marcados = tibble(), resumen = tibble())
      })
      
      # ✅ Registrar corta CORRECTAMENTE
      if (nrow(plan_cortas$arboles_marcados) > 0) {
        
        # Asegurarse que las columnas existen
        arboles_cortados <- plan_cortas$arboles_marcados %>%
          mutate(
            año_corta = año,
            rodal_cortado = rodal_id,  # ✅ Ahora sí existe
            metodo_corta = config_rodal$metodo
          )
        
        # Verificar columnas críticas
        if (!"volumen_m3" %in% names(arboles_cortados)) {
          warning("⚠️ Columna 'volumen_m3' faltante en árboles cortados")
          arboles_cortados <- arboles_cortados %>%
            mutate(volumen_m3 = 0)
        }
        
        registro_cortas[[length(registro_cortas) + 1]] <- arboles_cortados
      }
      
      # Aplicar corta
      arboles_actual <- aplicar_cortas(arboles_actual, plan_cortas, año_corta = año)
      
      cat("\n")
    }
  }
  
  # 5. GUARDAR ESTADO
  historial_completo[[año + 1]] <- arboles_actual %>% mutate(año_simulacion = año)
  historial_metricas[[año + 1]] <- calcular_metricas_estado(arboles_actual, CONFIG) %>%
    mutate(año_simulacion = año)
}

# Consolidar
df_historial <- bind_rows(historial_completo)
df_metricas <- bind_rows(historial_metricas)

# ✅ Consolidar cortas con manejo seguro
if (length(registro_cortas) > 0) {
  df_cortas <- bind_rows(registro_cortas)
  
  # Verificar y limpiar
  if (!"rodal_cortado" %in% names(df_cortas)) {
    df_cortas <- df_cortas %>% mutate(rodal_cortado = rodal)
  }
  if (!"volumen_m3" %in% names(df_cortas)) {
    df_cortas <- df_cortas %>% mutate(volumen_m3 = 0)
  }
} else {
  df_cortas <- tibble()
}

cat("\n✓ Simulación completada\n")

# ==============================================================================
# GRÁFICOS
# ==============================================================================

cat("\n[4/6] Generando gráficos...\n")

evolucion_rodal <- df_metricas %>%
  group_by(rodal, año_simulacion) %>%
  summarise(
    vol_ha = sum(vol_ha_m3, na.rm = TRUE),
    densidad_ha = sum(densidad_ha, na.rm = TRUE),
    .groups = "drop"
  )

p_volumen <- ggplot(evolucion_rodal, aes(x = año_simulacion, y = vol_ha, 
                                         color = factor(rodal), 
                                         group = rodal)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(data = PROGRAMA_CORTAS, 
             aes(xintercept = año_corta), 
             linetype = "dashed", alpha = 0.3) +
  labs(
    title = "Evolución del volumen por rodal (10 años)",
    subtitle = "Líneas verticales = años de corta",
    x = "Año", y = "Volumen (m³/ha)", color = "Rodal"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

p_densidad <- ggplot(evolucion_rodal, aes(x = año_simulacion, y = densidad_ha, 
                                          color = factor(rodal), 
                                          group = rodal)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(data = PROGRAMA_CORTAS, 
             aes(xintercept = año_corta), 
             linetype = "dashed", alpha = 0.3) +
  labs(
    title = "Evolución de la densidad por rodal",
    x = "Año", y = "Densidad (árboles/ha)", color = "Rodal"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

p_combined <- p_volumen / p_densidad
ggsave("graficos/evolucion_10años_rodales.png", p_combined, 
       width = 12, height = 10, dpi = 300)

cat("  ✓ Gráfico guardado\n")

# ==============================================================================
# TABLAS DE CORTE
# ==============================================================================

cat("\n[5/6] Calculando intensidad de corte...\n")

if (nrow(df_cortas) > 0) {
  
  intensidad_corte <- df_cortas %>%
    mutate(
      clase_d = asignar_clase_diametrica(diametro_normal, formato = "rango")
    ) %>%
    group_by(rodal_cortado, año_corta, genero_grupo, clase_d) %>%
    summarise(
      n_arboles = n(),
      vol_cortado_m3 = sum(volumen_m3, na.rm = TRUE),
      .groups = "drop"
    )
  
  resumen_corte_rodal <- df_cortas %>%
    group_by(rodal_cortado, año_corta, genero_grupo) %>%
    summarise(
      n_arboles = n(),
      vol_cortado_m3 = sum(volumen_m3, na.rm = TRUE),
      d_medio = mean(diametro_normal, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat(sprintf("  Árboles cortados: %d\n", nrow(df_cortas)))
  cat(sprintf("  Volumen cortado: %.2f m³\n", 
              sum(df_cortas$volumen_m3, na.rm = TRUE)))
} else {
  cat("  ℹ️ No se registraron cortas\n")
  intensidad_corte <- tibble()
  resumen_corte_rodal <- tibble()
}

# ==============================================================================
# EXPORTACIÓN LATEX
# ==============================================================================

cat("\n[6/6] Exportando tablas LaTeX...\n")

# Tabla 1: Inventario inicial
tabla_inicial <- arboles_inicial %>%
  filtrar_arboles_vivos() %>%
  group_by(rodal, genero_grupo) %>%
  summarise(
    n_arboles = n(),
    Vol_m3 = sum(volumen_m3, na.rm = TRUE),
    D_medio_cm = mean(diametro_normal, na.rm = TRUE),
    .groups = "drop"
  )

xtable_inicial <- xtable(tabla_inicial,
                         caption = "Inventario inicial por rodal y género",
                         label = "tab:inventario_inicial")

print(xtable_inicial,
      file = "tablas_latex/01_inventario_inicial.tex",
      include.rownames = FALSE,
      floating = TRUE,
      booktabs = TRUE)

cat("  ✓ Tablas exportadas\n")

# ==============================================================================
# GUARDAR RESULTADOS
# ==============================================================================

saveRDS(df_historial, "resultados/historial_completo_10años.rds")
saveRDS(df_metricas, "resultados/metricas_10años.rds")
saveRDS(df_cortas, "resultados/registro_cortas.rds")

write_csv(evolucion_rodal, "resultados/evolucion_rodal_10años.csv")

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

cat("\n")
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("║         ✓ SIMULACIÓN COMPLETADA                          ║\n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

cat("RESULTADOS:\n")
cat(sprintf("  • Años simulados:     10\n"))
cat(sprintf("  • Rodales:            %d\n", n_distinct(arboles_inicial$rodal)))

if (nrow(df_cortas) > 0) {
  cat(sprintf("  • Árboles cortados:   %d\n", nrow(df_cortas)))
  cat(sprintf("  • Volumen cortado:    %.2f m³\n", 
              sum(df_cortas$volumen_m3, na.rm = TRUE)))
} else {
  cat("  • Sin cortas registradas\n")
}

cat("\nARCHIVOS:\n")
cat("  📁 resultados/\n")
cat("  📁 graficos/\n")
cat("  📁 tablas_latex/\n\n")