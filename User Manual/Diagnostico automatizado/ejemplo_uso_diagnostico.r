# ==============================================================================
# EJEMPLO DE USO: SISTEMA DE DIAGNÓSTICO AUTOMATIZADO
# Cómo integrar el diagnóstico en tu workflow SIERRAFOR
# ==============================================================================

setwd("/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5")

library(tidyverse)

# ==============================================================================
# OPCIÓN 1: INTEGRAR EN WORKFLOW COMPLETO (RECOMENDADO)
# ==============================================================================

# En 40_WORKFLOW_COMPLETO.R, agregar después de la Fase 3:

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║     FASE 3.5: DIAGNÓSTICO Y ESTRATEGIA AUTOMATIZADA       ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")

# Cargar módulo de diagnóstico
source("modelov5/16_diagnostico_automatizado.R")

# Diagnosticar todos los rodales
resultados_diagnostico <- diagnosticar_todos_rodales(
  arboles_analisis,
  CONFIG,
  guardar = TRUE,
  verbose = TRUE
)

# Visualizar resumen
cat("\n[RESUMEN GENERAL]\n")
print(resultados_diagnostico$resumen)

# Opción A: USAR programa recomendado
cat("\n¿Deseas usar el programa de cortas RECOMENDADO por el diagnóstico? (s/n): ")
usar_recomendado <- readline()

if (tolower(usar_recomendado) == "s") {
  CONFIG$programa_cortas <- resultados_diagnostico$programa_cortas
  cat("✓ Programa de cortas actualizado con recomendaciones automáticas\n")
} else {
  cat("✓ Usando programa de cortas manual de 05_config_programa_cortas.R\n")
}

# Continuar con simulación normal...
cat("\n[5.2] Ejecutando simulación completa...\n")
source("modelov5/30_SIMULACION_10AÑOS_COMPLETA.R")

# ==============================================================================
# OPCIÓN 2: DIAGNÓSTICO INDIVIDUAL DE UN RODAL
# ==============================================================================

# Para diagnosticar solo el rodal 4 (ejemplo problemático)

source("modelov5/15_core_calculos.R")
source("modelov5/01_parametros_configuracion.R")
source("modelov5/16_diagnostico_automatizado.R")

# Cargar datos
arboles <- readRDS("datos_intermedios/arboles_analisis.rds")

# Diagnosticar rodal 4
arboles_r4 <- arboles %>% filter(rodal == 4)

diagnostico_r4 <- diagnosticar_rodal_automatico(
  arboles_r4,
  rodal_id = 4,
  config = CONFIG,
  verbose = TRUE
)

# Ver reporte completo
cat("\n")
cat(diagnostico_r4$reporte$texto)

# Ver programa de cortas recomendado
cat("\n[PROGRAMA DE CORTAS RECOMENDADO]\n")
print(diagnostico_r4$programa_cortas)

# Guardar reporte
writeLines(
  diagnostico_r4$reporte$texto,
  "reportes/diagnostico_rodal_04_detallado.txt"
)

# ==============================================================================
# OPCIÓN 3: COMPARAR ESTRATEGIAS (MANUAL vs RECOMENDADA)
# ==============================================================================

# Diagnosticar todos
diagnosticos <- diagnosticar_todos_rodales(arboles, CONFIG, guardar = FALSE)

# Crear tabla comparativa
comparacion <- diagnosticos$programa_cortas %>%
  select(rodal, año_corta_recom = año_corta, metodo_recom = metodo, 
         intensidad_recom = intensidad_pct) %>%
  left_join(
    CONFIG$programa_cortas %>%
      select(rodal, año_corta_manual = año_corta, metodo_manual = metodo,
             intensidad_manual = intensidad_pct),
    by = "rodal"
  )

cat("\n[COMPARACIÓN: MANUAL vs RECOMENDADO]\n")
cat("═══════════════════════════════════════════════════════════\n")
print(comparacion)

# Identificar diferencias significativas
diferencias <- comparacion %>%
  mutate(
    cambio_metodo = metodo_manual != metodo_recom,
    cambio_intensidad = abs(intensidad_manual - intensidad_recom) > 5,
    cambio_año = abs(año_corta_manual - año_corta_recom) > 1
  ) %>%
  filter(cambio_metodo | cambio_intensidad | cambio_año)

if (nrow(diferencias) > 0) {
  cat("\n⚠️  RODALES CON DIFERENCIAS SIGNIFICATIVAS:\n")
  print(diferencias %>% select(rodal, metodo_manual, metodo_recom, 
                               intensidad_manual, intensidad_recom))
}

# ==============================================================================
# OPCIÓN 4: GENERAR REPORTES PARA PMF
# ==============================================================================

# Generar todos los reportes en formato texto para incluir en PMF

source("modelov5/16_diagnostico_automatizado.R")

diagnosticos <- diagnosticar_todos_rodales(
  arboles_analisis,
  CONFIG,
  guardar = TRUE,
  verbose = FALSE  # Silencioso para producción
)

# Los reportes se guardan automáticamente en:
# reportes/diagnostico_rodal_01.txt
# reportes/diagnostico_rodal_02.txt
# ... etc.

cat("\n✓ Reportes generados para todos los rodales\n")
cat("  Ubicación: reportes/\n")
cat("  Formato: Texto plano (copiar/pegar en PMF)\n\n")

# Opcionalmente, consolidar en un solo documento
reportes_consolidados <- sapply(diagnosticos$diagnosticos, 
                                function(d) d$reporte$texto)

reporte_completo <- paste(reportes_consolidados, collapse = "\n\n\n")

writeLines(
  reporte_completo,
  "reportes/DIAGNOSTICO_CONSOLIDADO_TODOS_RODALES.txt"
)

cat("✓ Reporte consolidado generado:\n")
cat("  reportes/DIAGNOSTICO_CONSOLIDADO_TODOS_RODALES.txt\n\n")

# ==============================================================================
# OPCIÓN 5: ANÁLISIS EXPLORATORIO
# ==============================================================================

# Explorar métricas de todos los rodales

diagnosticos <- diagnosticar_todos_rodales(arboles, CONFIG, guardar = FALSE)

# Extraer métricas de todos los rodales
metricas_todos <- map_dfr(diagnosticos$diagnosticos, function(d) {
  tibble(
    rodal = d$rodal_id,
    clase = d$clasificacion$clase,
    G_actual = d$metricas$G_actual,
    ratio_G = d$metricas$ratio_G,
    IR = d$metricas$IR,
    IRe = d$metricas$IRe,
    IC = d$metricas$IC,
    CF = d$metricas$CF,
    densidad = d$metricas$densidad_ha,
    metodo_recom = d$estrategia$ciclo1$metodo,
    intensidad_recom = d$estrategia$ciclo1$intensidad
  )
})

# Resumen estadístico
cat("\n[RESUMEN ESTADÍSTICO DEL PREDIO]\n")
cat("═══════════════════════════════════════════════════════════\n")

summary_stats <- metricas_todos %>%
  summarise(
    n_rodales = n(),
    G_promedio = mean(G_actual),
    G_min = min(G_actual),
    G_max = max(G_actual),
    IR_promedio = mean(IR),
    densidad_promedio = mean(densidad)
  )

print(summary_stats)

cat("\n[CLASIFICACIÓN POR TIPO DE ESTRUCTURA]\n")
cat("═══════════════════════════════════════════════════════════\n")
table_clases <- table(metricas_todos$clase)
print(table_clases)

# Gráfico de métricas
library(ggplot2)

p1 <- ggplot(metricas_todos, aes(x = factor(rodal), y = G_actual, fill = clase)) +
  geom_col() +
  geom_hline(yintercept = CONFIG$g_objetivo, linetype = "dashed", color = "red") +
  labs(
    title = "Área basimétrica por rodal y clasificación",
    subtitle = sprintf("Línea roja = G objetivo (%.0f m²/ha)", CONFIG$g_objetivo),
    x = "Rodal",
    y = "G (m²/ha)",
    fill = "Clase"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("graficos/diagnostico_G_por_rodal.png", p1, width = 10, height = 6)

p2 <- ggplot(metricas_todos, aes(x = IR, y = IRe, color = clase, size = G_actual)) +
  geom_point(alpha = 0.7) +
  geom_vline(xintercept = 30, linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 15, linetype = "dashed", alpha = 0.5) +
  labs(
    title = "Regeneración vs Reserva por rodal",
    subtitle = "Líneas punteadas = umbrales mínimos",
    x = "Índice de Regeneración (%)",
    y = "Índice de Reserva (%)",
    color = "Clase",
    size = "G (m²/ha)"
  ) +
  theme_minimal()

ggsave("graficos/diagnostico_IR_vs_IRe.png", p2, width = 10, height = 6)

cat("\n✓ Gráficos guardados:\n")
cat("  • graficos/diagnostico_G_por_rodal.png\n")
cat("  • graficos/diagnostico_IR_vs_IRe.png\n")

# ==============================================================================
# OPCIÓN 6: VALIDAR UN RODAL ESPECÍFICO MANUALMENTE
# ==============================================================================

# Si no estás seguro del diagnóstico automático de un rodal

rodal_revisar <- 4

diag <- diagnosticar_rodal_automatico(
  arboles %>% filter(rodal == rodal_revisar),
  rodal_revisar,
  CONFIG
)

cat("\n[VALIDACIÓN MANUAL]\n")
cat("═══════════════════════════════════════════════════════════\n")
cat(sprintf("Clasificación automática: %s\n", diag$clasificacion$clase))
cat(sprintf("¿Es correcta? (s/n): "))
es_correcta <- readline()

if (tolower(es_correcta) == "n") {
  cat("\n¿Cuál sería la clasificación correcta?\n")
  cat("  1. A_J_INVERTIDA\n")
  cat("  2. B_CAMPANA\n")
  cat("  3. C_UNIFORME\n")
  cat("  4. D_EMPOBRECIDA\n")
  cat("Opción (1-4): ")
  
  opcion <- as.integer(readline())
  clases <- c("A_J_INVERTIDA", "B_CAMPANA", "C_UNIFORME", "D_EMPOBRECIDA")
  
  clase_manual <- clases[opcion]
  
  # Regenerar estrategia con clasificación manual
  estrategia_corregida <- generar_estrategia_30_anos(
    clase_manual,
    diag$metricas,
    rodal_revisar,
    CONFIG
  )
  
  cat(sprintf("\n✓ Estrategia regenerada con clase: %s\n", clase_manual))
  print(estrategia_corregida$programa_cortas)
  
  # Guardar corrección
  saveRDS(
    list(
      rodal = rodal_revisar,
      clasificacion_auto = diag$clasificacion$clase,
      clasificacion_manual = clase_manual,
      estrategia = estrategia_corregida
    ),
    sprintf("resultados/diagnostico_manual_rodal_%02d.rds", rodal_revisar)
  )
}

# ==============================================================================
# EJEMPLO COMPLETO: WORKFLOW TÍPICO
# ==============================================================================

# Paso 1: Cargar todo
source("modelov5/15_core_calculos.R")
source("modelov5/01_parametros_configuracion.R")
source("modelov5/16_diagnostico_automatizado.R")

arboles <- readRDS("datos_intermedios/arboles_analisis.rds")

# Paso 2: Diagnosticar
cat("\n=== DIAGNÓSTICO AUTOMATIZADO ===\n")
diagnosticos <- diagnosticar_todos_rodales(arboles, CONFIG, guardar = TRUE)

# Paso 3: Revisar resultados
cat("\n=== RESUMEN ===\n")
print(diagnosticos$resumen)

# Paso 4: Decidir qué hacer
cat("\n¿Qué deseas hacer?\n")
cat("  1. Usar programa recomendado en simulación\n")
cat("  2. Comparar con programa manual\n")
cat("  3. Ver reporte detallado de un rodal\n")
cat("  4. Generar gráficos\n")
cat("Opción (1-4): ")

opcion <- readline()

if (opcion == "1") {
  # Usar recomendaciones
  CONFIG$programa_cortas <- diagnosticos$programa_cortas
  source("modelov5/30_SIMULACION_10AÑOS_COMPLETA.R")
  
} else if (opcion == "2") {
  # Comparar
  comparacion <- diagnosticos$programa_cortas %>%
    rename(metodo_recom = metodo, int_recom = intensidad_pct) %>%
    left_join(
      CONFIG$programa_cortas %>%
        select(rodal, metodo_manual = metodo, int_manual = intensidad_pct),
      by = "rodal"
    )
  print(comparacion)
  
} else if (opcion == "3") {
  # Ver detalle
  cat("¿Qué rodal? ")
  r <- as.integer(readline())
  cat(diagnosticos$diagnosticos[[as.character(r)]]$reporte$texto)
  
} else if (opcion == "4") {
  # Gráficos
  # (código de gráficos del ejemplo anterior)
}

cat("\n✓ Proceso completado\n")

# ==============================================================================
# NOTAS FINALES
# ==============================================================================

# IMPORTANTE:
# 1. El diagnóstico es una HERRAMIENTA DE APOYO, no reemplaza criterio profesional
# 2. Siempre VALIDAR las recomendaciones con observaciones de campo
# 3. Los REPORTES pueden incluirse directamente en el PMF
# 4. Ajustar G_objetivo según calidad de sitio real
# 5. Re-diagnosticar después de cada ciclo de corta (años 10, 20, 30)

# ARCHIVOS GENERADOS:
# - resultados/diagnosticos_automatizados.rds (objeto R con todo)
# - resultados/programa_cortas_recomendado.csv (tabla para simulación)
# - reportes/diagnostico_rodal_XX.txt (uno por rodal)
# - reportes/DIAGNOSTICO_CONSOLIDADO_TODOS_RODALES.txt (todos juntos)
# - graficos/diagnostico_*.png (visualizaciones)

cat("\n═══════════════════════════════════════════════════════════\n")
cat("Para más información consultar:\n")
cat("  • SISTEMA_DIAGNOSTICO_AUTOMATIZADO.md (metodología completa)\n")
cat("  • 16_diagnostico_automatizado.R (código documentado)\n")
cat("═══════════════════════════════════════════════════════════\n\n")
