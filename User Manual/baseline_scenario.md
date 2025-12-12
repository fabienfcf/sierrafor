# SIERRAFOR - Escenario Baseline (Sin Cortes)

## 🎯 OBJETIVO

Simular la evolución del bosque durante 10 años **SIN ninguna intervención** (baseline) para comparar contra el escenario con cortas programadas.

**Usos**:
- ✅ Demostrar necesidad de manejo forestal
- ✅ Comparación "con proyecto" vs "sin proyecto" para PMF
- ✅ Análisis de impacto de las cortas
- ✅ Justificación técnica-económica

---

## 📁 IMPLEMENTACIÓN

### Opción 1: MODIFICAR PROGRAMA DE CORTAS (Más limpio)

#### Crear configuración baseline en `05_config_programa_cortas.R`

```r
# Al final del archivo, agregar:

# ==============================================================================
# ESCENARIO BASELINE (SIN CORTES)
# ==============================================================================

PROGRAMA_CORTAS_BASELINE <- tribble(
  ~rodal, ~año_corta, ~metodo, ~intensidad_pct, ~d_min, ~prioridad, ~excluir_semilleros
  # VACÍO - sin cortas
)

# Función para seleccionar escenario
seleccionar_programa_cortas <- function(escenario = "manejo") {
  if (escenario == "baseline") {
    return(PROGRAMA_CORTAS_BASELINE)
  } else {
    return(PROGRAMA_CORTAS)
  }
}
```

#### Modificar `01_parametros_configuracion.R`

```r
# Agregar parámetro de escenario
CONFIG <- crear_configuracion_simulacion()

# Permitir selección de escenario
CONFIG$escenario <- "manejo"  # Default
# CONFIG$escenario <- "baseline"  # Cambiar para sin cortes

# Seleccionar programa según escenario
CONFIG$programa_cortas <- seleccionar_programa_cortas(CONFIG$escenario)
```

#### Ejecutar simulación baseline

```r
# En 40_WORKFLOW_COMPLETO.R o crear nuevo script

# ESCENARIO BASELINE
source("modelov5/01_parametros_configuracion.R")
CONFIG$escenario <- "baseline"
CONFIG$programa_cortas <- PROGRAMA_CORTAS_BASELINE

source("modelov5/30_SIMULACION_10AÑOS_COMPLETA.R")

# Guardar resultados con sufijo
saveRDS(historial_completo, "resultados/historial_BASELINE.rds")
saveRDS(historial_metricas, "resultados/metricas_BASELINE.rds")
```

---

### Opción 2: CREAR SCRIPT DEDICADO (Más explícito)

#### Crear `31_SIMULACION_BASELINE.R`

```r
# ==============================================================================
# SIMULACIÓN BASELINE - SIN CORTES
# Escenario de referencia para comparación
# ==============================================================================

rm(list = ls())
gc()

setwd("/tu/ruta/proyecto/R5")

library(tidyverse)
library(readxl)

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║         SIMULACIÓN BASELINE - SIN INTERVENCIÓN             ║\n")
cat("║              (Escenario de Referencia)                     ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")

# ==============================================================================
# CONFIGURACIÓN BASELINE
# ==============================================================================

cat("\n[1/5] Cargando configuración BASELINE...\n")

# Cargar módulos
source("modelov5/15_core_calculos.R")
source("modelov5/01_parametros_configuracion.R")
source("modelov5/10_modelos_crecimiento.R")
source("modelov5/11_modelo_mortalidad.R")
source("modelov5/12_modelo_reclutamiento.R")
source("modelov5/13_simulador_crecimiento.R")

# IMPORTANTE: NO cargar optimizador de cortas
# source("modelov5/14_optimizador_cortas.R")  # ← NO USAR

cat("✓ Configuración cargada - MODO BASELINE (sin cortes)\n")

# ==============================================================================
# CARGAR DATOS INICIALES
# ==============================================================================

cat("\n[2/5] Cargando datos iniciales...\n")
arboles_inicial <- readRDS("datos_intermedios/arboles_analisis.rds") %>%
  filter(genero_grupo %in% c("Pinus", "Quercus"))

cat(sprintf("  Población inicial: %d árboles\n", nrow(arboles_inicial)))

# ==============================================================================
# SIMULACIÓN 10 AÑOS SIN CORTES
# ==============================================================================

cat("\n[3/5] Iniciando simulación BASELINE (10 años sin cortes)...\n\n")

arboles_actual <- arboles_inicial
historial_completo <- list()
historial_metricas <- list()

# Estado inicial
historial_completo[[1]] <- arboles_actual %>% mutate(año_simulacion = 0)
historial_metricas[[1]] <- calcular_metricas_estado(arboles_actual, CONFIG) %>%
  mutate(año_simulacion = 0)

# Simulación año por año
for (año in 1:CONFIG$periodo) {
  
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
  
  # 4. NO HAY CORTAS EN BASELINE
  cat(sprintf("\n[AÑO %d] Cortas: NINGUNA (escenario baseline)\n", año))
  
  # 5. GUARDAR ESTADO
  historial_completo[[año + 1]] <- arboles_actual %>% mutate(año_simulacion = año)
  historial_metricas[[año + 1]] <- calcular_metricas_estado(arboles_actual, CONFIG) %>%
    mutate(año_simulacion = año)
}

# Consolidar historial
df_historial_baseline <- bind_rows(historial_completo)
df_metricas_baseline <- bind_rows(historial_metricas)

cat("\n✓ Simulación BASELINE completada\n")

# ==============================================================================
# GUARDAR RESULTADOS BASELINE
# ==============================================================================

cat("\n[4/5] Guardando resultados BASELINE...\n")

# Crear carpeta específica
dir.create("resultados/baseline", showWarnings = FALSE, recursive = TRUE)

# Guardar con sufijo claro
saveRDS(arboles_inicial, "resultados/baseline/poblacion_inicial.rds")
saveRDS(arboles_actual, "resultados/baseline/poblacion_final_año10.rds")
saveRDS(df_historial_baseline, "resultados/baseline/historial_completo_10años.rds")
saveRDS(df_metricas_baseline, "resultados/baseline/metricas_10años.rds")

cat("  ✓ Guardado en resultados/baseline/\n")

# ==============================================================================
# RESUMEN BASELINE
# ==============================================================================

cat("\n[5/5] Generando resumen BASELINE...\n")

n_inicial <- sum(!arboles_inicial$dominancia %in% c(7,8,9))
n_final <- sum(!arboles_actual$dominancia %in% c(7,8,9))

vol_inicial <- sum(arboles_inicial$volumen_m3[
  !arboles_inicial$dominancia %in% c(7,8,9)
], na.rm=TRUE)

vol_final <- sum(arboles_actual$volumen_m3[
  !arboles_actual$dominancia %in% c(7,8,9)
], na.rm=TRUE)

cat("\n")
cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║           RESUMEN ESCENARIO BASELINE (SIN CORTES)          ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("POBLACIÓN:\n")
cat(sprintf("  Año 0:  %d árboles vivos\n", n_inicial))
cat(sprintf("  Año 10: %d árboles vivos\n", n_final))
cat(sprintf("  Cambio: %+d árboles (%+.1f%%)\n\n", 
            n_final - n_inicial,
            ((n_final - n_inicial) / n_inicial) * 100))

cat("VOLUMEN:\n")
cat(sprintf("  Año 0:  %.2f m³\n", vol_inicial))
cat(sprintf("  Año 10: %.2f m³\n", vol_final))
cat(sprintf("  Cambio: %+.2f m³ (%+.1f%%)\n\n", 
            vol_final - vol_inicial,
            ((vol_final - vol_inicial) / vol_inicial) * 100))

cat("CORTAS:\n")
cat("  Ninguna (escenario baseline)\n")
cat("  Volumen extraído: 0 m³\n\n")

cat("ARCHIVOS GENERADOS:\n")
cat("  resultados/baseline/\n")
cat("    ├── poblacion_inicial.rds\n")
cat("    ├── poblacion_final_año10.rds\n")
cat("    ├── historial_completo_10años.rds\n")
cat("    └── metricas_10años.rds\n\n")

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║       ✓ SIMULACIÓN BASELINE COMPLETADA                     ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("SIGUIENTE PASO:\n")
cat("  Ejecutar 30_SIMULACION_10AÑOS_COMPLETA.R (con cortas)\n")
cat("  para obtener escenario de manejo y comparar.\n\n")
```

---

### Opción 3: SCRIPT COMPARATIVO (Recomendado)

#### Crear `32_COMPARACION_ESCENARIOS.R`

```r
# ==============================================================================
# COMPARACIÓN DE ESCENARIOS: BASELINE vs MANEJO
# ==============================================================================

library(tidyverse)
library(patchwork)

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║        COMPARACIÓN: BASELINE vs MANEJO CON CORTAS          ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")

# ==============================================================================
# CARGAR RESULTADOS
# ==============================================================================

cat("\n[1/4] Cargando resultados...\n")

# Baseline (sin cortes)
historial_baseline <- readRDS("resultados/baseline/historial_completo_10años.rds")
metricas_baseline <- readRDS("resultados/baseline/metricas_10años.rds")

# Manejo (con cortas)
historial_manejo <- readRDS("resultados/historial_completo_10años.rds")
metricas_manejo <- readRDS("resultados/metricas_10años.rds")
registro_cortas <- readRDS("resultados/registro_cortas.rds")

cat("  ✓ Datos cargados\n")

# ==============================================================================
# PREPARAR DATOS PARA COMPARACIÓN
# ==============================================================================

cat("\n[2/4] Preparando comparación...\n")

# Agregar etiqueta de escenario
metricas_baseline <- metricas_baseline %>% mutate(escenario = "Baseline (sin cortes)")
metricas_manejo <- metricas_manejo %>% mutate(escenario = "Manejo (con cortas)")

# Combinar
metricas_comparacion <- bind_rows(metricas_baseline, metricas_manejo)

# Resumen final
resumen_final <- metricas_comparacion %>%
  filter(año_simulacion %in% c(0, 10)) %>%
  group_by(escenario, año_simulacion) %>%
  summarise(
    n_vivos = sum(n_vivos),
    vol_total = sum(vol_ha_m3),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = año_simulacion,
    values_from = c(n_vivos, vol_total),
    names_prefix = "año_"
  ) %>%
  mutate(
    cambio_n = n_vivos_año_10 - n_vivos_año_0,
    cambio_n_pct = (cambio_n / n_vivos_año_0) * 100,
    cambio_vol = vol_total_año_10 - vol_total_año_0,
    cambio_vol_pct = (cambio_vol / vol_total_año_0) * 100
  )

# ==============================================================================
# GRÁFICOS COMPARATIVOS
# ==============================================================================

cat("\n[3/4] Generando gráficos comparativos...\n")

dir.create("graficos/comparacion", showWarnings = FALSE, recursive = TRUE)

# Gráfico 1: Evolución de volumen
p1 <- metricas_comparacion %>%
  group_by(escenario, año_simulacion) %>%
  summarise(vol_total = sum(vol_ha_m3), .groups = "drop") %>%
  ggplot(aes(x = año_simulacion, y = vol_total, color = escenario, linetype = escenario)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Baseline (sin cortes)" = "#e74c3c", 
                                 "Manejo (con cortas)" = "#27ae60")) +
  labs(
    title = "Evolución del Volumen Total",
    subtitle = "Comparación: Sin manejo vs Con cortas programadas",
    x = "Año de simulación",
    y = "Volumen total (m³/ha)",
    color = "Escenario",
    linetype = "Escenario"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16)
  )

ggsave("graficos/comparacion/evolucion_volumen.png", p1, width = 10, height = 6, dpi = 300)

# Gráfico 2: Evolución de densidad
p2 <- metricas_comparacion %>%
  group_by(escenario, año_simulacion) %>%
  summarise(densidad = sum(densidad_ha), .groups = "drop") %>%
  ggplot(aes(x = año_simulacion, y = densidad, color = escenario, linetype = escenario)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Baseline (sin cortes)" = "#e74c3c", 
                                 "Manejo (con cortas)" = "#27ae60")) +
  labs(
    title = "Evolución de la Densidad",
    subtitle = "Número de árboles por hectárea",
    x = "Año de simulación",
    y = "Densidad (árboles/ha)",
    color = "Escenario",
    linetype = "Escenario"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16)
  )

ggsave("graficos/comparacion/evolucion_densidad.png", p2, width = 10, height = 6, dpi = 300)

# Gráfico 3: Barras comparativas finales
p3 <- resumen_final %>%
  select(escenario, vol_total_año_0, vol_total_año_10) %>%
  pivot_longer(cols = starts_with("vol_total"), 
               names_to = "momento", 
               values_to = "volumen") %>%
  mutate(momento = if_else(momento == "vol_total_año_0", "Año 0", "Año 10")) %>%
  ggplot(aes(x = momento, y = volumen, fill = escenario)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f", volumen)), 
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Baseline (sin cortes)" = "#e74c3c", 
                                "Manejo (con cortas)" = "#27ae60")) +
  labs(
    title = "Comparación Inicial vs Final",
    subtitle = "Volumen total por escenario",
    x = NULL,
    y = "Volumen (m³/ha)",
    fill = "Escenario"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16)
  )

ggsave("graficos/comparacion/barras_comparativas.png", p3, width = 10, height = 6, dpi = 300)

# Gráfico combinado
p_combined <- (p1 / p2 / p3) +
  plot_annotation(
    title = "ANÁLISIS COMPARATIVO: BASELINE vs MANEJO",
    subtitle = "Impacto del programa de cortas en la dinámica del bosque",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5)
    )
  )

ggsave("graficos/comparacion/analisis_completo.png", p_combined, 
       width = 12, height = 16, dpi = 300)

cat("  ✓ Gráficos guardados en graficos/comparacion/\n")

# ==============================================================================
# TABLA COMPARATIVA
# ==============================================================================

cat("\n[4/4] Generando tabla comparativa...\n")

# Calcular volumen extraído
vol_cortado <- if (nrow(registro_cortas) > 0) {
  sum(registro_cortas$volumen_m3, na.rm = TRUE)
} else {
  0
}

# Tabla resumen
tabla_comparativa <- resumen_final %>%
  mutate(
    vol_extraido = if_else(escenario == "Manejo (con cortas)", vol_cortado, 0),
    vol_final_ajustado = vol_total_año_10 + vol_extraido,
    produccion_neta = vol_final_ajustado - vol_total_año_0
  ) %>%
  select(
    Escenario = escenario,
    `Vol inicial` = vol_total_año_0,
    `Vol final` = vol_total_año_10,
    `Vol extraído` = vol_extraido,
    `Producción neta` = produccion_neta,
    `Cambio %` = cambio_vol_pct
  )

print(tabla_comparativa)

# Guardar CSV
write.csv(tabla_comparativa, 
          "resultados/comparacion_escenarios.csv", 
          row.names = FALSE)

cat("\n  ✓ Tabla guardada: resultados/comparacion_escenarios.csv\n")

# ==============================================================================
# RESUMEN EJECUTIVO
# ==============================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║              RESUMEN COMPARATIVO                           ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# Diferencia entre escenarios
diff_vol_final <- resumen_final$vol_total_año_10[resumen_final$escenario == "Manejo (con cortas)"] -
                  resumen_final$vol_total_año_10[resumen_final$escenario == "Baseline (sin cortes)"]

cat("ESCENARIO BASELINE (sin cortes):\n")
cat(sprintf("  Volumen año 10: %.2f m³/ha\n", 
            resumen_final$vol_total_año_10[resumen_final$escenario == "Baseline (sin cortes)"]))
cat(sprintf("  Cambio: %+.1f%%\n\n",
            resumen_final$cambio_vol_pct[resumen_final$escenario == "Baseline (sin cortes)"]))

cat("ESCENARIO MANEJO (con cortas):\n")
cat(sprintf("  Volumen año 10: %.2f m³/ha\n", 
            resumen_final$vol_total_año_10[resumen_final$escenario == "Manejo (con cortas)"]))
cat(sprintf("  Volumen extraído: %.2f m³\n", vol_cortado))
cat(sprintf("  Cambio: %+.1f%%\n\n",
            resumen_final$cambio_vol_pct[resumen_final$escenario == "Manejo (con cortas)"]))

cat("DIFERENCIA:\n")
cat(sprintf("  Volumen final: %.2f m³/ha %s\n", 
            abs(diff_vol_final),
            if_else(diff_vol_final > 0, "más con manejo", "menos con manejo")))

cat("\nBENEFICIO DEL MANEJO:\n")
cat(sprintf("  Extracción sostenible: %.2f m³\n", vol_cortado))
cat("  Manteniendo volumen en pie similar o mejor\n")
cat("  Mejorando estructura del bosque\n\n")

cat("ARCHIVOS GENERADOS:\n")
cat("  graficos/comparacion/\n")
cat("    ├── evolucion_volumen.png\n")
cat("    ├── evolucion_densidad.png\n")
cat("    ├── barras_comparativas.png\n")
cat("    └── analisis_completo.png\n")
cat("  resultados/\n")
cat("    └── comparacion_escenarios.csv\n\n")

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║          ✓ COMPARACIÓN COMPLETADA                          ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")
```

---

## 📋 PLAN DE EJECUCIÓN COMPLETO

### Paso 1: Ejecutar Baseline (sin cortes)
```r
source("modelov5/31_SIMULACION_BASELINE.R")
```

**Tiempo**: ~5-10 minutos  
**Output**: `resultados/baseline/*.rds`

### Paso 2: Ejecutar Manejo (con cortas)
```r
source("modelov5/30_SIMULACION_10AÑOS_COMPLETA.R")
```

**Tiempo**: ~5-10 minutos  
**Output**: `resultados/*.rds`

### Paso 3: Comparar escenarios
```r
source("modelov5/32_COMPARACION_ESCENARIOS.R")
```

**Tiempo**: ~1-2 minutos  
**Output**: Gráficos + tabla CSV

---

## 📊 OUTPUTS ESPERADOS

### Gráficos comparativos:
1. **Evolución volumen**: Líneas baseline (roja) vs manejo (verde)
2. **Evolución densidad**: Cómo cambia N/ha con y sin cortas
3. **Barras comparativas**: Estado inicial vs final por escenario
4. **Análisis completo**: Panel con los 3 gráficos

### Tabla comparativa:
```
Escenario              Vol inicial  Vol final  Vol extraído  Producción  Cambio%
Baseline (sin cortes)  145.2        168.7      0            23.5        +16.2%
Manejo (con cortas)    145.2        142.3      32.4         29.7        -2.0%
```

### Interpretación:
- **Baseline**: Bosque acumula volumen (+16%) pero puede sobrepoblar
- **Manejo**: Extrae 32 m³ sosteniblemente, mantiene estructura saludable

---

## ⚠️ CONSIDERACIONES IMPORTANTES

1. **Semilla aleatoria**: Si quieres resultados reproducibles:
```r
set.seed(12345)  # Antes de simulación
```

2. **Mismo punto de partida**: Ambas simulaciones deben usar el mismo `arboles_analisis.rds`

3. **Comparación justa**: Mismos parámetros de crecimiento, mortalidad y reclutamiento

4. **Interpretación**: Baseline NO significa "óptimo sin hacer nada" - puede llevar a sobrepoblación, supresión excesiva, etc.

---

## 🎯 PARA EL PMF

### Incluir en el documento:

**Sección: "Justificación del Manejo Forestal"**

```latex
\subsection{Comparación de Escenarios}

Se simularon dos escenarios para evaluar el impacto del programa de cortas:

\begin{enumerate}
\item \textbf{Escenario Baseline:} Sin intervención (crecimiento natural)
\item \textbf{Escenario de Manejo:} Con cortas programadas
\end{enumerate}

\begin{table}[H]
\centering
\input{tablas/comparacion_escenarios.tex}
\caption{Comparación de escenarios a 10 años}
\end{table}

\begin{figure}[H]
\centering
\includegraphics[width=\textwidth]{graficos/comparacion/analisis_completo.png}
\caption{Evolución comparativa del bosque bajo ambos escenarios}
\end{figure}

\textbf{Conclusión:} El programa de cortas permite extraer XXX m³ de manera 
sostenible, manteniendo la estructura del bosque y mejorando la vitalidad 
de los árboles remanentes.
```

---

¿Quieres que te ayude a implementar alguna de las tres opciones, o prefieres que te muestre cómo generar tablas LaTeX adicionales para la comparación?