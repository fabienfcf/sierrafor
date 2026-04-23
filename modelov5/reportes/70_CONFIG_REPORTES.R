# Establecer directorio raíz del proyecto
if (!exists("PROYECTO_ROOT")) {
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/2025/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"
}
setwd(PROYECTO_ROOT)

# ==============================================================================
# CONFIGURACIÓN DE REPORTES - SISTEMA SIERRAFOR
# ==============================================================================
#
# Este archivo controla qué tablas y gráficos se generan en el sistema de
# reportes PMF. Modificar TRUE/FALSE para activar/desactivar elementos.
#
# Uso:
#   source(file.path(PROYECTO_ROOT, "reportes/70_CONFIG_REPORTES.R"))
#   if (REPORTE_CONFIG$seccion_1_estado_inicial$tablas$T1_1_resumen_predio) {
#     # generar tabla
#   }
#
# ==============================================================================

REPORTE_CONFIG <- list(
  
  # ============================================================================
  # CONFIGURACIÓN GENERAL
  # ============================================================================
  
  version = "1.0",
  fecha_creacion = "2025-11-22",
  proyecto = "PMF Las Alazanas 2026-2036",
  
  # Control global de exportación
  exportar_tablas_latex = TRUE,
  exportar_tablas_csv = TRUE,
  exportar_graficos = TRUE,
  
  # Directorios de salida
  dir_tablas_latex = "tablas_latex",
  dir_csv = "resultados",
  dir_graficos = "graficos",
  
  # Configuración de gráficos
  formato_graficos = "png",      # png, pdf, svg, jpg
  dpi_graficos = 300,
  ancho_default_cm = 25,         # ancho en cm
  alto_default_cm = 20,          # alto en cm
  ancho_default_in = 10,         # ancho en pulgadas
  alto_default_in = 8,           # alto en pulgadas
  
  # Configuración de tablas LaTeX
  latex_booktabs = TRUE,
  latex_floating = TRUE,
  latex_caption_placement = "top",
  
  # Verbosidad
  mostrar_progreso = TRUE,
  mostrar_warnings = TRUE,
  
  # ============================================================================
  # SECCIÓN 1: ESTADO INICIAL DEL PREDIO (t₀)
  # ============================================================================
  
  seccion_1_estado_inicial = list(
    
    activar = TRUE,
    descripcion = "Caracterización del estado inicial del predio",
    
    # --------------------------------------------------------------------------
    # 1.1 BIODIVERSIDAD Y COMPOSICIÓN
    # --------------------------------------------------------------------------
    
    tablas = list(
      
      # T1.1 - Resumen general predio
      T1_1_resumen_predio = TRUE,
      # Contenido: UMM, superficie, n_rodales, n_sitios, pendiente_prom, exposición_prom
      
      # T1.2 - Composición florística predio
      T1_2_composicion_floristica = TRUE,
      # Contenido: género, n/ha, AB/ha ± SD, DMC ± SD, altura_prom ± SD (inter-UMM)
      
      # T1.4 - Índices de diversidad por UMM
      T1_4_diversidad = TRUE,
      # Contenido: rodal, índice_shannon, índice_simpson, riqueza_especifica
      
      # --------------------------------------------------------------------------
      # 1.2 ESTRUCTURA FORESTAL
      # --------------------------------------------------------------------------
      
      # T2.1 - Métricas dendrométricas por rodal (enfocado Pinus/Quercus)
      T2_1_metricas_rodal = TRUE,
      # Contenido: rodal, género, n/ha, AB/ha, Vol/ha, Ømed, hmed
      
      # T2.3 - Distribución diamétrica por género y por UMM
      T2_3_dist_diametrica_genero = TRUE,
      # Contenido: rodal, género, clase_d, n/ha, AB/ha, Vol/ha
      
      # T2.6 - Área basal por clase dominancia, por género, por UMM
      T2_6_ab_dominancia = TRUE,
      # Contenido: rodal, género, dominancia, n/ha, AB/ha
      
      # T2.7 - Muertos en pie por género y rodal (excluir tocones)
      T2_7_muertos_pie = TRUE,
      # Contenido: rodal, género, n/ha, AB/ha
      
      # T2.8 - Tocones por género y rodal
      T2_8_tocones = TRUE,
      # Contenido: rodal, género, n/ha, AB/ha, Vol/ha (inferido)
      
      # --------------------------------------------------------------------------
      # 1.3 CARACTERÍSTICAS BIOFÍSICAS DEL PREDIO
      # --------------------------------------------------------------------------
      
      # T3.1 - Erosión: % sitios por categoría por rodal
      T3_1_erosion = TRUE,
      # Contenido: rodal, cat_erosion, pct_sitios
      
      # T3.2 - Sanidad: distribución árboles por categoría por rodal
      T3_2_sanidad = TRUE,
      # Contenido: rodal, cat_sanidad, n_arboles, pct
      
      # T3.3 - Regeneración: densidad plántulas/ha por género y rodal (<7.5cm)
      T3_3_regeneracion = TRUE,
      # Contenido: rodal, género, n_plantulas_ha
      
      # T3.4 - Uso pecuario: intensidad por rodal
      T3_4_uso_pecuario = TRUE,
      # Contenido: rodal, codigo_uso_pecuario, intensidad
      
      # T3.5 - Características topográficas por rodal
      T3_5_caracteristicas_topograficas = TRUE
      # Contenido: rodal, pendiente_prom, exposicion, altitud_media
    ),
    
    graficos = list(
      
      # G1.2 - Abundancia relativa por especie (top 15)
      G1_2_abundancia_especies = TRUE,
      
      # G1.3 - Mapa temático: dominancia Pinus vs Quercus por rodal
      G1_3_mapa_dominancia = TRUE,
      
      # G2.1 - Distribución diamétrica total (Pinus vs Quercus superpuestos)
      G2_1_dist_diametrica_total = TRUE,
      
      # G2.3 - Distribución diamétrica por rodal (facets)
      G2_3_dist_diametrica_rodal = TRUE,
      
      # G2.6 - Densidad (N/ha) por rodal (barras ordenadas)
      G2_6_densidad_rodal = TRUE,
      
      # G2.7 - Volumen por hectárea por rodal (barras ordenadas)
      G2_7_volumen_rodal = TRUE,
      
      # EXTRA - Mapa de cantidad de combustible (kriging)
      G_extra_mapa_combustible = FALSE  # Requiere shapefiles y gstat
    )
  ),
  
  # ============================================================================
  # SECCIÓN 2: DINÁMICA Y PROYECCIONES (10 AÑOS)
  # ============================================================================
  
  seccion_2_dinamica = list(
    
    activar = TRUE,
    descripcion = "Proyección dinámica poblacional 10 años",
    
    # --------------------------------------------------------------------------
    # 2.1 CRECIMIENTO POBLACIONAL
    # --------------------------------------------------------------------------
    
    tablas = list(
      
      # T4.1 - Comparación estado inicial vs final por género por rodal
      T4_1_comparacion_inicial_final = TRUE,
      # Contenido: rodal, género, N_ini, N_fin, AB_ini, AB_fin, Vol_ini, Vol_fin, DMC_ini, DMC_fin, hmed_ini, hmed_fin
      
      # T4.3 - Incrementos absolutos y relativos por rodal
      T4_3_incrementos = TRUE,
      # Contenido: rodal, género, ΔN, ΔN_pct, ΔAB, ΔAB_pct, ΔDMC, ΔVol, ΔVol_pct
      
      # T4.4 - ICA (Incremento Corriente Anual) por género y rodal
      T4_4_ica = TRUE
      # Contenido: rodal, género, ICA_m3_ha_año, ICA_total_m3
    ),
    
    # --------------------------------------------------------------------------
    # 2.2 PROCESOS DEMOGRÁFICOS
    # --------------------------------------------------------------------------
    
    graficos = list(
      
      # G5.3 - Reclutamiento acumulado 10 años por género
      G5_3_reclutamiento = TRUE,
      
      # G5.4 - Balance demográfico: flujos entrada/salida (diagrama Sankey)
      G5_4_balance_demografico = TRUE
    )
  ),
  
  # ============================================================================
  # SECCIÓN 3: MANEJO Y APROVECHAMIENTO
  # ============================================================================
  
  seccion_3_manejo = list(
    
    activar = TRUE,
    descripcion = "Programa de cortas y análisis de aprovechamiento",
    
    # --------------------------------------------------------------------------
    # 3.1 PROGRAMA DE CORTAS - TABLAS OBLIGATORIAS NOM-152
    # --------------------------------------------------------------------------
    
    tablas_nom152 = list(
      
      # T6.1 - Cuadro 3 NOM-152: Existencias por UMM
      T6_1_cuadro3_nom152 = TRUE,
      # Contenido: UMM, superficie, especie, exist_reales_m3_ha, exist_reales_m3_umm,
      #            AB_m2_ha, intensidad_corta_pct, vol_residual_m3_ha, AB_residual_m2_ha,
      #            posibilidad_m3_ha, posibilidad_m3_umm
      
      # T6.2 - Cuadro 4 NOM-152: Resumen existencias predio
      T6_2_cuadro4_nom152 = TRUE,
      # Contenido: especie/género, exist_reales_totales_m3, posibilidad_m3, vol_residual_m3
      
      # T6.4 - Cuadro 6 NOM-152: Posibilidad anual y plan de cortas cronológico
      T6_4_cuadro6_nom152 = TRUE
      # Contenido: área_corta, UMM, superficie_ha, tratamiento_silvicola,
      #            posibilidad_genero_m3, vol_infraestructura_m3, posibilidad_total_m3
    ),
    
    # --------------------------------------------------------------------------
    # 3.1 PROGRAMA DE CORTAS - TABLAS COMPLEMENTARIAS
    # --------------------------------------------------------------------------
    
    tablas_complementarias = list(
      
      # T6.5 - Intensidad de corta por rodal
      T6_5_intensidad_corta = TRUE,
      # Contenido: rodal, n_cortados, pct_intensidad, vol_extraido_m3
      
      # T6.6 - Distribución cortas por clase diamétrica
      T6_6_dist_cortas = TRUE,
      # Contenido: clase_d, n_cortados, vol_cortado_m3
      
      # T6.7 - Volumen aprovechable por producto
      T6_7_vol_productos = TRUE,
      # Contenido: producto (trocería/celulosa/leña), volumen_m3, pct
      
      # T6.8 - Volumen residual post-corta por rodal
      T6_8_vol_residual = TRUE,
      # Contenido: rodal, vol_inicial_m3, vol_cortado_m3, vol_residual_m3, pct_residual
      
      # T6.9 - Cronograma anual de intervenciones
      T6_9_cronograma = TRUE,
      # Contenido: año, rodal, tratamiento, vol_m3, intensidad_pct
      
      # T6.10 - Comparación métodos corta (LIOCOURT vs ICA vs EXISTENCIAS)
      T6_10_comparacion_metodos = FALSE  # No ejecutar 3 simulaciones aún
    ),
    
    # --------------------------------------------------------------------------
    # 3.2 ANÁLISIS DE IMPACTO
    # --------------------------------------------------------------------------
    
    tablas_impacto = list(
      
      # T7.1 - Comparación con corta vs sin corta por rodal
      T7_1_con_vs_sin_corta = TRUE,
      # Contenido: rodal, N_sin_corta, N_con_corta, AB_sin_corta, AB_con_corta,
      #            Vol_sin_corta, Vol_con_corta
      
      # T7.2 - Sostenibilidad: ICA vs volumen aprovechado
      T7_2_sostenibilidad = TRUE,
      # Contenido: rodal, ICA_m3_año, vol_aprovechado_m3, balance_m3, sostenible (S/N)
      
      # T7.3 - Efecto corta en estructura diamétrica
      T7_3_efecto_estructura = TRUE
      # Contenido: clase_d, n_pre_corta, n_post_corta, delta_n, pct_cambio
    ),
    
    graficos = list(
      
      # G6.1 - Distribución diamétrica por género: t₀ vs t₁₀ (facets) + cortes
      G6_1_dist_cortes = TRUE,
      
      # G6.2 - Intensidad de corta % por rodal (barras ordenadas)
      G6_2_intensidad_rodal = TRUE,
      
      # G6.3 - Volumen aprovechable vs corta vs residual por rodal (stacked)
      G6_3_vol_aprovechable = TRUE,
      
      # G6.5 - Comparación distribuciones: actual vs objetivo Liocourt
      G6_5_actual_vs_liocourt = TRUE,
      
      # G7.1 - Escenarios: distribución diamétrica final sin vs con corta
      G7_1_escenarios = TRUE,
      
      # G7.2 - Distribución diamétrica: pre-corta vs post-corta vs proyección
      G7_2_dist_temporal = TRUE,
      
      # G7.3 - Balance sostenibilidad: ICA vs aprovechamiento anual
      G7_3_balance_sostenibilidad = TRUE
    )
  ),
  
  # ============================================================================
  # SECCIÓN 5: SÍNTESIS Y COMPARACIONES
  # ============================================================================
  
  seccion_5_sintesis = list(
    
    activar = TRUE,
    descripcion = "Resumen ejecutivo y comparaciones entre rodales",
    
    # --------------------------------------------------------------------------
    # 5.1 NIVEL PREDIO (AGREGADO)
    # --------------------------------------------------------------------------
    
    tablas = list(
      
      # T9.1 - Resumen ejecutivo predio
      T9_1_resumen_ejecutivo = TRUE,
      # Contenido: métrica, valor_inicial, valor_final, cambio, cambio_pct
      
      # T9.2 - Métricas dendrométricas totales predio
      T9_2_metricas_totales = TRUE,
      # Contenido: género, n_total, n_ha_prom, AB_total, AB_ha_prom, Vol_total, Vol_ha_prom
      
      # T9.3 - ICA total predio por género
      T9_3_ica_total = TRUE,
      # Contenido: género, ICA_m3_año, ICA_pct
      
      # T9.4 - Posibilidad anual total predio
      T9_4_posibilidad_total = TRUE,
      # Contenido: género, posibilidad_m3_año, posibilidad_acumulada_10años
      
      # --------------------------------------------------------------------------
      # 5.2 COMPARACIONES ENTRE RODALES
      # --------------------------------------------------------------------------
      
      # T10.1 - Ranking rodales por productividad
      T10_1_ranking_productividad = TRUE,
      # Contenido: rodal, Vol_ha, ICA, calidad_sitio, ranking, clasificacion
      
      # T10.2 - Clasificación rodales por tratamiento recomendado
      T10_2_clasificacion_tratamiento = TRUE
      # Contenido: rodal, tratamiento_recomendado, justificacion, prioridad
    ),
    
    graficos = list(
      
      # G9.1 - Composición predio: área por tipo superficie
      G9_1_area_tipo_superficie = TRUE,
      
      # G9.2 - Resumen visual: N árboles, AB, Vol inicial vs final
      G9_2_resumen_visual = TRUE,
      
      # G9.3 - Contribución rodales a volumen total (pie chart/treemap)
      G9_3_contribucion_rodales = TRUE,
      
      # G10.1 - Productividad relativa rodales (radar chart)
      G10_1_productividad_relativa = TRUE,
      
      # G10.2 - Mapa síntesis: clasificación rodales por productividad
      G10_2_mapa_sintesis = TRUE
    )
  ),
  
  # ============================================================================
  # CONFIGURACIÓN AVANZADA
  # ============================================================================
  
  avanzado = list(
    
    # Tolerancia para consideraciones numéricas
    tolerancia_cero = 1e-6,
    
    # Número de decimales por tipo de variable
    decimales = list(
      entero = 0,
      decimal1 = 1,
      decimal2 = 2,
      decimal3 = 3,
      porcentaje = 1,
      cientifico = 2
    ),
    
    # Umbrales para clasificaciones
    umbrales = list(
      productividad_alta = 150,    # Vol/ha > 150 m³
      productividad_media = 100,   # Vol/ha 100-150 m³
      productividad_baja = 100,    # Vol/ha < 100 m³
      
      intensidad_corta_baja = 20,  # < 20%
      intensidad_corta_media = 35, # 20-35%
      intensidad_corta_alta = 35   # > 35%
    ),
    
    # Códigos de clasificación
    clasificacion_superficie = list(
      produccion = TRUE,
      proteccion = FALSE,
      restauracion = FALSE
    )
  )
)

# ==============================================================================
# FUNCIONES AUXILIARES DE CONFIGURACIÓN
# ==============================================================================

#' Verificar si una tabla está activada
#'
#' @param seccion Nombre de la sección (ej: "seccion_1_estado_inicial")
#' @param id_tabla ID de la tabla (ej: "T1_1_resumen_predio")
#' @return Logical
tabla_activada <- function(seccion, id_tabla) {
  
  if (!seccion %in% names(REPORTE_CONFIG)) {
    warning(sprintf("Sección '%s' no existe en REPORTE_CONFIG", seccion))
    return(FALSE)
  }
  
  sec <- REPORTE_CONFIG[[seccion]]
  
  if (!sec$activar) {
    return(FALSE)
  }
  
  # Buscar en todas las sublistas de tablas
  tablas_lists <- c("tablas", "tablas_nom152", "tablas_complementarias", "tablas_impacto")
  
  for (tlist in tablas_lists) {
    if (tlist %in% names(sec)) {
      if (id_tabla %in% names(sec[[tlist]])) {
        return(sec[[tlist]][[id_tabla]])
      }
    }
  }
  
  warning(sprintf("Tabla '%s' no encontrada en sección '%s'", id_tabla, seccion))
  return(FALSE)
}

#' Verificar si un gráfico está activado
#'
#' @param seccion Nombre de la sección
#' @param id_grafico ID del gráfico
#' @return Logical
grafico_activado <- function(seccion, id_grafico) {
  
  if (!seccion %in% names(REPORTE_CONFIG)) {
    warning(sprintf("Sección '%s' no existe en REPORTE_CONFIG", seccion))
    return(FALSE)
  }
  
  sec <- REPORTE_CONFIG[[seccion]]
  
  if (!sec$activar) {
    return(FALSE)
  }
  
  if (!"graficos" %in% names(sec)) {
    return(FALSE)
  }
  
  if (!id_grafico %in% names(sec$graficos)) {
    warning(sprintf("Gráfico '%s' no encontrado en sección '%s'", id_grafico, seccion))
    return(FALSE)
  }
  
  return(sec$graficos[[id_grafico]])
}

#' Obtener lista de todas las tablas activadas
#'
#' @return Character vector con IDs de tablas activadas
obtener_tablas_activadas <- function() {
  
  tablas <- character(0)
  
  for (sec_name in names(REPORTE_CONFIG)) {
    
    if (!grepl("^seccion_", sec_name)) next
    
    sec <- REPORTE_CONFIG[[sec_name]]
    
    if (!sec$activar) next
    
    # Buscar en todas las sublistas
    tablas_lists <- c("tablas", "tablas_nom152", "tablas_complementarias", "tablas_impacto")
    
    for (tlist in tablas_lists) {
      if (tlist %in% names(sec)) {
        for (id_tabla in names(sec[[tlist]])) {
          if (sec[[tlist]][[id_tabla]]) {
            tablas <- c(tablas, id_tabla)
          }
        }
      }
    }
  }
  
  return(tablas)
}

#' Obtener lista de todos los gráficos activados
#'
#' @return Character vector con IDs de gráficos activados
obtener_graficos_activados <- function() {
  
  graficos <- character(0)
  
  for (sec_name in names(REPORTE_CONFIG)) {
    
    if (!grepl("^seccion_", sec_name)) next
    
    sec <- REPORTE_CONFIG[[sec_name]]
    
    if (!sec$activar) next
    
    if (!"graficos" %in% names(sec)) next
    
    for (id_grafico in names(sec$graficos)) {
      if (sec$graficos[[id_grafico]]) {
        graficos <- c(graficos, id_grafico)
      }
    }
  }
  
  return(graficos)
}

#' Imprimir resumen de configuración
#'
#' @return NULL (imprime en consola)
imprimir_resumen_config <- function() {
  
  cat("\n╔═══════════════════════════════════════════════════════════╗\n")
  cat("║         CONFIGURACIÓN DE REPORTES SIERRAFOR              ║\n")
  cat("╚═══════════════════════════════════════════════════════════╝\n\n")
  
  cat(sprintf("Proyecto: %s\n", REPORTE_CONFIG$proyecto))
  cat(sprintf("Versión:  %s\n", REPORTE_CONFIG$version))
  cat(sprintf("Fecha:    %s\n\n", REPORTE_CONFIG$fecha_creacion))
  
  cat("SECCIONES ACTIVADAS:\n")
  cat("════════════════════════════════════════════════════════════\n")
  
  for (sec_name in names(REPORTE_CONFIG)) {
    if (!grepl("^seccion_", sec_name)) next
    
    sec <- REPORTE_CONFIG[[sec_name]]
    
    status <- if (sec$activar) "✓" else "✗"
    cat(sprintf("  %s %s\n", status, sec$descripcion))
  }
  
  cat("\nTABLAS Y GRÁFICOS:\n")
  cat("════════════════════════════════════════════════════════════\n")
  
  n_tablas <- length(obtener_tablas_activadas())
  n_graficos <- length(obtener_graficos_activados())
  
  cat(sprintf("  📊 Tablas activadas:   %d\n", n_tablas))
  cat(sprintf("  📈 Gráficos activados: %d\n\n", n_graficos))
  
  cat("DIRECTORIOS DE SALIDA:\n")
  cat("════════════════════════════════════════════════════════════\n")
  cat(sprintf("  • Tablas LaTeX: %s/\n", REPORTE_CONFIG$dir_tablas_latex))
  cat(sprintf("  • CSV:          %s/\n", REPORTE_CONFIG$dir_csv))
  cat(sprintf("  • Gráficos:     %s/\n\n", REPORTE_CONFIG$dir_graficos))
}

# ==============================================================================
# INICIALIZACIÓN
# ==============================================================================

cat("\n✓ Configuración de reportes SIERRAFOR cargada\n")
cat(sprintf("  • %d tablas configuradas\n", length(obtener_tablas_activadas())))
cat(sprintf("  • %d gráficos configurados\n", length(obtener_graficos_activados())))
cat("\nUsar imprimir_resumen_config() para ver detalles completos\n")