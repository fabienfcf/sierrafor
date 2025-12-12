# ==============================================================================
# TABLA LATEX: DISTRIBUCIÓN DE SITIOS DE MUESTREO CON VOL/HA Y AB/HA
# ==============================================================================

library(tidyverse)
library(kableExtra)

crear_tabla_distribucion_sitios <- function(inventario, arboles_analisis = NULL, config = CONFIG) {
  
  # Verificar datos
  if (is.null(inventario$f01)) {
    stop("❌ No se encontró F01 en el inventario")
  }
  
  # Si no se pasan árboles procesados, construirlos
  if (is.null(arboles_analisis)) {
    cat("📊 Construyendo dataset de árboles...\n")
    source("modelov5/00_importar_inventario.R")
    arboles_analisis <- construir_arboles_analisis(inventario, config)
  }
  
  # Cargar códigos si no están cargados
  if (!exists("CODIGOS_EXPOSICION")) {
    source("modelov5/02_config_codigos.R")
  }
  
  # Cargar funciones de cálculo si no están
  if (!exists("filtrar_arboles_vivos")) {
    source("modelov5/15_core_calculos.R")
  }
  
  cat("📊 Calculando métricas dasométricas por sitio...\n")
  
  # ==============================================================================
  # CALCULAR VOL/HA Y AB/HA POR SITIO (Pinus + Quercus)
  # ==============================================================================
  
  # Filtrar árboles vivos de Pinus y Quercus
  vivos_pq <- filtrar_arboles_vivos(arboles_analisis) %>%
    filter(genero_grupo %in% c("Pinus", "Quercus"))
  
  # Calcular por sitio
  metricas_por_sitio <- vivos_pq %>%
    group_by(rodal, muestreo) %>%
    summarise(
      ab_total_m2 = sum(area_basal, na.rm = TRUE),
      vol_total_m3 = sum(volumen_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # CRÍTICO: cada sitio = 1 parcela individual
      ab_m2ha = expandir_a_hectarea(ab_total_m2, config$area_parcela_ha),
      vol_m3ha = expandir_a_hectarea(vol_total_m3, config$area_parcela_ha)
    )
  
  cat(sprintf("  ✓ Métricas calculadas para %d sitios\n", nrow(metricas_por_sitio)))
  
  # ==============================================================================
  # PREPARAR DATOS DE LA TABLA
  # ==============================================================================
  
  # Datos físicos del sitio (F01)
  tabla_datos <- inventario$f01 %>%
    select(rodal, muestreo, utm_x, utm_y, asnm, pendiente, exposicion) %>%
    mutate(
      exposicion = traducir_codigos(exposicion, "exposicion"),
      coordenadas_x = sprintf("%.5f", utm_x),
      coordenadas_y = sprintf("%.5f", utm_y)
    ) %>%
    select(-utm_x, -utm_y)
  
  # Unir con métricas dasométricas
  tabla_datos <- tabla_datos %>%
    left_join(
      metricas_por_sitio %>% 
        select(rodal, muestreo, ab_m2ha, vol_m3ha),
      by = c("rodal", "muestreo")
    ) %>%
    mutate(
      # Formatear a 2 decimales, NA para sitios sin Pinus/Quercus
      ab_m2ha = ifelse(is.na(ab_m2ha), "—", sprintf("%.2f", ab_m2ha)),
      vol_m3ha = ifelse(is.na(vol_m3ha), "—", sprintf("%.2f", vol_m3ha))
    )
  
  # Agregar superficie del rodal si existe
  if (!is.null(inventario$umm)) {
    tabla_datos <- tabla_datos %>%
      left_join(
        inventario$umm %>% 
          select(id, SUPERFICIE) %>%
          rename(rodal = id, superficie_rodal = SUPERFICIE),
        by = "rodal"
      ) %>%
      mutate(superficie_rodal = sprintf("%.2f", superficie_rodal))
  } else {
    tabla_datos <- tabla_datos %>% mutate(superficie_rodal = "N/A")
  }
  
  # Reordenar columnas finales
  tabla_datos <- tabla_datos %>%
    select(rodal, superficie_rodal, muestreo, 
           coordenadas_x, coordenadas_y, 
           asnm, pendiente, exposicion,
           ab_m2ha, vol_m3ha) %>%
    arrange(rodal, muestreo)
  
  # Detectar filas donde cambia el rodal (para líneas separadoras)
  filas_separacion <- which(diff(tabla_datos$rodal) != 0)
  
  cat("📄 Generando tabla LaTeX...\n")
  
  # ==============================================================================
  # CREAR TABLA LATEX
  # ==============================================================================
  
  tabla_latex <- tabla_datos %>%
    kbl(
      format = "latex",
      booktabs = TRUE,
      longtable = TRUE,
      col.names = c(
        "\\shortstack{Unidad\\\\Mínima de\\\\Manejo}",
        "\\shortstack{Superficie\\\\Rodal\\\\(ha)}",
        "\\shortstack{Sitio de\\\\Muestreo}",
        "\\shortstack{Coordenadas\\\\UTM X}",
        "\\shortstack{Coordenadas\\\\UTM Y}",
        "\\shortstack{Elevación\\\\(msnm)}",
        "\\shortstack{Pendiente\\\\(\\%)}",
        "Exposición",
        "\\shortstack{AB\\\\(m²/ha)}",
        "\\shortstack{Vol\\\\(m³/ha)}"
      ),
      align = c("c", "c", "c", "r", "r", "c", "c", "l", "r", "r"),
      caption = "Distribución y características de los sitios de muestreo por Unidad Mínima de Manejo (Pinus + Quercus)",
      escape = FALSE
    ) %>%
    kable_styling(
      latex_options = c("repeat_header"),
      font_size = 9,
      full_width = FALSE
    ) %>%
    row_spec(0, bold = TRUE) %>%
    collapse_rows(columns = 1:2, valign = "middle", latex_hline = "major")
  
  # Agregar líneas horizontales entre UMMs
  for (fila in filas_separacion) {
    tabla_latex <- tabla_latex %>%
      row_spec(fila, hline_after = TRUE)
  }
  
  cat("✓ Tabla generada exitosamente\n")
  cat(sprintf("  • Sitios totales: %d\n", nrow(tabla_datos)))
  cat(sprintf("  • Sitios con Pinus/Quercus: %d\n", 
              sum(tabla_datos$vol_m3ha != "—")))
  cat(sprintf("  • Columnas: %d (incluye AB/ha y Vol/ha)\n", ncol(tabla_datos)))
  
  # Guardar datos_raw en el entorno global para acceso posterior
  assign("DATOS_SITIOS_RAW", tabla_datos, envir = .GlobalEnv)
  cat("  • Datos raw guardados en: DATOS_SITIOS_RAW (variable global)\n")
  
  return(tabla_latex)
}

guardar_tabla_distribucion <- function(tabla_latex, 
                                       ruta_salida = "tablas_latex/tabla_distribucion_sitios.tex") {
  
  # Crear directorio si no existe
  dir_salida <- dirname(ruta_salida)
  if (!dir.exists(dir_salida)) {
    dir.create(dir_salida, recursive = TRUE)
  }
  
  # Guardar
  writeLines(as.character(tabla_latex), ruta_salida)
  
  cat(sprintf("\n✓ Tabla guardada en: %s\n", ruta_salida))
  cat("✓ Versión EXTENDIDA con AB/ha y Vol/ha por sitio\n")
  cat("✓ Solo Pinus + Quercus vivos\n")
}

exportar_csv_sitios <- function(datos_raw, 
                                ruta_salida = "resultados/tabla_sitios_metricas.csv") {
  
  # Crear directorio si no existe
  dir_salida <- dirname(ruta_salida)
  if (!dir.exists(dir_salida)) {
    dir.create(dir_salida, recursive = TRUE)
  }
  
  # Guardar CSV
  write_csv(datos_raw, ruta_salida)
  
  cat(sprintf("\n✓ CSV guardado en: %s\n", ruta_salida))
  cat(sprintf("  • %d filas × %d columnas\n", nrow(datos_raw), ncol(datos_raw)))
}