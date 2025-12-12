# ==============================================================================
# UTILS: INPUT/OUTPUT
# Funciones compartidas para lectura/escritura de archivos
# ==============================================================================
#
# PROPÓSITO:
#   Centralizar todas las funciones de I/O para evitar duplicación
#
# FUNCIONES:
#   - exportar_csv()
#   - guardar_resultados()
#   - cargar_resultados()
#   - verificar_archivos()
#
# DEPENDENCIAS:
#   tidyverse (para procesamiento)
#
# ==============================================================================

library(tidyverse)

#' Exportar dataframe a CSV
#'
#' Guarda un dataframe en formato CSV con encoding UTF-8
#'
#' @param df Dataframe a exportar
#' @param nombre_archivo Nombre del archivo (sin extensión)
#' @param carpeta Carpeta destino (default: "resultados")
#' @param timestamp Agregar timestamp al nombre (default: FALSE)
#'
#' @return Ruta completa del archivo guardado (invisible)
#'
#' @examples
#' exportar_csv(datos, "resumen_rodal", carpeta = "resultados")
#' exportar_csv(metricas, "metricas", timestamp = TRUE)
exportar_csv <- function(df, 
                        nombre_archivo, 
                        carpeta = "resultados",
                        timestamp = FALSE) {
  
  # Validar entrada
  if (!is.data.frame(df)) {
    stop("❌ Error: 'df' debe ser un dataframe")
  }
  
  if (nrow(df) == 0) {
    warning("⚠️  Advertencia: dataframe vacío, no se guardará")
    return(invisible(NULL))
  }
  
  # Crear carpeta si no existe
  dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)
  
  # Construir nombre con timestamp opcional
  if (timestamp) {
    timestamp_str <- format(Sys.time(), "_%Y%m%d_%H%M%S")
    nombre_final <- paste0(nombre_archivo, timestamp_str, ".csv")
  } else {
    nombre_final <- paste0(nombre_archivo, ".csv")
  }
  
  # Ruta completa
  ruta_completa <- file.path(carpeta, nombre_final)
  
  # Guardar
  write.csv(df, 
            ruta_completa, 
            row.names = FALSE, 
            fileEncoding = "UTF-8")
  
  # Mensaje
  cat(sprintf("✓ CSV guardado: %s (%d filas)\n", 
              ruta_completa, 
              nrow(df)))
  
  return(invisible(ruta_completa))
}


#' Guardar múltiples objetos a RDS
#'
#' Guarda una lista de objetos R en archivos .rds individuales
#'
#' @param lista_objetos Lista nombrada con objetos a guardar
#' @param carpeta Carpeta destino (default: "resultados")
#' @param timestamp Agregar timestamp a los nombres (default: FALSE)
#'
#' @return Vector con rutas de archivos guardados (invisible)
#'
#' @examples
#' resultados <- list(
#'   analisis_descriptivo = res_desc,
#'   historial_completo = historial,
#'   metricas_10años = metricas
#' )
#' guardar_resultados(resultados, carpeta = "resultados")
guardar_resultados <- function(lista_objetos, 
                               carpeta = "resultados",
                               timestamp = FALSE) {
  
  # Validar entrada
  if (!is.list(lista_objetos)) {
    stop("❌ Error: 'lista_objetos' debe ser una lista")
  }
  
  if (is.null(names(lista_objetos))) {
    stop("❌ Error: 'lista_objetos' debe ser una lista nombrada")
  }
  
  # Crear carpeta
  dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)
  
  # Guardar cada objeto
  rutas_guardadas <- character(length(lista_objetos))
  
  for (i in seq_along(lista_objetos)) {
    nombre <- names(lista_objetos)[i]
    objeto <- lista_objetos[[i]]
    
    # Construir nombre con timestamp opcional
    if (timestamp) {
      timestamp_str <- format(Sys.time(), "_%Y%m%d_%H%M%S")
      nombre_final <- paste0(nombre, timestamp_str, ".rds")
    } else {
      nombre_final <- paste0(nombre, ".rds")
    }
    
    # Ruta completa
    archivo <- file.path(carpeta, nombre_final)
    
    # Guardar
    saveRDS(objeto, archivo)
    
    # Mensaje
    cat(sprintf("✓ RDS guardado: %s (%.1f KB)\n", 
                archivo, 
                file.info(archivo)$size / 1024))
    
    rutas_guardadas[i] <- archivo
  }
  
  return(invisible(rutas_guardadas))
}


#' Cargar múltiples objetos desde RDS
#'
#' Carga una lista de archivos .rds y los devuelve como lista nombrada
#'
#' @param archivos Vector con nombres de archivos (sin extensión)
#' @param carpeta Carpeta origen (default: "resultados")
#'
#' @return Lista nombrada con objetos cargados
#'
#' @examples
#' datos <- cargar_resultados(
#'   c("arboles_analisis", "inventario_completo"),
#'   carpeta = "datos_intermedios"
#' )
cargar_resultados <- function(archivos, carpeta = "resultados") {
  
  # Inicializar lista
  resultados <- list()
  
  for (archivo in archivos) {
    # Agregar extensión si no la tiene
    if (!grepl("\\.rds$", archivo)) {
      archivo_completo <- paste0(archivo, ".rds")
    } else {
      archivo_completo <- archivo
    }
    
    # Ruta completa
    ruta <- file.path(carpeta, archivo_completo)
    
    # Verificar existencia
    if (!file.exists(ruta)) {
      warning(sprintf("⚠️  No encontrado: %s", ruta))
      next
    }
    
    # Cargar
    nombre <- gsub("\\.rds$", "", basename(ruta))
    resultados[[nombre]] <- readRDS(ruta)
    
    cat(sprintf("✓ Cargado: %s\n", ruta))
  }
  
  return(resultados)
}


#' Verificar existencia de archivos de resultados
#'
#' @param archivos Vector con nombres de archivos esperados
#' @param carpeta Carpeta donde buscar
#'
#' @return Dataframe con estado de cada archivo
verificar_archivos <- function(archivos, carpeta = "resultados") {
  
  estado <- data.frame(
    archivo = archivos,
    existe = FALSE,
    tamaño_kb = NA,
    fecha_modificacion = as.POSIXct(NA),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(archivos)) {
    archivo <- archivos[i]
    
    # Agregar extensión si no la tiene
    if (!grepl("\\.(csv|rds)$", archivo)) {
      # Buscar con ambas extensiones
      ruta_csv <- file.path(carpeta, paste0(archivo, ".csv"))
      ruta_rds <- file.path(carpeta, paste0(archivo, ".rds"))
      
      if (file.exists(ruta_csv)) {
        ruta <- ruta_csv
      } else if (file.exists(ruta_rds)) {
        ruta <- ruta_rds
      } else {
        next
      }
    } else {
      ruta <- file.path(carpeta, archivo)
    }
    
    if (file.exists(ruta)) {
      info <- file.info(ruta)
      estado$existe[i] <- TRUE
      estado$tamaño_kb[i] <- round(info$size / 1024, 1)
      estado$fecha_modificacion[i] <- info$mtime
    }
  }
  
  return(estado)
}

# ==============================================================================
# FIN DE ARCHIVO
# ==============================================================================
