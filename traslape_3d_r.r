# ==============================================================================
# ÍNDICE DE TRASLAPE 3D DE COPAS PARA SIERRAFOR
# Basado en: Zambrano et al. (2019) J Ecology, Sun et al. (2020) Sustainability
# ==============================================================================

library(dplyr)
library(tidyr)

# ==============================================================================
# 1. FUNCIÓN: ÁREA DE INTERSECCIÓN ENTRE DOS CÍRCULOS (2D)
# ==============================================================================

area_interseccion_circulos <- function(x1, y1, r1, x2, y2, r2) {
  #' Calcula área exacta de intersección entre dos círculos
  #' 
  #' @param x1,y1 Coordenadas centro círculo 1
  #' @param r1 Radio círculo 1
  #' @param x2,y2 Coordenadas centro círculo 2  
  #' @param r2 Radio círculo 2
  #' @return Área de intersección en unidades cuadradas
  
  d <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  
  # Caso 1: No se tocan
  if (d >= r1 + r2) return(0)
  
  # Caso 2: Uno dentro del otro
  if (d <= abs(r1 - r2)) return(pi * min(r1, r2)^2)
  
  # Caso 3: Intersección parcial
  alpha1 <- 2 * acos((d^2 + r1^2 - r2^2) / (2 * d * r1))
  alpha2 <- 2 * acos((d^2 + r2^2 - r1^2) / (2 * d * r2))
  
  area1 <- 0.5 * r1^2 * (alpha1 - sin(alpha1))
  area2 <- 0.5 * r2^2 * (alpha2 - sin(alpha2))
  
  return(area1 + area2)
}

# ==============================================================================
# 2. FUNCIÓN: TRASLAPE VERTICAL
# ==============================================================================

traslape_vertical <- function(min1, max1, min2, max2) {
  #' Verifica si dos rangos verticales se traslapan
  #' 
  #' @param min1,max1 Límites inferior y superior del rango 1
  #' @param min2,max2 Límites inferior y superior del rango 2
  #' @return Lista con: traslapan (lógico), altura_traslape (numérico)
  
  if (max1 < min2 || max2 < min1) {
    return(list(traslapan = FALSE, altura_traslape = 0))
  }
  
  overlap_min <- max(min1, min2)
  overlap_max <- min(max1, max2)
  altura <- overlap_max - overlap_min
  
  return(list(traslapan = TRUE, altura_traslape = altura))
}

# ==============================================================================
# 3. FUNCIÓN PRINCIPAL: ÍNDICE DE TRASLAPE 3D
# ==============================================================================

calcular_traslape_3d <- function(arboles_sitio) {
  #' Calcula índices de traslape 3D para todos los árboles de un sitio
  #' 
  #' @param arboles_sitio data.frame con columnas:
  #'   - arbol_id: identificador único
  #'   - x, y: coordenadas (m)
  #'   - radio_copa: radio promedio de copa (m)
  #'   - altura_total: altura total (m)
  #'   - altura_copa: altura al inicio de copa (m)
  #'   - dn: diámetro normal (cm)
  #' @return data.frame con índices por árbol
  
  n_arboles <- nrow(arboles_sitio)
  
  # Calcular área de copa para cada árbol
  arboles_sitio <- arboles_sitio %>%
    mutate(area_copa = pi * radio_copa^2)
  
  # Inicializar resultados
  resultados <- arboles_sitio %>%
    select(arbol_id, dn, altura_total, area_copa) %>%
    mutate(
      area_traslapada_2d = 0,
      area_traslapada_3d = 0,
      volumen_traslapado_3d = 0,
      n_competidores_2d = 0,
      n_competidores_3d = 0,
      indice_traslape_2d = 0,
      indice_traslape_3d = 0,
      competidores = list(character(0))
    )
  
  # Calcular traslapes para cada par
  for (i in 1:n_arboles) {
    focal <- arboles_sitio[i, ]
    
    for (j in 1:n_arboles) {
      if (i == j) next
      
      comp <- arboles_sitio[j, ]
      
      # ===== TRASLAPE HORIZONTAL (2D) =====
      area_int_2d <- area_interseccion_circulos(
        focal$x, focal$y, focal$radio_copa,
        comp$x, comp$y, comp$radio_copa
      )
      
      if (area_int_2d > 0.01) {  # Umbral mínimo 0.01 m²
        resultados$area_traslapada_2d[i] <- 
          resultados$area_traslapada_2d[i] + area_int_2d
        resultados$n_competidores_2d[i] <- 
          resultados$n_competidores_2d[i] + 1
        
        # ===== TRASLAPE VERTICAL (3D) =====
        focal_min <- focal$altura_copa
        focal_max <- focal$altura_total
        comp_min <- comp$altura_copa
        comp_max <- comp$altura_total
        
        vertical <- traslape_vertical(focal_min, focal_max, comp_min, comp_max)
        
        if (vertical$traslapan) {
          # Volumen de traslape (aproximación cilíndrica)
          vol_traslape <- area_int_2d * vertical$altura_traslape
          
          resultados$area_traslapada_3d[i] <- 
            resultados$area_traslapada_3d[i] + area_int_2d
          resultados$volumen_traslapado_3d[i] <- 
            resultados$volumen_traslapado_3d[i] + vol_traslape
          resultados$n_competidores_3d[i] <- 
            resultados$n_competidores_3d[i] + 1
          
          # Guardar ID del competidor
          resultados$competidores[[i]] <- c(
            resultados$competidores[[i]], 
            as.character(comp$arbol_id)
          )
        }
      }
    }
    
    # Calcular índices (% de copa traslapada)
    resultados$indice_traslape_2d[i] <- 
      (resultados$area_traslapada_2d[i] / focal$area_copa) * 100
    resultados$indice_traslape_3d[i] <- 
      (resultados$area_traslapada_3d[i] / focal$area_copa) * 100
  }
  
  return(resultados)
}

# ==============================================================================
# 4. FUNCIÓN: MÉTRICAS A NIVEL SITIO
# ==============================================================================

metricas_sitio <- function(resultados_arboles, arboles_sitio) {
  #' Calcula métricas agregadas del sitio
  #' 
  #' @param resultados_arboles Output de calcular_traslape_3d()
  #' @param arboles_sitio data.frame original
  #' @return Lista con métricas del sitio
  
  # Cobertura de copas (Crown Competition Factor - CCF)
  area_total_copas <- sum(arboles_sitio$area_copa, na.rm = TRUE)
  radio_sitio <- max(sqrt(arboles_sitio$x^2 + arboles_sitio$y^2) + 
                     arboles_sitio$radio_copa, na.rm = TRUE)
  area_sitio <- pi * radio_sitio^2
  ccf <- (area_total_copas / area_sitio) * 100
  
  # Índice de traslape del sitio
  # Área única traslapada (suma pares sin duplicar)
  pares_traslape <- 0
  for (i in 1:(nrow(arboles_sitio)-1)) {
    for (j in (i+1):nrow(arboles_sitio)) {
      focal <- arboles_sitio[i, ]
      comp <- arboles_sitio[j, ]
      area_int <- area_interseccion_circulos(
        focal$x, focal$y, focal$radio_copa,
        comp$x, comp$y, comp$radio_copa
      )
      pares_traslape <- pares_traslape + area_int
    }
  }
  
  indice_traslape_sitio <- (pares_traslape / area_total_copas) * 100
  
  # Proporción de árboles con competencia
  prop_competencia <- mean(resultados_arboles$n_competidores_3d > 0) * 100
  
  return(list(
    ccf = ccf,
    area_sitio = area_sitio,
    area_copas = area_total_copas,
    indice_traslape_sitio = indice_traslape_sitio,
    prop_arboles_competencia = prop_competencia,
    n_arboles = nrow(arboles_sitio),
    interpretacion_ccf = case_when(
      ccf < 40 ~ "Subpoblado",
      ccf > 70 ~ "Sobrepoblado", 
      TRUE ~ "Óptimo"
    )
  ))
}

# ==============================================================================
# 5. EJEMPLO DE USO CON F03
# ==============================================================================

# Supongamos que ya importaste F03 así:
# F03 <- read.xlsx("inventario_forestal.xlsx", sheet = "F03")

preparar_datos_f03 <- function(F03) {
  #' Prepara datos de F03 para cálculo de traslape
  #' 
  #' @param F03 data.frame con estructura SIPLAFOR
  #' @return data.frame procesado por sitio
  
  # Propagar SITIO hacia abajo (fill)
  F03 <- F03 %>%
    fill(SITIO, .direction = "down")
  
  # Preparar datos
  arboles <- F03 %>%
    filter(!is.na(`47.DN`), `47.DN` > 0) %>%
    mutate(
      # Convertir azimut-distancia a XY
      azimut_rad = (`58.AZT` / 180) * pi,
      x = `59.DIST` * sin(azimut_rad),
      y = `59.DIST` * cos(azimut_rad),
      
      # Radio de copa (promedio N-S y E-O)
      radio_copa = (`50.DCNS` + `51.DCEO`) / 4,
      
      # Altura de copa y total
      altura_total = `48.AT`,
      altura_copa = `49.AC`,
      
      # DN
      dn = `47.DN`,
      
      # ID único
      arbol_id = paste0("S", SITIO, "_A", row_number())
    ) %>%
    select(SITIO, arbol_id, x, y, radio_copa, 
           altura_total, altura_copa, dn)
  
  return(arboles)
}

# ==============================================================================
# APLICACIÓN COMPLETA
# ==============================================================================

# 1. Preparar datos
# arboles <- preparar_datos_f03(F03)

# 2. Calcular para cada sitio
# resultados_totales <- arboles %>%
#   group_by(SITIO) %>%
#   group_modify(~ calcular_traslape_3d(.x)) %>%
#   ungroup()

# 3. Métricas por sitio
# metricas_por_sitio <- arboles %>%
#   group_by(SITIO) %>%
#   group_modify(~ {
#     res_arboles <- calcular_traslape_3d(.x)
#     met_sitio <- metricas_sitio(res_arboles, .x)
#     tibble(
#       ccf = met_sitio$ccf,
#       indice_traslape = met_sitio$indice_traslape_sitio,
#       prop_competencia = met_sitio$prop_arboles_competencia,
#       interpretacion = met_sitio$interpretacion_ccf
#     )
#   }) %>%
#   ungroup()

# ==============================================================================
# EJEMPLO CON DATOS FICTICIOS
# ==============================================================================

# Datos del sitio 35 (real de F03)
ejemplo_sitio <- data.frame(
  arbol_id = 1:5,
  x = c(0, 1.59, -1.08, 2.88, 3.54),
  y = c(0, -2.54, -2.80, -3.20, -4.21),
  radio_copa = c(2.75, 1.25, 1.25, 2.00, 2.75),
  altura_total = c(9, 6, 7, 8, 9),
  altura_copa = c(4, 0, 0, 4, 2),
  dn = c(22, 7.5, 7.5, 17.5, 17.5)
)

# Calcular traslapes
resultados <- calcular_traslape_3d(ejemplo_sitio)
print(resultados)

# Métricas del sitio
metricas <- metricas_sitio(resultados, ejemplo_sitio)
print(metricas)

# ==============================================================================
# INTERPRETACIÓN DE RESULTADOS
# ==============================================================================

# indice_traslape_2d: % de copa del árbol traslapada horizontalmente
# indice_traslape_3d: % de copa traslapada en 3D (horizontal Y vertical)
# n_competidores_3d: número de vecinos con traslape 3D efectivo
# volumen_traslapado_3d: volumen total de traslape (m³)

# UMBRALES SUGERIDOS:
# - Índice < 20%: competencia baja → árbol dominante
# - Índice 20-50%: competencia moderada → candidato a liberación
# - Índice 50-100%: competencia alta → árbol suprimido
# - Índice > 100%: copas múltiples sobre el árbol → eliminar o dejar
