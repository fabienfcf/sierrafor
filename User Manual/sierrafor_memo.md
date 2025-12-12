# SIERRAFOR - Memo de Trabajo Futuro

## 📋 TAREAS PENDIENTES PRIORITARIAS

### 1. MEJORAS AL MODELO DE RECLUTAMIENTO 🌱
**Archivo**: `12_modelo_reclutamiento.R`  
**Estado**: ⚠️ Modelo actual demasiado simple  
**Prioridad**: ALTA  
**Documento técnico**: Ver artefacto "Mejoras al Modelo de Reclutamiento"

#### Mejora 1.1: Estimular regeneración después de aclareos
**Efecto**: ↑ 80% regeneración en años 1-3 post-corta, luego decae

**Implementación**:
```r
# Agregar campos a arboles_df:
tuvo_corta_reciente  # TRUE/FALSE
año_ultima_corta     # Año numérico

# Nueva función:
calcular_factor_apertura_dosel(arboles_rodal, año_actual, config)
  → Retorna 1.8 si hubo corta hace 1-3 años
  → Retorna 1.0 si no hay corta o pasaron >7 años
  → Decae linealmente entre años 4-7

# Nuevos parámetros CONFIG:
RECLUT_FACTOR_APERTURA_MAX <- 1.8  # 80% más
RECLUT_AÑOS_EFECTO_APERTURA <- 7   # Duración efecto
```

**Justificación ecológica**:
- Cortas abren dosel → más luz
- Más luz → más germinación y supervivencia
- Efecto temporal: dosel se cierra en 5-10 años

#### Mejora 1.2: Reducir regeneración cuando volumen muy alto
**Efecto**: ↓ 95% regeneración cuando >250 m³/ha

**Implementación**:
```r
# Nueva función:
calcular_factor_densidad(arboles_rodal, config)
  → Retorna 1.0 si 80-150 m³/ha (óptimo)
  → Retorna 0.05 si >250 m³/ha (crítico)
  → Decae linealmente entre 150-250 m³/ha

# Nuevos parámetros CONFIG:
RECLUT_VOL_OPTIMO_MIN <- 80   # m³/ha
RECLUT_VOL_OPTIMO_MAX <- 150  # m³/ha
RECLUT_VOL_CRITICO <- 250     # m³/ha
```

**Justificación ecológica**:
- Alta densidad → dosel cerrado → poca luz
- Poca luz → germinación y supervivencia nula
- Competencia por recursos

#### Mejora 1.3: Reducir regeneración cuando hay ganadería
**Efecto**: ↓ 20-80% según intensidad de pastoreo

**Implementación**:
```r
# Datos disponibles en F01:
uso_pecuario          # 1=Nula, 2=Baja, 3=Moderada, 4=Intensa
perturbacion1/2/3     # Código 8 = "Pastoreo"

# Nueva función:
calcular_factor_ganaderia(inventario_f01, rodal_id, config)
  → Retorna 0.80 si uso_pecuario = 2 (baja)
  → Retorna 0.50 si uso_pecuario = 3 (moderada)
  → Retorna 0.20 si uso_pecuario = 4 (intensa)

# Nuevos parámetros CONFIG:
RECLUT_FACTOR_GANADERIA <- list(
  nula = 1.00,
  baja = 0.80,      # -20%
  moderada = 0.50,  # -50%
  intensa = 0.20    # -80%
)
```

**Justificación ecológica**:
- Pisoteo de plántulas
- Ramoneo de brotes
- Compactación del suelo
- Literatura: reducción 40-80%

#### Combinación de factores:
```r
# Lógica de interacción:
factor_sitio = max(factor_apertura, factor_densidad)
  # Apertura y densidad son alternativos
  
factor_final = factor_sitio × factor_ganaderia
  # Ganadería es multiplicativo (siempre aplica)
  
tasa_ajustada = tasa_base × factor_final
```

**Ejemplo**:
```
Rodal post-corta (año 2) + ganadería moderada:
  factor_apertura = 1.8 (estimula)
  factor_densidad = 1.0 (normal)
  factor_sitio = max(1.8, 1.0) = 1.8
  factor_ganaderia = 0.5 (reduce)
  factor_final = 1.8 × 0.5 = 0.9
  tasa = 0.03 × 0.9 = 0.027
  
  Resultado: Corta estimula pero ganado limita el efecto
```

**Qué hay que hacer**:
1. ✅ Leer documento técnico completo (artefacto creado)
2. Agregar parámetros a `04_config_simulacion.R`
3. Crear `12_modelo_reclutamiento_v2.R` con funciones nuevas
4. Modificar `30_SIMULACION_10AÑOS_COMPLETA.R` para rastrear cortas
5. Actualizar firma de `aplicar_reclutamiento()` para recibir `inventario_f01`
6. Ejecutar tests y validar resultados
7. Calibrar parámetros según necesidad

**Prioridad de implementación**:
- **ALTA**: Mejora 1.1 (apertura post-corta) - más impactante
- **MEDIA**: Mejora 1.2 (densidad) - importante para realismo
- **BAJA-MEDIA**: Mejora 1.3 (ganadería) - si hay pastoreo activo

---

### 2. ANÁLISIS DE RIESGO DE INCENDIO
**Archivo**: `23_Main_incendio.R`  
**Estado**: ✅ Código completo, ⚠️ No integrado en workflow

**Qué hay que hacer**:
1. Descomentar línea en `40_WORKFLOW_COMPLETO.R` (aproximadamente línea 45)
2. Verificar que datos de F06 estén disponibles y completos
3. Ejecutar y revisar resultados
4. Ajustar umbrales de riesgo según características locales

**Output esperado**:
- `analisis_riesgo_incendio_completo.csv`
- Gráficos de riesgo por sitio
- Categorización: BAJO/MODERADO/ALTO/EXTREMO

---

### 2. VALIDACIÓN Y CALIBRACIÓN

**Parámetros a verificar**:

#### Tasas de crecimiento
```r
# Actualmente:
Pinus:   0.40 cm/año
Quercus: 0.30 cm/año

# Acción: Comparar con datos locales
# Verificar si es consistente con observaciones de campo
```

#### Mortalidad
```r
# Actualmente:
Base: 2% anual
Suprimidos: 6% anual

# Acción: Validar con inventarios sucesivos si disponibles
```

#### Q-factor (Liocourt)
```r
# Actualmente: 1.7
# Opciones:
# - 1.5: menos agresivo
# - 1.7: actual (recomendado)
# - 2.0: más agresivo

# Acción: Analizar distribuciones diamétricas resultantes
# Ajustar según objetivo de manejo
```

---

### 3. VERIFICACIONES CRÍTICAS ANTES DE CORTES

**Antes de aplicar programa de cortas**:

1. **Revisar volúmenes calculados**:
   ```r
   # Verificar que volúmenes sean razonables
   arboles_analisis %>%
     group_by(genero_grupo) %>%
     summarise(
       vol_medio = mean(volumen_m3, na.rm=TRUE),
       vol_max = max(volumen_m3, na.rm=TRUE)
     )
   ```

2. **Verificar distribuciones por rodal**:
   ```r
   # Asegurar que hay suficientes árboles >DMC
   arboles_analisis %>%
     group_by(rodal, genero_grupo) %>%
     summarise(
       n_cortables = sum(diametro_normal >= if_else(genero_grupo=="Pinus", 30, 25))
     )
   ```

3. **Validar programa de cortas**:
   ```r
   # Ya implementado en:
   validar_programa_cortas(PROGRAMA_CORTAS, CONFIG)
   ```

---

### 4. MEJORAS ESTRUCTURALES RECOMENDADAS

#### A. Logging y Trazabilidad
```r
# Agregar logging detallado en:
# - 10_modelos_crecimiento.R
# - 11_modelo_mortalidad.R
# - 14_optimizador_cortas.R

# Ejemplo:
log_simulacion <- tibble(
  año = integer(),
  evento = character(),
  rodal = integer(),
  n_afectados = integer(),
  detalle = character()
)
```

#### B. Tests Unitarios
```r
# Crear archivo: tests/test_core_calculos.R
# Probar:
# - calcular_volumen_arbol() con casos extremos
# - filtrar_arboles_vivos() con datos edge case
# - expandir_a_hectarea() con diferentes areas
```

#### C. Manejo de Errores
```r
# Mejorar validación en:
# - importar_inventario(): verificar columnas requeridas
# - aplicar_crecimiento(): catch NaN, Inf
# - calcular_plan_cortas(): verificar disponibilidad de árboles
```

---

### 5. ANÁLISIS ESPACIAL (FUTURO)

**Datos disponibles pero no usados**:
- Coordenadas UTM de cada sitio
- Información de pendiente
- Exposición

**Posibles análisis**:
1. Mapas de densidad/volumen
2. Correlación espacial de crecimiento
3. Optimización de accesibilidad para cortas
4. Análisis de conectividad de hábitat

---

## 🔧 PUNTOS DE ATENCIÓN PARA CORRECCIONES

### Cuando cambies el modelo, considera:

1. **No hacer parches temporales** (según preferencias del usuario)
   - Buscar soluciones sistémicas
   - Entender la lógica completa antes de modificar

2. **Verificar datos de entrada**:
   - Siempre referirse a `inventario_forestal.xlsx`
   - No inferir valores faltantes sin consultar

3. **Mantener coherencia en nomenclatura**:
   ```r
   # Siempre usar:
   genero_grupo  (no "genero" solo)
   dominancia    (no "dom")
   diametro_normal (no "dn" o "dap")
   ```

4. **Documentar cambios en parámetros**:
   ```r
   # Mal:
   Q_FACTOR <- 1.8
   
   # Bien:
   Q_FACTOR <- 1.8  # Cambiado de 1.7 - Ajuste para mayor extracción
                    # Basado en análisis de distribuciones 2025-10-28
   ```

---

## 📊 ANÁLISIS RECOMENDADOS POST-SIMULACIÓN

### 1. Comparación Inicial vs Final
```r
# Ya implementado en: 21_ANALISIS_RESULTADOS_DETALLADO.R
# Verificar:
# - Cambio en densidad por rodal
# - Cambio en volumen por género
# - Efectividad de cortas programadas
```

### 2. Distribuciones Diamétricas
```r
# Verificar que mantienen forma de J invertida
# Después de aplicar LIOCOURT

arboles_final %>%
  filter(genero_grupo == "Pinus") %>%
  ggplot(aes(x=diametro_normal)) +
  geom_histogram(binwidth=5) +
  scale_y_log10()  # Debe verse linear en log scale
```

### 3. Tasas Realizadas vs Esperadas
```r
# Comparar:
# - Mortalidad observada vs 2-6% esperado
# - Reclutamiento observado vs 3% esperado
# - Crecimiento observado vs tasas base

# Si hay desviaciones grandes, revisar parámetros
```

---

## 🚨 ERRORES COMUNES Y SOLUCIONES

### Error: "DOM 6 no encontrada"
**Causa**: Problema en join de códigos dominancia  
**Solución**: Verificar que `CODIGOS_DOMINANCIA` tiene columna `codigo` (no `codigo_dom`)

### Error: "Volumen NA después de crecimiento"
**Causa**: Parámetros alométricos faltantes  
**Solución**: 
```r
# Verificar que árbol tiene tipo, a, b, c
arboles_analisis %>%
  filter(is.na(tipo) | is.na(a) | is.na(b) | is.na(c))
```

### Warning: "No hay árboles candidatos para corta"
**Causa**: DMC muy alto o rodal joven  
**Solución**: 
- Revisar DMC en `05_config_programa_cortas.R`
- Verificar distribución diamétrica del rodal
- Considerar reducir d_min específico para ese rodal

### Error: "num_muestreos_realizados no existe"
**Causa**: Falta información de rodales en UMM  
**Solución**: Verificar que tabla UMM se importó correctamente

---

## 💡 MEJORES PRÁCTICAS

### Antes de ejecutar simulación completa:
1. ✅ Validar CONFIG con `validar_configuracion(CONFIG)`
2. ✅ Revisar programa de cortas con `validar_programa_cortas()`
3. ✅ Inspeccionar arboles_analisis: nrow(), summary(), glimpse()
4. ✅ Hacer prueba corta (1-2 años) antes de 10 años completos

### Durante desarrollo:
1. 🔍 Usar funciones de `15_core_calculos.R` siempre que sea posible
2. 📝 Comentar decisiones de diseño importantes
3. ⚠️ Agregar warnings informativos (no errors) para condiciones edge
4. ✨ Mantener código limpio y bien indentado

### Al modificar parámetros:
1. 💾 Guardar versión anterior con comentario
2. 📊 Ejecutar análisis comparativo
3. 📄 Documentar justificación del cambio
4. 🧪 Probar con datos de prueba primero

---

## 📚 REFERENCIAS IMPORTANTES

### Ecuaciones clave para revisar:
- Chapman-Richards: `02_config_especies.R` líneas 80-110
- Volumen: `15_core_calculos.R` líneas 50-90
- Liocourt: `14_optimizador_cortas.R` líneas 20-70

### Archivos que generan output final:
- `22_VERIFICACION_TABLAS_LATEX.R`: Tablas para PMF
- `21_ANALISIS_RESULTADOS_DETALLADO.R`: Gráficos comparativos
- `30_SIMULACION_10AÑOS_COMPLETA.R`: Historial y métricas

### Datos guardados importantes:
```
resultados/
├── analisis_descriptivo.rds       [Estado inicial]
├── historial_completo_10años.rds  [Año por año]
├── metricas_10años.rds            [Estadísticas agregadas]
└── registro_cortas.rds            [Árboles cortados]
```

---

## 🎯 OBJETIVO FINAL

**Generar Programa de Manejo Forestal conforme NOM-152**:
- ✅ Inventario inicial caracterizado
- ✅ Proyección 10 años validada
- ✅ Programa de cortas optimizado
- ✅ Tablas LaTeX listas para insertar
- ⚠️ Análisis de riesgo pendiente
- 📈 Monitoreo post-implementación recomendado

---

**Última actualización**: Octubre 2025  
**Versión modelo**: SIERRAFOR v2.0  
**Predio**: Ejido Las Alazanas  
**Período**: 2026-2036