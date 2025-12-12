# SIERRAFOR - Resumen Ejecutivo y Guía de Referencia

## 1. VISIÓN GENERAL

**SIERRAFOR** (Sistema de Gestión Forestal Dinámica para la Sierra Madre Oriental) es un modelo modular de simulación forestal diseñado para generar todos los datos necesarios para un Programa de Manejo Forestal (PMF) conforme a la NOM-152-SEMARNAT.

### Filosofía Central
- **Datos reales de campo** > tablas genéricas regionales
- **Correcciones estructurales** > parches temporales  
- **Realismo biológico** > conveniencia matemática
- **Modelos poblacionales** > fórmulas de interés compuesto

---

## 2. ARQUITECTURA DEL SISTEMA

### Estructura Modular

```
40_WORKFLOW_COMPLETO.R          [Orquestador principal]
│
├── 01_parametros_configuracion.R   [Integrador de CONFIG]
│   ├── 02_config_especies.R        [Biología y alometrías]
│   ├── 03_config_codigos.R         [Códigos SIPLAFOR]
│   ├── 04_config_simulacion.R      [Parámetros demográficos]
│   └── 05_config_programa_cortas.R [Calendario intervenciones]
│
├── 00_importar_inventario.R        [Lectura desde Excel]
├── 15_core_calculos.R              [Funciones puras reutilizables]
│
├── 10_modelos_crecimiento.R        [Chapman-Richards]
├── 11_modelo_mortalidad.R          [Tasa diferencial por dominancia]
├── 12_modelo_reclutamiento.R       [Ingreso nuevos árboles]
├── 13_simulador_crecimiento.R      [Orquestador simulación]
├── 14_optimizador_cortas.R         [Estrategias aprovechamiento]
├── 16_calcular_ica.R               [ICA biológico]
│
├── 20_analisis_descriptivo.R       [Análisis inicial inventario]
├── 30_SIMULACION_10AÑOS_COMPLETA.R [Simulación con cortas]
├── 32_tablas_pmf.R                 [Tablas LaTeX]
└── 33_graficos_pmf.R               [Visualizaciones]
```

### Objeto Central: CONFIG

CONFIG es una lista maestra que centraliza TODA la configuración del modelo:
- Parámetros temporales (periodo, inicio)
- Especies y ecuaciones alométricas
- Tasas de crecimiento, mortalidad, reclutamiento
- Parámetros de corta (DMC, Q-factor, tolerancia)
- Funciones de interpolación

**REGLA CRÍTICA**: NUNCA modificar CONFIG directamente. Siempre usar los archivos `02_`, `03_`, `04_`, `05_`.

---

## 3. FLUJO DE SIMULACIÓN

```
┌──────────────────────────────────┐
│ INVENTARIO INICIAL (t=0)         │
│ • SIPLAFOR estructura            │
│ • Datos individuales árbol       │
│ • Coordenadas UTM disponibles    │
└──────────────┬───────────────────┘
               │
               ▼
┌──────────────────────────────────┐
│ CÁLCULOS INICIALES               │
│ • Volumen (alometrías)           │
│ • Área basal                     │
│ • Expansión a hectárea           │
└──────────────┬───────────────────┘
               │
               ▼
┌──────────────────────────────────┐
│ CICLO ANUAL (n = 1 a 10)         │
│                                  │
│ [1] CRECIMIENTO                  │
│     Chapman-Richards             │
│     Modificadores dominancia     │
│                                  │
│ [2] MORTALIDAD                   │
│     Diferencial por dominancia   │
│     2% base, 6% suprimidos       │
│                                  │
│ [3] RECLUTAMIENTO                │
│     3% tasa base                 │
│     Ingreso D=7.5-12.5cm         │
│                                  │
│ [4] CORTAS (si programado)       │
│     LIOCOURT / ICA / EXISTENCIAS │
│                                  │
│ [5] RECÁLCULO MÉTRICAS           │
│     Vol, AB, densidad, etc.      │
└──────────────┬───────────────────┘
               │
               ▼
┌──────────────────────────────────┐
│ SALIDAS Y REPORTES               │
│ • Tablas LaTeX NOM-152           │
│ • PDFs por rodal                 │
│ • Distribuciones diamétricas     │
│ • Métricas dendrométricas        │
└──────────────────────────────────┘
```

---

## 4. MÉTODOS DE CORTA

### Comparación Estrategias

| Aspecto | LIOCOURT | ICA | EXISTENCIAS |
|---------|----------|-----|-------------|
| **Objetivo** | Estructura J-invertida | Cosechar crecimiento | Reducir existencias |
| **Volumen** | % exceso clases sobrepobladas | % ICA acumulado | % volumen actual |
| **Complejidad** | Alta | Media | Baja |
| **Uso típico** | Manejo irregular | Bosques productivos | Cortas sanitarias |

### LIOCOURT
- Mantiene distribución J-invertida (De Liocourt)
- DMC: Pinus=30cm, Quercus=25cm
- Q-factor: 1.7 (cociente árboles entre clases)
- Tolerancia: ±20%
- **Prioridad**: Suprimidos primero (dom 6→5→4→3→2→1)

### ICA (Incremento Corriente Anual)
- **Innovación clave de SIERRAFOR**: calcula ICA mediante simulación poblacional
- Simula 10 años SIN cortas para derivar ICA biológico
- Corrige vacío metodológico de manuales tradicionales
- Cosecha % del ICA acumulado (típicamente 70-90%)

### EXISTENCIAS
- Corta % del volumen total actual
- Simple, usado para saneamiento
- Sin considerar estructura o crecimiento

---

## 5. PARÁMETROS CLAVE

### Crecimiento Diamétrico Base
```
Pinus:   0.40 cm/año
Quercus: 0.30 cm/año
```
**Modificadores por dominancia**:
- Dom 1,2,4: 100% (árboles sanos vigorosos)
- Dom 3,5: 70% (intermedios/suprimidos)
- Dom 6: 40% (muy suprimidos)
- Dom 7,8,9: 0% (muertos)

### Mortalidad
```
Base: 2% anual
Suprimidos (dom 6): 6%
```

### Reclutamiento
```
Tasa: 3% de densidad actual
Diámetro ingreso: 7.5-12.5 cm
Dominancia: 6 (suprimidos)
Altura: Pinus 3.0m, Quercus 2.5m
```

### Cortas
```
DMC: Pinus 30cm, Quercus 25cm
Q-factor: 1.7
Tolerancia: ±20%
Área parcela: 0.1 ha
```

---

## 6. INNOVACIONES VS MÉTODOS TRADICIONALES

### Cálculo del ICA
**Métodos tradicionales PMF**:
- Asumen ICA sin explicar cálculo primer ciclo
- Usan fórmulas interés compuesto
- Aplican tablas genéricas regionales

**SIERRAFOR**:
- Simula 10 años sin cortas
- Deriva ICA del modelo poblacional completo
- Usa datos reales campo (cores, inventarios)
- Incluye mortalidad y reclutamiento

### Estructura de Datos
**SIPLAFOR tradicional**:
- Procesamiento agregado por parcela/rodal
- Cálculos simplificados

**SIERRAFOR**:
- Datos individuales por árbol
- IDs únicos permanentes
- Trazabilidad completa
- Permite análisis espacial (UTM disponible)

---

## 7. REQUISITOS NOM-152 QUE GENERA

### Tablas Obligatorias (LaTeX)
1. **Posibilidad anual** por especie/rodal
2. **Programa de cortas** cronológico
3. **Distribución de productos** por género (%)
4. **Clasificación de superficies** (producción/protección/restauración)
5. **Proyecciones estructurales** distribución diamétrica
6. **ICA por especie** justificado biológicamente

### Documentación PMF
- Análisis respuesta tratamientos previos
- Descripción biodiversidad y hábitats
- Infraestructura (caminos, monitoreo)
- Análisis económico (opcional pero recomendado)
- Mapas georeferenciados (QGIS)

---

## 8. FUNCIONES CRÍTICAS

### Siempre Usar
```r
filtrar_arboles_vivos(df)           # Antes de análisis poblacionales
calcular_volumen_arbol(d,h,...)     # Con ecuaciones especies-específicas
expandir_a_hectarea(valor, area)    # Para métricas /ha
```

### Nunca Hacer
- Modificar CONFIG directamente
- Olvidar filtrar árboles muertos (dom 7,8,9)
- Usar área total en lugar de área muestreada
- Asumir volúmenes sin recalcular tras cambios d/h

---

## 9. DATOS DE ENTRADA

### Formato SIPLAFOR (Excel)
**Hoja Principal**:
- `arbol_id`: Identificador único
- `rodal`, `muestreo`, `arbol`: Jerarquía
- `nombre_cientifico`: Especie completa
- `genero_grupo`: Pinus / Quercus
- `dominancia`: Código 1-9
- `diametro_normal`: cm (1.30m altura)
- `altura_total`: metros
- Coordenadas UTM (X,Y) [disponible, no usado actualmente]

**Validaciones automáticas**:
- Rangos válidos diámetro (7.5-60cm típico)
- Códigos dominancia válidos
- Especies reconocidas en CONFIG

---

## 10. SALIDAS GENERADAS

### Archivos Principales
```
resultados/
├── analisis_descriptivo.rds      # Estado inicial población
├── resultados_simulacion.rds     # Historia completa 10 años
├── ica_proyeccion.rds            # Cálculos ICA biológico
├── tablas_pmf/                   # LaTeX NOM-152
│   ├── posibilidad_anual.tex
│   ├── programa_cortas.tex
│   └── distribucion_productos.tex
├── reportes_pdf/                 # Por rodal
│   ├── rodal_01.pdf
│   └── ...
└── graficos/                     # Distribuciones, proyecciones
    ├── distribucion_diametrica.png
    └── proyeccion_volumenes.png
```

### Métricas Calculadas
- Densidad (árboles/ha)
- Área basal (m²/ha)
- Volumen total (m³/ha)
- ICA (m³/ha/año)
- Distribución por clases diamétricas
- Composición por especie/género

---

## 11. LIMITACIONES ACTUALES Y MEJORAS PENDIENTES

### ALTA PRIORIDAD: Modelo Reclutamiento
**Estado actual**: Tasa constante 3%

**Mejoras propuestas**:
1. **Factor apertura post-corta**: +80% años 1-3, decae a años 4-7
2. **Factor densidad**: -95% si >250 m³/ha (dosel cerrado)
3. **Factor ganadería**: -20% a -80% según intensidad (datos disponibles F01)

**Datos disponibles**: 
- ✅ Años desde última corta (rastrable)
- ✅ Volumen/ha por rodal (calculable)
- ✅ Intensidad ganadería (F01: `uso_pecuario`)

### Otras Mejoras
- Análisis riesgo incendio (código existe, no integrado)
- Validación parámetros con inventarios sucesivos
- Tests unitarios funciones críticas
- Análisis espacial (coords UTM disponibles)

---

## 12. BUENAS PRÁCTICAS

### Antes de Modificar Código
1. **Leer código completo** relacionado
2. **Entender estructura** y dependencias
3. **Verificar datos entrada** (Excel, RDS)
4. **Nunca inferir** si datos disponibles
5. **Correcciones estructurales** > parches

### Durante Desarrollo
- Usar `filtrar_arboles_vivos()` sistemáticamente
- Mantener IDs únicos permanentes
- Recalcular volúmenes tras cambios d/h
- Validar con datos reales campo
- Documentar asunciones biológicas

### Control de Calidad
- Comparar con inventario inicial
- Verificar balances (ingresos-egresos)
- Revisar distribuciones diamétricas
- Validar volumenes /ha esperados
- Comprobar consistencia entre módulos

---

## 13. EXPECTATIVAS MANUAL PMF

### Contenidos Obligatorios
1. **Objetivos claros** (producción, conservación, restauración)
2. **Inventario detallado** con metodología explícita
3. **Análisis estructural** y caracterización hábitats
4. **Respuesta tratamientos previos** (si existe historial)
5. **Clasificación superficies** NOM-152
6. **Programa cortas** justificado técnicamente
7. **Tratamientos silvícolas** por UMM
8. **Medidas conservación biodiversidad**
9. **Infraestructura** y su impacto
10. **Proyecciones estructurales** 10 años

### Calidad Técnica Esperada
- Métodos reproducibles
- Justificación biológica decisiones
- Datos campo verificables
- Análisis financiero (recomendado)
- Mapas georeferenciados profesionales
- Consideraciones ambientales integradas

---

## 14. COMANDOS RÁPIDOS

### Ejecutar Flujo Completo
```r
source("40_WORKFLOW_COMPLETO.R")
```

### Calcular Solo ICA
```r
source("41_WORKFLOW_calcular_ica.R")
```

### Generar Tablas y Gráficos PMF
```r
source("35_GENERAR_REPORTE_PMF.R")
```

### Cargar Resultados
```r
resultados <- readRDS("resultados/resultados_simulacion.rds")
ica_data <- readRDS("resultados/ica_proyeccion.rds")
```

---

## 15. CONTACTO Y SOPORTE

**Proyecto**: PMF Las Alazanas 2026-2036  
**Cliente**: CONAFOR  
**Normativa**: NOM-152-SEMARNAT-2023  
**Región**: Sierra Madre Oriental, México

**Archivos clave referencia**:
- `sierrafor_doc.md` - Documentación técnica completa
- `Manual PMF clima templado frio.pdf` - Guía oficial CONAFOR
- `inventarioforestal.xlsx` - Datos entrada SIPLAFOR
