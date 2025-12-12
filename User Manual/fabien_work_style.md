# Estilo de Trabajo de Fabián - Guía de Referencia

## 1. CUESTIONAMIENTO METODOLÓGICO DIRECTO

### Características
Fabián **cuestiona la lógica** directamente, sin rodeos. Identifica errores conceptuales con precisión.

### Ejemplos Reales

**Ejemplo 1: Q-factor invertido**
```
Fabián: "Me parece que calculas mal el q factor (al revés)"

Contexto: Claude calculaba q = N_i+1 / N_i 
Error detectado: Debía ser q = N_i / N_i+1 (clase menor/mayor)
Evidencia: Los valores (0.589 ≈ 1/1.7) confirmaban que estaba invertido
```

**Ejemplo 2: Lógica ilógica**
```
Fabián: "Es illogico:"
[muestra iteraciones donde N_ref baja pero volumen SUBE]

Iter 1: N_ref=30 → Vol exceso=7.48 m³ (148.5%)
Iter 2: N_ref=15 → Vol exceso=12.37 m³ (245.6%)
Iter 3: N_ref=8  → Vol exceso=14.14 m³ (280.7%)

Conclusión de Fabián: "Estás confundiendo dos cosas diferentes:
1. Volumen objetivo (ICA): Cuánto PUEDO cortar
2. Liocourt: DÓNDE debo cortar
No puedes hacer que coincidan ajustando N_ref"
```

**Ejemplo 3: Verificación de razonamiento**
```
Fabián: "Seguro? 'La segunda vez solo recalcula los ~6 reclutas nuevos'"

Claude había dicho: "actualizar_volumenes() solo recalcula reclutas"
Realidad: Recalcula TODOS los árboles
Fabián fuerza a Claude a verificar el código real, no asumir
```

---

## 2. VERIFICACIÓN MATEMÁTICA RIGUROSA

### Características
Fabián **verifica cálculos manualmente** y compara con datos fuente. Detecta errores de factor de expansión, áreas, volúmenes.

### Ejemplos Reales

**Ejemplo 1: Área de parcelas incorrecta**
```
Fabián: "Los sitios de muestreo son de 1,000 m² y no 500m²"

Impacto identificado:
- Con 500m² → Factor = 20
- Con 1000m² → Factor = 10
- Conclusión: "El modelo está DUPLICANDO todas las estimaciones /ha"

Raíz del problema: Hardcoded value 0.05 en lugar de 0.10
```

**Ejemplo 2: Lógica de volumen/ha**
```
Contexto: Rodal 6 con 4 parcelas de 0.05ha = 0.2ha total
Volumen total = 13.85 m³

Fabián plantea: "Entonces volumen/ha debería ser 13.85*4? 
Busca en core_calculos para encontrar respuesta... creo"

Razonamiento que presenta Fabián:
Tu lógica (incorrecta): 13.85/0.2 = 69.25 m³/ha
Lógica correcta del sistema: Usa factor expansión por parcela

→ Obliga a Claude a explicar la diferencia fundamental entre:
  - Volumen total del rodal (suma mediciones)
  - Volumen por hectárea (extrapolación)
```

**Ejemplo 3: Columnas incorrectas**
```
Fabián: "❌ Error: In argument: `dmc = case_when(...)`"

Análisis de Fabián:
"Además, hay un error [en el código]"
Muestra que usa `genero` cuando debería ser `genero_grupo`

Verificación sugerida:
"arboles_vivos %>% select(starts_with('gen')) %>% names()"
```

---

## 3. EXIGENCIA DE ANÁLISIS PROFUNDO

### Características
Fabián pide **análisis exhaustivos** y comprensión completa antes de modificar código.

### Ejemplos Reales

**Ejemplo 1: Revisión completa del proyecto**
```
Fabián: "Checa en detalle todos los archivos del proyecto. 
Quiero que me los describas uno por uno. Función por función.
Quiero que me describas las rutas de cálculo que hace el modelo, en detalle.
Este documento es la base ya que te preguntaré que lo leas antes de hacer nuevos cambios."

Énfasis especial solicitado:
* Cómo crece un árbol individual
* Cómo cambia una población
* Cuáles son las estrategias de corte
```

**Ejemplo 2: Búsqueda profunda**
```
Fabián: "como seguir mejorando el modelo? Take your time for in depth search"

→ No acepta respuestas rápidas
→ Espera investigación en documentación del proyecto
→ Espera búsquedas web de literatura científica
→ Espera análisis comparativo
```

**Ejemplo 3: Diagnóstico estructural**
```
Fabián: "Explica la lógica, la inserción con los módulos 0 a 6 (+ core_calculos).
Checa redundancias, inconsistencias, la lógica forestal."

→ No solo "haz que funcione"
→ Busca entender el sistema completo
→ Identifica deuda técnica
→ Verifica coherencia biológica
```

---

## 4. PREFERENCIA POR CORRECCIONES ESTRUCTURALES

### Características
**Rechaza parches temporales**, exige soluciones sistémicas. Elimina hardcoded values.

### Ejemplos Reales

**Ejemplo 1: Single point of truth**
```
Fabián sobre valores hardcoded:
"en core_calculos.R, quiero que se haga referencia a AREA_PARCELA_HA 
(y no escribir el numero que es fuente de error)"

Principio aplicado: "Un único punto de verdad"
- Si cambias área parcela, solo modificas CONFIG$area_parcela_ha
- Todo el sistema usa la misma configuración
- Mantiene consistencia
```

**Ejemplo 2: Búsqueda sistemática de hardcoded**
```
Fabián: "Deberías revisar TODOS los módulos buscando otros valores hardcoded:
grep -n '0.05' R5/*.R
grep -n '1.5' R5/*.R  # Q_FACTOR
grep -n '30\|25' R5/*.R  # DMC potenciales"

Identificación de deuda técnica:
1. Identificar todos los valores hardcoded
2. Verificar si ya están en CONFIG
3. Reemplazarlos por referencias a CONFIG
4. Agregar parámetro config = CONFIG a funciones
```

**Ejemplo 3: Refactorización vs parches**
```
Contexto: Múltiples lugares con `!dominancia %in% c(7,8,9)`

Fabián rechaza reemplazar case-by-case
Exige: "Usar filtrar_arboles_vivos() sistemáticamente"

Patrones de reemplazo sistemáticos:
1. En pipe con filter → filtrar_arboles_vivos(df)
2. Contar con sum → nrow(filtrar_arboles_vivos(df))
3. Dentro de pipeline → filtrar_arboles_vivos(df) %>% group_by(...)
```

---

## 5. COMUNICACIÓN DIRECTA SIN FLORITURAS

### Características
Fabián es **conciso, directo, sin halagos**. Usa preguntas cortas para confirmar entendimiento.

### Ejemplos Reales

**Ejemplo 1: Confirmación de comprensión**
```
Fabián: "o sea asi?"
[muestra código propuesto]

→ No elabora, va directo al código
→ Pide confirmación binaria: ¿sí o no?
→ No quiere explicaciones largas, quiere "✓ correcto" o "❌ incorrecto"
```

**Ejemplo 2: Señalamiento de error**
```
Fabián: "❌ NO, eso está MAL."

[Explica por qué sin rodeos]

Claude había dicho algo incorrecto
Fabián lo corta inmediatamente
→ Valora la honestidad directa
```

**Ejemplo 3: Cuestionamiento simple**
```
Fabián: "Seguro?"

Una sola palabra
Fuerza a Claude a:
1. Revisar su afirmación
2. Verificar en el código
3. Admitir si se equivocó
```

**Ejemplo 4: Sin halagos**
```
De las preferencias de usuario:
"Pas de flatteries" (Sin halagos)

→ No quiere: "¡Excelente pregunta!" 
→ No quiere: "Tienes toda la razón, eres muy perspicaz"
→ Quiere: Respuesta directa al punto
```

---

## 6. METODOLOGÍA DE VALIDACIÓN

### Características
Fabián **siempre vuelve a los datos origen**. Compara output con expectativa. Rastrea la cadena de cálculos.

### Ejemplos Reales

**Ejemplo 1: Referencia a datos fuente**
```
Fabián: "Si no estamos seguros de los datos de entrada, 
referirse a inventarioforestal.xlsx o arboles_analisis 
en lugar de inferir cosas."

→ No asumir
→ No inferir
→ Verificar en la fuente
```

**Ejemplo 2: Trazabilidad de cálculos**
```
Cuando detectó error en volumen/ha:
1. Identificó discrepancia en output
2. Trazó back: volumen total → área muestreada → factor expansión
3. Encontró raíz: área parcela incorrecta (500 vs 1000 m²)
4. Calculó impacto: duplicación de TODAS las métricas /ha
```

**Ejemplo 3: Verificación cruzada**
```
Para problema de dominancia en árboles muertos:
"Se aplican los crecimientos después de un filtrado de dominancia 
(en varias partes del código: aquí no?)"

→ Identifica que el filtro debe estar ANTES del crecimiento
→ Busca en múltiples módulos
→ Verifica consistencia del patrón
```

---

## 7. DETECCIÓN DE ERRORES CONCEPTUALES

### Características
Fabián identifica **confusiones fundamentales** en la lógica del modelo, no solo bugs de código.

### Ejemplos Reales

**Ejemplo 1: Confusión ICA vs Liocourt**
```
Problema detectado por Fabián:
"El problema es conceptual profundo: estás confundiendo dos cosas diferentes:
1. Volumen objetivo (ICA): Cuánto PUEDO cortar sosteniblemente
2. Liocourt: DÓNDE debo cortar para mantener estructura balanceada"

Implicación: "No puedes hacer que coincidan ajustando N_ref"

→ No es un bug de código
→ Es un error en el diseño del método
→ Requiere repensar la aproximación completa
```

**Ejemplo 2: Actualización redundante de volúmenes**
```
Claude propuso: "Actualizar volúmenes DOS veces (después de crecimiento Y reclutamiento)"

Fabián: "Seguro? 'La segunda vez solo recalcula los ~6 reclutas nuevos'"

Análisis realizado:
- actualizar_volumenes() recalcula TODOS los árboles, no solo reclutas
- aplicar_crecimiento_anual() YA calcula volúmenes internamente
- Conclusión: Calcular una sola vez al final es más eficiente

→ Detectó ineficiencia conceptual
→ Forzó verificación en el código
→ Corrigió el diseño del flujo
```

**Ejemplo 3: Tratamiento de árboles muertos**
```
Fabián: "la simulación se está haciendo sobre árbol de dominancia 7 
lo que no se vale porque está muerto"

Razonamiento:
- Dominancia 7,8,9 = muertos
- No deben crecer
- El filtro debe estar ANTES de aplicar crecimiento
- No DESPUÉS

→ Error lógico en el orden de operaciones
→ No es bug sintáctico
→ Es violación de principio biológico
```

---

## 8. REACCIÓN A ERRORES DE CLAUDE

### Características
Fabián **valora la admisión de errores**. Señala inmediatamente cuando Claude se equivoca.

### Ejemplos Reales

**Ejemplo 1: Corrección inmediata**
```
Claude: "La segunda vez solo recalcula los ~6 reclutas nuevos"

Fabián: "Seguro?"

Claude: "❌ NO, me equivoqué. Tienes toda la razón en cuestionarlo."
[Claude verifica el código]
Claude: "Recalcula el volumen de TODOS los árboles, no solo los reclutas nuevos"

Respuesta de Fabián: [Acepta la corrección, continúa]

→ Valora la honestidad
→ No castiga el error, castiga la no-verificación
→ Aprecia cuando Claude admite equivocación
```

**Ejemplo 2: Corrección con evidencia**
```
Claude propuso método incorrecto para Liocourt

Fabián: "Es illogico:" 
[Muestra tabla de iteraciones contradictorias]

Claude: "Tienes toda la razón, es completamente ilógico. 
El problema es conceptual profundo..."

→ Fabián presenta evidencia clara
→ Claude debe admitir el error fundamental
→ Debe proponer método correcto conceptualmente
```

---

## 9. PRIORIDADES EN ORDEN

### Metodología establecida por Fabián:

1. **Entender estructura completa antes de modificar**
   - "Checa en detalle todos los archivos"
   - "Función por función"
   - "Este documento es la base antes de hacer cambios"

2. **Buscar causa raíz, no síntomas**
   - No parchar el error superficial
   - Rastrear hasta el origen
   - Ejemplo: Error de columna → Raíz: hardcoded value

3. **Eliminar valores hardcoded (single point of truth)**
   - Centralizar en CONFIG
   - Un solo lugar para modificar
   - Propagación automática

4. **Mantener coherencia biológica**
   - Árboles muertos no crecen
   - Volúmenes deben ser consistentes con d y h
   - ICA es biológico, no arbitrario

5. **Correcciones sistémicas sobre parches**
   - No: "Arregla este caso"
   - Sí: "Arregla todos los casos similares de esta manera"

---

## 10. PHRASES CLAVE Y SU SIGNIFICADO

### Cuestionamiento
- **"Seguro?"** → Verifica tu afirmación en el código real
- **"Me parece que..."** → He detectado un error, pero te doy chance de verificar
- **"Es ilógico"** → Error conceptual fundamental, no solo de implementación
- **"explica mejor"** → Tu explicación fue superficial o confusa

### Requisitos
- **"Checa en detalle"** → Análisis exhaustivo requerido
- **"función por función"** → Documentación completa necesaria
- **"en profundidad"** → No respuestas rápidas, investigar a fondo
- **"Take your time for in depth search"** → Usa todas las herramientas disponibles

### Validación
- **"Busca en..."** → Ve a la fuente de datos, no asumas
- **"Referirse a inventarioforestal.xlsx"** → Los datos reales son la autoridad
- **"o sea así?"** → Confirmación de entendimiento, responder sí/no

### Corrección
- **"No queremos parches"** → Solución sistémica requerida
- **"queremos corregir la estructura"** → Problema de diseño, no de código
- **"Checa redundancias, inconsistencias"** → Deuda técnica debe eliminarse

---

## 11. EJEMPLOS DE INTERACCIONES TÍPICAS

### Patrón 1: Detección de Error
```
Fabián: [Muestra output con error]
        "❌ Error: In argument: `dmc = case_when(...)`"
        "Además, hay un error"

Claude: [Debe identificar causa y proponer fix]

Fabián: [Confirma o sigue cuestionando]
```

### Patrón 2: Cuestionamiento Metodológico
```
Fabián: "Es illogico:"
        [Muestra evidencia del problema]

Claude: [Debe admitir error Y explicar causa conceptual]

Fabián: [Acepta o profundiza más]
```

### Patrón 3: Solicitud de Análisis
```
Fabián: "Checa [X] en detalle. Función por función."

Claude: [Análisis exhaustivo con búsquedas en proyecto]

Fabián: [Pregunta específica basada en el análisis]
```

### Patrón 4: Confirmación Rápida
```
Fabián: "o sea así?" [código]

Claude: "✓ Sí, correcto" O "❌ No, [explicación]"

Fabián: [Implementa o ajusta]
```

---

## 12. LO QUE FABIÁN VALORA

### ✅ Aprecia:
- Admisión honesta de errores
- Verificación en código antes de afirmar
- Explicaciones con evidencia (tablas, cálculos)
- Soluciones sistémicas
- Análisis profundos cuando se piden
- Respuestas directas sin rodeos
- Coherencia biológica en modelos

### ❌ Rechaza:
- Halagos y florituras
- Asumir sin verificar
- Parches temporales
- Explicaciones superficiales
- Valores hardcoded sin justificación
- Respuestas que no admiten errores claros
- Lógica que viola principios forestales

---

## 13. APLICACIÓN PRÁCTICA

### Cuando Fabián pregunta algo:

1. **Si es verificable** → Buscar en proyecto/código PRIMERO
2. **Si es cuestionamiento** → Revisar la lógica, probablemente hay error
3. **Si dice "Seguro?"** → Claude se equivocó, verificar y admitir
4. **Si pide análisis profundo** → Usar múltiples búsquedas, web_search si necesario
5. **Si muestra error** → Rastrear causa raíz, no solo el síntoma

### Cuando propones solución a Fabián:

1. **Mostrar evidencia** (output de código, cálculos)
2. **Explicar razonamiento** (¿por qué esta solución?)
3. **Identificar impacto** (¿qué más se afecta?)
4. **Ser directo** (no: "podríamos tal vez...", sí: "la solución es...")
5. **Si hay duda** (admitirlo y ofrecer verificar más)

---

## RESUMEN EJECUTIVO

**Fabián es un investigador forestal que:**
- Cuestiona la metodología directamente
- Verifica cálculos matemáticos manualmente  
- Exige análisis profundos y exhaustivos
- Prefiere correcciones estructurales sobre parches
- Se comunica de forma directa sin florituras
- Vuelve siempre a los datos fuente
- Detecta errores conceptuales, no solo bugs
- Valora la admisión honesta de errores
- Prioriza coherencia biológica en modelos
- Rechaza valores hardcoded y busca "single point of truth"

**Cómo trabajar efectivamente con Fabián:**
1. Lee todo el contexto antes de responder
2. Verifica en el código antes de afirmar
3. Admite errores honestamente cuando los cometes
4. Busca soluciones sistémicas, no parches
5. Sé directo y conciso
6. Cuando dudes, ofrece verificar más
7. Prioriza coherencia biológica sobre conveniencia matemática
8. Elimina hardcoded values sistemáticamente