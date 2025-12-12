#!/bin/bash
# ==============================================================================
# VALIDACIÓN: Verificar que la refactorización funcionó correctamente
# ==============================================================================

set -e

PROJECT_ROOT="/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5"

if [ ! -d "$PROJECT_ROOT" ]; then
    echo "❌ ERROR: No se encontró el directorio del proyecto"
    exit 1
fi

cd "$PROJECT_ROOT"

echo "════════════════════════════════════════════════════════════"
echo "  VALIDACIÓN POST-REFACTORIZACIÓN"
echo "════════════════════════════════════════════════════════════"
echo ""

# 1. Verificar estructura de carpetas
echo "[1/6] Verificando estructura de carpetas..."
CARPETAS_REQUERIDAS=(
    "modelov5/config"
    "modelov5/core"
    "modelov5/analisis"
    "modelov5/utils"
    "modelov5/tests"
    "modelov5/deprecated"
    "modelov5/generadores"
    "modelov5/reportes"
    "modelov5/workflows"
    "modelov5/simulaciones"
)

TODO_OK=true
for carpeta in "${CARPETAS_REQUERIDAS[@]}"; do
    if [ -d "$carpeta" ]; then
        echo "  ✓ $carpeta"
    else
        echo "  ⚠️  Falta: $carpeta"
        TODO_OK=false
    fi
done

# 2. Verificar archivos CORE (en core/)
echo ""
echo "[2/6] Verificando archivos CORE (en core/)..."
ARCHIVOS_CORE=(
    "modelov5/core/10_modelos_crecimiento.R"
    "modelov5/core/11_modelo_mortalidad.R"
    "modelov5/core/12_modelo_reclutamiento.R"
    "modelov5/core/13_simulador_crecimiento.R"
    "modelov5/core/14_optimizador_cortas.R"
    "modelov5/core/15_core_calculos.R"
    "modelov5/core/16_calcular_ica.R"
)

CORE_OK=0
for archivo in "${ARCHIVOS_CORE[@]}"; do
    if [ -f "$archivo" ]; then
        echo "  ✓ $(basename $archivo)"
        CORE_OK=$((CORE_OK + 1))
    else
        echo "  ❌ Falta: $(basename $archivo)"
        TODO_OK=false
    fi
done

# 3. Verificar archivos CONFIG (en config/)
echo ""
echo "[3/6] Verificando archivos CONFIG (en config/)..."
ARCHIVOS_CONFIG=(
    "modelov5/config/00_importar_inventario.R"
    "modelov5/config/01_parametros_configuracion.R"
    "modelov5/config/02_config_especies.R"
    "modelov5/config/03_config_codigos.R"
    "modelov5/config/04_config_simulacion.R"
    "modelov5/config/05_config_programa_cortas.R"
)

CONFIG_OK=0
for archivo in "${ARCHIVOS_CONFIG[@]}"; do
    if [ -f "$archivo" ]; then
        echo "  ✓ $(basename $archivo)"
        CONFIG_OK=$((CONFIG_OK + 1))
    else
        echo "  ⚠️  Falta: $(basename $archivo)"
    fi
done

# 4. Verificar utils
echo ""
echo "[4/6] Verificando UTILS..."
if [ -f "modelov5/utils/io.R" ]; then
    echo "  ✓ utils/io.R existe"
    
    # Verificar que tiene las funciones esperadas
    FUNCIONES=("exportar_csv" "guardar_resultados" "cargar_resultados" "verificar_archivos")
    for func in "${FUNCIONES[@]}"; do
        if grep -q "^$func <- function" modelov5/utils/io.R 2>/dev/null; then
            echo "    ✓ $func()"
        else
            echo "    ⚠️  No se encontró: $func()"
        fi
    done
else
    echo "  ⚠️  utils/io.R no existe (aún no ejecutado Fase 2)"
fi

if [ -f "modelov5/utils/utils_metricas.R" ]; then
    echo "  ✓ utils/utils_metricas.R"
fi

if [ -f "modelov5/utils/utils_validacion.R" ]; then
    echo "  ✓ utils/utils_validacion.R"
fi

# 5. Verificar archivos deprecated
echo ""
echo "[5/6] Verificando archivos DEPRECATED..."
DEPRECADOS=(
    "modelov5/deprecated/20_analisis_descriptivo_old.R"
    "modelov5/deprecated/50_crear_tabla_descriptiva_muestreo.r"
    "modelov5/deprecated/51_EJECUTAR_ANÁLISIS_DESCRIPTIVO.R"
)

MOVED_COUNT=0
for archivo in "${DEPRECADOS[@]}"; do
    if [ -f "$archivo" ]; then
        echo "  ✓ $(basename $archivo)"
        MOVED_COUNT=$((MOVED_COUNT + 1))
    fi
done

if [ $MOVED_COUNT -eq 0 ]; then
    echo "  ℹ️  Ningún archivo en deprecated/ (Fase 1 no ejecutada o archivos no existían)"
elif [ $MOVED_COUNT -lt 3 ]; then
    echo "  ℹ️  Solo $MOVED_COUNT/3 archivos en deprecated/"
fi

# 6. Contar archivos por ubicación
echo ""
echo "[6/6] Contando archivos..."

TOTAL_RAIZ=$(find modelov5/ -maxdepth 1 -name "*.R" -o -name "*.r" 2>/dev/null | wc -l)
TOTAL_CONFIG=$(find modelov5/config -name "*.R" 2>/dev/null | wc -l || echo "0")
TOTAL_CORE=$(find modelov5/core -name "*.R" 2>/dev/null | wc -l || echo "0")
TOTAL_ANALISIS=$(find modelov5/analisis -name "*.R" 2>/dev/null | wc -l || echo "0")
TOTAL_GENERADORES=$(find modelov5/generadores -name "*.R" 2>/dev/null | wc -l || echo "0")
TOTAL_REPORTES=$(find modelov5/reportes -name "*.R" 2>/dev/null | wc -l || echo "0")
TOTAL_WORKFLOWS=$(find modelov5/workflows -name "*.R" -o -name "*.r" 2>/dev/null | wc -l || echo "0")
TOTAL_SIMULACIONES=$(find modelov5/simulaciones -name "*.R" 2>/dev/null | wc -l || echo "0")
TOTAL_OPCIONAL=$(find modelov5/opcional -name "*.R" 2>/dev/null | wc -l || echo "0")
TOTAL_UTILS=$(find modelov5/utils -name "*.R" 2>/dev/null | wc -l || echo "0")
TOTAL_DEPRECATED=$(find modelov5/deprecated -name "*.R" -o -name "*.r" 2>/dev/null | wc -l || echo "0")
TOTAL_TESTS=$(find modelov5/tests -name "*.R" -o -name "*.r" 2>/dev/null | wc -l || echo "0")

echo "  📁 Raíz (modelov5/):     $TOTAL_RAIZ archivos"
echo "  📁 config/:              $TOTAL_CONFIG archivos"
echo "  📁 core/:                $TOTAL_CORE archivos"
echo "  📁 analisis/:            $TOTAL_ANALISIS archivos"
echo "  📁 simulaciones/:        $TOTAL_SIMULACIONES archivos"
echo "  📁 generadores/:         $TOTAL_GENERADORES archivos"
echo "  📁 reportes/:            $TOTAL_REPORTES archivos"
echo "  📁 workflows/:           $TOTAL_WORKFLOWS archivos"
echo "  📁 opcional/:            $TOTAL_OPCIONAL archivos"
echo "  📁 utils/:               $TOTAL_UTILS archivos"
echo "  📁 tests/:               $TOTAL_TESTS archivos"
echo "  📁 deprecated/:          $TOTAL_DEPRECATED archivos"
echo "  ─────────────────────────────"
TOTAL_ORGANIZADOS=$((TOTAL_CONFIG + TOTAL_CORE + TOTAL_ANALISIS + TOTAL_GENERADORES + TOTAL_REPORTES + TOTAL_WORKFLOWS + TOTAL_SIMULACIONES + TOTAL_OPCIONAL + TOTAL_UTILS))
echo "  Total organizados:       $TOTAL_ORGANIZADOS archivos"
echo "  Total en raíz (ideal=0): $TOTAL_RAIZ archivos"

# 7. Resumen final
echo ""
echo "════════════════════════════════════════════════════════════"
echo "  RESUMEN VALIDACIÓN"
echo "════════════════════════════════════════════════════════════"
echo ""

# Estado de carpetas
if [ "$TODO_OK" = true ]; then
    echo "✓ Estructura de carpetas completa"
else
    echo "⚠️  Faltan algunas carpetas"
fi

# Estado de core
if [ "$CORE_OK" -eq 7 ]; then
    echo "✓ Todos los archivos CORE en core/ (7/7)"
else
    echo "⚠️  Archivos CORE: $CORE_OK/7"
fi

# Estado de config
if [ "$CONFIG_OK" -eq 6 ]; then
    echo "✓ Todos los archivos CONFIG en config/ (6/6)"
else
    echo "⚠️  Archivos CONFIG: $CONFIG_OK/6"
fi

# Estado de organización
if [ "$TOTAL_RAIZ" -eq 0 ]; then
    echo "✓ Todos los archivos organizados (0 en raíz)"
elif [ "$TOTAL_RAIZ" -lt 5 ]; then
    echo "⚠️  Casi todos organizados ($TOTAL_RAIZ en raíz)"
else
    echo "ℹ️  Aún hay archivos en raíz: $TOTAL_RAIZ"
fi

# Tests
if [ "$TOTAL_TESTS" -ge 6 ]; then
    echo "✓ Tests reorganizados ($TOTAL_TESTS en tests/)"
elif [ "$TOTAL_TESTS" -gt 0 ]; then
    echo "⚠️  Algunos tests movidos ($TOTAL_TESTS en tests/)"
else
    echo "ℹ️  Tests aún no reorganizados"
fi

# Deprecated
if [ "$TOTAL_DEPRECATED" -ge 2 ]; then
    echo "✓ Archivos obsoletos movidos ($TOTAL_DEPRECATED en deprecated/)"
elif [ "$TOTAL_DEPRECATED" -gt 0 ]; then
    echo "ℹ️  Algunos archivos en deprecated/ ($TOTAL_DEPRECATED)"
else
    echo "ℹ️  Fase 1 (limpieza) no ejecutada o archivos no existían"
fi

echo ""

# Evaluación final
if [ "$TODO_OK" = true ] && [ "$CORE_OK" -eq 7 ] && [ "$CONFIG_OK" -eq 6 ] && [ "$TOTAL_RAIZ" -lt 3 ]; then
    echo "🎉 REFACTORIZACIÓN EXITOSA"
    echo ""
    echo "Siguiente paso - Probar que funciona:"
    echo "  cd $PROJECT_ROOT/modelov5/workflows"
    echo "  Rscript 40_WORKFLOW_COMPLETO.R"
elif [ "$TOTAL_ORGANIZADOS" -gt 25 ]; then
    echo "✓ REORGANIZACIÓN AVANZADA"
    echo ""
    echo "La mayoría de archivos están organizados."
    echo "Algunos archivos pueden quedar en raíz (OK si son pocos)."
    echo ""
    echo "Para testing:"
    echo "  cd $PROJECT_ROOT/modelov5/workflows"
    echo "  Rscript 40_WORKFLOW_COMPLETO.R"
else
    echo "⚠️  REFACTORIZACIÓN INCOMPLETA"
    echo ""
    echo "Fases ejecutadas:"
    [ -d "modelov5/tests" ] && echo "  ✓ Fase 1 (limpieza)"
    [ -f "modelov5/utils/io.R" ] && echo "  ✓ Fase 2 (utils)"
    [ "$CORE_OK" -gt 0 ] && echo "  ✓ Fase 3 (reorganizar) - parcial"
    echo ""
    echo "Ejecuta las fases faltantes según README_COMPLETO.md"
fi

echo ""
