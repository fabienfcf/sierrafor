#!/bin/bash
# ==============================================================================
# VALIDACIÓN: Verificar que la refactorización no rompió nada
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
echo "[1/5] Verificando estructura de carpetas..."
CARPETAS_REQUERIDAS=(
    "modelov5/config"
    "modelov5/core"
    "modelov5/analisis"
    "modelov5/utils"
    "modelov5/tests"
    "modelov5/deprecated"
)

TODO_OK=true
for carpeta in "${CARPETAS_REQUERIDAS[@]}"; do
    if [ -d "$carpeta" ]; then
        echo "  ✓ $carpeta"
    else
        echo "  ❌ Falta: $carpeta"
        TODO_OK=false
    fi
done

# 2. Verificar archivos core
echo ""
echo "[2/5] Verificando archivos core..."
ARCHIVOS_CORE=(
    "modelov5/15_core_calculos.R"
    "modelov5/10_modelos_crecimiento.R"
    "modelov5/11_modelo_mortalidad.R"
    "modelov5/12_modelo_reclutamiento.R"
    "modelov5/13_simulador_crecimiento.R"
    "modelov5/14_optimizador_cortas.R"
    "modelov5/16_calcular_ica.R"
)

for archivo in "${ARCHIVOS_CORE[@]}"; do
    if [ -f "$archivo" ]; then
        echo "  ✓ $(basename $archivo)"
    else
        echo "  ❌ Falta: $(basename $archivo)"
        TODO_OK=false
    fi
done

# 3. Verificar utils nuevos
echo ""
echo "[3/5] Verificando utils nuevos..."
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

# 4. Verificar archivos deprecated
echo ""
echo "[4/5] Verificando archivos deprecated..."
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
    echo "  ⚠️  Ningún archivo movido a deprecated/ (Fase 1 no ejecutada)"
elif [ $MOVED_COUNT -lt 3 ]; then
    echo "  ⚠️  Solo $MOVED_COUNT/3 archivos en deprecated/"
fi

# 5. Contar archivos
echo ""
echo "[5/5] Contando archivos..."
TOTAL_R=$(find modelov5/ -name "*.R" -o -name "*.r" 2>/dev/null | grep -v deprecated | grep -v tests | wc -l)
TOTAL_DEPRECATED=$(find modelov5/deprecated -name "*.R" -o -name "*.r" 2>/dev/null | wc -l || echo "0")
TOTAL_TESTS=$(find modelov5/tests -name "*.R" -o -name "*.r" 2>/dev/null | wc -l || echo "0")

echo "  Archivos activos:    $TOTAL_R"
echo "  Archivos deprecated: $TOTAL_DEPRECATED"
echo "  Archivos de tests:   $TOTAL_TESTS"
echo "  ─────────────────────────────"
echo "  Total (sin deprecated): $((TOTAL_R + TOTAL_TESTS))"

# 6. Resumen final
echo ""
echo "════════════════════════════════════════════════════════════"
echo "  RESUMEN VALIDACIÓN"
echo "════════════════════════════════════════════════════════════"
echo ""

if [ "$TODO_OK" = true ]; then
    echo "✓ Estructura de carpetas correcta"
else
    echo "⚠️  Faltan algunas carpetas (revisar arriba)"
fi

if [ "$TOTAL_R" -lt 35 ]; then
    echo "✓ Número de archivos reducido ($TOTAL_R activos)"
elif [ "$TOTAL_R" -lt 40 ]; then
    echo "⚠️  Archivos en progreso de reducción ($TOTAL_R activos)"
else
    echo "ℹ️  Aún hay muchos archivos activos ($TOTAL_R)"
fi

if [ "$TOTAL_DEPRECATED" -ge 3 ]; then
    echo "✓ Archivos obsoletos movidos ($TOTAL_DEPRECATED en deprecated/)"
elif [ "$TOTAL_DEPRECATED" -gt 0 ]; then
    echo "⚠️  Algunos archivos movidos ($TOTAL_DEPRECATED en deprecated/)"
else
    echo "ℹ️  Fase 1 (limpieza) aún no ejecutada"
fi

if [ "$TOTAL_TESTS" -ge 6 ]; then
    echo "✓ Tests reorganizados ($TOTAL_TESTS en tests/)"
elif [ "$TOTAL_TESTS" -gt 0 ]; then
    echo "⚠️  Algunos tests movidos ($TOTAL_TESTS en tests/)"
else
    echo "ℹ️  Tests aún no reorganizados"
fi

echo ""
echo "Para ejecutar test funcional completo:"
echo "  cd $PROJECT_ROOT/modelov5"
echo "  Rscript 40_WORKFLOW_COMPLETO.R"
echo ""
