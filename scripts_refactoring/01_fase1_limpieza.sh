#!/bin/bash
# ==============================================================================
# FASE 1: Eliminar archivos obsoletos y reorganizar tests
# ==============================================================================

set -e

PROJECT_ROOT="/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5"

if [ ! -d "$PROJECT_ROOT/modelov5" ]; then
    echo "❌ ERROR: No se encontró modelov5/"
    echo "Por favor, edita este script y actualiza PROJECT_ROOT"
    exit 1
fi

cd "$PROJECT_ROOT/modelov5"

echo "════════════════════════════════════════════════════════════"
echo "  FASE 1: LIMPIEZA Y REORGANIZACIÓN"
echo "════════════════════════════════════════════════════════════"
echo ""

# 1. Crear carpeta deprecated si no existe
mkdir -p deprecated/

# 2. Mover archivos obsoletos
echo "[1/3] Moviendo archivos obsoletos..."
OBSOLETOS=(
    "20_analisis_descriptivo_old.R"
    "50_crear_tabla_descriptiva_muestreo.r"
    "51_EJECUTAR_ANÁLISIS_DESCRIPTIVO.R"
)

for archivo in "${OBSOLETOS[@]}"; do
    if [ -f "$archivo" ]; then
        mv "$archivo" deprecated/
        echo "  ✓ Movido: $archivo → deprecated/"
    else
        echo "  ⚠ No encontrado: $archivo (quizás ya fue movido)"
    fi
done

# 3. Reorganizar tests
echo "[2/3] Reorganizando tests..."
mkdir -p tests/

TESTS=(
    "test_calculos_especificos.r"
    "test_config.r"
    "test_generador_graficos.r"
    "test_generador_tablas.r"
    "test_refactorizacion.R"
    "TEST_INTEGRACION_TS_TC.R"
    "22_VERIFICACION_TABLAS_LATEX.R"
)

for archivo in "${TESTS[@]}"; do
    if [ -f "$archivo" ]; then
        mv "$archivo" tests/
        echo "  ✓ Movido: $archivo → tests/"
    else
        echo "  ⚠ No encontrado: $archivo"
    fi
done

# 4. Resumen
echo "[3/3] Resumen de cambios..."
echo ""
echo "📁 deprecated/:"
ls -1 deprecated/ 2>/dev/null | wc -l | xargs echo "  Archivos:"
ls -1 deprecated/ 2>/dev/null | sed 's/^/    • /' || echo "  (vacío)"

echo ""
echo "📁 tests/:"
ls -1 tests/ 2>/dev/null | wc -l | xargs echo "  Archivos:"
ls -1 tests/ 2>/dev/null | sed 's/^/    • /' | head -5 || echo "  (vacío)"

echo ""
echo "════════════════════════════════════════════════════════════"
echo "  ✓ FASE 1 COMPLETADA"
echo "════════════════════════════════════════════════════════════"
echo ""
echo "Cambios realizados:"
echo "  • Archivos obsoletos → deprecated/"
echo "  • Archivos de testing → tests/"
echo ""
echo "Siguiente paso:"
echo "  bash 02_fase2_crear_utils_io.sh"
echo ""
