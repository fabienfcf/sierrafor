#!/bin/bash
# ==============================================================================
# FASE 3: REORGANIZAR ARCHIVOS EN ESTRUCTURA MODULAR
# ==============================================================================

set -e

PROJECT_ROOT="/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5"

if [ ! -d "$PROJECT_ROOT/modelov5" ]; then
    echo "❌ ERROR: No se encontró modelov5/"
    exit 1
fi

cd "$PROJECT_ROOT/modelov5"

echo "════════════════════════════════════════════════════════════"
echo "  FASE 3: REORGANIZAR ARCHIVOS EN ESTRUCTURA MODULAR"
echo "════════════════════════════════════════════════════════════"
echo ""
echo "⚠️  ADVERTENCIA: Este script moverá archivos a sus carpetas"
echo "   correspondientes (config/, core/, analisis/, etc.)"
echo ""
read -p "¿Continuar? (y/n) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Operación cancelada"
    exit 0
fi
echo ""

# Crear backup antes de mover
echo "[0/7] Creando backup pre-reorganización..."
BACKUP_DIR="../backups/antes_reorganizacion_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$BACKUP_DIR"
cp -r . "$BACKUP_DIR/modelov5"
echo "  ✓ Backup en: $BACKUP_DIR"
echo ""

# 1. MOVER CONFIG (00-05)
echo "[1/7] Moviendo archivos CONFIG..."
ARCHIVOS_CONFIG=(
    "00_importar_inventario.R"
    "01_parametros_configuracion.R"
    "02_config_especies.R"
    "03_config_codigos.R"
    "04_config_simulacion.R"
    "05_config_programa_cortas.R"
)

for archivo in "${ARCHIVOS_CONFIG[@]}"; do
    if [ -f "$archivo" ]; then
        mv "$archivo" config/
        echo "  ✓ $archivo → config/"
    else
        echo "  ⚠ No encontrado: $archivo"
    fi
done

# 2. MOVER CORE (10-16)
echo ""
echo "[2/7] Moviendo archivos CORE..."
ARCHIVOS_CORE=(
    "10_modelos_crecimiento.R"
    "11_modelo_mortalidad.R"
    "12_modelo_reclutamiento.R"
    "13_simulador_crecimiento.R"
    "14_optimizador_cortas.R"
    "15_core_calculos.R"
    "16_calcular_ica.R"
)

for archivo in "${ARCHIVOS_CORE[@]}"; do
    if [ -f "$archivo" ]; then
        mv "$archivo" core/
        echo "  ✓ $archivo → core/"
    else
        echo "  ⚠ No encontrado: $archivo"
    fi
done

# 3. MOVER ANÁLISIS (20-21)
echo ""
echo "[3/7] Moviendo archivos ANÁLISIS..."
ARCHIVOS_ANALISIS=(
    "20_analisis_descriptivo.R"
    "21_ANALISIS_RESULTADOS_DETALLADO.R"
    "31_stat_x_rodal.R"
)

for archivo in "${ARCHIVOS_ANALISIS[@]}"; do
    if [ -f "$archivo" ]; then
        mv "$archivo" analisis/
        echo "  ✓ $archivo → analisis/"
    else
        echo "  ⚠ No encontrado: $archivo"
    fi
done

# 4. MOVER SIMULACIONES (30)
echo ""
echo "[4/7] Moviendo archivos SIMULACIONES..."
mkdir -p simulaciones/
ARCHIVOS_SIM=(
    "30_SIMULACION_10AÑOS_COMPLETA.R"
)

for archivo in "${ARCHIVOS_SIM[@]}"; do
    if [ -f "$archivo" ]; then
        mv "$archivo" simulaciones/
        echo "  ✓ $archivo → simulaciones/"
    else
        echo "  ⚠ No encontrado: $archivo"
    fi
done

# 5. MOVER GENERADORES (32, 33, 50, 51, 52, 61)
echo ""
echo "[5/7] Moviendo archivos GENERADORES..."
mkdir -p generadores/
ARCHIVOS_GEN=(
    "32_tablas_pmf.R"
    "33_graficos_pmf.R"
    "50_GENERADOR_TABLAS.R"
    "51_GENERADOR_GRAFICOS.R"
    "52_CALCULOS_ESPECIFICOS.R"
    "61_generar_tabla_descriptiva_sitios.R"
)

for archivo in "${ARCHIVOS_GEN[@]}"; do
    if [ -f "$archivo" ]; then
        mv "$archivo" generadores/
        echo "  ✓ $archivo → generadores/"
    else
        echo "  ⚠ No encontrado: $archivo"
    fi
done

# 6. MOVER REPORTES (35, 70)
echo ""
echo "[6/7] Moviendo archivos REPORTES..."
mkdir -p reportes/
ARCHIVOS_REP=(
    "35_GENERAR_REPORTE_PMF.R"
    "70_CONFIG_REPORTES.R"
)

for archivo in "${ARCHIVOS_REP[@]}"; do
    if [ -f "$archivo" ]; then
        mv "$archivo" reportes/
        echo "  ✓ $archivo → reportes/"
    else
        echo "  ⚠ No encontrado: $archivo"
    fi
done

# 7. MOVER WORKFLOWS (40, 41)
echo ""
echo "[7/7] Moviendo archivos WORKFLOWS..."
mkdir -p workflows/
ARCHIVOS_WORK=(
    "40_WORKFLOW_COMPLETO.R"
    "41_WORKFLOW_calcular_ica.r"
)

for archivo in "${ARCHIVOS_WORK[@]}"; do
    if [ -f "$archivo" ]; then
        mv "$archivo" workflows/
        echo "  ✓ $archivo → workflows/"
    else
        echo "  ⚠ No encontrado: $archivo"
    fi
done

# 8. MOVER OPCIONALES (23)
echo ""
echo "[8/8] Moviendo archivos OPCIONALES..."
mkdir -p opcional/
ARCHIVOS_OPC=(
    "23_Main_incendio.R"
)

for archivo in "${ARCHIVOS_OPC[@]}"; do
    if [ -f "$archivo" ]; then
        mv "$archivo" opcional/
        echo "  ✓ $archivo → opcional/"
    else
        echo "  ⚠ No encontrado: $archivo"
    fi
done

# Resumen final
echo ""
echo "════════════════════════════════════════════════════════════"
echo "  ✓ REORGANIZACIÓN COMPLETADA"
echo "════════════════════════════════════════════════════════════"
echo ""
echo "Estructura nueva:"
echo "  📁 config/       $(ls config/*.R 2>/dev/null | wc -l) archivos"
echo "  📁 core/         $(ls core/*.R 2>/dev/null | wc -l) archivos"
echo "  📁 analisis/     $(ls analisis/*.R 2>/dev/null | wc -l) archivos"
echo "  📁 simulaciones/ $(ls simulaciones/*.R 2>/dev/null | wc -l) archivos"
echo "  📁 generadores/  $(ls generadores/*.R 2>/dev/null | wc -l) archivos"
echo "  📁 reportes/     $(ls reportes/*.R 2>/dev/null | wc -l) archivos"
echo "  📁 workflows/    $(ls workflows/*.R workflows/*.r 2>/dev/null | wc -l) archivos"
echo "  📁 opcional/     $(ls opcional/*.R 2>/dev/null | wc -l) archivos"
echo "  📁 utils/        $(ls utils/*.R 2>/dev/null | wc -l) archivos"
echo "  📁 tests/        $(ls tests/*.R tests/*.r 2>/dev/null | wc -l) archivos"
echo "  📁 deprecated/   $(ls deprecated/*.R deprecated/*.r 2>/dev/null | wc -l) archivos"
echo ""
echo "Archivos aún en raíz:"
ls *.R *.r 2>/dev/null | wc -l | xargs echo "  "
echo ""
echo "⚠️  IMPORTANTE: Ahora debes actualizar las rutas source() en los archivos"
echo "   Ejecuta: bash 04_actualizar_rutas_source.sh"
echo ""
