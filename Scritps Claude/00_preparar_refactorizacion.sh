#!/bin/bash
# ==============================================================================
# SIERRAFOR: Preparación para refactorización
# ==============================================================================

set -e  # Exit on error

PROJECT_ROOT="/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5"
cd "$PROJECT_ROOT"

echo "════════════════════════════════════════════════════════════"
echo "  PREPARACIÓN PARA REFACTORIZACIÓN SIERRAFOR"
echo "════════════════════════════════════════════════════════════"

# 1. Crear backup completo
echo "[1/4] Creando backup completo..."
BACKUP_DIR="backups/$(date +%Y%m%d_%H%M%S)"
mkdir -p "$BACKUP_DIR"
cp -r modelov5/ "$BACKUP_DIR/"
echo "✓ Backup creado en: $BACKUP_DIR"

# 2. Crear estructura de carpetas nuevas
echo "[1/4] Creando estructura de carpetas..."
mkdir -p modelov5/{config,core,analisis,generadores,reportes,utils,workflows,opcional,tests}
echo "✓ Carpetas creadas"

# 3. Git: crear branch (si existe repo)
if [ -d .git ]; then
    echo "[3/4] Creando branch git..."
    git checkout -b refactoring-$(date +%Y%m%d) 2>/dev/null || true
    git add .
    git commit -m "Pre-refactorización: backup antes de cambios" 2>/dev/null || true
    echo "✓ Branch creado: refactoring-$(date +%Y%m%d)"
else
    echo "[3/4] No hay repo git. Continuando..."
fi

# 4. Verificar que todo esté respaldado
echo "[4/4] Verificando backup..."
ARCHIVOS_ORIGINALES=$(ls modelov5/*.R modelov5/*.r 2>/dev/null | wc -l)
ARCHIVOS_BACKUP=$(ls "$BACKUP_DIR"/modelov5/*.R "$BACKUP_DIR"/modelov5/*.r 2>/dev/null | wc -l)

if [ "$ARCHIVOS_ORIGINALES" -eq "$ARCHIVOS_BACKUP" ]; then
    echo "✓ Backup verificado: $ARCHIVOS_BACKUP archivos"
else
    echo "❌ ERROR: Backup incompleto"
    exit 1
fi

echo ""
echo "════════════════════════════════════════════════════════════"
echo "  ✓ PREPARACIÓN COMPLETADA"
echo "════════════════════════════════════════════════════════════"
echo ""
echo "Directorio backup: $BACKUP_DIR"
echo "Siguiente paso: bash 01_fase1_limpieza.sh"
