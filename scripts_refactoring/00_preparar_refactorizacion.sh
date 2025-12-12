#!/bin/bash
# ==============================================================================
# SIERRAFOR: Preparación para refactorización
# ==============================================================================

set -e  # Exit on error

PROJECT_ROOT="/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5"

# Verificar que el directorio existe
if [ ! -d "$PROJECT_ROOT" ]; then
    echo "❌ ERROR: No se encontró el directorio del proyecto"
    echo "   Esperado: $PROJECT_ROOT"
    echo ""
    echo "Por favor, edita este script y actualiza PROJECT_ROOT con la ruta correcta"
    exit 1
fi

cd "$PROJECT_ROOT"

echo "════════════════════════════════════════════════════════════"
echo "  PREPARACIÓN PARA REFACTORIZACIÓN SIERRAFOR"
echo "════════════════════════════════════════════════════════════"
echo ""
echo "Directorio de trabajo: $PROJECT_ROOT"
echo ""

# 1. Crear backup completo
echo "[1/4] Creando backup completo..."
BACKUP_DIR="backups/$(date +%Y%m%d_%H%M%S)"
mkdir -p "$BACKUP_DIR"

if [ -d "modelov5" ]; then
    cp -r modelov5/ "$BACKUP_DIR/"
    echo "✓ Backup creado en: $BACKUP_DIR"
else
    echo "❌ ERROR: No se encontró la carpeta modelov5/"
    exit 1
fi

# 2. Crear estructura de carpetas nuevas
echo "[2/4] Creando estructura de carpetas..."
mkdir -p modelov5/{config,core,analisis,generadores,reportes,utils,workflows,opcional,tests,deprecated}
echo "✓ Carpetas creadas"

# 3. Git: crear branch (si existe repo)
if [ -d .git ]; then
    echo "[3/4] Creando branch git..."
    BRANCH_NAME="refactoring-$(date +%Y%m%d)"
    git checkout -b "$BRANCH_NAME" 2>/dev/null || echo "  (branch ya existe o no hay git)"
    git add . 2>/dev/null || true
    git commit -m "Pre-refactorización: backup antes de cambios" 2>/dev/null || echo "  (nada que commitear)"
    echo "✓ Branch git preparado"
else
    echo "[3/4] No hay repo git. Continuando sin control de versiones..."
fi

# 4. Verificar que todo esté respaldado
echo "[4/4] Verificando backup..."
ARCHIVOS_ORIGINALES=$(find modelov5/ -name "*.R" -o -name "*.r" 2>/dev/null | wc -l)
ARCHIVOS_BACKUP=$(find "$BACKUP_DIR"/modelov5/ -name "*.R" -o -name "*.r" 2>/dev/null | wc -l)

if [ "$ARCHIVOS_ORIGINALES" -eq "$ARCHIVOS_BACKUP" ]; then
    echo "✓ Backup verificado: $ARCHIVOS_BACKUP archivos"
else
    echo "❌ ERROR: Backup incompleto"
    echo "   Originales: $ARCHIVOS_ORIGINALES"
    echo "   Backup: $ARCHIVOS_BACKUP"
    exit 1
fi

echo ""
echo "════════════════════════════════════════════════════════════"
echo "  ✓ PREPARACIÓN COMPLETADA"
echo "════════════════════════════════════════════════════════════"
echo ""
echo "Backup completo guardado en: $BACKUP_DIR"
echo ""
echo "Siguiente paso:"
echo "  bash 01_fase1_limpieza.sh"
echo ""
