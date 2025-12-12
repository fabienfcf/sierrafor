#!/bin/bash
# ==============================================================================
# ROLLBACK: Revertir cambios si algo salió mal
# ==============================================================================

PROJECT_ROOT="/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5"

if [ ! -d "$PROJECT_ROOT" ]; then
    echo "❌ ERROR: No se encontró el directorio del proyecto"
    exit 1
fi

cd "$PROJECT_ROOT"

echo "════════════════════════════════════════════════════════════"
echo "  ⚠️  ROLLBACK DE REFACTORIZACIÓN"
echo "════════════════════════════════════════════════════════════"
echo ""

# Encontrar último backup
ULTIMO_BACKUP=$(ls -td backups/*/ 2>/dev/null | head -1)

if [ -z "$ULTIMO_BACKUP" ]; then
    echo "❌ No se encontró ningún backup en backups/"
    echo ""
    echo "Backups disponibles:"
    ls -lh backups/ 2>/dev/null || echo "  (carpeta backups/ vacía o no existe)"
    exit 1
fi

echo "Último backup encontrado:"
echo "  $ULTIMO_BACKUP"
echo ""

# Mostrar contenido del backup
echo "Contenido del backup:"
ARCHIVOS_BACKUP=$(find "$ULTIMO_BACKUP" -name "*.R" -o -name "*.r" 2>/dev/null | wc -l)
echo "  $ARCHIVOS_BACKUP archivos .R/.r"
echo ""

# Confirmar con usuario
read -p "¿Restaurar desde este backup? (y/n) " -n 1 -r
echo
echo

if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo "Iniciando restauración..."
    echo ""
    
    # Backup del estado actual antes de restaurar
    if [ -d "modelov5" ]; then
        BACKUP_ACTUAL="backups/antes_rollback_$(date +%Y%m%d_%H%M%S)"
        mkdir -p "$BACKUP_ACTUAL"
        echo "[1/3] Guardando estado actual en $BACKUP_ACTUAL..."
        cp -r modelov5/ "$BACKUP_ACTUAL/"
        echo "  ✓ Estado actual respaldado"
    fi
    
    # Eliminar modelov5 actual
    echo "[2/3] Eliminando modelov5 actual..."
    rm -rf modelov5/
    echo "  ✓ Eliminado"
    
    # Restaurar desde backup
    echo "[3/3] Restaurando desde backup..."
    cp -r "${ULTIMO_BACKUP}modelov5" .
    echo "  ✓ Restaurado"
    
    echo ""
    echo "════════════════════════════════════════════════════════════"
    echo "  ✓ ROLLBACK COMPLETADO"
    echo "════════════════════════════════════════════════════════════"
    echo ""
    echo "Archivos restaurados desde: $ULTIMO_BACKUP"
    echo "Estado actual guardado en: $BACKUP_ACTUAL"
    echo ""
    echo "Puedes verificar con:"
    echo "  ls -la modelov5/"
    echo ""
    
else
    echo "Rollback cancelado"
    echo ""
fi
