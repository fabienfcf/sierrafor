#!/bin/bash
# ==============================================================================
# FASE 5: CONVERTIR RUTAS RELATIVAS A ABSOLUTAS CON PROYECTO_ROOT
# ==============================================================================

set -e

PROJECT_ROOT="/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5"

if [ ! -d "$PROJECT_ROOT/modelov5" ]; then
    echo "❌ ERROR: No se encontró modelov5/"
    exit 1
fi

cd "$PROJECT_ROOT/modelov5"

echo "════════════════════════════════════════════════════════════"
echo "  FASE 5: CONVERTIR RUTAS A ABSOLUTAS (PROYECTO_ROOT)"
echo "════════════════════════════════════════════════════════════"
echo ""
echo "Este script convertirá todas las rutas source() relativas"
echo "a rutas absolutas usando file.path(PROYECTO_ROOT, ...)"
echo ""
read -p "¿Continuar? (y/n) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Operación cancelada"
    exit 0
fi
echo ""

# Crear backup
echo "[0/4] Creando backup..."
BACKUP_DIR="../backups/antes_rutas_absolutas_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$BACKUP_DIR"
cp -r . "$BACKUP_DIR/modelov5"
echo "  ✓ Backup en: $BACKUP_DIR"
echo ""

# Definir carpetas de rutas válidas
CARPETAS_RUTAS=(
    "config"
    "core"
    "analisis"
    "simulaciones"
    "generadores"
    "reportes"
    "workflows"
    "opcional"
    "utils"
)

echo "[1/4] Buscando archivos con source() relativo..."
ARCHIVOS_CON_SOURCE=$(grep -rl "source(\"config/\|source(\"core/\|source(\"analisis/\|source(\"simulaciones/\|source(\"generadores/\|source(\"reportes/\|source(\"workflows/\|source(\"opcional/\|source(\"utils/" . --include="*.R" --include="*.r" 2>/dev/null | grep -v deprecated | grep -v backup || true)

if [ -z "$ARCHIVOS_CON_SOURCE" ]; then
    echo "  ℹ️  No se encontraron archivos con source() relativo"
    exit 0
fi

NUM_ARCHIVOS=$(echo "$ARCHIVOS_CON_SOURCE" | wc -l)
echo "  ✓ Encontrados $NUM_ARCHIVOS archivos"
echo ""

echo "[2/4] Agregando PROYECTO_ROOT a archivos que no lo tienen..."
CONTADOR_ROOT=0

for archivo in $ARCHIVOS_CON_SOURCE; do
    # Verificar si ya tiene PROYECTO_ROOT
    if grep -q "PROYECTO_ROOT" "$archivo" 2>/dev/null; then
        continue
    fi
    
    # Crear backup temporal
    cp "$archivo" "$archivo.bak.tmp"
    
    # Encontrar dónde insertar (después de rm(list=ls()), library(), o al inicio)
    if grep -q "^rm(list" "$archivo"; then
        # Insertar después de rm(list = ls())
        sed -i '/^rm(list/a\
\
# Establecer directorio raíz del proyecto\
if (!exists("PROYECTO_ROOT")) {\
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"\
}\
setwd(PROYECTO_ROOT)' "$archivo"
    else
        # Insertar al inicio del archivo
        sed -i '1i\
# Establecer directorio raíz del proyecto\
if (!exists("PROYECTO_ROOT")) {\
  PROYECTO_ROOT <- "/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5/modelov5"\
}\
setwd(PROYECTO_ROOT)\
' "$archivo"
    fi
    
    echo "  ✓ Agregado PROYECTO_ROOT a: $archivo"
    CONTADOR_ROOT=$((CONTADOR_ROOT + 1))
done

echo "  ✓ PROYECTO_ROOT agregado a $CONTADOR_ROOT archivos"
echo ""

echo "[3/4] Convirtiendo source() a rutas absolutas..."
CONTADOR_CONVERSIONES=0

for archivo in $ARCHIVOS_CON_SOURCE; do
    CAMBIOS_EN_ARCHIVO=0
    
    # Crear backup temporal
    cp "$archivo" "$archivo.bak.tmp"
    
    # Para cada carpeta, convertir las rutas
    for carpeta in "${CARPETAS_RUTAS[@]}"; do
        # Patrón: source("carpeta/archivo.R")
        # Reemplazo: source(file.path(PROYECTO_ROOT, "carpeta/archivo.R"))
        
        if grep -q "source(\"$carpeta/" "$archivo" 2>/dev/null; then
            sed -i "s|source(\"$carpeta/\([^\"]*\)\")|source(file.path(PROYECTO_ROOT, \"$carpeta/\1\"))|g" "$archivo"
            CAMBIOS_EN_ARCHIVO=$((CAMBIOS_EN_ARCHIVO + 1))
        fi
        
        # También con comillas simples
        if grep -q "source('$carpeta/" "$archivo" 2>/dev/null; then
            sed -i "s|source('$carpeta/\([^']*\)')|source(file.path(PROYECTO_ROOT, '$carpeta/\1'))|g" "$archivo"
            CAMBIOS_EN_ARCHIVO=$((CAMBIOS_EN_ARCHIVO + 1))
        fi
    done
    
    if [ $CAMBIOS_EN_ARCHIVO -gt 0 ]; then
        echo "  ✓ Convertido: $archivo ($CAMBIOS_EN_ARCHIVO rutas)"
        CONTADOR_CONVERSIONES=$((CONTADOR_CONVERSIONES + 1))
    fi
    
    # Eliminar backup temporal
    rm -f "$archivo.bak.tmp"
done

echo "  ✓ $CONTADOR_CONVERSIONES archivos modificados"
echo ""

echo "[4/4] Verificando conversiones..."
echo ""
echo "Ejemplos de conversiones realizadas:"
echo "  ANTES: source(\"config/01_parametros_configuracion.R\")"
echo "  DESPUÉS: source(file.path(PROYECTO_ROOT, \"config/01_parametros_configuracion.R\"))"
echo ""

# Mostrar algunos ejemplos reales
echo "Archivos modificados (primeros 5):"
echo "$ARCHIVOS_CON_SOURCE" | head -5 | while read archivo; do
    echo "  • $archivo"
    grep "file.path(PROYECTO_ROOT" "$archivo" 2>/dev/null | head -2 | sed 's/^/    /'
done

echo ""
echo "════════════════════════════════════════════════════════════"
echo "  ✓ CONVERSIÓN COMPLETADA"
echo "════════════════════════════════════════════════════════════"
echo ""
echo "Cambios realizados:"
echo "  • PROYECTO_ROOT agregado a: $CONTADOR_ROOT archivos"
echo "  • Rutas convertidas en: $CONTADOR_CONVERSIONES archivos"
echo ""
echo "Ahora todas las rutas son absolutas y funcionarán desde"
echo "cualquier directorio de trabajo."
echo ""
echo "Siguiente paso:"
echo "  cd workflows/"
echo "  Rscript 40_WORKFLOW_COMPLETO.R"
echo ""
echo "O desde modelov5/:"
echo "  Rscript workflows/40_WORKFLOW_COMPLETO.R"
echo ""
