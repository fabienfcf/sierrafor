#!/bin/bash
# ==============================================================================
# FASE 4: ACTUALIZAR RUTAS source() EN TODOS LOS ARCHIVOS
# ==============================================================================

set -e

PROJECT_ROOT="/home/fabien/Documents/CONAFOR/Consultoria/Las Alazanas/PMF - 2026 - 2036/Inventario Forestal 102025/R5"

if [ ! -d "$PROJECT_ROOT/modelov5" ]; then
    echo "❌ ERROR: No se encontró modelov5/"
    exit 1
fi

cd "$PROJECT_ROOT/modelov5"

echo "════════════════════════════════════════════════════════════"
echo "  FASE 4: ACTUALIZAR RUTAS source() EN ARCHIVOS"
echo "════════════════════════════════════════════════════════════"
echo ""
echo "Este script actualizará todas las rutas source() para reflejar"
echo "la nueva estructura de carpetas."
echo ""
read -p "¿Continuar? (y/n) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Operación cancelada"
    exit 0
fi
echo ""

# Crear backup
echo "[0/2] Creando backup..."
BACKUP_DIR="../backups/antes_actualizar_rutas_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$BACKUP_DIR"
cp -r . "$BACKUP_DIR/modelov5"
echo "  ✓ Backup en: $BACKUP_DIR"
echo ""

# Definir mapeo de archivos a sus nuevas rutas
echo "[1/2] Preparando mapeo de rutas..."
declare -A RUTA_NUEVA

# CONFIG
RUTA_NUEVA["00_importar_inventario.R"]="config/00_importar_inventario.R"
RUTA_NUEVA["01_parametros_configuracion.R"]="config/01_parametros_configuracion.R"
RUTA_NUEVA["02_config_especies.R"]="config/02_config_especies.R"
RUTA_NUEVA["03_config_codigos.R"]="config/03_config_codigos.R"
RUTA_NUEVA["04_config_simulacion.R"]="config/04_config_simulacion.R"
RUTA_NUEVA["05_config_programa_cortas.R"]="config/05_config_programa_cortas.R"

# CORE
RUTA_NUEVA["10_modelos_crecimiento.R"]="core/10_modelos_crecimiento.R"
RUTA_NUEVA["11_modelo_mortalidad.R"]="core/11_modelo_mortalidad.R"
RUTA_NUEVA["12_modelo_reclutamiento.R"]="core/12_modelo_reclutamiento.R"
RUTA_NUEVA["13_simulador_crecimiento.R"]="core/13_simulador_crecimiento.R"
RUTA_NUEVA["14_optimizador_cortas.R"]="core/14_optimizador_cortas.R"
RUTA_NUEVA["15_core_calculos.R"]="core/15_core_calculos.R"
RUTA_NUEVA["16_calcular_ica.R"]="core/16_calcular_ica.R"

# ANALISIS
RUTA_NUEVA["20_analisis_descriptivo.R"]="analisis/20_analisis_descriptivo.R"
RUTA_NUEVA["21_ANALISIS_RESULTADOS_DETALLADO.R"]="analisis/21_ANALISIS_RESULTADOS_DETALLADO.R"
RUTA_NUEVA["31_stat_x_rodal.R"]="analisis/31_stat_x_rodal.R"

# SIMULACIONES
RUTA_NUEVA["30_SIMULACION_10AÑOS_COMPLETA.R"]="simulaciones/30_SIMULACION_10AÑOS_COMPLETA.R"

# GENERADORES
RUTA_NUEVA["32_tablas_pmf.R"]="generadores/32_tablas_pmf.R"
RUTA_NUEVA["33_graficos_pmf.R"]="generadores/33_graficos_pmf.R"
RUTA_NUEVA["50_GENERADOR_TABLAS.R"]="generadores/50_GENERADOR_TABLAS.R"
RUTA_NUEVA["51_GENERADOR_GRAFICOS.R"]="generadores/51_GENERADOR_GRAFICOS.R"
RUTA_NUEVA["52_CALCULOS_ESPECIFICOS.R"]="generadores/52_CALCULOS_ESPECIFICOS.R"
RUTA_NUEVA["61_generar_tabla_descriptiva_sitios.R"]="generadores/61_generar_tabla_descriptiva_sitios.R"

# REPORTES
RUTA_NUEVA["35_GENERAR_REPORTE_PMF.R"]="reportes/35_GENERAR_REPORTE_PMF.R"
RUTA_NUEVA["70_CONFIG_REPORTES.R"]="reportes/70_CONFIG_REPORTES.R"

# WORKFLOWS
RUTA_NUEVA["40_WORKFLOW_COMPLETO.R"]="workflows/40_WORKFLOW_COMPLETO.R"
RUTA_NUEVA["41_WORKFLOW_calcular_ica.r"]="workflows/41_WORKFLOW_calcular_ica.r"

# OPCIONAL
RUTA_NUEVA["23_Main_incendio.R"]="opcional/23_Main_incendio.R"

# UTILS
RUTA_NUEVA["utils_metricas.R"]="utils/utils_metricas.R"
RUTA_NUEVA["utils_validacion.R"]="utils/utils_validacion.R"

echo "  ✓ Mapeo preparado: ${#RUTA_NUEVA[@]} archivos"
echo ""

# Actualizar referencias en todos los archivos R
echo "[2/2] Actualizando referencias source()..."
CONTADOR=0

# Encontrar todos los archivos .R y .r (excepto deprecated y backups)
ARCHIVOS_R=$(find . -name "*.R" -o -name "*.r" | grep -v deprecated | grep -v "/backups/" | grep -v "^\./backups")

for archivo in $ARCHIVOS_R; do
    # Verificar que el archivo existe
    if [ ! -f "$archivo" ]; then
        continue
    fi
    
    CAMBIOS_EN_ARCHIVO=0
    
    # Para cada mapeo, buscar y reemplazar
    for archivo_viejo in "${!RUTA_NUEVA[@]}"; do
        archivo_nuevo="${RUTA_NUEVA[$archivo_viejo]}"
        
        # Patrones a buscar (con y sin modelov5/)
        PATRON1="source(\"modelov5/$archivo_viejo\")"
        PATRON2="source('modelov5/$archivo_viejo')"
        PATRON3="source(\"$archivo_viejo\")"
        PATRON4="source('$archivo_viejo')"
        
        REEMPLAZO1="source(\"$archivo_nuevo\")"
        REEMPLAZO2="source('$archivo_nuevo')"
        
        # Reemplazar con sed
        if grep -q "source.*$archivo_viejo" "$archivo" 2>/dev/null; then
            # Hacer backup del archivo
            cp "$archivo" "$archivo.bak.tmp"
            
            # Reemplazar todas las variantes
            sed -i "s|source(\"modelov5/$archivo_viejo\")|source(\"$archivo_nuevo\")|g" "$archivo"
            sed -i "s|source('modelov5/$archivo_viejo')|source('$archivo_nuevo')|g" "$archivo"
            sed -i "s|source(\"$archivo_viejo\")|source(\"$archivo_nuevo\")|g" "$archivo"
            sed -i "s|source('$archivo_viejo')|source('$archivo_nuevo')|g" "$archivo"
            
            CAMBIOS_EN_ARCHIVO=$((CAMBIOS_EN_ARCHIVO + 1))
        fi
    done
    
    if [ $CAMBIOS_EN_ARCHIVO -gt 0 ]; then
        echo "  ✓ Actualizado: $archivo ($CAMBIOS_EN_ARCHIVO referencias)"
        CONTADOR=$((CONTADOR + 1))
        
        # Eliminar backup temporal
        rm -f "$archivo.bak.tmp"
    fi
done

echo ""
echo "════════════════════════════════════════════════════════════"
echo "  ✓ ACTUALIZACIÓN COMPLETADA"
echo "════════════════════════════════════════════════════════════"
echo ""
echo "Archivos modificados: $CONTADOR"
echo ""
echo "Ejemplos de cambios realizados:"
echo "  source(\"10_modelos_crecimiento.R\")"
echo "  → source(\"core/10_modelos_crecimiento.R\")"
echo ""
echo "  source(\"modelov5/01_parametros_configuracion.R\")"
echo "  → source(\"config/01_parametros_configuracion.R\")"
echo ""
echo "Siguiente paso:"
echo "  Probar que todo funciona con:"
echo "  cd workflows/"
echo "  Rscript 40_WORKFLOW_COMPLETO.R"
echo ""
