# TABLAS LATEX - PROGRAMA DE MANEJO FORESTAL

## Descripción

Este directorio contiene las tablas generadas automáticamente para el
Programa de Manejo Forestal del Ejido Las Alazanas, conforme a la
NOM-152-SEMARNAT-2023.

## Archivos principales

### Tablas principales

1. `01_inventario_inicial.tex` - Estado inicial del inventario por rodal y género
2. `02_comparacion_inicial_final.tex` - Comparación estado inicial vs proyección 10 años
3. `03_intensidad_corte_rodal.tex` - Resumen de intensidad de corte
4. `04_corte_por_clase_diametrica.tex` - Detalle de corta por clase diamétrica

### Tablas por rodal

- `05_corta_rodal_XX.tex` - Programa de corta específico para cada rodal (0 rodales)

## Uso

### Opción 1: Documento completo

Compilar el documento maestro que incluye todas las tablas:

```bash
pdflatex 00_documento_maestro.tex
```

### Opción 2: Incluir tablas individuales

En tu documento LaTeX principal:

```latex
\input{tablas_latex/01_inventario_inicial.tex}
```

## Requisitos LaTeX

- Paquete `booktabs` para tablas profesionales
- Paquete `longtable` para tablas que ocupan múltiples páginas
- Paquete `babel` con opción `spanish` para formato en español

## Notas

- Archivos generados: 2025-11-20 09:00:57
- Total de archivos: 17
- Tablas principales: 1
- Tablas por rodal: 0

## Regeneración

Para regenerar las tablas, ejecutar:

```r
source('30_SIMULACION_10AÑOS_COMPLETA.R')
```

