# Barreras de acceso en salud en Colombia

Gráficas, con datos públicos y reportes oficiales, sobre dos barreras de acceso al sistema de salud
colombiano: la **espera para servicios especializados** y la **dificultad para acceder a
medicamentos de alto costo**. Cubren el rendimiento histórico y el panorama actual.

El análisis está hecho en R (tidyverse + ggplot2). Las gráficas se exportan en PNG y SVG para usarlas
en presentaciones, y [Quarto](https://quarto.org) reúne en una página web las que conviene ver de
forma interactiva.

## Estructura

```
├── _quarto.yml            # configuración del sitio
├── index.qmd              # página con las gráficas
├── R/
│   ├── setup.R            # paquetes y funciones; cada script lo carga
│   ├── tema.R             # tema ggplot, paletas, escalas e hitos de política
│   ├── utils.R            # guardar figuras
│   ├── moca.R             # descarga de reportes del Observatorio de Calidad
│   └── poblacion.R        # proyecciones de población del DANE
├── scripts/
│   ├── 00_instalar_paquetes.R
│   ├── 02_descargar_moca.R  # tiempos de espera por especialidad y departamento
│   ├── 03_graficas_espera_nacional.R
│   ├── 04_preparar_mapa.R   # datos y límites departamentales del mapa de espera
│   ├── 05_preparar_especialistas.R   # especialistas por 100.000 habitantes (ReTHUS)
│   └── 06_preparar_citas_per_capita.R  # citas por 1.000 habitantes frente a espera
├── data/
│   ├── fuentes.csv        # catálogo de fuentes
│   ├── geo/               # límites departamentales (DANE, MGN 2025)
│   ├── raw/               # datos crudos (no se versionan)
│   └── processed/         # datos limpios
└── figures/               # gráficas exportadas (SVG y PNG)
```

## Cómo reproducir

Requisitos: R (>= 4.3), [Quarto](https://quarto.org/docs/get-started/) y, opcionalmente, RStudio.

```r
# 1. Instalar paquetes y crear renv.lock (una sola vez)
source("scripts/00_instalar_paquetes.R")

# 2. Descargar los datos
source("scripts/02_descargar_moca.R")   # unos 25 minutos; se puede reanudar

# 3. Generar las gráficas en figures/ y los datos del mapa
source("scripts/03_graficas_espera_nacional.R")
source("scripts/04_preparar_mapa.R")
source("scripts/05_preparar_especialistas.R")
source("scripts/06_preparar_citas_per_capita.R")
```

```bash
# 4. Ver el sitio localmente
quarto preview
```

En un equipo nuevo, `renv::restore()` instala las versiones exactas de `renv.lock`.

### Datos del Observatorio Nacional de Calidad en Salud

`scripts/02_descargar_moca.R` automatiza la exportación a CSV del total país y de cada departamento desde el
[reporte por IPS](https://rssvr2.sispro.gov.co/IndicadoresMOCA/) (dominio "Experiencia de la
atención"): tiempos de espera para asignación de cita de medicina general, medicina
interna, pediatría, ginecología, obstetricia y cirugía general (numerador, denominador y días
promedio), de 2017 a la fecha. Con `moca_descargar_todo(solo_total = FALSE)` baja además el detalle
por municipio e IPS.

Hay que tener en cuenta que:

- Los periodos son semestrales hasta 2019 y trimestrales desde 2020.
- El reporte no trae totales municipales: se calculan sumando numerador y denominador por IPS.
- Los datos los reportan las IPS. Hay valores de 0 días que parecen errores de reporte, y el
  último corte es preliminar.

## Publicación

Con `freeze: auto`, Quarto guarda los resultados de cada `.qmd` en `_freeze/`. Después de
renderizar localmente (`quarto render`), se hace commit de `_freeze/` y GitHub Actions publica el
sitio en la rama `gh-pages` sin ejecutar R.

Primera vez: ejecutar `quarto publish gh-pages` localmente y, en *Settings → Pages*, elegir la rama
`gh-pages` como origen.

## Editar los textos de las gráficas

Cada script de gráficas empieza con un bloque `# Textos` donde están títulos, subtítulos, ejes,
fuente, nombres de las categorías y etiquetas. Después de editarlo, se vuelve a ejecutar el script.
Los hitos verticales (`eventos_politica`) se editan en `R/tema.R`.

## Estilo de las gráficas

`R/tema.R` define `theme_informe()`, aplicado por defecto, y estas funciones:

- `scale_colour_informe()` / `scale_fill_informe()`: paleta categórica de 8 colores, en orden fijo
  y validada para daltonismo.
- `scale_fill_informe_c()`: rampa secuencial azul para mapas y heatmaps.
- `scale_fill_informe_div()`: escala divergente azul–rojo.
- `colores_destacar(niveles, "NUEVA EPS")`: resalta una o varias categorías y deja el resto en gris.
- `capa_eventos()`: marca hitos de política (pandemia y presupuestos máximos en 2020, intervención
  de la Nueva EPS en 2024).
- `fuente("Supersalud, PQRD 2019–2025")`: texto estándar para `caption`.

## Fuentes

Ver `data/fuentes.csv`. Los datos pertenecen a sus respectivas entidades; este repositorio solo
contiene el código de análisis.

## Licencia

Código bajo licencia MIT.
