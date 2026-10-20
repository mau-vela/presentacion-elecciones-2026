# Proyecto: Presentación Elecciones 2026 (RNEC)

## Qué es esto
Documento interactivo de scrollytelling sobre las elecciones 2026 (Congreso y
Presidencia). Archivo principal: informe_scroll.qmd (Quarto closeread-html).
Estilos en estilos.scss. Los datos se preparan en 00_preparar_datos.R, que guarda
todo en .rds autosuficientes que el .qmd solo lee. Se publica en Posit Connect
Cloud (tier gratuito, 4 GB) por auto-deploy de GitHub.

## Estado del proyecto: hechos que NO se deben romper

Esto es contexto que una sesión nueva no puede deducir leyendo el código. Antes de
tocar el `.qmd` o el `.R`, tenerlo presente.

### Estructura del informe y anclas (para el mapa interactivo externo)
El informe tiene **tres secciones grandes**, no cuatro. En octubre de 2026 se
reorganizó así:

1. **Infraestructura electoral** (antes "Datos generales" / "Divipole"): censo,
   puestos, mesas, distribución territorial y exterior.
2. **Elecciones a Congreso**: candidaturas, preconteo, comparación, participación y
   resultados.
3. **Presidencia**: las mismas cinco subsecciones.

La antigua sección "La inscripción de candidaturas" **ya no existe como sección
propia**. Su contenido se repartió:
- La tabla por corporación y circunscripción se partió en dos. La parte de Congreso
  abre `congreso-candidaturas`; las dos vueltas presidenciales abren
  `presidencia-candidaturas`, con pestañas de primera y segunda vuelta.
- El ritmo de inscripción y las organizaciones con personería jurídica quedaron en
  `congreso-candidaturas`, en ese orden detrás de la tabla.

Los títulos con `{#nombre}` son el destino de los botones del mapa interactivo hecho
en Articulate Storyline por un diseñador externo, que será la portada del sitio y
enlaza con `informe_scroll.html#nombre`. **No renombrarlas ni quitarlas** sin avisar,
porque romperían los enlaces:
`infraestructura-electoral`, `divipole`, `congreso`, `presidencia`, y
`{congreso|presidencia}-{candidaturas|preconteo|comparacion|participacion|resultados}`.

### Secciones ocultas, no borradas
Dos secciones de "Comparación territorial" de la velocidad del preconteo (una en
Congreso, otra en Presidencia) están **envueltas en comentarios HTML**, no eliminadas.
Se buscan por la marca `SECCIÓN OCULTA` en el `.qmd`. Quarto no las publica ni ejecuta
sus celdas OJS. No "arreglarlas" ni reactivarlas sin que se pida.

### Orden mujeres/hombres en el DIVIPOL 2026
En el TXT del DIVIPOL, las posiciones **93-100 son MUJERES** y **101-108 son HOMBRES**.
`leer_divipol()` las tenía invertidas y se corrigió. Comprobado contra el censo por
puesto de enero de 2026 y contra el DIVIPOL de 2022. No volver a invertirlas.

### Fuente de las candidaturas de Congreso 2026
Para 2026 manda el **`CANDIDATOS.TXT` del día electoral** (archivos básicos del
preconteo), no el archivo de inscripción `CANDIATURAS2026.xlsx`. El TXT trae las
candidaturas que llegaron al tarjetón; el Excel trae las inscritas al cierre, que son
algunas más. En el TXT, las líneas **sin cédula son las organizaciones que inscriben**,
no personas, y por eso el filtro de género las descarta.

Con ese criterio se corrigió el **desglose de CITREP por circunscripción**, que ahora
sale del TXT (`codcorp 007`, número de circunscripción en las posiciones 9-11) y da
**237** en 2026. Antes salía del Excel y daba 246, mientras el total de la misma sección
decía 237.

**La circunscripción internacional de Cámara no necesita tratamiento aparte.** No
aparece en `CIRCUNSCRIPCION.TXT` ni tiene corporación propia, pero sí está en
`CANDIDATOS.TXT` **dentro de Territorial, con territorio 88**: son 40 candidaturas, con
códigos de candidato 401 a 403. Queda así equiparada con 2022, donde el script suma
`DESC_CIRC == "INTERNACIONAL"` a Territorial. No agregarlas desde el archivo de
inscripción, porque se duplican.

### Lectura de Consulados 2018
La hoja "Consulados " de `Divipol Presidente 2018.xlsx` se lee **completa**, sin rango.
El rango anterior `A1:L233` dejaba por fuera tres puestos de Venezuela (22.177 de censo
y 33 mesas). La fila TOTAL del final se descarta con el filtro de `dd` numérico.

### Secciones aparte del R (no alimentan el informe)
Al final de `00_preparar_datos.R` hay dos secciones marcadas `SECCION APARTE` que
generan archivos para uso externo en `../divipole/` (el exterior por país en
`procesamiento/`, el DIVIPOL de Congreso 2026 en `congreso/`) y **no crean ningún
`.rds` ni alimentan el `.qmd`**:
- Exterior por país 2018-2022-2026. El "municipio" del DIVIPOL en el exterior es una
  **jurisdicción consular, no un país**: Riad está dentro de EGIPTO, Dakar dentro de
  GHANA. El país se asigna puesto por puesto con reglas explícitas.
- DIVIPOL de Congreso 2026 en xlsx, leído del TXT.

### Scripts del libro
`scripts/libro_datos_*.R` son de **solo lectura**: leen los `.rds` y muestran cifras en
consola para redactar los apartados del libro. No escriben nada del proyecto.

## El libro "Balance electoral 2026"

Producto aparte del informe, basado en la misma presentación. Varias personas
entregaron textos de análisis por sección, compilados en
`../Presentación elecciones 2026 información para compartir/Libro/Consolidado v1.docx`
(al lado está `Libro resultados electorales.docx`, una versión anterior y corta).

El trabajo es **revisar y corregir** esos textos, no escribirlos desde cero: sacar de
ahí cada apartado, quitar lo que suene a IA (usar la skill `estilo-escritura`),
corregir afirmaciones erróneas o imprecisas contra los datos, complementar lo que
falte y agregar a cada sección una **gráfica, tabla o mapa estático** que la
represente mejor (identidad en `registraduria-visual-identity`, diseño en
`diseno-documentos-excel-dashboards-html`). Nunca poner en el libro avisos de datos
incompletos o posibles errores (ver memoria y skills).

### Formato y ubicación
- Se trabaja en **Markdown** en la carpeta `Libro`, siguiendo los estilos, y al final
  se genera el **Word** para entregar.
- El libro tiene **más secciones que el informe** y la estructura exacta se va
  definiendo a medida que se crea; no forzar un molde único.
- El Word se arma desde `Libro/libro/` (ver la memoria del pipeline): un `.Rmd` por
  capítulo, `_comun.R` con paleta y estilos, `datos/preparar_capNN.R` de solo lectura
  y `generar_word.R`. El original `Consolidado v1.docx` se lee por su copia en
  markdown en `Libro/fuente/`, nunca abriendo el `.docx`.

### Reglas fijas de presentación del libro
- **Fuente única en todo el libro**: `Fuente: Registraduría Nacional del Estado Civil.`
  No nombrar el DIVIPOL, los `.rds` ni ningún archivo interno.
- **Los títulos y subtítulos de cada figura y de cada tabla se escriben en el
  markdown del capítulo, encima del elemento**, con los estilos `Titulo figura` y
  `Subtitulo figura`, y la fuente debajo con el estilo `Fuente`. Nunca dentro del
  `ggplot` (`labs(title=)`, `subtitle=`) ni como `fig.cap` o `tab.cap`, para que
  quien revise pueda corregirlos sin tocar el código de R. Lo mismo con los rótulos
  de cada panel cuando una figura tiene varios.
- Las cifras del texto van con código en línea (`r num(...)`), no escritas a mano.
- **Los títulos nombran el dato, no lo interpretan.** Vale para secciones, gráficas y
  tablas: "Censo electoral en el exterior", no "Una red electoral en 81 países"; y
  "Candidaturas a la Cámara por departamento", no "Dónde crecieron las candidaturas".
  Nada de conclusiones ni de preguntas en los títulos. El detalle está en el skill
  `estilo-escritura`.
- **El cuerpo del texto va en negro**, no en azul. El azul `#003057` queda para los
  títulos, los encabezados y los elementos de las figuras y tablas.
- **Las conclusiones no llevan subsecciones en la tabla de contenido.** Los títulos de
  tema dentro de conclusiones van con el estilo `Tema conclusiones`, que no tiene nivel
  de esquema y por eso no entra al índice.
- **Conclusiones e introducción se van llenando con cada sección**, no al final de
  todo. La introducción suma un párrafo introductorio por tema, sin cifras. Las
  conclusiones suman un bloque corto con bullets por tema, con los datos a resaltar.
  El primer párrafo de las conclusiones queda reservado para la lectura de conjunto.

### Estructura de referencia de un apartado (cuando aplique)
- **Apartado**, en torno a una página, con cinco partes numeradas: título;
  descripción de lo que muestra; comparación con 2018 y 2022; observación sobre el
  contexto o la gestión de la Registraduría; conclusión. Tono técnico y objetivo, sin
  calificativos como "excelente" o "exitosa". Dentro de cada parte, los indicadores van
  siempre en el mismo orden: primero primera vuelta y luego segunda, primero municipios
  y luego departamentos.
- **Datos curiosos**, entre 25 y 45 palabras cada uno, una sola idea, sustentados en
  la gráfica, en formato de pregunta corta y respuesta puntual. Deben aportar algo que
  no sea evidente a simple vista en la gráfica.

### Datos
Las cifras se sacan con los scripts `scripts/libro_datos_*.R` (de solo lectura; ver
"Scripts del libro" arriba). Si falta un dato, se crea un script nuevo de solo lectura
en esa carpeta, nunca se modifica `00_preparar_datos.R`.

## Publicación en el sitio de la RNEC

Además de Posit Connect Cloud, el informe se va a publicar en el sitio institucional.

- Es un **sitio estático**: no necesita R ni base de datos en el servidor.
- El paquete para informática se arma en
  `../Presentación elecciones 2026 información para compartir/entrega web Registraduria/`,
  que está **fuera del repositorio** y por eso no entra a git.
- Lo que se entrega: el HTML renderizado, la carpeta `informe_scroll_files`, los seis
  `.geojson`, `puestos_mapa.json` (puntos del mapa de puestos de la sección Divipole,
  unos 2 MB) y las dos imágenes de `assets` que el informe usa.
- Requisitos que informática debe habilitar: registrar `.geojson` como
  `application/json`, activar compresión gzip (el HTML pesa 47 MiB y baja a 9,6 MiB) y
  revisar los dominios externos (`server.arcgisonline.com`, `unpkg.com`,
  `fonts.googleapis.com` y `fonts.gstatic.com`).
- **Abrir el HTML con doble clic no funciona**: el navegador bloquea la lectura de los
  `.geojson` y los mapas salen en blanco. Para probar hay que servirlo
  (`python -m http.server`).
- Ojo con el peso: el HTML está en 47 MiB y GitHub advierte desde 50 MiB.

### Portada interactiva (Storyline) — pendiente de versión definitiva

Un diseñador externo (Carlos Bermúdez) hizo en Articulate Storyline 360 una portada
con introducción, un mapa de navegación con botones hacia cada sección, y un cierre.
Va a ser la **puerta de entrada del sitio**, y el informe queda como página aparte en
la misma carpeta. La versión recibida está en
`../Presentación elecciones 2026 información para compartir/storyline/` y **no es
definitiva**.

Lo que estaba pendiente cuando se revisó:
- Los textos de introducción y cierre son "Lorem ipsum".
- Los botones del mapa **no enlazan al informe todavía**. Se le pidió configurarlos
  como "Saltar a URL/archivo" hacia `informe_scroll.html#ancla`, en la ventana actual.
- Falta el archivo fuente `.story`, necesario para cualquier cambio posterior.
- Falta definir si Presidencia lleva en el mapa los mismos subtemas que el informe.

Cuando llegue la versión definitiva hay que:
1. Verificar que los 15 enlaces apunten a las anclas correctas.
2. Renombrar su `story.html` a `index.html`, para que sea lo que se abre al entrar a
   la carpeta. El informe conserva el nombre `informe_scroll.html`.
3. Rehacer el paquete de `entrega web Registraduria` con las dos piezas juntas, y
   probarlo servido en local antes de entregarlo.
4. Actualizar la nota para informática: el Storyline agrega unos 8 MB, usa fuentes
   `.woff` (registrar como `font/woff`) y envía estadísticas de uso a
   `metrics.articulate.com`, que se puede bloquear sin que deje de funcionar.

## Colores y tipografía: la identidad oficial manda

La Registraduría entregó la paleta y la fuente que hay que usar. Están en
`../Presentación elecciones 2026 información para compartir/Colores y fuente/`
(`Colores Elecciones 2026.pdf` y la carpeta `Fuente Monsterrat`). El detalle
completo está en el skill `registraduria-visual-identity`; lo esencial:

- **Cinco rampas de seis tonos.** Cada rampa es el color base mezclado con
  blanco en pasos de 12% (0, 12, 24, 36, 48, 60%). Las bases son
  azul `#003057`, turquesa `#07d0c5`, verde `#04af27`, rojo `#af0407` y
  naranja `#c96807`.
- **Si hace falta un tono que no está, se calcula con esa misma regla**
  (`canal + (255 − canal) × porcentaje`), nunca se inventa un color nuevo. Los
  pasos adicionales que ya se usan son 70, 78, 86, 92 y 96%.
- **La rampa azul hace de escala de grises.** En este sistema no hay grises
  aparte: los `#334155`, `#94A3B8`, `#E2E8F0` y similares ya no existen.
- **Verde y turquesa solo como relleno, nunca como texto**: no alcanzan
  contraste. Por eso el cambio positivo va en azul `#003057` y el negativo en
  rojo `#af0407`.
- **El color más claro legible como texto sobre blanco es `#5c7a94`.**
- **Tipografía única: Montserrat**, en Regular 400, Medium 500 y Bold 700.
  Nunca 800 ni 900, que no se entregaron. Se carga desde Google Fonts.
- **Excepción: los colores de partido** (`color_map` en
  `scripts/00_preparar_datos.R`) se quedan como están. Repintarlos
  tergiversaría a los partidos.

Las constantes están definidas en dos sitios que conviene reutilizar en vez de
escribir hex sueltos: `RNEC_AZUL`, `RNEC_SEQ`, `RNEC_SEQ_N`, `RNEC_DIV` y
compañía en el chunk `setup` del `.qmd` para los mapas de R, y las celdas OJS
`paleta-rnec` (`rnec_seq`, `rnec_div`) y `paleta-rnec-clases` (`rnec_part`,
`rnec_leyenda_clases`) para las escalas de d3 y Plot.

**Los coropletas necesitan la rampa ampliada, no los seis pasos publicados.**
El más claro de los seis, `#99acbc`, ya es un tono medio: un mapa construido
solo con ellos sale plano. Por eso `RNEC_SEQ` arranca en `#dbe2e7` (pasos 70,
78 y 86%). Y cuando los datos se amontonan —la participación municipal vive
entre 40% y 60%, el censo del exterior va de 11 a 405.325— la escala continua
tampoco sirve: van clases explícitas, más finas donde están los datos
(`RNEC_CORTES_PART` y `RNEC_CORTES_CENSO`). Los cortes de participación son los
mismos en Senado, Cámara y las dos vueltas, para que los cuatro mapas se puedan
comparar entre sí.

**Para coropletas con muchas clases, solo sirve la rampa azul.** Medida en L*,
la azul va de 89 a 19, unos 10 puntos por paso; la naranja se detiene en
`#c96807` y va de 93 a 54, unos 5 puntos por paso, y por debajo de 5 dos
rellenos contiguos se ven iguales. La naranja aguanta tres o cuatro clases; más
allá de eso, o si hay que distinguir los valores altos entre sí, va azul. Y en
una variable sesgada hay que cerrar las clases de arriba para que los grandes
no queden contiguos: con los cortes del censo del exterior, Canadá cae en
`#3d627f` y Estados Unidos en `#003057`, con Venezuela en medio.

## Diseño y Presentación Visual
- DEBES mantener una estructura de diseño muy agradable, limpia y profesional (estilo SaaS / Dashboard moderno).
- El informe es para proyectar en vivo: EVITA crear secciones con scroll vertical excesivo. Los bloques visuales y mapas deben caber bien en la pantalla.
- Toda gráfica o mapa debe ir acompañado de un texto breve y muy resumido que explique los hallazgos. Este texto DEBE ser dinámico y recalcularse según el filtro activo.
- Mantén un tono estrictamente analítico, político e institucional.
- NO uses adjetivos valorativos, conclusiones subjetivas o palabras redundantes (evita términos como "sobrerrepresentado", "modelo predominante", "curiosamente"). Limítate a señalar objetivamente quién tiene la mayor proporción, el mayor aumento o la mayor caída.

## Tablas y Listados
- NO uses colores de fondo sólidos para colorear celdas completas de datos numéricos. Usa colores en el texto o componentes sutiles.
- Para deltas/cambios (ej. 2026 vs 2022), usa etiquetas tipo "píldora" (badges): fondo `#dbe2e7` con texto `#003057` para +, fondo `#f9ebeb` con texto `#af0407` para -, en negrita y con bordes redondeados.
- Mantén las cabeceras limpias (`#ebeef2` o fondos blancos con bordes `#dbe2e7`). Reserva el azul fuerte `#003057` solo para las filas de "Total general".
- Los mapas SIEMPRE van acompañados de una tabla lateral.
  - Si es departamental: Muestra todos los departamentos ordenados según la variable.
  - Si es municipal: Muestra solo un top 20 (permitiendo filtrar entre top 20 más altos o más bajos).

## Mapas (Leaflet) y Basemap
- Basemap ÚNICO: Esri World_Light_Gray_Base. NUNCA uses CARTO ni CartoDB (hoy estampa "API KEY REQUIRED" sobre los mapas).
  - En R: NUNCA uses `addProviderTiles("CartoDB.Positron")` ni URLs de `cartocdn`. Usa siempre `addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}")`.
  - En OJS: usa la misma URL de Esri como string literal.
- Conviven mapas OJS-Leaflet y R-leaflet. Para que no choquen, cada celda OJS hace su require con NOMBRE PROPIO (L_di, L_cm, etc.), nunca un `L` genérico.
- NUNCA uses interpolación de strings (`${"url"}`) para cargar el CSS de Leaflet, VS Code lo corrompe. Usa HTML literal: html`<link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css">`.
- Patrón de inicialización OJS: `const div = html`<div style="height:..."></div>`; const map = L_xx.map(div,...); ...addTo(map); div.value = map; return div;` (NUNCA uses `yield`).
- Patrón de actualización OJS (CRÍTICO): En la celda reactiva que lee `map.value`, DEBES limpiar las capas anteriores antes de agregar nuevas: `if (map._dl) map.removeLayer(map._dl); if (map._legend) map.removeControl(map._legend);`.
- En mapas departamentales, el nombre del departamento debe ser visible en el diseño o inferible fácilmente, no dependas solo del tooltip (mouse hover).
- Mapas R-leaflet con ~1.100 polígonos municipales son pesados: no apiles muchos. Los OJS-Leaflet no consumen memoria.

## Reglas para Observable Plot
- Para evitar que las etiquetas numéricas corten las líneas del gráfico, aplica SIEMPRE un borde blanco al texto: `stroke: "white", strokeWidth: 4, paintOrder: "stroke"`.
- Si hay múltiples líneas/puntos, NUNCA uses un `dy` estático. Calcula el `dy` dinámicamente comparando el valor actual con los adyacentes para evitar cruces.
- Usa SIEMPRE `insetTop` e `insetBottom` en el eje Y para evitar que los textos se estrellen con los bordes o títulos.
- Si los cambios entre categorías son minúsculos (ej. 0% a 1%), NO uses gráficos de líneas (slope charts) ni flechas. Usa barras agrupadas (dodged bars) con `Plot.barX` usando la propiedad `fy`.
- Las leyendas personalizadas complejas deben organizarse usando CSS Grid (`display: grid; grid-template-columns: repeat(auto-fill, minmax(200px, 1fr));`) para crear columnas perfectas.

## Manejo de Datos (R hacia OJS)
- 00_preparar_datos.R hace el trabajo pesado y guarda `.rds`. El `.qmd` solo lee. NO reproceses CSV/Excel crudos salvo orden explícita.
- NUNCA pases objetos `sf` pesados a OJS incrustados dentro de un `.rds`. Exporta las geometrías como `.geojson` separados y cárgalos en OJS usando `FileAttachment("...").json()`.
- El `.rds` solo debe contener dataframes tabulares ligeros.
- Manejo de NAs: Trata explícitamente los datos sin cruce espacial (ej. COD_DPTO 88 / Voto en el Exterior). Asígnales un nombre "Exterior" para las tablas, pero asegura que el filtro OJS de Leaflet los excluya (`d => d.coddepto != null`) para no romper el mapa buscando centroides inexistentes.
- Manejo seguro de tooltips/textos: Usa optional chaining en JS (`d?.valor || 0`) y validaciones en R (`if (is.na(x)) "—"`) para evitar errores en variables reactivas.

## Convenciones Electorales (Estables)
- Dos códigos de municipio distintos, NO intercambiables:
  - `code_RNEC` (código de la RNEC). `COD_DPTO = floor(code_RNEC/1000)`. COD_DPTO 88 = exterior.
  - `codmpio` (DANE, entero). `coddepto = floor(codmpio/1000)`.
- `puesto` / `codpuesto` son CHARACTER (los ceros a la izquierda importan).
- Exterior (COD_DPTO 88): los puestos 81-86 son días de votación anticipada con censo falso; se consolidan al puesto del domingo.
- Curules: D'Hondt para Senado nacional y Cámara territorial; Hamilton/Hare para circunscripciones especiales. Total 283 (validados contra E-26).

## Cómo trabajar
- Cuando te pida un cambio, edítalo directamente en el archivo (no me pases el
  código para que yo lo pegue). 
- Archivos principales: informe_scroll.qmd, estilos.scss, 00_preparar_datos.R.
  Edítalos cuando yo te lo pida explícitamente; no los reescribas por iniciativa
  propia (p.ej. mientras exploras o pruebas algo) sin confirmarme antes.
- Siempre solo modificar los rds que sean necesario para la sección o cambios que dice la instrucción.
  Si son rds que no se usan en la instrucción que estoy pidiendo nunca cambiarlos.  
- El resto de archivos los puedes crear y editar con libertad para la tarea. A excepción si es un archivo que me puede
  cambiar el resto de la presentación, en ese caso si preguntar si lo puede modificar. 
- Si un requisito es ambiguo, pregunta antes de escribir código.
- Responde siempre en español.
- Nunca agregar notas de pie de pagina si no explícitamente solicito incluirlas.
- Los porcentajes van pegados al signo: `45%`, `12,3%`. Nunca `45 %`.
- Después de cambiar el `.qmd` o los `.rds`, recordar que la versión publicada en
  Posit solo se actualiza al hacer commit y push, porque despliega desde GitHub.
- Hay otro proyecto en la carpeta vecina, `../Presentación elecciones 2026 información
  para compartir/visor-gemini-publicacion/`, que es un visor web de otra persona con su
  propio CLAUDE.md. No mezclar los dos.

### Comandos: formas exactas para no pedir permiso
Las reglas de .claude/settings.json solo funcionan si el comando se escribe tal
cual. Respeta estas formas al pie de la letra.

- NUNCA uses `awk`. Todo lo que hago con él ya lo cubren `sed` y `grep`, que no
  piden permiso:
  - rango de líneas: `sed -n 'A,Bp' archivo`  (no `awk 'NR>=A && NR<=B'`)
  - una línea suelta: `sed -n 'Np' archivo`
  - filtrar por columna: `grep -oE '...'` o `cut -d: -f2`
- NUNCA uses `python - <<'PY'` ni `Rscript` con heredoc. Escribe el script con la
  herramienta Write en temporales y luego ejecútalo por ruta. Así el script queda
  visible en pantalla antes de correr, y la ruta está autorizada:
  - `python /tmp/x.py`
  - `"/c/Program Files/R/R-4.6.1/bin/Rscript.exe" /tmp/x.R`   (con comillas, y sin
    `timeout` delante: usa el parámetro timeout de la propia herramienta Bash)
- Para editar informe_scroll.qmd, estilos.scss o 00_preparar_datos.R usa Edit/Write,
  nunca un script que reescriba el archivo. Edit muestra el diff.
- Antes de tocar esos tres archivos, respáldalos con `cp <archivo> /tmp/...`.
- Crea el directorio temporal una sola vez por sesión (`mkdir -p /tmp/<nombre>`),
  no uno por tarea.
- Si la ruta de R cambia de versión, hay que actualizar el patrón en
  .claude/settings.json (hoy apunta a R-4.6.1).

## Anti-Clichés Visuales (Reglas Estrictas de Diseño UI)
- PROHIBIDO el "Diseño típico de IA" (cajas grises genéricas, sombras difusas gigantes, botones azules estándar).
- PROHIBIDO el "KPI Genérico": NUNCA uses bordes izquierdos gruesos de colores (`border-left: 4px solid...`) para decorar tarjetas o métricas.
- Controles modernos: NUNCA uses botones de radio nativos (`<input type="radio">`). Si necesitas filtros (ej. Senado/Cámara), diseña controles segmentados (segmented controls) o botones tipo píldora (pill buttons) que cambien de fondo al estar activos.
- Tipografía y números: Montserrat, siempre. Los números gigantes de los KPIs deben ir alineados a la izquierda, con un tamaño masivo (ej. 40px), tracking apretado, en Bold 700, y TODOS en el azul principal `#003057`. 
- Jerarquía por color: NO pongas cada número principal de un color distinto. Reserva los colores de alerta (rojo oscuro, verde oscuro, ámbar) ÚNICAMENTE para los pequeños deltas (las "píldoras" de cambio de porcentaje).
- Separación limpia: Jerarquía visual por color y peso tipográfico, NO por cajas. Evita encerrar todo en bordes y NO uses líneas verticales para separar bloques. Usa CSS Grid con `gap` (espacios en blanco) para separar elementos.