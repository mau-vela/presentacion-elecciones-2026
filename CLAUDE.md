# Proyecto: Presentación Elecciones 2026 (RNEC)

## Qué es esto
Documento interactivo de scrollytelling sobre las elecciones 2026 (Congreso y
Presidencia). Archivo principal: informe_scroll.qmd (Quarto closeread-html).
Estilos en estilos.scss. Los datos se preparan en 00_preparar_datos.R, que guarda
todo en .rds autosuficientes que el .qmd solo lee. Se publica en Posit Connect
Cloud (tier gratuito, 4 GB) por auto-deploy de GitHub.

## Diseño y Presentación Visual
- DEBES mantener una estructura de diseño muy agradable, limpia y profesional (estilo SaaS / Dashboard moderno).
- El informe es para proyectar en vivo: EVITA crear secciones con scroll vertical excesivo. Los bloques visuales y mapas deben caber bien en la pantalla.
- Toda gráfica o mapa debe ir acompañado de un texto breve y muy resumido que explique los hallazgos. Este texto DEBE ser dinámico y recalcularse según el filtro activo.
- Mantén un tono estrictamente analítico, político e institucional.
- NO uses adjetivos valorativos, conclusiones subjetivas o palabras redundantes (evita términos como "sobrerrepresentado", "modelo predominante", "curiosamente"). Limítate a señalar objetivamente quién tiene la mayor proporción, el mayor aumento o la mayor caída.

## Tablas y Listados
- NO uses colores de fondo sólidos para colorear celdas completas de datos numéricos. Usa colores en el texto o componentes sutiles.
- Para deltas/cambios (ej. 2026 vs 2022), usa etiquetas tipo "píldora" (badges): fondos muy suaves (azul claro para +, rojo claro para -) con texto oscuro en negrita y bordes redondeados.
- Mantén las cabeceras limpias (gris claro o fondos blancos con bordes sutiles). Reserva los colores oscuros fuertes (ej. #0B2E63) solo para las filas de "Total general".
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
- Tipografía y números: Usa tipografías modernas (familia sans-serif geométrica). Los números gigantes de los KPIs deben ir alineados a la izquierda, con un tamaño masivo (ej. 40px), tracking apretado, y TODOS en el color principal (#0B2E63 o #1E293B). 
- Jerarquía por color: NO pongas cada número principal de un color distinto. Reserva los colores de alerta (rojo oscuro, verde oscuro, ámbar) ÚNICAMENTE para los pequeños deltas (las "píldoras" de cambio de porcentaje).
- Separación limpia: Jerarquía visual por color y peso tipográfico, NO por cajas. Evita encerrar todo en bordes y NO uses líneas verticales para separar bloques. Usa CSS Grid con `gap` (espacios en blanco) para separar elementos.