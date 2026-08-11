#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: setup
library(dplyr); library(leaflet); library(sf); library(scales); library(htmltools)

# Todo viene de un solo archivo -> el proyecto es autosuficiente (publicable).
datos_div <- readRDS("datos_divipole.rds")
sf_mpios  <- datos_div$sf_mpios
sf_deptos <- datos_div$sf_deptos

datos_cand <- readRDS("datos_candidaturas.rds")
datos_pre <- readRDS("datos_preconteo.rds")
datos_esc_gral <- readRDS("datos_escrutinio_gral.rds")
datos_esc_curules <- readRDS("datos_escrutinio_curules.rds")
datos_part <- readRDS("datos_participacion_final.rds")


#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: datos-ojs
ojs_define(resumen_ojs = datos_div$resumen)
ojs_define(paises_ojs  = data.frame(n = datos_div$n_paises))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: mapa-fn
mapa_divipole <- function(eleccion_sel) {
  mp <- sf_mpios %>% mutate(codmpio = as.integer(codmpio)) %>%
    left_join(filter(datos_div$mpio_2026, eleccion == eleccion_sel) %>%
                mutate(codmpio = as.integer(codmpio)), by = "codmpio")
  dp <- sf_deptos %>% mutate(coddepto = as.integer(coddepto)) %>%
    left_join(filter(datos_div$depto_2026, eleccion == eleccion_sel) %>%
                mutate(coddepto = as.integer(coddepto)), by = "coddepto")

  pal_mp <- colorNumeric("YlGnBu", domain = log10(mp$censo + 1), na.color = "#eef2f6")
  pal_dp <- colorNumeric("YlGnBu", domain = log10(dp$censo + 1), na.color = "#eef2f6")

  lab_mp <- sprintf(
    "<b>%s</b><br>Puestos: %s &nbsp;·&nbsp; Mesas: %s<br>Censo: %s<br>Hombres: %s &nbsp;·&nbsp; Mujeres: %s",
    mp$Municipio, comma(mp$n_puestos), comma(mp$n_mesas), comma(mp$censo),
    comma(mp$censo_h), comma(mp$censo_m)) %>% lapply(HTML)
  lab_dp <- sprintf(
    "<b>%s</b><br>Puestos: %s &nbsp;·&nbsp; Mesas: %s<br>Censo: %s<br>Hombres: %s &nbsp;·&nbsp; Mujeres: %s",
    dp$Depto, comma(dp$n_puestos), comma(dp$n_mesas), comma(dp$censo),
    comma(dp$censo_h), comma(dp$censo_m)) %>% lapply(HTML)

  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = mp, weight = 0.3, color = "#ffffff",
                fillColor = ~pal_mp(log10(censo + 1)), fillOpacity = 0.85,
                label = lab_mp, group = "Municipios",
                highlightOptions = highlightOptions(weight = 1.5, color = "#0B2E63", bringToFront = TRUE)) %>%
    addPolygons(data = dp, weight = 0.6, color = "#ffffff",
                fillColor = ~pal_dp(log10(censo + 1)), fillOpacity = 0.85,
                label = lab_dp, group = "Departamentos",
                highlightOptions = highlightOptions(weight = 2, color = "#0B2E63", bringToFront = TRUE)) %>%
    addLayersControl(baseGroups = c("Municipios", "Departamentos"),
                     options = layersControlOptions(collapsed = FALSE))
}
#
#
#
#
#
mapa_divipole("Congreso")
```
#
#
mapa_divipole("Presidencia")
```
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: mapa-mundo
ext <- datos_div$ext_mundo
pal_w <- colorNumeric("YlOrRd", domain = log10(ext$censo + 1), na.color = "#e9edf2")
lab_w <- ifelse(is.na(ext$censo),
  sprintf("<b>%s</b><br>Sin consulado habilitado", ext$name),
  sprintf("<b>%s</b><br>Censo: %s<br>Puestos: %s &nbsp;·&nbsp; Mesas: %s",
          coalesce(ext$pais, ext$name), comma(ext$censo), comma(ext$n_puestos), comma(ext$n_mesas))) %>%
  lapply(HTML)

leaflet(ext) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 0.3, color = "#ffffff",
              fillColor = ~pal_w(log10(censo + 1)),
              fillOpacity = ~ifelse(is.na(censo), 0.12, 0.9),
              label = lab_w,
              highlightOptions = highlightOptions(weight = 1.5, color = "#0B2E63", bringToFront = TRUE)) %>%
  setView(lng = 0, lat = 20, zoom = 1)
```
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: cand-helpers
maxc_cc <- datos_cand$tabla_corpcirc %>% filter(!circ %in% c("Subtotal","Total general")) %>%
  { max(c(.$candidatos_2022, .$candidatos_2026), na.rm = TRUE) }
barra <- function(v, maxv, fill) {
  if (is.na(v)) return("—")
  w <- if (maxv > 0) 100 * v / maxv else 0
  sprintf('<div style="display:flex;align-items:center;gap:8px;"><span style="min-width:46px;text-align:right;font-variant-numeric:tabular-nums;color:#0B2E63;font-weight:500;">%s</span><div style="flex:1;background:#EAF0F6;border-radius:4px;height:13px;"><div style="width:%.1f%%;background:%s;height:100%%;border-radius:4px;"></div></div></div>',
          formatC(v, format="d", big.mark="."), w, fill)
}
tinte <- function(p) { if (is.na(p)) return("#f6f6f6")
  t <- max(0, min(1, (p - 0.30) / 0.25))
  sprintf("#%02X%02X%02X", round(255+t*(188-255)), round(255+t*(220-255)), round(255+t*(154-255))) }
pctf <- function(p) if (is.na(p)) "—" else gsub("\\.", ",", sprintf("%.1f%%", 100*p))

tarjetas_pres <- function(vuelta_sel) {
  d <- datos_cand$cand_presidencia %>% filter(vuelta == vuelta_sel)
  tot <- nrow(d); muj <- sum(d$genero == "F", na.rm = TRUE)
  cards <- paste(sprintf(
    '<div class="cand-card"><div class="cand-nom">%s</div><div class="cand-part">%s</div><span class="cand-gen %s">%s</span></div>',
    d$nomcandi, d$partido, ifelse(d$genero=="F","f","m"), ifelse(d$genero=="F","Mujer","Hombre")), collapse="")
  htmltools::HTML(sprintf(
    '<div class="fichas"><div class="ficha destacada"><div class="ficha-valor">%d</div><div class="ficha-label">Candidaturas</div></div><div class="ficha"><div class="ficha-valor">%s%%</div><div class="ficha-label">Mujeres</div></div></div><div class="cand-grid">%s</div>',
    tot, gsub("\\.", ",", sprintf("%.1f", 100*muj/max(tot,1))), cards))
}
#
#
#
#
#
#
#| label: tabla-corpcirc
tc <- datos_cand$tabla_corpcirc
fila_cc <- function(r) {
  if (r$circ %in% c("Subtotal","Total general")) {
    et <- if (r$circ=="Total general") "Total general" else paste0("Subtotal ", r$corp)
    bg <- if (r$circ=="Total general") "#DCE6F2" else "#EAF0F6"
    sprintf('<tr style="background:%s;font-weight:500;border-top:0.5px solid var(--border-strong);"><td style="padding:11px 12px;">%s</td><td style="padding:11px 12px;font-variant-numeric:tabular-nums;">%s</td><td style="padding:11px 12px;font-variant-numeric:tabular-nums;">%s</td><td style="padding:11px 12px;text-align:center;font-variant-numeric:tabular-nums;">%s</td><td style="padding:11px 12px;text-align:center;font-variant-numeric:tabular-nums;">%s</td></tr>',
      bg, et, formatC(r$candidatos_2022,format="d",big.mark="."), formatC(r$candidatos_2026,format="d",big.mark="."),
      pctf(r$pct_muj_2022), pctf(r$pct_muj_2026))
  } else {
    et <- sprintf('<div style="font-size:11.5px;color:var(--text-secondary);">%s</div><div style="font-weight:500;">%s</div>', r$corp, r$circ)
    sprintf('<tr style="border-top:0.5px solid var(--border);"><td style="padding:9px 12px;">%s</td><td style="padding:9px 12px;">%s</td><td style="padding:9px 12px;">%s</td><td style="padding:9px 12px;text-align:center;background:%s;font-variant-numeric:tabular-nums;">%s</td><td style="padding:9px 12px;text-align:center;background:%s;font-variant-numeric:tabular-nums;">%s</td></tr>',
      et, barra(r$candidatos_2022, maxc_cc, "#8BBCE0"), barra(r$candidatos_2026, maxc_cc, "#355C7D"),
      tinte(r$pct_muj_2022), pctf(r$pct_muj_2022), tinte(r$pct_muj_2026), pctf(r$pct_muj_2026))
  }
}
filas <- paste(vapply(seq_len(nrow(tc)), function(i) fila_cc(tc[i,]), character(1)), collapse="")
htmltools::HTML(sprintf('<table style="width:100%%;border-collapse:collapse;font-size:13px;border:0.5px solid var(--border);border-radius:12px;overflow:hidden;"><thead><tr style="background:#0B2E63;color:#fff;"><th style="text-align:left;padding:10px 12px;font-weight:500;">Circunscripción</th><th style="text-align:left;padding:10px 12px;font-weight:500;">Candidaturas 2022</th><th style="text-align:left;padding:10px 12px;font-weight:500;">Candidaturas 2026</th><th style="text-align:center;padding:10px 12px;font-weight:500;">%% mujeres 2022</th><th style="text-align:center;padding:10px 12px;font-weight:500;">%% mujeres 2026</th></tr></thead><tbody>%s</tbody></table>', filas))
#
#
#
#
tarjetas_pres("Presidencia 1ª vuelta")
#
#
#
#
tarjetas_pres("Presidencia 2ª vuelta")
#
#
#
#
#
#
#
#
#
#
#| label: fechas-ojs
ojs_define(fechas_ojs = datos_cand$conteo_fechas)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: tipo-ojs
ojs_define(tipo_ojs = datos_cand$tipo_wide)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: filtros-ojs
ojs_define(hm_ojs = datos_cand$hm_base)
ojs_define(sincoal_ojs = datos_cand$sincoal)
ojs_define(citrep_ojs = datos_cand$citrep_cambio)
ojs_define(npart_ojs = datos_cand$n_part)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: mapa-camara
cm <- sf_deptos %>% mutate(coddepto = as.integer(coddepto)) %>%
  left_join(datos_cand$camara_depto, by = "coddepto")
maxabs <- max(abs(cm$cambio), na.rm = TRUE)
pal_c <- colorNumeric(c("#2171b5", "#f7f7f7", "#cb181d"), domain = c(-maxabs, maxabs), na.color = "#eeeeee")
lab_c <- sprintf("<b>%s</b><br>2022: %s &nbsp;·&nbsp; 2026: %s<br>Cambio: %s",
  cm$Depto, comma(cm$n_2022), comma(cm$n_2026),
  ifelse(is.na(cm$cambio), "—", sprintf("%+d", cm$cambio))) %>% lapply(htmltools::HTML)
leaflet(cm) %>% addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 0.5, color = "#ffffff", fillColor = ~pal_c(cambio), fillOpacity = 0.85,
              label = lab_c,
              highlightOptions = highlightOptions(weight = 2, color = "#0B2E63", bringToFront = TRUE)) %>%
  addLegend(pal = pal_c, values = ~cambio, title = "Cambio 2026 − 2022", position = "bottomright")
```
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: preconteo-ojs
ojs_define(vel_ojs = datos_pre$vel_2026)
ojs_define(hitos_ojs = datos_pre$hitos_mesas)
ojs_define(comp_ojs = datos_pre$comp_22_26)
ojs_define(hitoscomp_ojs = datos_pre$hitos_comp)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: mapa-preconteo-senado
dep <- sf_deptos %>% mutate(coddepto = as.integer(coddepto)) %>%
  left_join(datos_pre$dep_senado %>% mutate(coddepto = as.integer(coddepto)), by = "coddepto")
ord_hora <- c("4PM–6PM","6PM–8PM","8PM–9PM","9PM–10PM","10PM–12AM","12AM–2AM","2AM o más")
pal_base <- c("#DCE6F1","#BDD7EE","#9BC2E6","#7FB3E6","#5B9BD5","#2F75B5","#1F4E79")

dep$cat_f <- factor(dep$cat_hora, levels = ord_hora)
pal_h <- colorFactor(pal_base, levels = ord_hora, na.color = "#eeeeee")

lab_s <- sprintf("<b>%s</b><br>99%% a las %s", dep$Depto,
                 ifelse(is.na(dep$hora_99_txt), "—", dep$hora_99_txt)) %>% lapply(htmltools::HTML)

leaflet(dep) %>% addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 0.5, color = "#ffffff", fillColor = ~pal_h(cat_f), fillOpacity = 0.88,
              label = lab_s,
              highlightOptions = highlightOptions(weight = 2, color = "#0B2E63", bringToFront = TRUE)) %>%
  addLegend(pal = pal_h, values = factor(ord_hora, levels = ord_hora), title = "Hora de llegada al 99%", position = "bottomright")
```
#
#
#
#
#
#
#
#
#
#
#
#| label: esc
ojs_define(esc_gral_ojs = datos_esc_gral)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: curules-ojs
ojs_define(curules_gral_ojs = datos_esc_curules)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: part-ojs
ojs_define(part_nac_ojs = datos_part$nacional)
ojs_define(part_total_ojs = datos_part$total_congreso)
ojs_define(part_dep_ojs = datos_part$departamental)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: mapa-part-dep
sf_dep <- datos_part$sf_deptos %>% mutate(coddepto = as.integer(coddepto))
part_dep <- datos_part$departamental %>% filter(corp_tabla == "Senado") %>%
  mutate(coddepto = as.integer(coddepto))
mp <- sf_dep %>% left_join(part_dep, by = "coddepto")

mapa_dep <- function(var, titulo, es_cambio) {
  vals <- mp[[var]]
  if (es_cambio) {
    m <- max(abs(vals), na.rm = TRUE)
    pal <- colorNumeric(c("#cb181d", "#f7f7f7", "#2171b5"), domain = c(-m, m), na.color = "#eeeeee")
  } else {
    pal <- colorNumeric("Blues", domain = vals, na.color = "#eeeeee")
  }
  lab <- sprintf("<b>%s</b><br>2026: %s%%<br>vs 2022: %s pp &nbsp;·&nbsp; vs 2018: %s pp",
    mp$Depto, formatC(mp$part_2026, 1, format = "f"),
    formatC(mp$cambio_22, 1, format = "f"), formatC(mp$cambio_18, 1, format = "f")) %>%
    lapply(htmltools::HTML)
  leaflet(mp, height = 540) %>% addProviderTiles("CartoDB.Positron") %>%
    addPolygons(weight = 0.6, color = "#ffffff", fillColor = pal(vals), fillOpacity = 0.88,
                label = lab,
                highlightOptions = highlightOptions(weight = 2, color = "#0B2E63", bringToFront = TRUE)) %>%
    { if (es_cambio) {
        addLegend(., pal = pal, values = vals, title = titulo, position = "bottomright",
                  bins = 5, opacity = 0.9,
                  labFormat = labelFormat(suffix = " pp"))
      } else {
        addLegend(., pal = pal, values = vals, title = titulo, position = "bottomright",
                  labFormat = labelFormat(suffix = "%"))
      } }
}
#
#
#
#
#
mapa_dep("part_2026", "Participación 2026 (%)", FALSE)
```
#
#
mapa_dep("cambio_22", "Cambio 2026 − 2022 (pp)", TRUE)
```
#
#
mapa_dep("cambio_18", "Cambio 2026 − 2018 (pp)", TRUE)
```
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
