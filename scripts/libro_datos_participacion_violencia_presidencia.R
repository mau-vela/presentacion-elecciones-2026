# =====================================================================
# Datos para el libro · Presidencia · "Participación y violencia"
# ---------------------------------------------------------------------
# Script de solo lectura: toma los .rds y el .geojson que ya usa el informe
# y muestra en consola las cifras que sustentan el texto del libro. No
# escribe ni modifica ningún archivo del proyecto.
# =====================================================================
suppressMessages({library(dplyr); library(tidyr); library(sf)})
setwd("C:/Users/nande/Dropbox/Trabajo Registraduria/Presentación elecciones 2026")
options(width = 200, dplyr.summarise.inform = FALSE)

moe  <- readRDS("datos_riesgo_moe_presidencia.rds")
geo  <- st_read("riesgo_moe_presidencia.geojson", quiet = TRUE) %>% st_drop_geometry()
pmun <- readRDS("datos_pres_municipal.rds")$municipal

cat("\n===== 1. Riesgo de violencia: con y sin riesgo =====\n")
print(as.data.frame(moe$plot_barras %>% filter(variable == "Riesgo de violencia")), row.names = FALSE)

cat("\n===== 2. Riesgo de violencia por nivel =====\n")
print(as.data.frame(moe$plot_lineas %>% filter(variable == "Riesgo de violencia") %>%
  select(vuelta, nivel, part_media, n_mun)), row.names = FALSE)

cat("\n===== 3. Brecha sin riesgo - con riesgo, todos los factores =====\n")
print(as.data.frame(moe$plot_barras %>% select(vuelta, variable, grupo, part_media) %>%
  pivot_wider(names_from = grupo, values_from = part_media) %>%
  mutate(brecha = `Sin riesgo` - `Con riesgo`) %>% arrange(vuelta, desc(brecha))), row.names = FALSE)

cat("\n===== 4. Cruce riesgo x participacion (mapa) =====\n")
for (s in c("1v", "2v")) {
  cat("--", s, "\n")
  print(table(geo[[paste0("grupo_mapa_", s)]]))
}

cat("\n===== 5. Departamentos criticos (riesgo + participacion baja) =====\n")
print(as.data.frame(moe$tabla %>% select(vuelta, Depto, n_mun, part_prom)), row.names = FALSE)

cat("\n===== 6. Municipios con riesgo extremo: menor y mayor participacion =====\n")
for (s in c("1v", "2v")) {
  x <- geo %>% filter(riesgo_txt == "Riesgo Extremo") %>%
    transmute(Municipio, Depto, part = .data[[paste0("part_pres_", s)]]) %>% filter(!is.na(part))
  cat("--", s, "| municipios con riesgo extremo:", nrow(x), "\n  mas baja:\n")
  print(as.data.frame(x %>% arrange(part) %>% head(8)), row.names = FALSE)
  cat("  mas alta:\n"); print(as.data.frame(x %>% arrange(desc(part)) %>% head(8)), row.names = FALSE)
}

cat("\n===== 7. Historico: participacion 2018-2022-2026 segun el riesgo de violencia 2026 =====\n")
# Mismos municipios clasificados con el mapa MOE 2026; la participacion de 2018
# y 2022 se reconstruye con los cambios en puntos que trae el rds municipal.
hist <- pmun %>%
  mutate(part_2022 = part_2026 - cambio_22, part_2018 = part_2026 - cambio_18) %>%
  inner_join(geo %>% select(codmpio, riesgo_txt), by = "codmpio")
print(as.data.frame(hist %>%
  mutate(grupo = if_else(riesgo_txt == "Sin Riesgo", "Sin riesgo", "Con riesgo")) %>%
  group_by(vuelta, grupo) %>%
  summarise(n = n(), p2018 = mean(part_2018, na.rm = TRUE), p2022 = mean(part_2022, na.rm = TRUE),
            p2026 = mean(part_2026, na.rm = TRUE))), row.names = FALSE)
print(as.data.frame(hist %>% group_by(vuelta, riesgo_txt) %>%
  summarise(n = n(), p2018 = mean(part_2018, na.rm = TRUE), p2022 = mean(part_2022, na.rm = TRUE),
            p2026 = mean(part_2026, na.rm = TRUE))), row.names = FALSE)

cat("\n===== 8. Cortes de participacion baja/alta (terciles municipales por vuelta) =====\n")
print(as.data.frame(pmun %>% filter(codmpio %in% geo$codmpio, !is.na(part_2026)) %>% group_by(vuelta) %>%
  summarise(q1 = quantile(part_2026, 1/3), q2 = quantile(part_2026, 2/3), n = n())), row.names = FALSE)

cat("\n===== 9. Censo 2026 (DIVIPOL Presidencia) y participacion ponderada por censo =====\n")
censo <- readRDS("datos_divipole.rds")$mpio_2026 %>% filter(eleccion == "Presidencia") %>%
  transmute(codmpio = as.integer(codmpio), censo)
pond <- pmun %>% mutate(codmpio = as.integer(codmpio)) %>%
  inner_join(geo %>% transmute(codmpio = as.integer(codmpio), riesgo_txt), by = "codmpio") %>%
  inner_join(censo, by = "codmpio")
print(as.data.frame(pond %>% filter(vuelta == "Primera vuelta") %>%
  group_by(riesgo_txt) %>% summarise(n = n(), censo = sum(censo)) %>%
  mutate(pct_censo = 100 * censo / sum(censo))), row.names = FALSE)
print(as.data.frame(pond %>% mutate(grupo = if_else(riesgo_txt == "Sin Riesgo", "Sin riesgo", "Con riesgo")) %>%
  group_by(vuelta, grupo) %>%
  summarise(simple = mean(part_2026, na.rm = TRUE),
            ponderada = sum(part_2026 * censo, na.rm = TRUE) / sum(censo[!is.na(part_2026)]))), row.names = FALSE)

cat("\n===== 10. Cambio entre primera y segunda vuelta por nivel de riesgo =====\n")
print(as.data.frame(moe$plot_lineas %>% filter(variable == "Riesgo de violencia") %>%
  select(vuelta, nivel, part_media) %>% pivot_wider(names_from = vuelta, values_from = part_media) %>%
  mutate(cambio = `Segunda vuelta` - `Primera vuelta`)), row.names = FALSE)

cat("\n===== 11. Con riesgo y participacion alta: por departamento =====\n")
for (s in c("1v", "2v")) {
  cat("--", s, "\n")
  print(geo %>% filter(.data[[paste0("grupo_mapa_", s)]] == "Con riesgo - Part. alta") %>% count(Depto, sort = TRUE) %>% head(6))
  cat("riesgo extremo dentro de este grupo:",
      sum(geo[[paste0("grupo_mapa_", s)]] == "Con riesgo - Part. alta" & geo$riesgo_txt == "Riesgo Extremo"), "\n")
}

cat("\n===== 12. Municipios con riesgo de mayor censo y participacion ponderada por nivel =====\n")
print(as.data.frame(pond %>% filter(vuelta == "Primera vuelta", riesgo_txt != "Sin Riesgo") %>%
  arrange(desc(censo)) %>% select(Municipio, Depto, riesgo_txt, censo, part_2026) %>% head(10)), row.names = FALSE)
print(as.data.frame(pond %>% group_by(vuelta, riesgo_txt) %>%
  summarise(n = n(), simple = mean(part_2026, na.rm = TRUE),
            ponderada = sum(part_2026 * censo, na.rm = TRUE) / sum(censo[!is.na(part_2026)]))), row.names = FALSE)
cat("\nMunicipios pequenos (censo < 20.000) vs resto, primera vuelta:\n")
print(as.data.frame(pond %>% filter(vuelta == "Primera vuelta") %>%
  mutate(tam = if_else(censo < 20000, "< 20.000", ">= 20.000"),
         grupo = if_else(riesgo_txt == "Sin Riesgo", "Sin riesgo", "Con riesgo")) %>%
  group_by(tam, grupo) %>% summarise(n = n(), simple = mean(part_2026, na.rm = TRUE))), row.names = FALSE)

cat("\n===== 13. Puestos y mesas 2026 (DIVIPOL Presidencia) segun riesgo de violencia =====\n")
div <- readRDS("datos_divipole.rds")
pm <- div$mpio_2026 %>% filter(eleccion == "Presidencia") %>%
  transmute(codmpio = as.integer(codmpio), n_puestos, n_mesas, censo) %>%
  inner_join(geo %>% transmute(codmpio = as.integer(codmpio), riesgo_txt), by = "codmpio")
print(as.data.frame(pm %>% group_by(riesgo_txt) %>%
  summarise(mun = n(), puestos = sum(n_puestos), mesas = sum(n_mesas))), row.names = FALSE)
print(as.data.frame(pm %>% mutate(g = if_else(riesgo_txt == "Sin Riesgo", "Sin riesgo", "Con riesgo")) %>%
  group_by(g) %>% summarise(mun = n(), puestos = sum(n_puestos), mesas = sum(n_mesas))), row.names = FALSE)
print(as.data.frame(div$resumen %>% filter(eleccion == "Presidencia", annoh == 2026)), row.names = FALSE)

cat("\n===== 14. Proporcion de municipios con participacion baja segun riesgo =====\n")
for (s in c("1v", "2v")) {
  g <- geo[[paste0("grupo_mapa_", s)]]
  cat(s, "| con riesgo y part. baja:", sum(g == "Con riesgo - Part. baja"), "de", sum(geo$riesgo_txt != "Sin Riesgo"),
      "| sin riesgo y part. baja:", sum(g == "Sin riesgo - Part. baja"), "de", sum(geo$riesgo_txt == "Sin Riesgo"), "\n")
}
