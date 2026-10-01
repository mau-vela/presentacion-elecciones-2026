# =====================================================================
# Datos para el libro · Presidencia · "Ganador por municipio"
# ---------------------------------------------------------------------
# Script de solo lectura. Muestra en consola las cifras del texto del libro,
# sin nombres de candidaturas. No escribe ni modifica ningún archivo.
#   - 2026: datos_ganador_presidencia.rds (el mismo del informe).
#   - 2018 y 2022: base armonizada de mesas del proyecto, con el mismo método
#     (porcentajes sobre votos válidos = candidaturas + blanco; el primer y
#     segundo lugar solo entre candidaturas).
# =====================================================================
suppressMessages({library(dplyr); library(tidyr)})
setwd("C:/Users/nande/Dropbox/Trabajo Registraduria/Presentación elecciones 2026")
options(width = 200, dplyr.summarise.inform = FALSE)

g26 <- readRDS("datos_ganador_presidencia.rds")
esc <- readRDS("datos_escrutinio_presidencia.rds")
arm <- readRDS("../Presidencia 2026/datos_elecciones_pasadas/presidencia_preconteo_escrutinio_armonizado.rds")$presidencia_mesa_candidato %>%
  filter(anno %in% c(2018, 2022), fuente == "escrutinio", !is.na(codmpio))

# ---- margen municipal 2018 y 2022 con el método del informe ----
mun_hist <- arm %>%
  group_by(anno, vuelta, codmpio, codcandi) %>% summarise(votos = sum(votos, na.rm = TRUE)) %>%
  group_by(anno, vuelta, codmpio) %>%
  mutate(validos = sum(votos[!codcandi %in% c(997, 998)])) %>%
  filter(!codcandi %in% c(996, 997, 998)) %>%
  arrange(desc(votos), .by_group = TRUE) %>%
  summarise(codg = first(codcandi), v1 = votos[1], v2 = votos[2], validos = first(validos)) %>%
  mutate(pct_ganador = 100 * v1 / validos, ventaja_pp = 100 * (v1 - v2) / validos, dif_votos = v1 - v2,
         vuelta = if_else(vuelta == "1ra", "Primera vuelta", "Segunda vuelta")) %>% ungroup()

mun26 <- g26$municipal %>% transmute(anno = 2026, vuelta, codmpio, codg = ganador, pct_ganador, ventaja_pp,
                                     dif_votos = votos_ganador - votos_segundo, validos = votos_validos)
todo <- bind_rows(mun_hist %>% mutate(codg = as.character(codg)) %>% select(names(mun26)), mun26)

cat("\n===== 1. Margen entre primer y segundo lugar por municipio =====\n")
print(as.data.frame(todo %>% group_by(vuelta, anno) %>% summarise(
  municipios = n(), candidaturas_con_triunfos = n_distinct(codg),
  mediana_ventaja = median(ventaja_pp), ventaja_menor_5 = sum(ventaja_pp < 5), ventaja_menor_1 = sum(ventaja_pp < 1),
  menos_100_votos = sum(dif_votos < 100), ventaja_mayor_50 = sum(ventaja_pp > 50),
  ganador_mas_70 = sum(pct_ganador > 70), ganador_menos_40 = sum(pct_ganador < 40),
  pct_validos_en_menor_5 = 100 * sum(validos[ventaja_pp < 5]) / sum(validos)) %>% arrange(vuelta, anno)), row.names = FALSE)

cat("\n===== 2. Municipios 2026 mas reñidos y mas holgados (sin candidaturas) =====\n")
for (v in c("Primera vuelta", "Segunda vuelta")) {
  x <- g26$municipal %>% filter(vuelta == v)
  cat("--", v, "\n")
  print(as.data.frame(x %>% arrange(ventaja_pp) %>% transmute(Municipio, Depto, ventaja_pp = round(ventaja_pp, 2),
        dif_votos = votos_ganador - votos_segundo, votos_validos) %>% head(6)), row.names = FALSE)
  print(as.data.frame(x %>% arrange(desc(ventaja_pp)) %>% transmute(Municipio, Depto, ventaja_pp = round(ventaja_pp, 1),
        pct_ganador = round(pct_ganador, 1)) %>% head(4)), row.names = FALSE)
}

cat("\n===== 3. Departamentos 2026: margen entre primero y segundo =====\n")
dep <- g26$departamental %>% mutate(ventaja = pct_ganador - pct_segundo)
print(as.data.frame(dep %>% group_by(vuelta) %>% summarise(n = n(), menor_5 = sum(ventaja < 5), menor_2 = sum(ventaja < 2),
  mediana = median(ventaja), candidaturas_con_triunfos = n_distinct(ganador))), row.names = FALSE)
print(as.data.frame(dep %>% arrange(vuelta, ventaja) %>% group_by(vuelta) %>% slice(1:4) %>%
  transmute(Depto, ventaja = round(ventaja, 2))), row.names = FALSE)

cat("\n===== 4. Margen nacional entre primero y segundo, sobre votos validos (sin nombres) =====\n")
print(as.data.frame(esc$candidatos %>% group_by(vuelta, anno) %>%
  mutate(validos = sum(votos_escrutinio[!codcandi %in% c(997, 998)])) %>%
  filter(!es_especial) %>% arrange(desc(votos_escrutinio), .by_group = TRUE) %>%
  summarise(pct_1 = 100 * votos_escrutinio[1] / first(validos), pct_2 = 100 * votos_escrutinio[2] / first(validos),
            ventaja_pp = pct_1 - pct_2, dif_votos = votos_escrutinio[1] - votos_escrutinio[2])), row.names = FALSE)

cat("\n===== 5. Preconteo frente a escrutinio (consolidacion de resultados) =====\n")
print(as.data.frame(esc$general %>% select(vuelta, anno, votos_preconteo, votos_escrutinio, mesas_escrutinio, cambio_neto, cambio_neto_pct)), row.names = FALSE)
cat("\nCandidaturas: mayor cambio porcentual entre preconteo y escrutinio (sin nombres)\n")
print(as.data.frame(esc$candidatos %>% filter(!es_especial) %>% group_by(vuelta, anno) %>%
  summarise(max_abs_cambio_pct = max(abs(cambio_neto_pct)), orden_igual = TRUE)), row.names = FALSE)
cat("\nOrden de las candidaturas igual en preconteo y escrutinio:\n")
print(as.data.frame(esc$candidatos %>% filter(!es_especial) %>% group_by(vuelta, anno) %>%
  summarise(mismo_orden = identical(order(-votos_preconteo), order(-votos_escrutinio)))), row.names = FALSE)
