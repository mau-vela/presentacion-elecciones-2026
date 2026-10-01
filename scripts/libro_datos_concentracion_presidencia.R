# =====================================================================
# Datos para el libro · Presidencia · "Concentración de votos"
# ---------------------------------------------------------------------
# Script de solo lectura sobre datos_concentracion_presidencia.rds, el mismo
# del informe. Los resultados se presentan agregados, sin identificar
# candidaturas. El porcentaje de cada mesa es sobre votos válidos
# (candidaturas + voto en blanco, sin nulos ni tarjetones no marcados).
# =====================================================================
suppressMessages({library(dplyr); library(tidyr)})
setwd("C:/Users/nande/Dropbox/Trabajo Registraduria/Presentación elecciones 2026")
options(width = 200, dplyr.summarise.inform = FALSE)

cc  <- readRDS("datos_concentracion_presidencia.rds")
esc <- readRDS("datos_escrutinio_presidencia.rds")

# Votos válidos nacionales por elección (candidaturas + blanco)
validos_nal <- esc$candidatos %>% filter(!codcandi %in% c(997, 998)) %>%
  group_by(vuelta, anno) %>% summarise(validos_nal = sum(votos_escrutinio)) %>%
  filter(anno %in% c(2022, 2026)) %>% mutate(v = if_else(vuelta == "Primera vuelta", 1L, 2L))

# bin 0..15 = [60 + bin*2,5 ; ...), bin 16 = 100% exacto
mesas <- cc$mesas %>% mutate(piso = if_else(bin == 16, 100, 60 + bin * 2.5))
tot_mesas <- cc$mesas_total %>% group_by(anno, vuelta) %>% summarise(mesas_pais = sum(total_mesas))

cat("\n===== 1. Mesas donde alguna de las candidaturas mas votadas supero cada umbral =====\n")
res <- lapply(c(60, 80, 90, 95, 100), function(u)
  mesas %>% filter(piso >= u) %>% group_by(anno, vuelta) %>%
    summarise(umbral = u, mesas = sum(n_mesas), votos = sum(votos_cand))) %>% bind_rows() %>%
  left_join(tot_mesas, by = c("anno", "vuelta")) %>%
  left_join(validos_nal %>% select(anno, v, validos_nal), by = c("anno", "vuelta" = "v")) %>%
  mutate(pct_mesas = 100 * mesas / mesas_pais, pct_votos = 100 * votos / validos_nal,
         vuelta = if_else(vuelta == 1L, "Primera vuelta", "Segunda vuelta")) %>%
  select(vuelta, anno, umbral, mesas, mesas_pais, pct_mesas, votos, pct_votos) %>%
  arrange(vuelta, umbral, anno)
print(as.data.frame(res), row.names = FALSE)

cat("\n===== 2. Municipios donde alguna candidatura supero cada umbral =====\n")
mp <- lapply(c(60, 80, 90, 95), function(u)
  cc$mpios %>% filter(pct >= u) %>% group_by(anno, vuelta) %>%
    summarise(umbral = u, municipios = n_distinct(codmpio), votos = sum(votos_cand))) %>% bind_rows() %>%
  left_join(validos_nal %>% select(anno, v, validos_nal), by = c("anno", "vuelta" = "v")) %>%
  mutate(pct_votos = 100 * votos / validos_nal,
         vuelta = if_else(vuelta == 1L, "Primera vuelta", "Segunda vuelta")) %>%
  arrange(vuelta, umbral, anno)
print(as.data.frame(mp %>% select(vuelta, anno, umbral, municipios, votos, pct_votos)), row.names = FALSE)

cat("\n===== 3. Mesas con 100% exacto: tamano tipico =====\n")
print(as.data.frame(mesas %>% filter(bin == 16) %>% group_by(anno, vuelta) %>%
  summarise(mesas = sum(n_mesas), votos = sum(votos_cand), votos_por_mesa = votos / mesas,
            municipios = n_distinct(codmpio)) %>%
  mutate(vuelta = if_else(vuelta == 1L, "Primera vuelta", "Segunda vuelta"))), row.names = FALSE)

cat("\n===== 4. Diferencia nacional entre primero y segundo (para dimensionar) =====\n")
print(as.data.frame(esc$candidatos %>% filter(!es_especial, anno %in% c(2022, 2026)) %>%
  group_by(vuelta, anno) %>% arrange(desc(votos_escrutinio), .by_group = TRUE) %>%
  summarise(dif_votos = votos_escrutinio[1] - votos_escrutinio[2])), row.names = FALSE)

cat("\n===== 5. Departamentos con mas mesas sobre 90% en 2026 =====\n")
info <- cc$info_mpio
print(as.data.frame(mesas %>% filter(anno == 2026, piso >= 90) %>%
  left_join(info, by = "codmpio") %>% group_by(vuelta, Depto) %>%
  summarise(mesas = sum(n_mesas)) %>% arrange(vuelta, desc(mesas)) %>% group_by(vuelta) %>% slice(1:6) %>%
  mutate(vuelta = if_else(vuelta == 1L, "Primera vuelta", "Segunda vuelta"))), row.names = FALSE)
