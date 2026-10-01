# =====================================================================
# Datos para el libro · Presidencia · "Votos en blanco, nulos y no marcados"
# ---------------------------------------------------------------------
# Script de solo lectura sobre datos_ve_presidencia.rds, el mismo del informe.
# Los porcentajes son sobre el total de votos depositados.
# =====================================================================
suppressMessages({library(dplyr); library(tidyr)})
setwd("C:/Users/nande/Dropbox/Trabajo Registraduria/Presentación elecciones 2026")
options(width = 200, dplyr.summarise.inform = FALSE)

ve <- readRDS("datos_ve_presidencia.rds")

cat("\n===== 1. Nacional por vuelta, tipo de voto y anio =====\n")
print(as.data.frame(ve$nacional %>% arrange(vuelta, tipo_voto, annoh)), row.names = FALSE)

cat("\n===== 2. Nulos + no marcados juntos (nacional) =====\n")
print(as.data.frame(ve$nacional %>% filter(tipo_voto != "Votos en blanco") %>%
  group_by(vuelta, annoh) %>% summarise(votos = sum(votos), tot = first(tot), pct = 100 * votos / tot)), row.names = FALSE)

cat("\n===== 3. Departamentos: extremos en 2026 =====\n")
for (t in c("Votos nulos", "Votos no marcados")) {
  for (v in c("Primera vuelta", "Segunda vuelta")) {
    d <- ve$depto %>% filter(tipo_voto == t, vuelta == v)
    cat("--", t, "|", v, "| mediana", round(median(d$pct_2026), 2),
        "| rango", round(min(d$pct_2026), 2), "a", round(max(d$pct_2026), 2), "\n")
    print(as.data.frame(d %>% arrange(desc(pct_2026)) %>% transmute(Depto, pct_2026 = round(pct_2026, 2),
      pct_2022 = round(pct_2022, 2), dif_22 = round(dif_22, 2)) %>% head(5)), row.names = FALSE)
    print(as.data.frame(d %>% arrange(pct_2026) %>% transmute(Depto, pct_2026 = round(pct_2026, 2)) %>% head(3)), row.names = FALSE)
  }
}

cat("\n===== 4. Departamentos: cuantos suben y cuantos bajan frente a 2022 y 2018 =====\n")
print(as.data.frame(ve$depto %>% group_by(vuelta, tipo_voto) %>%
  summarise(n = n(), suben_22 = sum(dif_22 > 0), bajan_22 = sum(dif_22 < 0),
            suben_18 = sum(dif_18 > 0), bajan_18 = sum(dif_18 < 0),
            mediana_2026 = median(pct_2026), mediana_2022 = median(pct_2022), mediana_2018 = median(pct_2018))),
  row.names = FALSE)

cat("\n===== 5. Departamentos con mayor aumento y mayor caida frente a 2022 (nulos y no marcados) =====\n")
for (t in c("Votos nulos", "Votos no marcados")) {
  for (v in c("Primera vuelta", "Segunda vuelta")) {
    d <- ve$depto %>% filter(tipo_voto == t, vuelta == v) %>% arrange(desc(dif_22))
    cat("--", t, "|", v, "\n")
    print(as.data.frame(bind_rows(head(d, 3), tail(d, 2)) %>%
      transmute(Depto, pct_2022 = round(pct_2022, 2), pct_2026 = round(pct_2026, 2), dif_22 = round(dif_22, 2))), row.names = FALSE)
  }
}

cat("\n===== 6. Exterior, si aparece como fila departamental =====\n")
print(as.data.frame(ve$depto %>% filter(is.na(coddepto) | Depto == "Exterior") %>% head(10)), row.names = FALSE)
