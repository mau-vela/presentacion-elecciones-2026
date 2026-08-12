# =====================================================================3
# 00_preparar_datos.R   —   Preparacion de datos del informe ----
# Rutas RELATIVAS a: ...\Presentacion elecciones 2026
# =====================================================================3
library(dplyr); library(tidyr); library(stringr); library(readr);library(readxl)
library(readxl); library(sf); library(rstudioapi); library(stringr)
library(rnaturalearth)   # mapa mundial (install.packages c("rnaturalearth","rnaturalearthdata"))
library(lubridate);library(purrr)
Sys.setlocale("LC_ALL", "en_US.UTF-8")

current_path <- getActiveDocumentContext()$path
setwd(dirname(dirname(current_path)))

# ---- RUTAS  ------------------------------------------------2
ruta_general       <- "../datos generales"
ruta_participacion <- "../Participación electoral"
ruta_basicos_cong  <- "../Congreso 2026/datos/PRECONTEO/DIA_ELECTORAL_08_MARZO/ArchivosBasicos_Dia electoral_congreso2026"
ruta_basicos_pres  <- "../Presidencia 2026/preconteo segunda vuelta/datos/archivos básicos"

load(file.path(ruta_general, "municipios.rda"))   # 'todos_municipios'
map_rnec_codmpio <- todos_municipios %>% distinct(code_RNEC, codmpio, Depto, Municipio)
map_depto_nom    <- todos_municipios %>% mutate(coddepto = floor(codmpio/1000)) %>% distinct(coddepto, Depto)
map_rnec_dane_dep <- todos_municipios %>%
  mutate(cod_rnec_dep = code_RNEC %/% 1000, coddepto = floor(codmpio / 1000)) %>%
  distinct(cod_rnec_dep, coddepto)

fw   <- function(x, a, b) str_sub(x, a, b)
num0 <- function(x) suppressWarnings(as.numeric(str_trim(x)))
chr0 <- function(x) str_squish(x)

# =====================================================================3
#  SECCION 01 — DIVIPOLE ----
# =====================================================================3


leer_divipol <- function(ruta_basicos) {
  archivo <- list.files(ruta_basicos, pattern = "^DIVIPOL.*\\.TXT$",
                        full.names = TRUE, ignore.case = TRUE)[1]
  if (is.na(archivo)) stop("No encontre DIVIPOL en: ", ruta_basicos)
  lineas <- readLines(archivo, encoding = "latin1", warn = FALSE)
  tibble(linea = lineas) %>% filter(nchar(linea) >= 146) %>%
    transmute(
      COD_DPTO = num0(fw(linea,1,2)), codmun = num0(fw(linea,3,5)), zona = num0(fw(linea,6,7)),
      puesto = str_pad(chr0(fw(linea,8,9)), 2, pad="0"),
      Depto_divipol = chr0(fw(linea,10,21)), Municipio_divipol = chr0(fw(linea,22,51)),
      nompuesto = chr0(fw(linea,52,91)),
      potencial_hombres = num0(fw(linea,93,100)), potencial_mujeres = num0(fw(linea,101,108)),
      n_mesas_puesto = num0(fw(linea,109,114))
    ) %>%
    mutate(censo = potencial_hombres + potencial_mujeres, code_RNEC = COD_DPTO*1000 + codmun) %>%
    distinct(code_RNEC, zona, puesto, .keep_all = TRUE)
}

# Corrige censo artificial exterior (puestos 81-86). NO toca mesas (igual que tu codigo).
# Marca los dias anticipados del exterior (81-86) para NO contarlos como
# puestos/mesas nuevos y no inflar el censo.
corregir_censo_exterior <- function(df) {
  df %>% mutate(
    dia_anticipado = COD_DPTO == 88 & as.character(puesto) %in% as.character(81:86),
    potencial_hombres = ifelse(dia_anticipado, NA_real_, potencial_hombres),
    potencial_mujeres = ifelse(dia_anticipado, NA_real_, potencial_mujeres),
    censo             = ifelse(dia_anticipado, NA_real_, censo),
    n_mesas_puesto    = ifelse(dia_anticipado, NA_real_, n_mesas_puesto)
  )
}

# Normaliza un censo historico (con columna 'mesas' si existe) al esquema comun
norm_hist <- function(df) {
  if (!"mesas" %in% names(df)) df$mesas <- NA_real_
  df %>% transmute(
    COD_DPTO = as.numeric(coddepto), codmun = as.numeric(codmun), zona = as.numeric(zona),
    puesto = as.character(puesto),
    potencial_hombres = as.numeric(hombres), potencial_mujeres = as.numeric(mujeres),
    censo = as.numeric(censo), n_mesas_puesto = as.numeric(mesas),
    code_RNEC = as.numeric(coddepto)*1000 + as.numeric(codmun)
  )
}

# Lector generico de censo Excel historico (detecta la columna 'mesas' si existe)
leer_censo_excel <- function(path, sheet, skip = 4) {
  raw <- read_excel(path, sheet = sheet, skip = skip)
  names(raw) <- tolower(names(raw))
  out <- raw %>% transmute(
    coddepto = as.numeric(dd), codmun = as.numeric(mm), zona = as.numeric(zz),
    puesto = str_remove(as.character(pp), "^0+(?!$)"),
    hombres = as.integer(hombres), mujeres = as.integer(mujeres), censo = as.integer(total)
  )
  out$mesas <- if ("mesas" %in% names(raw)) as.integer(raw[["mesas"]]) else NA_real_
  out %>% filter(!is.na(coddepto))
}

# ---- 2026 ----------------------------------------------------------3
divipol_cong_26 <- leer_divipol(ruta_basicos_cong) %>% corregir_censo_exterior()
divipol_pres_26 <- leer_divipol(ruta_basicos_pres) %>% corregir_censo_exterior()

# ---- HISTORICO 2018 / 2022 -----------------------------------------3
rp <- function(...) file.path(ruta_participacion, "datos", ...)

# 2018 Congreso (fwf UNIFICADO: ya incluye exterior; TRAE mesas)
censo_congreso_2018 <- read_fwf(rp("DIVIPOL_20180223_111842_01_UNIFICADO.txt"),
                                fwf_widths(c(2,3,2,2,12,30,40,1,8,8,6,102),
                                           c("coddepto","codmun","zona","puesto","nomdepto","nommun","nompuesto","flag","mujeres","hombres","mesas","comuna")),
                                locale = locale(encoding = "Latin1"), col_types = cols(.default = col_character())) %>%
  mutate(coddepto=as.integer(coddepto), codmun=as.integer(codmun), zona=as.integer(zona),
         puesto=str_remove(puesto,"^0+(?!$)"), mujeres=as.integer(mujeres), hombres=as.integer(hombres),
         mesas=as.integer(mesas), censo=mujeres+hombres) %>%
  select(coddepto, codmun, zona, puesto, hombres, mujeres, censo, mesas)

# 2018 Presidencia (base madre SIN exterior + consulados). No trae mesas.
pres_2018_base <- read_excel(rp("Divipol Presidente 2018.xlsx"), sheet="Base Madre ", range="A1:N10999") %>%
  transmute(coddepto=as.numeric(`Code Depto`), codmun=as.numeric(`Code Muni`), zona=as.numeric(Zona),
            puesto=str_remove(as.character(Puesto),"^0+(?!$)"),
            hombres=as.integer(hombres), mujeres=as.integer(mujeres), censo=as.integer(total))
pres_2018_cons <- read_excel(rp("Divipol Presidente 2018.xlsx"), sheet="Consulados ", range="A1:L233") %>%
  transmute(coddepto=as.numeric(dd), codmun=as.numeric(mm), zona=as.numeric(zz),
            puesto=str_remove(as.character(pp),"^0+(?!$)"),
            hombres=as.integer(hombres), mujeres=as.integer(mujeres), censo=as.integer(total))
censo_pres_2018 <- bind_rows(pres_2018_base %>% filter(coddepto != 88), pres_2018_cons)
censo_pres_2018$mesas <- NA_real_

# 2022 (ambos traen 'mesas' en el Excel; el lector la detecta sola)
censo_congreso_2022 <- leer_censo_excel(rp("Divipole_Elecciones_Congreso_13-03-2022.xlsx"), "divipol_20220207_161440")
censo_pres_2022     <- leer_censo_excel(rp("Divipole_Elección_Presidente_2022.xlsx"),      "divipol_20220422_222259")

# Normalizar + corregir exterior
cong_2018 <- censo_congreso_2018 %>% norm_hist() %>% corregir_censo_exterior()
cong_2022 <- censo_congreso_2022 %>% norm_hist() %>% corregir_censo_exterior()
pres_2018 <- censo_pres_2018     %>% norm_hist() %>% corregir_censo_exterior()
pres_2022 <- censo_pres_2022     %>% norm_hist() %>% corregir_censo_exterior()

# ---- (A) Resumen Total / Nacional / Exterior por eleccion y anio ----3
metricas <- function(df) {
  df %>% summarise(
    n_puestos = n_distinct(paste(code_RNEC, zona, puesto)[!(COD_DPTO == 88 & as.character(puesto) %in% as.character(81:86))]),
    n_mesas   = if (all(is.na(n_mesas_puesto))) NA_real_ else sum(n_mesas_puesto, na.rm = TRUE),
    censo     = sum(censo, na.rm = TRUE),
    censo_h   = sum(potencial_hombres, na.rm = TRUE),
    censo_m   = sum(potencial_mujeres, na.rm = TRUE)
  )
}
resumir_niveles <- function(df, eleccion, annoh) {
  bind_rows(
    metricas(df)                            %>% mutate(ambito = "Total"),
    metricas(df %>% filter(COD_DPTO != 88)) %>% mutate(ambito = "Nacional"),
    metricas(df %>% filter(COD_DPTO == 88)) %>% mutate(ambito = "Exterior")
  ) %>% mutate(eleccion = eleccion, annoh = annoh, .before = 1)
}
div_resumen <- bind_rows(
  resumir_niveles(divipol_cong_26, "Congreso", 2026),  resumir_niveles(divipol_pres_26, "Presidencia", 2026),
  resumir_niveles(cong_2018, "Congreso", 2018),        resumir_niveles(cong_2022, "Congreso", 2022),
  resumir_niveles(pres_2018, "Presidencia", 2018),     resumir_niveles(pres_2022, "Presidencia", 2022)
)

# ---- (B) Municipio y departamento 2026 (mapa nacional) -------------3
resumir_mpio <- function(df, eleccion) {
  df %>% filter(COD_DPTO != 88) %>% group_by(code_RNEC) %>%
    summarise(n_puestos=n_distinct(paste(zona,puesto)),
              n_mesas=sum(n_mesas_puesto,na.rm=TRUE), censo=sum(censo,na.rm=TRUE),
              censo_h=sum(potencial_hombres,na.rm=TRUE), censo_m=sum(potencial_mujeres,na.rm=TRUE), .groups="drop") %>%
    left_join(map_rnec_codmpio, by="code_RNEC") %>% filter(!is.na(codmpio)) %>% mutate(eleccion=eleccion)
}
div_mpio_2026 <- bind_rows(resumir_mpio(divipol_cong_26,"Congreso"), resumir_mpio(divipol_pres_26,"Presidencia")) %>%
  mutate(codmpio = as.integer(codmpio))
div_depto_2026 <- div_mpio_2026 %>% mutate(coddepto = as.integer(floor(codmpio/1000))) %>%
  group_by(eleccion, coddepto) %>%
  summarise(across(c(n_puestos,n_mesas,censo,censo_h,censo_m), \(x) sum(x,na.rm=TRUE)), .groups="drop") %>%
  left_join(map_depto_nom, by="coddepto")

# ---- (C) Exterior por pais 2026 (tu diccionario) -> mapa mundial ----3
dic_pais_ext <- divipol_cong_26 %>% filter(COD_DPTO == 88) %>% distinct(nompuesto, zona) %>%
  mutate(pais = case_when(
    nompuesto == "Accra Consulado" ~ "Ghana",
    nompuesto == "Dakar" ~ "Senegal",
    nompuesto=="Abu Dhabi - Consulado"~"Emiratos Árabes Unidos", nompuesto=="Albacete"~"España",
    nompuesto=="Alicante"~"España", nompuesto=="Almería"~"España", nompuesto=="Amsterdam Consulado"~"Países Bajos",
    nompuesto=="Ankara Consulado"~"Turquía", nompuesto=="Antofagasta Consulado"~"Chile", nompuesto=="Argel Consulado"~"Argelia",
    nompuesto=="Asunción - Consulado"~"Paraguay", nompuesto=="Atenas - Grecia"~"Grecia", str_detect(nompuesto,"^Atlanta")~"Estados Unidos",
    nompuesto=="Auckland - Consulado"~"Nueva Zelanda", nompuesto=="Baku Consulado"~"Azerbaiyán", nompuesto=="Bangkok Consulado"~"Tailandia",
    str_detect(nompuesto,"^Barcelona")~"España", nompuesto=="Barquisimeto"~"Venezuela", nompuesto=="Beijing - Consulado"~"China",
    nompuesto=="Beirut - Consulado"~"Líbano", nompuesto=="Belmopán"~"Belice", nompuesto=="Belo Horizonte"~"Brasil",
    nompuesto=="Berlin Consulado"~"Alemania", nompuesto=="Berna - Consulado"~"Suiza", nompuesto=="Bilbao - Consulado"~"España",
    str_detect(nompuesto,"^Boston")~"Estados Unidos", nompuesto=="Brasilia Consulado"~"Brasil", nompuesto=="Bremen"~"Alemania",
    nompuesto=="Bridgetown"~"Barbados", nompuesto=="Brisbane"~"Australia", nompuesto=="Bruselas Consulado"~"Bélgica",
    nompuesto=="Bucarest"~"Rumania", nompuesto=="Budapest Consulado"~"Hungría", nompuesto=="Buenos Aires Consulado"~"Argentina",
    nompuesto=="Calgary Consulado"~"Canadá", nompuesto=="Canberra Consulado"~"Australia", nompuesto=="Cancún Consulado"~"México",
    nompuesto=="Caracas - Consulado"~"Venezuela", nompuesto=="Chicago - Consulado"~"Estados Unidos",
    nompuesto=="Ciudad de México - Consulado"~"México", nompuesto=="Ciudad del Cabo"~"Sudáfrica", nompuesto=="Columbus, Ohio"~"Estados Unidos",
    nompuesto=="Colón - Consulado"~"Panamá", nompuesto=="Copenhague Consulado"~"Dinamarca", nompuesto=="Cuenca"~"Ecuador",
    nompuesto=="Curitiba"~"Brasil", nompuesto=="Córdoba"~"Argentina", nompuesto=="Doha"~"Catar", nompuesto=="Dublín Consulado"~"Irlanda",
    nompuesto=="El Cairo - Consulado"~"Egipto", nompuesto=="Esmeraldas - Consulado"~"Ecuador", nompuesto=="Estambul Consulado"~"Turquía",
    nompuesto=="Estocolmo - Consulado"~"Suecia", nompuesto=="Florencia"~"Italia", nompuesto=="Florianopolis"~"Brasil",
    nompuesto=="Fortaleza"~"Brasil", nompuesto=="Foz de Iguazu"~"Brasil", nompuesto=="Frankfurt Consulado"~"Alemania",
    nompuesto=="Georgetown"~"Guyana", nompuesto=="Ginebra"~"Suiza", nompuesto=="Graz"~"Austria", nompuesto=="Guadalajara Consulado"~"México",
    nompuesto=="Guangzhou Consulado"~"China", str_detect(nompuesto,"^Guasdualito")~"Venezuela", nompuesto=="Guatemala - Consulado"~"Guatemala",
    nompuesto=="Guayaquil - Consulado"~"Ecuador", nompuesto=="Génova"~"Italia", nompuesto=="Hamburgo"~"Alemania",
    nompuesto=="Hanoi Consulado"~"Vietnam", nompuesto=="Hawaii"~"Estados Unidos", nompuesto=="Helsinki - Consulado"~"Finlandia",
    nompuesto=="Hong Kong - Consulado"~"Hong Kong", str_detect(nompuesto,"^Houston")~"Estados Unidos", nompuesto=="Ibiza"~"España",
    nompuesto=="Iquique"~"Chile", nompuesto=="Iquitos - Consulado"~"Perú", nompuesto=="Islas Caimán"~"Islas Caimán",
    nompuesto=="Jakarta Consulado"~"Indonesia", nompuesto=="Jaqué Consulado"~"Panamá", nompuesto=="Kansas City"~"Estados Unidos",
    nompuesto=="Kingston - Consulado"~"Jamaica", nompuesto=="Kualalumpur - Consulado"~"Malasia", nompuesto=="La Habana - Consulado"~"Cuba",
    nompuesto=="La Haya"~"Países Bajos", nompuesto=="La Paz Consulado"~"Bolivia", nompuesto=="Lanzarote"~"España",
    nompuesto=="Lima - Consulado"~"Perú", nompuesto=="Limassol - Chipre"~"Chipre", nompuesto=="Lisboa - Consulado"~"Portugal",
    nompuesto=="Logroño"~"España", nompuesto=="London"~"Canadá", nompuesto=="Londres - Consulado"~"Reino Unido",
    nompuesto=="Londres - Edimburgo"~"Reino Unido", str_detect(nompuesto,"^Los Angeles")~"Estados Unidos", nompuesto=="Lugano"~"Suiza",
    nompuesto=="Lugo"~"España", nompuesto=="Luxemburgo"~"Luxemburgo", nompuesto=="Madrid - Consulado"~"España",
    nompuesto=="Malta - La Valeta"~"Malta", nompuesto=="Managua - Consulado"~"Nicaragua", nompuesto=="Manaos - Consulado"~"Brasil",
    nompuesto=="Manila Consulado"~"Filipinas", nompuesto=="Manta"~"Ecuador", str_detect(nompuesto,"^Maracaibo")~"Venezuela",
    nompuesto=="Melbourne"~"Australia", nompuesto=="Mendoza"~"Argentina", str_detect(nompuesto,"^Miami")~"Estados Unidos",
    nompuesto=="Michigan"~"Estados Unidos", nompuesto=="Milán Consulado"~"Italia", nompuesto=="Minnesota"~"Estados Unidos",
    nompuesto=="Missouri"~"Estados Unidos", nompuesto=="Monterrey Consulado"~"México", nompuesto=="Montevideo - Consulado"~"Uruguay",
    nompuesto=="Montreal Consulado"~"Canadá", nompuesto=="Murcia"~"España", nompuesto=="Málaga"~"España", nompuesto=="Mérida"~"México",
    nompuesto=="Nagoya"~"Japón", nompuesto=="Nairobi - Consulado"~"Kenia", nompuesto=="Nantes"~"Francia",
    str_detect(nompuesto,"^Newark")~"Estados Unidos", nompuesto=="Nueva Delhi - Consulado"~"India", nompuesto=="Nueva Loja - Consulado"~"Ecuador",
    str_detect(nompuesto,"^Nueva York")~"Estados Unidos", nompuesto=="Nápoles"~"Italia", nompuesto=="Oporto"~"Portugal",
    nompuesto=="Oranjestad Consulado"~"Aruba", str_detect(nompuesto,"^Orlando")~"Estados Unidos", nompuesto=="Osaka"~"Japón",
    nompuesto=="Oslo - Consulado"~"Noruega", nompuesto=="Ottawa Consulado"~"Canadá", nompuesto=="Oviedo - Bilbao"~"España",
    nompuesto=="Palma de Mallorca - Consulado"~"España", nompuesto=="Palmas de Gran Canaria - Consulado"~"España",
    nompuesto=="Pamplona"~"España", nompuesto=="Panama - Consulado"~"Panamá", nompuesto=="Panama - David"~"Panamá",
    nompuesto=="Paris - Consulado"~"Francia", nompuesto=="Paris - Estrasburgo"~"Francia", nompuesto=="Paris - Lyon"~"Francia",
    nompuesto=="Paris - Montpellier"~"Francia", nompuesto=="Paris - Toulouse"~"Francia", nompuesto=="Perth"~"Australia",
    nompuesto=="Porto Alegre"~"Brasil", nompuesto=="Praga"~"Chequia", nompuesto=="Pretoria - Consulado"~"Sudáfrica",
    nompuesto=="Puerto Ayacucho"~"Venezuela", nompuesto=="Puerto España - Consulado"~"Trinidad y Tobago", nompuesto=="Puerto La Cruz"~"Venezuela",
    nompuesto=="Puerto Obaldía - Consulado"~"Panamá", nompuesto=="Puerto Ordaz"~"Venezuela", nompuesto=="Puerto Ordaz - Bolívar"~"Venezuela",
    nompuesto=="Puerto Principe - Consulado"~"Haití", nompuesto=="Quito - Consulado"~"Ecuador", nompuesto=="Rabat Consulado"~"Marruecos",
    nompuesto=="Ramallah - Palestina"~"Palestina", nompuesto=="Recife"~"Brasil", nompuesto=="Riad"~"Arabia Saudita",
    nompuesto=="Roma - Consulado"~"Italia", nompuesto=="Río de Janeiro - Consulado"~"Brasil", nompuesto=="Salt Lake City"~"Estados Unidos",
    str_detect(nompuesto,"^San Antonio del Táchira")~"Venezuela", str_detect(nompuesto,"^San Cristóbal")~"Venezuela",
    nompuesto=="San Fernando de Atabapo"~"Venezuela", str_detect(nompuesto,"^San Francisco")~"Estados Unidos",
    nompuesto=="San José - Consulado"~"Costa Rica", nompuesto=="San Juan de Puerto Rico - Consulado"~"Puerto Rico",
    nompuesto=="San Salvador - Consulado"~"El Salvador", nompuesto=="Santa Cruz de Tenerife"~"España",
    nompuesto=="Santa Cruz de la Sierra"~"Bolivia", nompuesto=="Santiago Consulado"~"Chile", nompuesto=="Santo Domingo - Consulado"~"República Dominicana",
    nompuesto=="Santo Domingo Tsachilas - Consulado"~"Ecuador", nompuesto=="Sao Paulo Consulado"~"Brasil", nompuesto=="Seul - Consulado"~"Corea del Sur",
    nompuesto=="Sevilla - Consulado"~"España", nompuesto=="Shanghai Consulado"~"China", nompuesto=="Singapur - Consulado"~"Singapur",
    nompuesto=="Stuttgart"~"Alemania", nompuesto=="Sydney Consulado"~"Australia", nompuesto=="Tabatinga Consulado"~"Brasil",
    nompuesto=="Tarragona"~"España", nompuesto=="Tegucigalpa - Consulado"~"Honduras", nompuesto=="Tel Aviv - Consulado"~"Israel",
    nompuesto=="Tokio - Consulado"~"Japón", nompuesto=="Toronto Consulado"~"Canadá", nompuesto=="Tulcán - Consulado"~"Ecuador",
    nompuesto=="Turín"~"Italia", nompuesto=="Valencia" & zona==5~"Venezuela", nompuesto=="Valencia - Consulado"~"España",
    nompuesto=="Valladolid"~"España", nompuesto=="Vancouver Consulado"~"Canadá", nompuesto=="Varsovia - Consulado"~"Polonia",
    nompuesto=="Viena Consulado"~"Austria", nompuesto=="Villahermosa"~"México", nompuesto=="Washington - Consulado"~"Estados Unidos",
    nompuesto=="Wellington"~"Nueva Zelanda", nompuesto=="Willemstad Consulado"~"Curazao", nompuesto=="Zaragoza"~"España",
    nompuesto=="Zurich"~"Suiza", TRUE ~ NA_character_))

paises_iso <- c("Ghana" = "GHA","Senegal" = "SEN", "Emiratos Árabes Unidos"="ARE","España"="ESP","Países Bajos"="NLD","Turquía"="TUR","Chile"="CHL",
                "Argelia"="DZA","Paraguay"="PRY","Grecia"="GRC","Estados Unidos"="USA","Nueva Zelanda"="NZL","Azerbaiyán"="AZE",
                "Tailandia"="THA","Venezuela"="VEN","China"="CHN","Líbano"="LBN","Belice"="BLZ","Brasil"="BRA","Alemania"="DEU",
                "Suiza"="CHE","Barbados"="BRB","Australia"="AUS","Bélgica"="BEL","Rumania"="ROU","Hungría"="HUN","Argentina"="ARG",
                "Canadá"="CAN","México"="MEX","Sudáfrica"="ZAF","Panamá"="PAN","Dinamarca"="DNK","Ecuador"="ECU","Catar"="QAT",
                "Irlanda"="IRL","Egipto"="EGY","Suecia"="SWE","Guyana"="GUY","Austria"="AUT","Guatemala"="GTM","Italia"="ITA",
                "Vietnam"="VNM","Finlandia"="FIN","Hong Kong"="HKG","Islas Caimán"="CYM","Indonesia"="IDN","Jamaica"="JAM",
                "Malasia"="MYS","Cuba"="CUB","Bolivia"="BOL","Perú"="PER","Chipre"="CYP","Portugal"="PRT","Reino Unido"="GBR",
                "Luxemburgo"="LUX","Malta"="MLT","Nicaragua"="NIC","Filipinas"="PHL","Uruguay"="URY","Japón"="JPN","Kenia"="KEN",
                "Francia"="FRA","India"="IND","Aruba"="ABW","Noruega"="NOR","Chequia"="CZE","Trinidad y Tobago"="TTO","Haití"="HTI",
                "Marruecos"="MAR","Palestina"="PSE","Arabia Saudita"="SAU","Costa Rica"="CRI","Puerto Rico"="PRI","El Salvador"="SLV",
                "República Dominicana"="DOM","Corea del Sur"="KOR","Singapur"="SGP","Honduras"="HND","Polonia"="POL","Israel"="ISR","Curazao"="CUW")
dic_pais_ext <- dic_pais_ext %>% mutate(iso3 = unname(paises_iso[pais]))

ext_pais_2026 <- divipol_cong_26 %>% filter(COD_DPTO == 88) %>%
  left_join(dic_pais_ext, by = c("nompuesto","zona")) %>%
  group_by(iso3, pais) %>%
  summarise(n_puestos = n_distinct(paste(code_RNEC, zona, puesto)),
            n_mesas = sum(n_mesas_puesto, na.rm = TRUE),
            censo = sum(censo, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(iso3))
n_paises_2026 <- n_distinct(ext_pais_2026$pais)

# Mapa mundial: une a paises del mundo por iso3 (los ausentes quedan NA -> gris)
mundo <- ne_countries(scale = 50, type = "map_units", returnclass = "sf")
col_iso <- intersect(c("adm0_a3","gu_a3","iso_a3","sov_a3"), names(mundo))[1]
ext_mundo_sf <- mundo %>% mutate(iso3 = .data[[col_iso]]) %>%
  select(iso3, name) %>% left_join(ext_pais_2026, by = "iso3")

# ---- (E) Geometrias DENTRO del rds (proyecto autosuficiente) --------3
# Solo se guarda la geometria + la llave; los datos se unen en el .qmd.
sf_mpios_geom  <- readRDS(file.path(ruta_general, "shapes", "muni_simpl_sanandres_cache.rds"))$sf_obj %>%
  mutate(codmpio = as.integer(codmpio)) %>% select(codmpio)
sf_deptos_geom <- readRDS(file.path(ruta_general, "shapes", "departamento_simpl_sanandres_cache.rds"))$sf_obj %>%
  mutate(coddepto = as.integer(cod_depto)) %>% select(coddepto)

# ---- GUARDAR -------------------------------------------------------3
datos_divipole <- list(
  resumen    = div_resumen,
  mpio_2026  = div_mpio_2026,
  depto_2026 = div_depto_2026,
  ext_mundo  = ext_mundo_sf,
  n_paises   = n_paises_2026,
  sf_mpios   = sf_mpios_geom,
  sf_deptos  = sf_deptos_geom
)
saveRDS(datos_divipole, "datos_divipole.rds")
message("Listo: datos_divipole.rds  |  paises exterior 2026 = ", n_paises_2026)


# =====================================================================3
#  SECCION 02 — INSCRIPCION DE CANDIDATURAS ----
# =====================================================================3
ruta_inscr <- "../inscripción de candidatos 2026/datos"   

leer_lineas_txt <- function(a) readLines(a, warn = FALSE, encoding = "latin1")
leer_unico <- function(carpeta, patron) {
  a <- list.files(carpeta, pattern = patron, full.names = TRUE, ignore.case = TRUE)
  stopifnot(length(a) == 1); a
}

# --- 2022: candidato-nivel (corp, circ, genero) ---
cand22 <- read_excel(file.path(ruta_inscr, "Congreso_2022.xls")) %>%
  mutate(
    DESC_CORP = str_to_upper(str_squish(DESC_CORP)),
    DESC_CIRC = str_to_upper(str_squish(DESC_CIRC)),
    DESC_CIRC = str_replace_all(DESC_CIRC, "Í", "I"),
    DESC_CIRC = str_replace_all(DESC_CIRC, "Á", "A"),
    DESC_CIRC = str_replace_all(DESC_CIRC, "É", "E"),
    DESC_CIRC = str_replace_all(DESC_CIRC, "Ó", "O"),
    DESC_CIRC = str_replace_all(DESC_CIRC, "Ú", "U"),
    corp = case_when(DESC_CORP == "SENADO" ~ "Senado", DESC_CORP == "CAMARA" ~ "Cámara", TRUE ~ NA_character_),
    circ = case_when(
      DESC_CIRC == "NACIONAL" ~ "Nacional", DESC_CIRC == "TERRITORIAL" ~ "Territorial",
      DESC_CIRC %in% c("INDIGENAS", "INDIGENA") ~ "Indígena",
      DESC_CIRC == "AFRODESCENDIENTES" ~ "Afrodescendientes",
      DESC_CIRC == "CITREP" ~ "CITREP", DESC_CIRC == "INTERNACIONAL" ~ "Territorial",
      TRUE ~ str_to_title(DESC_CIRC))
  ) %>%
  transmute(annoh = 2022, corp, circ, genero = GENERO) %>%
  filter(!is.na(corp), circ %in% c("Nacional", "Territorial", "Indígena", "Afrodescendientes", "CITREP"))

cand22_citrep <- read_excel(file.path(ruta_inscr, "Congreso_2022.xls"), sheet = "CITREP_2022") %>%
  transmute(annoh = 2022, corp = "Cámara", circ = "CITREP", genero = GENERO)
cand22 <- bind_rows(cand22 %>% filter(!(corp == "Cámara" & circ == "CITREP")), cand22_citrep)

# --- 2026: candidato-nivel desde el CANDIDATOS.TXT (total autoritativo) ---
circ_2026 <- {
  a <- leer_unico(ruta_basicos_cong, "^CIRCUNSCRIPCION.*\\.(TXT|txt)$")
  tibble(linea = leer_lineas_txt(a)) %>% filter(nchar(linea) >= 101) %>%
    transmute(codcirc = fw(linea, 1, 1), circ_txt = chr0(fw(linea, 2, 101)))
}
cand26 <- {
  a <- leer_unico(ruta_basicos_cong, "^CANDIDATOS.*\\.(TXT|txt)$")
  tibble(linea = leer_lineas_txt(a)) %>% filter(nchar(linea) >= 138) %>%
    transmute(codcorp = fw(linea, 1, 3), codcirc = fw(linea, 4, 4), genero = chr0(fw(linea, 136, 136))) %>%
    left_join(circ_2026, by = "codcirc") %>%
    mutate(
      corp = case_when(codcorp == "001" ~ "Senado", codcorp %in% c("002", "007") ~ "Cámara", TRUE ~ NA_character_),
      circ = case_when(
        codcirc == "0" ~ "Nacional", codcirc == "1" ~ "Territorial", codcirc == "4" ~ "Indígena",
        codcirc == "5" ~ "Afrodescendientes", codcirc == "9" ~ "CITREP",
        TRUE ~ str_to_title(str_to_upper(str_squish(circ_txt))))
    ) %>%
    transmute(annoh = 2026, corp, circ, genero) %>%
    filter(!is.na(corp), genero != "", circ %in% c("Nacional", "Territorial", "Indígena", "Afrodescendientes", "CITREP"))
}

cand_all <- bind_rows(cand22, cand26)

# --- Resumen por corp/circ + subtotales + total, ancho por año ---
res_cc <- cand_all %>% group_by(annoh, corp, circ) %>%
  summarise(candidatos = n(), mujeres = sum(genero == "F", na.rm = TRUE), .groups = "drop")
sub_cc <- res_cc %>% group_by(annoh, corp) %>%
  summarise(candidatos = sum(candidatos), mujeres = sum(mujeres), .groups = "drop") %>%
  mutate(circ = "Subtotal")
tot_cc <- res_cc %>% group_by(annoh) %>%
  summarise(candidatos = sum(candidatos), mujeres = sum(mujeres), .groups = "drop") %>%
  mutate(corp = "Total", circ = "Total general")

tabla_corpcirc <- bind_rows(res_cc, sub_cc, tot_cc) %>%
  mutate(pct_muj = if_else(candidatos > 0, mujeres / candidatos, NA_real_)) %>%
  pivot_wider(names_from = annoh, values_from = c(candidatos, mujeres, pct_muj)) %>%
  mutate(across(starts_with("candidatos"), ~coalesce(., 0L)),
         across(starts_with("mujeres"), ~coalesce(., 0L))) %>%
  arrange(factor(corp, levels = c("Senado", "Cámara", "Total")),
          case_when(circ == "Nacional" ~ 1, circ == "Territorial" ~ 2, circ == "Indígena" ~ 3,
                    circ == "Afrodescendientes" ~ 4, circ == "CITREP" ~ 5,
                    circ == "Subtotal" ~ 98, circ == "Total general" ~ 99, TRUE ~ 100))

## ---- Tipo de organizacion (coaliciones, GSC, partidos...) ----------
recode_tipo_part <- function(x) {
  x <- str_to_upper(str_squish(x))
  x <- str_replace_all(x, "Í","I"); x <- str_replace_all(x, "Á","A")
  x <- str_replace_all(x, "É","E"); x <- str_replace_all(x, "Ó","O"); x <- str_replace_all(x, "Ú","U")
  case_when(
    x == "PARTIDO O MOVIMIENTO POLITICO CON PERSONERIA JURIDICA" ~ "Partido con personería jurídica",
    x == "MOVIMIENTOS SOCIALES O GRUPOS SIGNIFICATIVOS DE CIUDADANOS" ~ "Movimientos sociales o GSC",
    x == "ORGANIZACIONES INDIGENAS" ~ "Organizaciones indígenas",
    x == "ORGANIZACIONES AFRODESCENDIENTES" ~ "Organizaciones afrodescendientes",
    x == "COALICIONES" ~ "Coaliciones",
    x == "ORGANIZACIONES SOCIALES" ~ "Organizaciones sociales",
    x == "ORGANIZACIONES SOCIALES O GSC CITREP" ~ "Org. sociales o GSC CITREP",
    TRUE ~ NA_character_)
}

# 2022 (nivel candidato, con tipo)
c22g <- read_excel(file.path(ruta_inscr, "Congreso_2022.xls")) %>%
  mutate(
    DESC_CORP = str_to_upper(str_squish(DESC_CORP)),
    DESC_CIRC = str_to_upper(str_squish(DESC_CIRC)),
    DESC_CIRC = str_replace_all(DESC_CIRC, "Í","I"), DESC_CIRC = str_replace_all(DESC_CIRC, "Á","A"),
    DESC_CIRC = str_replace_all(DESC_CIRC, "É","E"), DESC_CIRC = str_replace_all(DESC_CIRC, "Ó","O"),
    DESC_CIRC = str_replace_all(DESC_CIRC, "Ú","U"),
    corp = case_when(DESC_CORP=="SENADO"~"Senado", DESC_CORP=="CAMARA"~"Cámara", TRUE~NA_character_),
    circ = case_when(DESC_CIRC=="NACIONAL"~"Nacional", DESC_CIRC=="TERRITORIAL"~"Territorial",
                     DESC_CIRC %in% c("INDIGENA","INDIGENAS")~"Indígena", DESC_CIRC=="AFRODESCENDIENTES"~"Afrodescendientes",
                     DESC_CIRC=="CITREP"~"CITREP", DESC_CIRC=="INTERNACIONAL"~"Territorial", TRUE~str_to_title(DESC_CIRC))
  ) %>%
  transmute(annoh=2022, corp, circ, codparti=as.numeric(COD_PARTIDO),
            DESC_TPPART=str_to_upper(str_squish(DESC_TPPART)), genero=GENERO) %>%
  filter(!is.na(corp), genero != "")
c22g_citrep <- read_excel(file.path(ruta_inscr, "Congreso_2022.xls"), sheet="CITREP_2022") %>%
  transmute(annoh=2022, corp="Cámara", circ="CITREP", codparti=as.numeric(COD_PARTIDO),
            DESC_TPPART="ORGANIZACIONES SOCIALES O GSC CITREP", genero=GENERO) %>% filter(genero != "")
cand22_tipo <- bind_rows(c22g %>% filter(!(corp=="Cámara" & circ=="CITREP")), c22g_citrep) %>%
  mutate(tipo_part = recode_tipo_part(DESC_TPPART))

# Insumos 2026
partidos_2026 <- {
  a <- leer_unico(ruta_basicos_cong, "^PARTIDOS.*\\.(TXT|txt)$")
  tibble(linea = leer_lineas_txt(a)) %>% filter(nchar(linea) >= 205) %>%
    transmute(codparti = as.numeric(fw(linea,1,5)), nomparti = chr0(fw(linea,6,205)))
}
dptos <- read_excel(file.path(ruta_inscr, "Candidatos Congreso 15Feb2022.xlsx")) %>%
  rename(Departamento = DESC_DPTO) %>%
  filter(!is.na(COD_DPTO), !is.na(Departamento), Departamento != "#ERROR!") %>%
  distinct(COD_DPTO, Departamento)

# Base vieja 2026 (trae el tipo de organizacion por cedula / por grupo)
c26_viejo <- read_excel(file.path(ruta_inscr, "CANDIATURAS2026.xlsx")) %>%
  transmute(
    DESC_CORP = `Descripción Corporación`, Departamento = `Descripción Departamento`,
    DESC_CIRC = `Descripción Circunscripción`, DESC_TPPART = `Descripción Tipo Agrupación`,
    COD_PARTIDO = as.numeric(`Código Agrupación`), DESC_PARTIDO = `Nombre Agrupación`,
    CEDULA = as.character(`Número de Identificación`), GENERO = `Género`
  ) %>%
  mutate(
    DESC_CORP = str_to_upper(str_squish(DESC_CORP)),
    DESC_CIRC = str_to_upper(str_squish(DESC_CIRC)),
    DESC_CIRC = str_replace_all(DESC_CIRC, "Í","I"), DESC_CIRC = str_replace_all(DESC_CIRC, "Á","A"),
    DESC_CIRC = str_replace_all(DESC_CIRC, "É","E"), DESC_CIRC = str_replace_all(DESC_CIRC, "Ó","O"),
    DESC_CIRC = str_replace_all(DESC_CIRC, "Ú","U"),
    DESC_TPPART = str_to_upper(str_squish(DESC_TPPART)),
    DESC_TPPART = case_when(
      DESC_TPPART == "GRUPO SIGNIFICATIVOS DE CIUDADANOS" ~ "MOVIMIENTOS SOCIALES O GRUPOS SIGNIFICATIVOS DE CIUDADANOS",
      DESC_TPPART == "ORGANIZACIONES INDÍGENAS" ~ "ORGANIZACIONES INDIGENAS",
      DESC_TPPART == "PARTIDO O MOVIMIENTO POLÍTICO CON PERSONERÍA JURÍDICA" ~ "PARTIDO O MOVIMIENTO POLITICO CON PERSONERIA JURIDICA",
      TRUE ~ DESC_TPPART),
    corp = case_when(DESC_CORP=="SENADO"~"Senado", DESC_CORP=="CAMARA"~"Cámara", TRUE~NA_character_),
    circ = case_when(DESC_CIRC=="NACIONAL"~"Nacional", DESC_CIRC=="TERRITORIAL DEPARTAMENTAL"~"Territorial",
                     DESC_CIRC %in% c("INDIGENA","INDIGENAS")~"Indígena",
                     DESC_CIRC %in% c("AFRODESCENDIENTES","AFRO-DESCENDIENTES")~"Afrodescendientes",
                     DESC_CIRC=="INTERNACIONAL"~"Territorial", DESC_CIRC=="CITREP"~"CITREP", TRUE~str_to_title(DESC_CIRC)),
    territorio_join = case_when(circ=="Territorial"~Departamento, circ=="CITREP"~Departamento, TRUE~NA_character_),
    territorio_join = str_squish(territorio_join),
    territorio_join = if_else(circ=="CITREP", as.character(parse_number(territorio_join)), territorio_join),
    CEDULA = str_squish(str_remove_all(CEDULA, "\\.0$"))
  ) %>%
  left_join(dptos %>% select(Departamento, COD_DPTO), by="Departamento") %>%
  mutate(territorio_join = case_when(circ=="Territorial"~as.character(COD_DPTO),
                                     circ=="CITREP"~territorio_join, TRUE~territorio_join),
         DESC_TPPART = ifelse(circ=="CITREP", "ORGANIZACIONES SOCIALES O GSC CITREP", DESC_TPPART)) %>%
  transmute(corp, circ, codparti=COD_PARTIDO, cedula=CEDULA, territorio_join, DESC_TPPART) %>%
  filter(!is.na(corp), !is.na(circ))

# Base autoritativa 2026 (TXT) a nivel candidato
c26_base <- {
  a <- leer_unico(ruta_basicos_cong, "^CANDIDATOS.*\\.(TXT|txt)$")
  tibble(linea = leer_lineas_txt(a)) %>% filter(nchar(linea) >= 138) %>%
    transmute(
      codcorp=fw(linea,1,3), codcirc=fw(linea,4,4), territorio_raw=as.numeric(fw(linea,5,6)),
      codparti=as.numeric(fw(linea,12,16)), cedula=chr0(fw(linea,121,135)), genero=chr0(fw(linea,136,136))
    ) %>% left_join(circ_2026, by="codcirc") %>%
    mutate(
      corp = case_when(codcorp=="001"~"Senado", codcorp %in% c("002","007")~"Cámara", TRUE~NA_character_),
      circ = case_when(codcorp=="007"~"CITREP", codcirc=="0"~"Nacional", codcirc=="1"~"Territorial",
                       codcirc=="4"~"Indígena", codcirc=="5"~"Afrodescendientes", codcirc=="9"~"CITREP",
                       TRUE~str_to_title(str_to_upper(str_squish(circ_txt)))),
      territorio_join = if_else(circ %in% c("Territorial","CITREP"), as.character(territorio_raw), NA_character_),
      cedula = str_squish(str_remove_all(cedula, "\\.0$"))
    ) %>%
    transmute(annoh=2026, corp, circ, codparti, cedula, genero, territorio_join) %>%
    filter(!is.na(corp), genero != "", circ %in% c("Nacional","Territorial","Indígena","Afrodescendientes","CITREP"))
}

lookup_ced <- c26_viejo %>% filter(!is.na(cedula), cedula!="", !is.na(DESC_TPPART)) %>%
  distinct(cedula, .keep_all=TRUE) %>% select(cedula, tp_ced=DESC_TPPART)
lookup_grp <- c26_viejo %>% filter(!is.na(DESC_TPPART)) %>%
  group_by(corp, circ, codparti, territorio_join) %>%
  summarise(tp_grp = if_else(n_distinct(DESC_TPPART)==1, first(DESC_TPPART), NA_character_), .groups="drop")

cand26_tipo <- c26_base %>%
  left_join(lookup_ced, by="cedula") %>%
  left_join(lookup_grp, by=c("corp","circ","codparti","territorio_join")) %>%
  mutate(
    DESC_TPPART = coalesce(tp_ced, tp_grp),
    DESC_TPPART = case_when(circ=="CITREP" & is.na(DESC_TPPART) ~ "ORGANIZACIONES SOCIALES O GSC CITREP", TRUE~DESC_TPPART),
    DESC_TPPART = case_when(
      is.na(DESC_TPPART) & circ=="Territorial" & territorio_join=="56" & codparti %in% c(3011,3117) ~ "COALICIONES",
      is.na(DESC_TPPART) & circ=="Territorial" & territorio_join=="56" & codparti %in% c(1,3,8,11,20,26) ~ "PARTIDO O MOVIMIENTO POLITICO CON PERSONERIA JURIDICA",
      TRUE ~ DESC_TPPART),
    tipo_part = recode_tipo_part(DESC_TPPART), annoh = 2026)

n_na_tipo <- sum(is.na(cand26_tipo$tipo_part))
if (n_na_tipo > 0) warning("Candidaturas 2026 sin tipo de organizacion: ", n_na_tipo)

# Resumen por tipo, ambos anios
res_tipo <- bind_rows(cand22_tipo %>% select(annoh, tipo_part),
                      cand26_tipo %>% filter(!is.na(tipo_part)) %>% select(annoh, tipo_part)) %>%
  count(annoh, tipo_part, name="candidaturas") %>%
  group_by(annoh) %>% mutate(pct = candidaturas / sum(candidaturas)) %>% ungroup()
tipo_wide <- res_tipo %>%
  pivot_wider(names_from=annoh, values_from=c(candidaturas, pct)) %>%
  rename(cand_2022=candidaturas_2022, cand_2026=candidaturas_2026, pct_2022=pct_2022, pct_2026=pct_2026) %>%
  mutate(across(c(cand_2022, cand_2026), ~coalesce(., 0L)),
         across(c(pct_2022, pct_2026), ~coalesce(., 0)))

## ---- Cambio de candidaturas Camara Territorial por depto ------------
cam26 <- c26_base %>% filter(corp == "Cámara", circ == "Territorial") %>%
  mutate(coddepto = as.integer(territorio_join)) %>% count(coddepto, name = "n_2026")

# 2022: leer el departamento del Excel 
# 2022: el Excel trae DESC_DPTO (nombre), no el codigo -> unir con 'dptos'
cam22 <- read_excel(file.path(ruta_inscr, "Congreso_2022.xls")) %>%
  mutate(DESC_CORP = str_to_upper(str_squish(DESC_CORP)),
         DESC_CIRC = str_to_upper(str_squish(DESC_CIRC)),
         Departamento = str_squish(DESC_DPTO)) %>%
  filter(DESC_CORP == "CAMARA", DESC_CIRC == "TERRITORIAL") %>%
  left_join(dptos %>% mutate(Departamento = str_squish(Departamento)), by = "Departamento") %>%
  mutate(coddepto = as.integer(COD_DPTO)) %>%
  filter(!is.na(coddepto)) %>%
  count(coddepto, name = "n_2022")

camara_depto <- full_join(cam22, cam26, by = "coddepto") %>%   # aqui 'coddepto' aun es RNEC
  rename(cod_rnec_dep = coddepto) %>%
  mutate(n_2022 = coalesce(n_2022, 0L), n_2026 = coalesce(n_2026, 0L),
         cambio = n_2026 - n_2022) %>%
  left_join(map_rnec_dane_dep, by = "cod_rnec_dep") %>%   # -> coddepto DANE
  left_join(map_depto_nom, by = "coddepto") %>%
  filter(!is.na(coddepto))

library(lubridate)

# --- 2026: Fecha Género E6, con tope al 15-dic-2025 ---
fechas_26 <- read_excel(file.path(ruta_inscr, "CANDIATURAS2026.xlsx")) %>%
  transmute(fecha_dt = ymd_hms(`Fecha Género E6`, tz = "America/Bogota")) %>%
  mutate(fecha_dt = pmin(fecha_dt, ymd_hms("2025-12-15 23:59:59", tz = "America/Bogota")),
         fecha = as_date(fecha_dt)) %>%
  filter(!is.na(fecha))
inicio_26 <- ymd("2025-11-08")

# --- 2022: principal (archivo viejo) + CITREP (hoja de Congreso_2022) ---
fechas_22_main <- read_excel(file.path(ruta_inscr, "Candidatos Congreso 15Feb2022.xlsx")) %>%
  transmute(fecha_dt = ymd_hms(FECHA_CREA_E6, tz = "America/Bogota"))
fechas_22_citrep <- read_excel(file.path(ruta_inscr, "Congreso_2022.xls"), sheet = "CITREP_2022") %>%
  transmute(fecha_dt = parse_date_time(`FECHA GENERADO E-6`, orders = "%d-%m-%Y-%H:%M", tz = "America/Bogota"))
fechas_22 <- bind_rows(fechas_22_main, fechas_22_citrep) %>%
  mutate(fecha_dt = pmin(fecha_dt, ymd_hms("2021-12-15 23:59:59", tz = "America/Bogota")),
         fecha = as_date(fecha_dt)) %>%
  filter(!is.na(fecha))
inicio_22 <- ymd("2021-11-13")

conteo_fechas <- bind_rows(
  fechas_26 %>% count(fecha, name = "n") %>% mutate(annoh = 2026, dia = as.integer(fecha - inicio_26 + 1)),
  fechas_22 %>% count(fecha, name = "n") %>% mutate(annoh = 2022, dia = as.integer(fecha - inicio_22 + 1))
) %>%
  group_by(annoh) %>% mutate(pct = n / sum(n)) %>% ungroup() %>%
  mutate(fecha_txt = format(fecha, "%d %b")) %>%
  filter(dia >= 1) %>%
  select(annoh, dia, fecha_txt, n, pct)

## ---- Cuatro temas para el selector ---------------------------------3
# Nombres de agrupacion: 2026 desde PARTIDOS.TXT, 2022 desde el Excel.

map_citrep_lbl <- tibble::tribble(
  ~n_CITREP, ~lbl,
  1L,"1. Cauca-Nariño-Valle", 2L,"2. Arauca", 3L,"3. Bajo Cauca", 4L,"4. Catatumbo",
  5L,"5. Caquetá-Huila", 6L,"6. Chocó", 7L,"7. Sur de Meta-Guaviare", 8L,"8. Montes de María",
  9L,"9. Pacífico Cauca-Valle", 10L,"10. Pacífico Nariño", 11L,"11. Putumayo",
  12L,"12. Cesar-Guajira-Magdalena", 13L,"13. Sur de Bolívar", 14L,"14. Sur de Córdoba",
  15L,"15. Sur de Tolima", 16L,"16. Urabá")

# 2026: del TXT (codcorp 007 = CITREP); n_CITREP en el territorio (pos 5-6)
citrep_26 <- c26_viejo %>% filter(circ == "CITREP") %>%
  mutate(n_CITREP = as.integer(territorio_join)) %>%
  filter(!is.na(n_CITREP)) %>% count(n_CITREP, name = "n_2026")
# 2022: hoja CITREP_2022
citrep_22 <- read_excel(file.path(ruta_inscr, "Congreso_2022.xls"), sheet = "CITREP_2022") %>%
  transmute(n_CITREP = as.integer(`N# CITREP`)) %>% filter(!is.na(n_CITREP)) %>%
  count(n_CITREP, name = "n_2022")

citrep_cambio <- map_citrep_lbl %>%
  left_join(citrep_22, by = "n_CITREP") %>% left_join(citrep_26, by = "n_CITREP") %>%
  mutate(n_2022 = coalesce(n_2022, 0L), n_2026 = coalesce(n_2026, 0L),
         cambio = n_2026 - n_2022)

# ===== Cuatro temas del selector =====3
nom26 <- partidos_2026 %>% transmute(codparti, nom = str_squish(nomparti))
nom22 <- read_excel(file.path(ruta_inscr, "Congreso_2022.xls")) %>%
  transmute(codparti = as.numeric(COD_PARTIDO), nom = str_squish(DESC_PARTIDO)) %>%
  distinct(codparti, .keep_all = TRUE)

# --- GSC y Coaliciones: hombres/mujeres por corporacion-circunscripcion ---
hm_base <- bind_rows(cand26_tipo %>% mutate(annoh = 2026L),
                     cand22_tipo %>% mutate(annoh = 2022L)) %>%
  filter(tipo_part %in% c("Movimientos sociales o GSC", "Coaliciones"), genero %in% c("F","M")) %>%
  transmute(tema = if_else(tipo_part == "Coaliciones", "Coaliciones", "GSC"),
            annoh, corpcirc = paste(corp, circ, sep = " · "),
            genero = if_else(genero == "F", "Mujeres", "Hombres")) %>%
  count(tema, annoh, corpcirc, genero, name = "n")

# --- Sin coalicion: por partido, con ceros (listas maestras + mapeo de nombres) ---
master_2022 <- tibble(partido = c("Centro Democrático","Cambio Radical","Partido Liberal","Partido Conservador",
                                  "Partido de la U","Alianza Verde","Polo Democrático Alternativo","MIRA","Colombia Justa Libres","Comunes","MAIS",
                                  "AICO","ASI","ADA","Colombia Renaciente","Unión Patriótica – UP","Nuevo Liberalismo","Verde Oxígeno",
                                  "Salvación Nacional","Colombia Humana","Partido Comunista Colombiano","Dignidad y Compromiso"))
master_2026 <- tibble(partido = c("Centro Democrático","Cambio Radical","Partido Liberal","Partido Conservador",
                                  "Partido de la U","Alianza Verde","Pacto Histórico","MIRA","Colombia Justa Libres","Comunes","MAIS","AICO","ASI",
                                  "ADA","Colombia Renaciente","Nuevo Liberalismo","Verde Oxígeno","Salvación Nacional","Colombia Humana",
                                  "Dignidad y Compromiso","Partido del Trabajo","En Marcha","Liga Anticorrupción","Partido Demócrata",
                                  "Partido Ecologista","Fuerza de la Paz","Esperanza Democrática"))

map22 <- function(x) case_when(
  str_detect(x,"LIBERAL COLOMBIANO")~"Partido Liberal", str_detect(x,"CONSERVADOR COLOMBIANO")~"Partido Conservador",
  str_detect(x,"CAMBIO RADICAL")~"Cambio Radical", str_detect(x,"CENTRO DEMOCRÁTICO")~"Centro Democrático",
  str_detect(x,"UNIÓN POR LA GENTE")~"Partido de la U", str_detect(x,"ALIANZA VERDE")~"Alianza Verde",
  str_detect(x,"POLO DEMOCRÁTICO")~"Polo Democrático Alternativo", str_detect(x,"MIRA")~"MIRA",
  str_detect(x,"COLOMBIA JUSTA LIBRES")~"Colombia Justa Libres", str_detect(x,"PARTIDO COMUNES")~"Comunes",
  str_detect(x,"ALTERNATIVO INDÍGENA SOCIAL")~"MAIS", str_detect(x,"AUTORIDADES INDÍGENAS DE COLOMBIA")~"AICO",
  str_detect(x,"ALIANZA SOCIAL INDEPENDIENTE")~"ASI", str_detect(x,"\\bADA\\b")~"ADA",
  str_detect(x,"COLOMBIA RENACIENTE")~"Colombia Renaciente", str_detect(x,"UNIÓN PATRIÓTICA")~"Unión Patriótica – UP",
  str_detect(x,"NUEVO LIBERALISMO")~"Nuevo Liberalismo", str_detect(x,"OXÍGENO")~"Verde Oxígeno",
  str_detect(x,"SALVACIÓN NACIONAL")~"Salvación Nacional", str_detect(x,"COLOMBIA HUMANA")~"Colombia Humana",
  str_detect(x,"PARTIDO COMUNISTA")~"Partido Comunista Colombiano", str_detect(x,"DIGNIDAD")~"Dignidad y Compromiso",
  TRUE~NA_character_)
map26 <- function(x) case_when(
  str_detect(x,"LIBERAL COLOMBIANO")~"Partido Liberal", str_detect(x,"CONSERVADOR COLOMBIANO")~"Partido Conservador",
  str_detect(x,"CAMBIO RADICAL")~"Cambio Radical", str_detect(x,"CENTRO DEMOCRÁTICO")~"Centro Democrático",
  str_detect(x,"UNIÓN POR LA GENTE")~"Partido de la U", str_detect(x,"ALIANZA VERDE")~"Alianza Verde",
  str_detect(x,"PACTO HISTÓRICO")~"Pacto Histórico", str_detect(x,"MIRA")~"MIRA",
  str_detect(x,"COLOMBIA JUSTA LIBRES")~"Colombia Justa Libres", str_detect(x,"PARTIDO COMUNES")~"Comunes",
  str_detect(x,"ALTERNATIVO INDÍGENA Y SOCIAL")~"MAIS", str_detect(x,"AUTORIDADES INDÍGENAS DE COLOMBIA")~"AICO",
  str_detect(x,"ALIANZA SOCIAL INDEPENDIENTE")~"ASI", str_detect(x,"ALIANZA DEMOCRÁTICA AMPLIA")~"ADA",
  str_detect(x,"COLOMBIA RENACIENTE")~"Colombia Renaciente", str_detect(x,"NUEVO LIBERALISMO")~"Nuevo Liberalismo",
  str_detect(x,"OXÍGENO")~"Verde Oxígeno", str_detect(x,"SALVACIÓN NACIONAL")~"Salvación Nacional",
  str_detect(x,"COLOMBIA HUMANA")~"Colombia Humana", str_detect(x,"DIGNIDAD")~"Dignidad y Compromiso",
  str_detect(x,"DEL TRABAJO DE COLOMBIA")~"Partido del Trabajo", str_detect(x,"EN MARCHA")~"En Marcha",
  str_detect(x,"LIGA GOBERNANTES")~"Liga Anticorrupción", str_detect(x,"DEMÓCRATA COLOMBIANO")~"Partido Demócrata",
  str_detect(x,"ECOLOGISTA COLOMBIANO")~"Partido Ecologista", str_detect(x,"LA FUERZA DE LA PAZ")~"Fuerza de la Paz",
  str_detect(x,"ESPERANZA DEMOCRÁTICA")~"Esperanza Democrática", TRUE~NA_character_)

fix_pers <- function(df) mutate(df, tipo_part = if_else(codparti %in% c(156,186) &
                                                          tipo_part == "Organizaciones indígenas", "Partido con personería jurídica", tipo_part))

sc26 <- cand26_tipo %>% fix_pers() %>% left_join(nom26, by="codparti") %>%
  filter(tipo_part == "Partido con personería jurídica") %>%
  mutate(partido = map26(str_to_upper(nom))) %>% filter(!is.na(partido)) %>%
  count(partido, name="n") %>% right_join(master_2026, by="partido") %>%
  mutate(n = coalesce(n, 0L), annoh = 2026L)
sc22 <- cand22_tipo %>% fix_pers() %>% left_join(nom22, by="codparti") %>%
  filter(tipo_part == "Partido con personería jurídica") %>%
  mutate(partido = map22(str_to_upper(nom))) %>% filter(!is.na(partido)) %>%
  count(partido, name="n") %>% right_join(master_2022, by="partido") %>%
  mutate(n = coalesce(n, 0L), annoh = 2022L)
sincoal <- bind_rows(sc26, sc22)
n_part  <- tibble(annoh = c(2022L, 2026L), n_partidos = c(22L, 27L))


## ===== Candidaturas de Presidencia (1ra y 2da vuelta) =====
ruta_basicos_pres2 <- ruta_basicos_pres  # 2da vuelta (ya definida arriba)
ruta_basicos_pres1 <- "../Presidencia 2026/preconteo/datos/archivos básicos"  

leer_candidatos_pres <- function(carpeta, codificacion = "UTF-8") {
  fc <- list.files(carpeta, "^CANDIDATOS\\.TXT$", full.names = TRUE, ignore.case = TRUE)[1]
  fp <- list.files(carpeta, "^PARTIDOS\\.TXT$",   full.names = TRUE, ignore.case = TRUE)[1]
  
  if (is.na(fc)) stop("No encontre CANDIDATOS.TXT en: ", carpeta)
  
  anchos <- c(3,1,2,3,2,5,3,1,50,50,15,1,2)
  ini <- cumsum(c(1, head(anchos, -1)))
  fin <- cumsum(anchos)
  
  # Leer líneas sin advertencias
  lc <- readLines(fc, warn = FALSE)
  
  # Convertir la codificación de forma condicional según la vuelta
  if (codificacion == "latin1") {
    lc <- iconv(lc, from = "latin1", to = "UTF-8")
  } else {
    lc <- iconv(lc, from = "UTF-8", to = "UTF-8", sub = "")
  }
  
  lc <- lc[!is.na(lc) & nchar(lc) > 0]
  
  cand <- tibble(
    codparti = as.integer(str_trim(str_sub(lc, ini[6],  fin[6]))),
    codcandi = as.integer(str_trim(str_sub(lc, ini[7],  fin[7]))),
    nombre   = str_trim(str_sub(lc, ini[9],  fin[9])),
    apellido = str_trim(str_sub(lc, ini[10], fin[10])),
    genero   = str_trim(str_sub(lc, ini[12], fin[12]))
  )
  
  # Repetir el proceso para el archivo de PARTIDOS
  lp <- readLines(fp, warn = FALSE)
  
  if (codificacion == "latin1") {
    lp <- iconv(lp, from = "latin1", to = "UTF-8")
  } else {
    lp <- iconv(lp, from = "UTF-8", to = "UTF-8", sub = "")
  }
  
  lp <- lp[!is.na(lp) & nchar(lp) > 0]
  
  part <- tibble(
    codparti = as.integer(str_trim(str_sub(lp, 1, 5))),
    partido  = str_squish(str_sub(lp, 6, 210))
  )
  
  cand %>% 
    left_join(part, by = "codparti") %>%
    distinct(codcandi, .keep_all = TRUE) %>%
    transmute(
      nomcandi = str_squish(paste(nombre, apellido)), 
      partido, 
      genero
    )
}

# Consolidar ambas vueltas pasándole a cada una SU codificación correcta
cand_presidencia <- bind_rows(
  leer_candidatos_pres(ruta_basicos_pres1, codificacion = "UTF-8") %>% 
    mutate(vuelta = "Presidencia 1ª vuelta"),
  
  leer_candidatos_pres(ruta_basicos_pres2, codificacion = "latin1") %>% 
    mutate(vuelta = "Presidencia 2ª vuelta")
) %>%
  # Corrección manual de seguridad para Iván Cepeda en segunda vuelta
  mutate(
    genero = if_else(str_detect(nomcandi, "(?i)CEPEDA CASTRO"), "M", genero)
  )

# ---- GUARDAR (ahora con las dos tablas) ----------------------------3
datos_candidaturas <- list(tabla_corpcirc = tabla_corpcirc, tipo_wide = tipo_wide,
                           camara_depto = camara_depto, conteo_fechas = conteo_fechas, citrep_cambio = citrep_cambio,
                           hm_base = hm_base, sincoal = sincoal, n_part = n_part, cand_presidencia = cand_presidencia)
saveRDS(datos_candidaturas, "datos_candidaturas.rds")
message("Listo: datos_candidaturas.rds")


# =====================================================================3
#  SECCION 03-1 PRECONTEO CONGRESO----
# =====================================================================3

tz_use <- "America/Bogota"

# rutas
ruta_basicos_pre <- "../Congreso 2026/datos/PRECONTEO/DIA_ELECTORAL_08_MARZO/ArchivosBasicos_Dia electoral_congreso2026"
ruta_2026_bol    <- "../Congreso 2026/extaer info 2026/CONGRESO_2026_boletines_20260311_1830.xlsx"
ruta_2026_dep    <- "../Congreso 2026/extaer info 2026/CONGRESO_2026_deptos_20260311_1922.xlsx"
ruta_2022_bol    <- "../Congreso 2026/extraer info 2022/CONGRESO_2022_boletines_20260311_1524.xlsx"
ruta_censo_cong22 <- "../Participación electoral/datos/Divipole_Elecciones_Congreso_13-03-2022.xlsx"
ruta_mmv_mesas   <- "../Congreso 2026/datos/base_mmv_mesas_2026.rds"

base_mmv_mesas <- readRDS(ruta_mmv_mesas)

# ---- Helpers (del Rmd de preconteo) ----3
num0 <- function(x) suppressWarnings(as.numeric(str_trim(x)))
chr0 <- function(x) str_squish(str_trim(x))
code_RNEC_fun <- function(d, m) as.numeric(str_c(str_pad(d, 2, pad="0"), str_pad(m, 3, pad="0")))
a_num <- function(x) { if (is.numeric(x)) return(x)
  parse_number(as.character(x), locale = locale(decimal_mark=".", grouping_mark=",")) }
leer_lineas <- function(a, encoding="latin1") readLines(a, warn=FALSE, encoding=encoding)
leer_archivo_unico <- function(carpeta, patron) {
  a <- list.files(carpeta, pattern=patron, full.names=TRUE, ignore.case=TRUE); stopifnot(length(a)==1); a }

arreglar_hora <- function(x, fecha_eleccion=NA_character_, hora_inicio=16, tz="America/Bogota") {
  if (is.na(fecha_eleccion)) return(as.POSIXct(rep(NA, length(x)), origin="1970-01-01", tz=tz))
  out <- as.POSIXct(rep(NA, length(x)), origin="1970-01-01", tz=tz); hora_txt <- rep(NA_character_, length(x))
  if (inherits(x, "POSIXt")) { hora_txt <- ifelse(is.na(x), NA_character_, format(x, "%H:%M:%S"))
  } else if (is.numeric(x)) { x_tmp <- as.POSIXct(x, origin="1899-12-30", tz="UTC")
  hora_txt <- ifelse(is.na(x_tmp), NA_character_, format(x_tmp, "%H:%M:%S", tz="UTC"))
  } else { x_chr <- trimws(as.character(x)); x_chr[x_chr %in% c("","NA","NaT")] <- NA_character_
  tiene_hms <- !is.na(x_chr) & str_detect(x_chr, "^\\d{1,2}:\\d{2}:\\d{2}$")
  tiene_hm  <- !is.na(x_chr) & str_detect(x_chr, "^\\d{1,2}:\\d{2}$")
  hora_txt[tiene_hms] <- x_chr[tiene_hms]; hora_txt[tiene_hm] <- paste0(x_chr[tiene_hm], ":00")
  resto <- !is.na(x_chr) & !tiene_hms & !tiene_hm
  if (any(resto)) { x_parse <- suppressWarnings(parse_date_time(x_chr[resto],
                                                                orders=c("Ymd HMS","Ymd HM","dmY HMS","dmY HM","mdY HMS","mdY HM"), tz="UTC"))
  hora_txt[resto] <- ifelse(is.na(x_parse), NA_character_, format(x_parse, "%H:%M:%S", tz="UTC")) } }
  ok <- !is.na(hora_txt)
  out[ok] <- as.POSIXct(paste(fecha_eleccion, hora_txt[ok]), format="%Y-%m-%d %H:%M:%S", tz=tz)
  out[ok & hour(out) < hora_inicio] <- out[ok & hour(out) < hora_inicio] + days(1)
  out }

# ---- Totales de mesas por elección ----3
divipol_2026 <- {
  a <- leer_archivo_unico(ruta_basicos_pre, "^DIVIPOL.*\\.(TXT|txt)$")
  tibble(linea = leer_lineas(a)) %>% filter(nchar(linea) >= 146) %>%
    transmute(COD_DPTO=num0(fw(linea,1,2)), codmun=num0(fw(linea,3,5)), zona=num0(fw(linea,6,7)),
              code_RNEC=code_RNEC_fun(COD_DPTO,codmun), mesas=num0(fw(linea,109,114)))
}
mesas_totales_2026 <- tibble(
  eleccion = c("Senado 2026","Cámara 2026","Consultas 2026","CITREP 2026"),
  mesas_totales = c(sum(divipol_2026$mesas, na.rm=TRUE), sum(divipol_2026$mesas, na.rm=TRUE),
                    sum(divipol_2026$mesas, na.rm=TRUE),
                    divipol_2026 %>% left_join(todos_municipios %>% select(code_RNEC, CTEP) %>% distinct(), by="code_RNEC") %>%
                      filter(!is.na(CTEP), CTEP != 0, zona == 99) %>% summarise(t=sum(mesas, na.rm=TRUE)) %>% pull(t)))

censo_congreso_2022 <- read_excel(ruta_censo_cong22, sheet="divipol_20220207_161440", range="A5:L12517") %>%
  transmute(coddepto=as.numeric(dd), codmun=as.numeric(mm), zona=as.numeric(zz), puesto=pp,
            mesas=as.numeric(mesas), code_RNEC=1000*coddepto+codmun)
censo_citrep_2022 <- censo_congreso_2022 %>%
  left_join(todos_municipios %>% select(code_RNEC, CTEP) %>% distinct(), by="code_RNEC") %>%
  filter(!is.na(CTEP), CTEP != 0, zona == 99)
mesas_totales_2022 <- tibble(
  eleccion = c("Senado 2022","Cámara 2022","CITREP 2022","Consulta Centro Esperanza 2022",
               "Consulta Pacto Histórico 2022","Consulta Equipo Colombia 2022"),
  mesas_totales = c(rep(sum(censo_congreso_2022$mesas, na.rm=TRUE), 2), sum(censo_citrep_2022$mesas, na.rm=TRUE),
                    rep(sum(censo_congreso_2022$mesas, na.rm=TRUE), 3)))
mesas_totales_ref <- bind_rows(mesas_totales_2026, mesas_totales_2022)

# ---- Referencia de departamentos 2026 ----3
depto_ref_2026 <- tribble(~COD_DPTO, ~departamento,
                          60,"AMAZONAS",1,"ANTIOQUIA",40,"ARAUCA",3,"ATLANTICO",16,"BOGOTA D.C.",5,"BOLIVAR",7,"BOYACA",9,"CALDAS",
                          44,"CAQUETA",46,"CASANARE",11,"CAUCA",12,"CESAR",17,"CHOCO",88,"CONSULADOS",13,"CORDOBA",15,"CUNDINAMARCA",
                          50,"GUAINIA",54,"GUAVIARE",19,"HUILA",48,"LA GUAJIRA",21,"MAGDALENA",52,"META",23,"NARIÑO",25,"NORTE DE SAN",
                          64,"PUTUMAYO",26,"QUINDIO",24,"RISARALDA",56,"SAN ANDRES",27,"SANTANDER",28,"SUCRE",29,"TOLIMA",31,"VALLE",
                          68,"VAUPES",72,"VICHADA")

# ---- Normalización ----3
normalizar_boletines <- function(df, fecha_eleccion=NA_character_, nombre_eleccion=NA_character_, anio=NA_real_, mesas_totales_ref) {
  out <- df %>% rename(
    mesas_informadas = any_of(c("MESAS INFORMADAS","mesas_informadas")),
    votos = any_of(c("VOTANTES","votos")),
    boletin = any_of(c("BOLETÍN","BOLETIN","boletin")),
    hora = any_of(c("HORA","hora"))) %>%
    transmute(eleccion=nombre_eleccion, boletin=a_num(boletin), mesas_informadas=a_num(mesas_informadas),
              votos=a_num(votos), hora=arreglar_hora(hora, fecha_eleccion=fecha_eleccion, tz=tz_use)) %>%
    filter(!is.na(boletin), boletin <= 70) %>% arrange(boletin)
  max_votos <- max(out$votos, na.rm=TRUE)
  total_mesas <- mesas_totales_ref %>% filter(eleccion==nombre_eleccion) %>% pull(mesas_totales) %>% first()
  out %>% mutate(mesas_totales=total_mesas, porc_mesas_total=mesas_informadas/mesas_totales,
                 porc_votos=if (is.finite(max_votos) && max_votos>0) votos/max_votos else NA_real_)
}
normalizar_deptos <- function(df, fecha_eleccion="2026-03-08", nombre_eleccion=NA_character_, anio=2026, depto_ref=depto_ref_2026) {
  df %>% rename(departamento = any_of(c("DEPARTAMENTO","Departamento","departamento"))) %>%
    mutate(departamento=str_squish(as.character(departamento))) %>% left_join(depto_ref, by="departamento") %>%
    pivot_longer(cols=-c(departamento, COD_DPTO), names_to=c(".value","boletin"),
                 names_pattern="(BOLETÍN|BOLETIN|HORA)\\s*(\\d+)") %>%
    transmute(eleccion=nombre_eleccion, COD_DPTO, departamento, boletin=a_num(boletin),
              hora=arreglar_hora(HORA, fecha_eleccion=fecha_eleccion, tz=tz_use)) %>%
    filter(!is.na(boletin), boletin <= 70, !is.na(hora)) %>% arrange(eleccion, COD_DPTO, boletin)
}

mapa_nacional <- tribble(~ruta, ~sheet, ~eleccion, ~fecha_eleccion,
                         ruta_2026_bol,"Senado","Senado 2026","2026-03-08", ruta_2026_bol,"Camara","Cámara 2026","2026-03-08",
                         ruta_2026_bol,"CITREP","CITREP 2026","2026-03-08", ruta_2026_bol,"Consulta","Consultas 2026","2026-03-08",
                         ruta_2022_bol,"Senado","Senado 2022","2022-03-13", ruta_2022_bol,"Camara","Cámara 2022","2022-03-13",
                         ruta_2022_bol,"CITREP","CITREP 2022","2022-03-13",
                         ruta_2022_bol,"Consulta_CentroEsperanza","Consulta Centro Esperanza 2022","2022-03-13",
                         ruta_2022_bol,"Consulta_PactoHistorico","Consulta Pacto Histórico 2022","2022-03-13",
                         ruta_2022_bol,"Consulta_EquipoColombia","Consulta Equipo Colombia 2022","2022-03-13")
mapa_depto_2026 <- tribble(~sheet, ~eleccion,
                           "Senado","Senado 2026","Camara","Cámara 2026","CITREP","CITREP 2026","Consulta","Consultas 2026")

boletines_nacionales <- pmap_dfr(mapa_nacional, function(ruta, sheet, eleccion, fecha_eleccion) {
  read_excel(ruta, sheet=sheet) %>%
    normalizar_boletines(fecha_eleccion=fecha_eleccion, nombre_eleccion=eleccion, mesas_totales_ref=mesas_totales_ref) })
boletines_departamentales <- pmap_dfr(mapa_depto_2026, function(sheet, eleccion) {
  read_excel(ruta_2026_dep, sheet=sheet) %>%
    normalizar_deptos(fecha_eleccion="2026-03-08", nombre_eleccion=eleccion, depto_ref=depto_ref_2026) })
boletines <- list(nacionales = boletines_nacionales, departamentales = boletines_departamentales)

# ---- Agregados ligeros para la presentación ----3
hh <- function(x) str_replace(format(x, "%I:%M %p"), "^0", "")
inicio26 <- ymd_hms("2026-03-08 16:00:00", tz="America/Bogota")

vel_2026 <- boletines$nacionales %>%
  filter(eleccion %in% c("Senado 2026","Cámara 2026","CITREP 2026","Consultas 2026"), !is.na(hora), !is.na(porc_votos)) %>%
  transmute(eleccion=str_remove(eleccion," 2026"),
            hora_num=as.numeric(difftime(hora, inicio26, units="hours")), hora_txt=hh(hora),
            porc_votos, porc_mesas=porc_mesas_total) %>% arrange(eleccion, hora_num)

hito <- function(df, p) { x <- df %>% filter(porc_mesas_total >= p) %>% arrange(hora)
if (nrow(x)==0) NA_character_ else hh(x$hora[1]) }
hitos_mesas <- boletines$nacionales %>%
  filter(eleccion %in% c("Senado 2026","Cámara 2026","CITREP 2026","Consultas 2026"), !is.na(hora), !is.na(porc_mesas_total)) %>%
  group_by(eleccion) %>%
  group_modify(~ tibble(h25=hito(.x,.25), h50=hito(.x,.50), h75=hito(.x,.75), h90=hito(.x,.90))) %>%
  ungroup() %>% mutate(eleccion=str_remove(eleccion," 2026"),
                       eleccion=factor(eleccion, levels=c("Senado","Cámara","CITREP","Consultas"))) %>% arrange(eleccion) %>%
  mutate(eleccion=as.character(eleccion))

df_comp <- bind_rows(
  boletines$nacionales %>% filter(eleccion %in% c("Senado 2022","Senado 2026")) %>% mutate(corp="Senado", fecha=if_else(str_detect(eleccion,"2022"), as.Date("2022-03-13"), as.Date("2026-03-08"))),
  boletines$nacionales %>% filter(eleccion %in% c("Cámara 2022","Cámara 2026")) %>% mutate(corp="Cámara", fecha=if_else(str_detect(eleccion,"2022"), as.Date("2022-03-13"), as.Date("2026-03-08"))),
  boletines$nacionales %>% filter(eleccion %in% c("CITREP 2022","CITREP 2026")) %>% mutate(corp="CITREP", fecha=if_else(str_detect(eleccion,"2022"), as.Date("2022-03-13"), as.Date("2026-03-08"))),
  boletines$nacionales %>% filter(eleccion %in% c("Consulta Centro Esperanza 2022","Consulta Equipo Colombia 2022","Consulta Pacto Histórico 2022","Consultas 2026")) %>% mutate(corp="Consultas", fecha=if_else(str_detect(eleccion,"2022"), as.Date("2022-03-13"), as.Date("2026-03-08")))
) %>% filter(!is.na(hora), !is.na(porc_mesas_total)) %>%
  mutate(inicio=as.POSIXct(paste(fecha,"16:00:00"), tz="America/Bogota"),
         horas_desde_inicio=as.numeric(difftime(hora, inicio, units="hours"))) %>%
  arrange(corp, eleccion, horas_desde_inicio)

comp_22_26 <- df_comp %>% transmute(corp, eleccion, hora_num=horas_desde_inicio,
                                    hora_txt=hh(as.POSIXct("2000-01-01 16:00:00", tz="America/Bogota") + dhours(horas_desde_inicio)),
                                    porc_mesas=porc_mesas_total)
hitos_comp <- df_comp %>% group_by(corp, eleccion) %>%
  summarise(h50_num=horas_desde_inicio[which.min(abs(porc_mesas_total-0.50))],
            h99_num=horas_desde_inicio[which.min(abs(porc_mesas_total-0.99))],
            porc_99=porc_mesas_total[which.min(abs(porc_mesas_total-0.99))], .groups="drop")

# ---- Departamental Senado: hora de llegada al 99% ----3
hora_inicio <- ymd_hms("2026-03-08 16:00:00", tz="America/Bogota")
sen_bol_dep <- base_mmv_mesas %>% filter(corporacion=="SEN") %>%
  group_by(COD_DPTO, primer_boletin) %>% summarise(votos_boletin=sum(votos_primer_boletin, na.rm=TRUE), .groups="drop")
sen_tot_dep <- base_mmv_mesas %>% filter(corporacion=="SEN") %>%
  group_by(COD_DPTO) %>% summarise(votos_finales=sum(votos_finales, na.rm=TRUE), .groups="drop")
sen_99_depto <- boletines_departamentales %>% filter(eleccion=="Senado 2026") %>%
  select(COD_DPTO, departamento, boletin, hora) %>%
  left_join(sen_bol_dep, by=c("COD_DPTO","boletin"="primer_boletin")) %>%
  left_join(sen_tot_dep, by="COD_DPTO") %>%
  mutate(votos_boletin=replace_na(votos_boletin, 0)) %>% arrange(COD_DPTO, boletin) %>%
  group_by(COD_DPTO, departamento, votos_finales) %>%
  mutate(pct=cumsum(votos_boletin)/votos_finales) %>% ungroup() %>%
  group_by(COD_DPTO, departamento) %>%
  summarise(hora_99=if (any(pct>=0.99, na.rm=TRUE)) min(hora[pct>=0.99], na.rm=TRUE) else max(hora, na.rm=TRUE), .groups="drop") %>%
  mutate(horas=as.numeric(difftime(hora_99, hora_inicio, units="hours")),
         cat_hora=case_when(horas<2~"4PM–6PM", horas<4~"6PM–8PM", horas<5~"8PM–9PM", horas<6~"9PM–10PM",
                            horas<8~"10PM–12AM", horas<10~"12AM–2AM", TRUE~"2AM o más"))

dep_bridge <- todos_municipios %>% mutate(COD_DPTO=floor(code_RNEC/1000), coddepto=floor(codmpio/1000)) %>%
  distinct(COD_DPTO, coddepto)
dep_senado <- sen_99_depto %>% left_join(dep_bridge, by="COD_DPTO") %>%
  transmute(coddepto, Depto=departamento, hora_99_txt=hh(hora_99), cat_hora)

# ---- GUARDAR ----3
saveRDS(list(vel_2026=vel_2026, hitos_mesas=hitos_mesas, comp_22_26=comp_22_26,
             hitos_comp=hitos_comp, dep_senado=dep_senado), "datos_preconteo.rds")
message("Listo: datos_preconteo.rds")

#######################################################################3
# SECCION 03-2 COMPARACIÓN PRECONTEO - ESCRUTINIO CONGRESO ----
#######################################################################3

# --- 1. Funciones auxiliares ---

# Función para crear la llave de mesa si no viene en la base
agregar_llave_mesa <- function(df) {
  if (!"llave_mesa" %in% names(df)) {
    df <- df %>%
      mutate(
        temp_dpto = if("COD_DPTO" %in% names(.)) COD_DPTO else if("coddepto" %in% names(.)) coddepto else 0,
        temp_mun = if("codmun" %in% names(.)) codmun else 0,
        temp_zona = if("zona" %in% names(.)) zona else 0,
        temp_puesto = if("puesto" %in% names(.)) puesto else 0,
        temp_mesa = if("nummesa" %in% names(.)) nummesa else if("mesa" %in% names(.)) mesa else 0,
        llave_mesa = str_c(
          str_pad(suppressWarnings(as.numeric(temp_dpto)), 2, pad = "0"),
          str_pad(suppressWarnings(as.numeric(temp_mun)), 3, pad = "0"),
          str_pad(suppressWarnings(as.numeric(temp_zona)), 2, pad = "0"),
          str_pad(str_trim(as.character(temp_puesto)), 2, pad = "0"),
          str_pad(suppressWarnings(as.numeric(temp_mesa)), 6, pad = "0")
        )
      ) %>%
      select(-temp_dpto, -temp_mun, -temp_zona, -temp_puesto, -temp_mesa)
  }
  return(df)
}

# Función genérica para calcular las mesas comparables
generar_resumen_comparable <- function(pre_data, esc_data, anno_val) {
  
  # Asegurar que ambas bases tengan llave_mesa
  pre_data <- agregar_llave_mesa(pre_data)
  esc_data <- agregar_llave_mesa(esc_data)
  
  # Agrupar por mesa para tener el total de votos por mesa en pre y esc
  pre_mesa <- pre_data %>% group_by(corp_tabla, circ_tabla, llave_mesa) %>% summarise(votos_pre = sum(votos, na.rm = TRUE), .groups = "drop")
  esc_mesa <- esc_data %>% group_by(corp_tabla, circ_tabla, llave_mesa) %>% summarise(votos_esc = sum(votos, na.rm = TRUE), .groups = "drop")
  
  # Totales absolutos
  totales_pre <- pre_mesa %>% group_by(corp_tabla, circ_tabla) %>% summarise(Votos_preconteo = sum(votos_pre, na.rm = TRUE), .groups = "drop")
  totales_esc <- esc_mesa %>% group_by(corp_tabla, circ_tabla) %>% summarise(Votos_escrutinio = sum(votos_esc, na.rm = TRUE), .groups = "drop")
  
  # Inner join para encontrar mesas comparables
  comparables <- pre_mesa %>%
    inner_join(esc_mesa, by = c("corp_tabla", "circ_tabla", "llave_mesa")) %>%
    group_by(corp_tabla, circ_tabla) %>%
    summarise(
      Votos_preconteo_comparable = sum(votos_pre, na.rm = TRUE),
      Votos_escrutinio_comparable = sum(votos_esc, na.rm = TRUE),
      Cambio_neto_mesas_comparables = sum(votos_esc - votos_pre, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Unir todo
  detalle <- full_join(totales_pre, totales_esc, by = c("corp_tabla", "circ_tabla")) %>%
    left_join(comparables, by = c("corp_tabla", "circ_tabla")) %>%
    mutate(across(where(is.numeric), ~replace_na(., 0)))
  
  # Función auxiliar para subtotales
  calc_subtotal <- function(df, corp_name, circ_name) {
    df %>% summarise(
      corp_tabla = corp_name,
      circ_tabla = circ_name,
      Votos_preconteo = sum(Votos_preconteo),
      Votos_escrutinio = sum(Votos_escrutinio),
      Votos_preconteo_comparable = sum(Votos_preconteo_comparable),
      Votos_escrutinio_comparable = sum(Votos_escrutinio_comparable),
      Cambio_neto_mesas_comparables = sum(Cambio_neto_mesas_comparables),
      .groups = "drop"
    )
  }
  
  sub_sen <- detalle %>% filter(corp_tabla == "Senado") %>% calc_subtotal("Senado", "Subtotal")
  sub_cam <- detalle %>% filter(corp_tabla == "Cámara") %>% calc_subtotal("Cámara", "Subtotal")
  sub_cong <- bind_rows(sub_sen, sub_cam, detalle %>% filter(corp_tabla == "CITREP")) %>% calc_subtotal("Congreso", "Subtotal Congreso")
  
  resumen <- bind_rows(
    detalle %>% filter(corp_tabla == "Senado"), sub_sen,
    detalle %>% filter(corp_tabla == "Cámara"), sub_cam,
    detalle %>% filter(corp_tabla == "CITREP"), sub_cong,
    detalle %>% filter(corp_tabla == "Consultas")
  ) %>%
    mutate(
      anno = anno_val,
      Cambio_neto_pct = if_else(Votos_preconteo_comparable > 0, 100 * Cambio_neto_mesas_comparables / Votos_preconteo_comparable, NA_real_)
    )
  
  return(resumen)
}

# --- 2. Procesamiento 2026 ---
ruta_esc_def <- file.path("../Congreso 2026/datos/ESCRUTINIO", "escrutinio_definitivo")
escrutinio_definitivo <- readRDS(file.path(ruta_esc_def, "escrutinio_definitivo.rds"))

# Cargar escrutinio de consultas 2026
fecha_esc_26 <- "20 abril"
ruta_esc_cns_26 <- file.path("../Congreso 2026/datos/ESCRUTINIO", "MMV_ESC", fecha_esc_26, "escrutinio.rds")
escrutinio_consultas_26 <- readRDS(ruta_esc_cns_26) %>% 
  filter(corp == "CONSULTAS" | corp == "CNS" | corporacion_sigla == "CNS")

preconteo_sen <- readRDS("../Congreso 2026/datos/preconteo_base_sen.rds")
preconteo_cam <- readRDS("../Congreso 2026/datos/preconteo_base_cam.rds")
preconteo_ctp <- readRDS("../Congreso 2026/datos/preconteo_base_ctp.rds")
preconteo_cns <- readRDS("../Congreso 2026/datos/preconteo_base_cns.rds")

pre_todos_26 <- bind_rows(preconteo_sen, preconteo_cam, preconteo_ctp, preconteo_cns) %>%
  mutate(
    codcirc = suppressWarnings(as.numeric(codcirc)),
    circ_tabla = case_when(
      corp == "SENADO" & codcirc == 0 ~ "Nacional",
      corp == "SENADO" & codcirc == 4 ~ "Indígenas",
      corp == "CAMARA" & codcirc == 1 ~ "Territorial departamental",
      corp == "CAMARA" & codcirc == 4 ~ "Indígenas",
      corp == "CAMARA" & codcirc == 5 ~ "Afrodescendientes",
      corp == "CITREP" ~ "CITREP",
      corp == "CONSULTAS" ~ "Nacional",
      TRUE ~ coalesce(as.character(circ), "Sin circunscripción")
    ),
    corp_tabla = case_when(
      corp == "CAMARA" ~ "Cámara",
      corp == "SENADO" ~ "Senado",
      corp == "CITREP" ~ "CITREP",
      corp == "CONSULTAS" ~ "Consultas",
      TRUE ~ corp
    )
  )

# Unir escrutinio definitivo (Congreso) y escrutinio (Consultas)
esc_def_26 <- escrutinio_definitivo %>%
  mutate(
    codcirc = suppressWarnings(as.numeric(codcirc)),
    circ_tabla = case_when(
      corp == "SENADO" & codcirc == 0 ~ "Nacional",
      corp == "SENADO" & codcirc == 4 ~ "Indígenas",
      corp == "CAMARA" & codcirc == 1 ~ "Territorial departamental",
      corp == "CAMARA" & codcirc == 4 ~ "Indígenas",
      corp == "CAMARA" & codcirc == 5 ~ "Afrodescendientes",
      corp == "CITREP" ~ "CITREP",
      TRUE ~ coalesce(as.character(circ), "Sin circunscripción")
    ),
    corp_tabla = case_when(
      corp == "CAMARA" ~ "Cámara",
      corp == "SENADO" ~ "Senado",
      corp == "CITREP" ~ "CITREP",
      TRUE ~ corp
    )
  )

esc_cns_26 <- escrutinio_consultas_26 %>%
  mutate(
    codcirc = suppressWarnings(as.numeric(codcirc)),
    circ_tabla = "Nacional",
    corp_tabla = "Consultas"
  )

esc_todos_26 <- bind_rows(esc_def_26, esc_cns_26)

resumen_26 <- generar_resumen_comparable(pre_todos_26, esc_todos_26, 2026)

# --- 3. Procesamiento 2022 ---
pre_sen_22 <- readRDS("../Congreso 2026/datos/2022/pre_sen_2022.rds")
pre_cam_22 <- readRDS("../Congreso 2026/datos/2022/pre_cam_2022.rds")
pre_ctp_22 <- readRDS("../Congreso 2026/datos/2022/pre_ctp_2022.rds")
esc_sen_22 <- readRDS("../Congreso 2026/datos/2022/esc_sen_2022.rds")
esc_cam_22 <- readRDS("../Congreso 2026/datos/2022/esc_cam_2022.rds")
esc_ctp_22 <- readRDS("../Congreso 2026/datos/2022/esc_ctp_2022.rds")

pre_todos_22 <- bind_rows(pre_sen_22 %>% mutate(corp = "SENADO"), pre_cam_22 %>% mutate(corp = "CAMARA"), pre_ctp_22 %>% mutate(corp = "CITREP")) %>%
  mutate(codcirc = suppressWarnings(as.numeric(codcirc)), circ_tabla = case_when(corp == "SENADO" & codcirc == 0 ~ "Nacional", corp == "SENADO" & codcirc == 4 ~ "Indígenas", corp == "CAMARA" & codcirc == 1 ~ "Territorial departamental", corp == "CAMARA" & codcirc == 4 ~ "Indígenas", corp == "CAMARA" & codcirc == 5 ~ "Afrodescendientes", corp == "CITREP" ~ "CITREP", TRUE ~ "Sin circunscripción"), corp_tabla = case_when(corp == "CAMARA" ~ "Cámara", corp == "SENADO" ~ "Senado", corp == "CITREP" ~ "CITREP", TRUE ~ corp))

esc_todos_22 <- bind_rows(esc_sen_22 %>% mutate(corp = "SENADO"), esc_cam_22 %>% mutate(corp = "CAMARA"), esc_ctp_22 %>% mutate(corp = "CITREP")) %>%
  mutate(codcirc = suppressWarnings(as.numeric(codcirc)), circ_tabla = case_when(corp == "SENADO" & codcirc == 0 ~ "Nacional", corp == "SENADO" & codcirc == 4 ~ "Indígenas", corp == "CAMARA" & codcirc == 1 ~ "Territorial departamental", corp == "CAMARA" & codcirc == 4 ~ "Indígenas", corp == "CAMARA" & codcirc == 5 ~ "Afrodescendientes", corp == "CITREP" ~ "CITREP", TRUE ~ "Sin circunscripción"), corp_tabla = case_when(corp == "CAMARA" ~ "Cámara", corp == "SENADO" ~ "Senado", corp == "CITREP" ~ "CITREP", TRUE ~ corp))

resumen_22 <- generar_resumen_comparable(pre_todos_22, esc_todos_22, 2022)

# --- 4. Procesamiento 2018 ---
pre_sen_18 <- readRDS("../Congreso 2026/datos/2018/PRECONTEO/preconteo_2018_sen_partido.rds")
pre_cam_18 <- readRDS("../Congreso 2026/datos/2018/PRECONTEO/preconteo_2018_cam_partido.rds")
esc_sen_18 <- readRDS("../Congreso 2026/datos/2018/ESCRUTINIO/escrutinio_2018_sen_partido.rds")
esc_cam_18 <- readRDS("../Congreso 2026/datos/2018/ESCRUTINIO/escrutinio_2018_cam_partido.rds")

pre_todos_18 <- bind_rows(pre_sen_18 %>% mutate(corp = "SENADO"), pre_cam_18 %>% mutate(corp = "CAMARA")) %>% mutate(codcirc = suppressWarnings(as.numeric(codcirc)), circ_tabla = case_when(corp == "SENADO" & codcirc == 0 ~ "Nacional", corp == "SENADO" & codcirc == 4 ~ "Indígenas", corp == "CAMARA" & codcirc == 1 ~ "Territorial departamental", corp == "CAMARA" & codcirc == 4 ~ "Indígenas", corp == "CAMARA" & codcirc == 5 ~ "Afrodescendientes", TRUE ~ "Sin circunscripción"), corp_tabla = case_when(corp == "CAMARA" ~ "Cámara", corp == "SENADO" ~ "Senado", TRUE ~ corp), votos = suppressWarnings(as.numeric(votos)))
esc_todos_18 <- bind_rows(esc_sen_18 %>% mutate(corp = "SENADO"), esc_cam_18 %>% mutate(corp = "CAMARA")) %>% mutate(codcirc = suppressWarnings(as.numeric(codcirc)), circ_tabla = case_when(corp == "SENADO" & codcirc == 0 ~ "Nacional", corp == "SENADO" & codcirc == 4 ~ "Indígenas", corp == "CAMARA" & codcirc == 1 ~ "Territorial departamental", corp == "CAMARA" & codcirc == 4 ~ "Indígenas", corp == "CAMARA" & codcirc == 5 ~ "Afrodescendientes", TRUE ~ "Sin circunscripción"), corp_tabla = case_when(corp == "CAMARA" ~ "Cámara", corp == "SENADO" ~ "Senado", TRUE ~ corp), votos = suppressWarnings(as.numeric(votos)))

resumen_18 <- generar_resumen_comparable(pre_todos_18, esc_todos_18, 2018)

# --- 5. Consolidar y Guardar ---
resumen_gral <- bind_rows(resumen_26, resumen_22, resumen_18) %>%
  mutate(
    orden_corp = case_when(corp_tabla == "Senado" ~ 1, corp_tabla == "Cámara" ~ 2, corp_tabla == "CITREP" ~ 3, corp_tabla == "Congreso" ~ 4, corp_tabla == "Consultas" ~ 5, TRUE ~ 99),
    orden_circ = case_when(circ_tabla == "Subtotal" ~ 98, circ_tabla == "Subtotal Congreso" ~ 99, TRUE ~ 1)
  ) %>% arrange(anno, orden_corp, orden_circ, desc(Votos_preconteo))

saveRDS(resumen_gral, "datos_escrutinio_gral.rds")
message("Listo: datos_escrutinio_gral.rds (Con Consultas 2026 incluidas)")



#COMPARACIÓN DE CURULES PRECONTEO VS ESCRUTINIO


# 1. Definir referencias necesarias
map_citrep_lbl <- tibble::tribble(
  ~CTEP, ~lbl,
  1L,  "1. Cauca-Nariño-Valle", 2L,  "2. Arauca", 3L,  "3. Bajo Cauca", 4L,  "4. Catatumbo",
  5L,  "5. Caquetá-Huila", 6L,  "6. Chocó", 7L,  "7. Sur de Meta-Guaviare", 8L,  "8. Montes de María",
  9L,  "9. Pacífico Cauca-Valle", 10L, "10. Pacífico Nariño", 11L, "11. Putumayo", 12L, "12. Cesar-Guajira-Magdalena",
  13L, "13. Sur de Bolívar", 14L, "14. Sur de Córdoba", 15L, "15. Sur de Tolima", 16L, "16. Urabá"
)

depto_ref_ok <- todos_municipios %>%
  transmute(COD_DPTO = as.numeric(substr(str_pad(code_RNEC, 5, pad = "0"), 1, 2)), Depto) %>%
  filter(!is.na(COD_DPTO), !is.na(Depto), Depto != "") %>%
  distinct(COD_DPTO, Depto) %>%
  group_by(COD_DPTO) %>%
  summarise(Depto = first(Depto), .groups = "drop")

# Función generadora de tabla de Senado
gen_senado <- function(pre_tot, esc_tot, anno_val) {
  
  # Normalización
  if("curules_partido" %in% names(pre_tot) && !"curules_pre" %in% names(pre_tot)) pre_tot <- pre_tot %>% rename(curules_pre = curules_partido)
  if("curules_partido" %in% names(esc_tot) && !"curules_esc" %in% names(esc_tot)) esc_tot <- esc_tot %>% rename(curules_esc = curules_partido)
  
  pre <- pre_tot %>% filter(corp == "SENADO") %>%
    mutate(codcirc = suppressWarnings(as.numeric(codcirc)),
           circ_tabla = if_else(codcirc == 4, "Circunscripción indígena", "Circunscripción nacional")) %>%
    group_by(nomparti, circ_tabla) %>% summarise(curules_pre = sum(curules_pre, na.rm=TRUE), .groups="drop")
  
  esc <- esc_tot %>% filter(corp == "SENADO") %>%
    mutate(codcirc = suppressWarnings(as.numeric(codcirc)),
           circ_tabla = if_else(codcirc == 4, "Circunscripción indígena", "Circunscripción nacional")) %>%
    group_by(nomparti, circ_tabla) %>% summarise(curules_esc = sum(curules_esc, na.rm=TRUE), .groups="drop")
  
  full_join(pre, esc, by = c("nomparti", "circ_tabla")) %>%
    mutate(curules_pre = coalesce(curules_pre, 0L), curules_esc = coalesce(curules_esc, 0L), diferencia = curules_esc - curules_pre,
           corp = "Senado", anno = anno_val, Territorio = NA_character_, circ_ord = if_else(circ_tabla == "Circunscripción nacional", 1L, 2L)) %>%
    filter(curules_pre > 0 | curules_esc > 0) %>%
    rename(Partido = nomparti, Circunscripcion = circ_tabla)
}

# Función generadora de tabla de Cámara
gen_camara <- function(pre_tot, esc_tot, anno_val) {
  
  # --- Normalización robusta de columnas ---
  if("curules_partido" %in% names(pre_tot) && !"curules_pre" %in% names(pre_tot)) pre_tot <- pre_tot %>% rename(curules_pre = curules_partido)
  if("curules_partido" %in% names(esc_tot) && !"curules_esc" %in% names(esc_tot)) esc_tot <- esc_tot %>% rename(curules_esc = curules_partido)
  
  if("coddepto" %in% names(pre_tot) && !"COD_DPTO" %in% names(pre_tot)) pre_tot <- pre_tot %>% rename(COD_DPTO = coddepto)
  if("coddepto" %in% names(esc_tot) && !"COD_DPTO" %in% names(esc_tot)) esc_tot <- esc_tot %>% rename(COD_DPTO = coddepto)
  if(!"COD_DPTO" %in% names(pre_tot)) pre_tot <- pre_tot %>% mutate(COD_DPTO = NA_real_)
  if(!"COD_DPTO" %in% names(esc_tot)) esc_tot <- esc_tot %>% mutate(COD_DPTO = NA_real_)
  
  if("nomdepto" %in% names(pre_tot) && !"Depto" %in% names(pre_tot)) pre_tot <- pre_tot %>% rename(Depto = nomdepto)
  if("nomdepto" %in% names(esc_tot) && !"Depto" %in% names(esc_tot)) esc_tot <- esc_tot %>% rename(Depto = nomdepto)
  
  # Si aún falta Depto, se une con la tabla maestra
  if(!"Depto" %in% names(pre_tot)) pre_tot <- pre_tot %>% left_join(depto_ref_ok, by = "COD_DPTO")
  if(!"Depto" %in% names(esc_tot)) esc_tot <- esc_tot %>% left_join(depto_ref_ok, by = "COD_DPTO")
  
  # Para años que no tenían CITREP (ej. 2018)
  if(!"CTEP" %in% names(pre_tot)) pre_tot <- pre_tot %>% mutate(CTEP = NA_integer_)
  if(!"CTEP" %in% names(esc_tot)) esc_tot <- esc_tot %>% mutate(CTEP = NA_integer_)
 
  # Bloque 1 (Territorial)
  pre_1 <- pre_tot %>% filter(corp == "CAMARA", codcirc == 1, curules_pre > 0) %>% distinct(COD_DPTO, Depto, nomparti, curules_pre)
  esc_1 <- esc_tot %>% filter(corp == "CAMARA", codcirc == 1, curules_esc > 0) %>% distinct(COD_DPTO, Depto, nomparti, curules_esc)
  
  b1 <- full_join(pre_1, esc_1, by = c("COD_DPTO", "nomparti"), suffix = c("_pre", "_esc")) %>%
    mutate(Depto = coalesce(Depto_pre, Depto_esc), curules_pre = coalesce(curules_pre, 0L), curules_esc = coalesce(curules_esc, 0L)) %>%
    group_by(nomparti) %>%
    summarise(curules_pre = sum(curules_pre, na.rm=TRUE), curules_esc = sum(curules_esc, na.rm=TRUE),
              Territorio = paste(sort(unique(Depto)), collapse=", "), diferencia = curules_esc - curules_pre, circ_ord = 1L, Circunscripcion = "Territorial", .groups="drop") %>%
    filter(curules_pre > 0 | curules_esc > 0)
  
  # Bloque CITREP
  pre_ctp <- pre_tot %>% filter(corp == "CITREP", curules_pre > 0) %>% select(nomparti, CTEP, curules_pre)
  esc_ctp <- esc_tot %>% filter(corp == "CITREP", curules_esc > 0) %>% select(nomparti, CTEP, curules_esc)
  
  b_ctp <- full_join(pre_ctp, esc_ctp, by = c("nomparti", "CTEP")) %>%
    mutate(curules_pre = coalesce(curules_pre, 0L), curules_esc = coalesce(curules_esc, 0L), diferencia = curules_esc - curules_pre, circ_ord = 2L, Circunscripcion = "CITREP") %>%
    filter(curules_pre > 0 | curules_esc > 0) %>% left_join(map_citrep_lbl, by = "CTEP") %>% rename(Territorio = lbl) %>% arrange(CTEP) %>%
    select(circ_ord, Circunscripcion, nomparti, Territorio, curules_pre, curules_esc, diferencia)
  
  # Bloque Indigena
  pre_4 <- pre_tot %>% filter(corp == "CAMARA", codcirc == 4, curules_pre > 0) %>% select(nomparti, curules_pre)
  esc_4 <- esc_tot %>% filter(corp == "CAMARA", codcirc == 4, curules_esc > 0) %>% select(nomparti, curules_esc)
  
  b4 <- full_join(pre_4, esc_4, by = "nomparti") %>%
    mutate(curules_pre = coalesce(curules_pre, 0L), curules_esc = coalesce(curules_esc, 0L), diferencia = curules_esc - curules_pre, Territorio = "", circ_ord = 3L, Circunscripcion = "Indígena") %>%
    filter(curules_pre > 0 | curules_esc > 0)
  
  # Bloque Afro
  pre_5 <- pre_tot %>% filter(corp == "CAMARA", codcirc == 5, curules_pre > 0) %>% select(nomparti, curules_pre)
  esc_5 <- esc_tot %>% filter(corp == "CAMARA", codcirc == 5, curules_esc > 0) %>% select(nomparti, curules_esc)
  
  b5 <- full_join(pre_5, esc_5, by = "nomparti") %>%
    mutate(curules_pre = coalesce(curules_pre, 0L), curules_esc = coalesce(curules_esc, 0L), diferencia = curules_esc - curules_pre, Territorio = "", circ_ord = 4L, Circunscripcion = "Afrodescendiente") %>%
    filter(curules_pre > 0 | curules_esc > 0)
  
  bind_rows(b1, b_ctp, b4, b5) %>% rename(Partido = nomparti) %>% mutate(corp = "Cámara", anno = anno_val)
}

# 2. Cargar datos base y generar consolidado
c_pre_26 <- readRDS(file.path("../Congreso 2026/datos/ESCRUTINIO", "curules_pre_total.rds"))
c_esc_26 <- readRDS(file.path("../Congreso 2026/datos/ESCRUTINIO", "curules_esc_total.rds"))
c_pre_22 <- readRDS("../Congreso 2026/datos/2022/curules_pre_total.rds")
c_esc_22 <- readRDS("../Congreso 2026/datos/2022/curules_partido_total.rds")
c_pre_18 <- readRDS("../Congreso 2026/datos/2018/curules_pre_total_2018.rds")
c_esc_18 <- readRDS("../Congreso 2026/datos/2018/curules_partido_total_2018.rds")

curules_gral <- bind_rows(
  gen_senado(c_pre_26, c_esc_26, 2026),
  gen_camara(c_pre_26, c_esc_26, 2026),
  gen_senado(c_pre_22, c_esc_22, 2022),
  gen_camara(c_pre_22, c_esc_22, 2022),
  gen_senado(c_pre_18, c_esc_18, 2018),
  gen_camara(c_pre_18, c_esc_18, 2018)
) %>% arrange(anno, corp, circ_ord, desc(curules_pre))

saveRDS(curules_gral, "datos_escrutinio_curules.rds")
message("Listo: datos_escrutinio_curules.rds")


#######################################################################3
# SECCION 03-3 PARTICIPACIÓN ELECTORAL ----
#######################################################################3
# 1. Cargar la base maestra (De aquí sacamos el censo por puesto y los votos por corporación)
ruta_rds <- "../Congreso 2026/datos/ESCRUTINIO" 
base_congreso <- readRDS(file.path(ruta_rds, "base_congreso_2018_2026.rds"))

base_dep_part <- base_congreso %>%
  filter(
    annoh %in% c(2018, 2022, 2026),
    (corp == "SENADO" & codcirc %in% c(0, 4)) |
      (corp == "CAMARA" & codcirc %in% c(1, 4, 5))
  ) %>%
  mutate(
    id_puesto = paste0(
      str_pad(as.character(code_RNEC), 5, pad = "0"), "-",
      str_pad(as.character(zona), 2, pad = "0"), "-",
      str_pad(as.character(puesto), 2, pad = "0")
    ),
    corp_mapa = case_when(
      corp == "SENADO" ~ "Senado",
      corp == "CAMARA" ~ "Cámara",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(corp_mapa))

# Censo Nacional, Departamental y Municipal (Convirtiendo código RNEC a código DANE)
censo_divipol_hist <- bind_rows(
  cong_2018 %>% mutate(annoh = 2018),
  cong_2022 %>% mutate(annoh = 2022),
  divipol_cong_26 %>% mutate(annoh = 2026)
) %>%
  left_join(map_rnec_codmpio %>% select(code_RNEC, codmpio), by = "code_RNEC") %>%
  mutate(coddepto = floor(codmpio / 1000)) %>%
  filter(!is.na(codmpio))

censo_nac <- censo_divipol_hist %>%
  group_by(annoh) %>%
  summarise(censo_total = sum(censo, na.rm = TRUE), .groups = "drop")

censo_dep <- censo_divipol_hist %>%
  group_by(annoh, coddepto) %>%
  summarise(censo_total = sum(censo, na.rm = TRUE), .groups = "drop")

# Votos Nacional y Departamental por corporación
votos_nac <- base_dep_part %>%
  group_by(annoh, corp_mapa) %>%
  summarise(votos_total = sum(votos, na.rm = TRUE), .groups = "drop")

votos_dep <- base_dep_part %>%
  group_by(annoh, corp_mapa, coddepto) %>%
  summarise(votos_total = sum(votos, na.rm = TRUE), .groups = "drop")

part_nac <- votos_nac %>%
  left_join(censo_nac, by = "annoh") %>%
  mutate(participacion = 100 * votos_total / censo_total, corp_tabla = corp_mapa) %>% 
  select(-corp_mapa)

# Consolidar a nivel departamental
todos_deptos <- todos_municipios %>%  mutate(coddepto=floor(codmpio/1000)) %>% 
  mutate(COD_DPTO=floor(code_RNEC/1000)) %>%  distinct(coddepto, .keep_all=T) %>% 
  select(Depto, coddepto, COD_DPTO)
grilla_dep <- expand_grid(
  annoh = c(2018, 2022, 2026),
  corp_tabla = c("Senado", "Cámara"),
  coddepto = sort(unique(todos_deptos$coddepto))
)

part_dep <- grilla_dep %>%
  left_join(votos_dep %>% rename(corp_tabla = corp_mapa), by = c("annoh", "corp_tabla", "coddepto")) %>%
  left_join(censo_dep, by = c("annoh", "coddepto")) %>%
  left_join(todos_deptos %>% select(coddepto, Depto), by = "coddepto") %>%
  mutate(
    votos_total = replace_na(votos_total, 0),
    participacion = if_else(!is.na(censo_total) & censo_total > 0, 100 * votos_total / censo_total, NA_real_)
  ) %>% filter(!is.na(Depto))

part_dep_comp <- part_dep %>%
  select(annoh, corp_tabla, coddepto, Depto, participacion) %>%
  pivot_wider(names_from = annoh, values_from = participacion, names_prefix = "part_") %>%
  mutate(
    cambio_22 = part_2026 - part_2022,
    cambio_18 = part_2026 - part_2018
  ) %>% arrange(coddepto)

# 2. PARTICIPACIÓN TOTAL CONGRESO
calc_total_congreso_mesa <- function(anno_val) {
  
  if (anno_val == 2026) {
    df <- readRDS("../Congreso 2026/datos/ESCRUTINIO/escrutinio_definitivo/escrutinio_definitivo.rds") %>%
      filter((corp == "SENADO" & codcirc %in% c(0, 4)) | (corp == "CAMARA" & codcirc %in% c(1, 4, 5, 9))) %>%
      mutate(grupo_total = if_else(corp == "SENADO", "Senado", if_else(codcirc == 9, "CITREP", "Cámara")))
    
  } else if (anno_val == 2022) {
    sen <- readRDS("../Congreso 2026/datos/2022/esc_sen_2022.rds") %>% mutate(grupo_total = "Senado")
    cam <- readRDS("../Congreso 2026/datos/2022/esc_cam_2022.rds") %>% mutate(grupo_total = "Cámara")
    df <- bind_rows(sen, cam)
    
    if(file.exists("../Congreso 2026/datos/2022/esc_ctp_2022.rds")) {
      ctp <- readRDS("../Congreso 2026/datos/2022/esc_ctp_2022.rds") %>% mutate(grupo_total = "CITREP")
      df <- bind_rows(df, ctp)
    }
    
    df <- df %>%
      mutate(
        c_dpto = if("COD_DPTO" %in% names(.)) COD_DPTO else coddepto,
        c_mpio = if("codmun" %in% names(.)) codmun else codmpio,
        c_mesa = if("nummesa" %in% names(.)) nummesa else mesa,
        llave_mesa = paste0(
          str_pad(as.character(c_dpto), 2, pad = "0"),
          str_pad(as.character(c_mpio), 3, pad = "0"),
          str_pad(as.character(zona), 2, pad = "0"),
          str_pad(as.character(puesto), 2, pad = "0"),
          str_pad(as.character(c_mesa), 4, pad = "0")
        )
      )
    
  } else if (anno_val == 2018) {
    sen <- readRDS("../Congreso 2026/datos/2018/ESCRUTINIO/escrutinio_2018_sen_partido.rds") %>% mutate(grupo_total = "Senado")
    cam <- readRDS("../Congreso 2026/datos/2018/ESCRUTINIO/escrutinio_2018_cam_partido.rds") %>% mutate(grupo_total = "Cámara")
    df <- bind_rows(sen, cam)
    
    df <- df %>%
      mutate(
        c_dpto = if("COD_DPTO" %in% names(.)) COD_DPTO else coddepto,
        c_mpio = if("codmun" %in% names(.)) codmun else codmpio,
        c_mesa = if("nummesa" %in% names(.)) nummesa else mesa,
        llave_mesa = paste0(
          str_pad(as.character(c_dpto), 2, pad = "0"),
          str_pad(as.character(c_mpio), 3, pad = "0"),
          str_pad(as.character(zona), 2, pad = "0"),
          str_pad(as.character(puesto), 2, pad = "0"),
          str_pad(as.character(c_mesa), 4, pad = "0")
        )
      )
  }
  
  v_mesa <- df %>%
    group_by(llave_mesa, grupo_total) %>%
    summarise(votos = sum(as.numeric(votos), na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = grupo_total, values_from = votos, values_fill = 0)
  
  if (!"Senado" %in% names(v_mesa)) v_mesa$Senado <- 0
  if (!"Cámara" %in% names(v_mesa)) v_mesa$Cámara <- 0
  if (!"CITREP" %in% names(v_mesa)) v_mesa$CITREP <- 0
  
  v_mesa <- v_mesa %>% mutate(max_votos = pmax(Senado, Cámara, CITREP, na.rm = TRUE))
  
  tot_votos <- sum(v_mesa$max_votos, na.rm = TRUE)
  tot_censo <- censo_nac %>% filter(annoh == anno_val) %>% pull(censo_total)
  
  tibble(annoh = anno_val, part_congreso = 100 * tot_votos / tot_censo)
}

part_congreso_total <- bind_rows(
  calc_total_congreso_mesa(2026),
  calc_total_congreso_mesa(2022),
  calc_total_congreso_mesa(2018)
)

# 4. Shapefile para el mapa de Quarto
ruta_general <- "../datos generales"
sf_dep_part <- readRDS(file.path(ruta_general, "shapes", "departamento_simpl_sanandres_cache.rds"))$sf_obj %>%
  mutate(coddepto = as.integer(cod_depto)) %>% select(coddepto)


# =====================================================================3
#  SECCION 03-4 PARTICIPACIÓN MUNICIPAL ----
# =====================================================================3

censo_mpio <- censo_divipol_hist %>%
  group_by(annoh, codmpio) %>% 
  summarise(censo_total = sum(censo, na.rm = TRUE), .groups = "drop")

votos_mpio <- base_dep_part %>%
  group_by(annoh, corp_mapa, codmpio) %>% summarise(votos_total = sum(votos, na.rm = TRUE), .groups = "drop")

muni_info <- todos_municipios %>% distinct(codmpio, Municipio, Depto)
grilla_mpio <- expand_grid(annoh = c(2018, 2022, 2026),
                           corp_tabla = c("Senado", "Cámara"),
                           codmpio = sort(unique(muni_info$codmpio)))

part_mpio <- grilla_mpio %>%
  left_join(votos_mpio %>% rename(corp_tabla = corp_mapa), by = c("annoh", "corp_tabla", "codmpio")) %>%
  left_join(censo_mpio, by = c("annoh", "codmpio")) %>%
  left_join(muni_info, by = "codmpio") %>%
  mutate(votos_total = replace_na(votos_total, 0),
         participacion = if_else(!is.na(censo_total) & censo_total > 0, 100 * votos_total / censo_total, NA_real_)) %>%
  filter(!is.na(Municipio))

part_mpio_comp <- part_mpio %>%
  select(annoh, corp_tabla, codmpio, Municipio, Depto, participacion) %>%
  pivot_wider(names_from = annoh, values_from = participacion, names_prefix = "part_") %>%
  mutate(cambio_22 = part_2026 - part_2022, cambio_18 = part_2026 - part_2018) %>%
  arrange(codmpio)

# Comparación Senado - Cámara (2026): positivo = mayor participación en Senado
muni_dif <- part_mpio_comp %>%
  select(codmpio, Municipio, Depto, corp_tabla, part_2026) %>%
  pivot_wider(names_from = corp_tabla, values_from = part_2026) %>%
  rename(part_sen = Senado, part_cam = `Cámara`) %>%
  mutate(dif_sc = part_sen - part_cam)

# Geometría municipal para el mapa
sf_mpi_part <- readRDS(file.path(ruta_general, "shapes", "muni_simpl_sanandres_cache.rds"))$sf_obj %>%
  mutate(codmpio = as.integer(codmpio)) %>% select(codmpio)

# Guardar
saveRDS(list(nacional = part_nac, departamental = part_dep_comp,
             total_congreso = part_congreso_total, sf_deptos = sf_dep_part,
             municipal = part_mpio_comp, municipal_dif = muni_dif, sf_mpios = sf_mpi_part),
        "datos_participacion_final.rds")

# =====================================================================3
#  SECCION 03-5 PARTICIPACIÓN EXTERIOR ----
# =====================================================================3

# 1. Censo y participación anual

censo_ext <- div_resumen %>% 
  filter(eleccion == "Congreso", ambito == "Exterior") %>% 
  select(annoh, censo)

part_ext_congreso <- base_congreso %>% 
  filter(COD_DPTO == 88, annoh %in% c(2018, 2022, 2026), corp %in% c("SENADO", "CAMARA")) %>% 
  mutate(corp = case_when(corp == "SENADO" ~ "Senado", corp == "CAMARA" ~ "Cámara", TRUE ~ corp)) %>% 
  group_by(annoh, corp) %>% 
  summarise(votos = sum(votos, na.rm = TRUE), .groups = "drop") %>% 
  left_join(censo_ext, by = "annoh") %>% 
  mutate(participacion = votos / censo, part_pct = 100 * participacion, annoh = factor(annoh, levels = c(2018, 2022, 2026)))

# 2. Votos por día en 2026 (Agrupado por corporación y día)
ruta_esc_def <- file.path("../Congreso 2026/datos/ESCRUTINIO")
base_congreso_exterior_por_dia <- readRDS(file.path(ruta_esc_def, "base_congreso_exterior_por_dia_2018_2026.rds"))


votos_ext_dia_2026 <- base_congreso_exterior_por_dia %>% 
  filter(COD_DPTO == 88, annoh == 2026, corp %in% c("SENADO", "CAMARA")) %>% 
  mutate(
    corp = case_when(corp == "SENADO" ~ "Senado", corp == "CAMARA" ~ "Cámara", TRUE ~ corp),
    dia = case_when(
      puesto == 81 ~ "Lunes", puesto == 82 ~ "Martes", puesto == 83 ~ "Miércoles",
      puesto == 84 ~ "Jueves", puesto == 85 ~ "Viernes", puesto == 86 ~ "Sábado", TRUE ~ "Domingo"
    ),
    dia = factor(dia, levels = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo"))
  ) %>% 
  group_by(corp, dia) %>% 
  summarise(votos = sum(votos, na.rm = TRUE), .groups = "drop") %>% 
  group_by(corp) %>% 
  mutate(prop = votos / sum(votos), prop_pct = 100 * prop, resaltar = if_else(dia == "Domingo", "Domingo", "Lunes a sábado")) %>% 
  ungroup()

# 3. Diccionario y datos mundiales
puestos_ext_2026 <- base_congreso %>% filter(COD_DPTO == 88, annoh == 2026) %>% distinct(nompuesto, zona)

# Diccionario (El mismo que ya tenías)
dic_pais_ext <- puestos_ext_2026 %>% 
  mutate(pais = case_when(
    nompuesto == "Accra Consulado" ~ "Ghana", nompuesto == "Dakar" ~ "Senegal", nompuesto == "Abu Dhabi - Consulado" ~ "Emiratos Árabes Unidos",
    nompuesto == "Albacete" ~ "España", nompuesto == "Alicante" ~ "España", nompuesto == "Almería" ~ "España",
    nompuesto == "Amsterdam Consulado" ~ "Países Bajos", nompuesto == "Ankara Consulado" ~ "Turquía", nompuesto == "Antofagasta Consulado" ~ "Chile",
    nompuesto == "Argel Consulado" ~ "Argelia", nompuesto == "Asunción - Consulado" ~ "Paraguay", nompuesto == "Atenas - Grecia" ~ "Grecia",
    str_detect(nompuesto, "^Atlanta") ~ "Estados Unidos", nompuesto == "Auckland - Consulado" ~ "Nueva Zelanda", nompuesto == "Baku Consulado" ~ "Azerbaiyán",
    nompuesto == "Bangkok Consulado" ~ "Tailandia", str_detect(nompuesto, "^Barcelona") ~ "España", nompuesto == "Barquisimeto" ~ "Venezuela",
    nompuesto == "Beijing - Consulado" ~ "China", nompuesto == "Beirut - Consulado" ~ "Líbano", nompuesto == "Belmopán" ~ "Belice",
    nompuesto == "Belo Horizonte" ~ "Brasil", nompuesto == "Berlin Consulado" ~ "Alemania", nompuesto == "Berna - Consulado" ~ "Suiza",
    nompuesto == "Bilbao - Consulado" ~ "España", str_detect(nompuesto, "^Boston") ~ "Estados Unidos", nompuesto == "Brasilia Consulado" ~ "Brasil",
    nompuesto == "Bremen" ~ "Alemania", nompuesto == "Bridgetown" ~ "Barbados", nompuesto == "Brisbane" ~ "Australia",
    nompuesto == "Bruselas Consulado" ~ "Bélgica", nompuesto == "Bucarest" ~ "Rumania", nompuesto == "Budapest Consulado" ~ "Hungría",
    nompuesto == "Buenos Aires Consulado" ~ "Argentina", nompuesto == "Calgary Consulado" ~ "Canadá", nompuesto == "Canberra Consulado" ~ "Australia",
    nompuesto == "Cancún Consulado" ~ "México", nompuesto == "Caracas - Consulado" ~ "Venezuela", nompuesto == "Chicago - Consulado" ~ "Estados Unidos",
    nompuesto == "Ciudad de México - Consulado" ~ "México", nompuesto == "Ciudad del Cabo" ~ "Sudáfrica", nompuesto == "Columbus, Ohio" ~ "Estados Unidos",
    nompuesto == "Colón - Consulado" ~ "Panamá", nompuesto == "Copenhague Consulado" ~ "Dinamarca", nompuesto == "Cuenca" ~ "Ecuador",
    nompuesto == "Curitiba" ~ "Brasil", nompuesto == "Córdoba" ~ "Argentina", nompuesto == "Doha" ~ "Catar", nompuesto == "Dublín Consulado" ~ "Irlanda",
    nompuesto == "El Cairo - Consulado" ~ "Egipto", nompuesto == "Esmeraldas - Consulado" ~ "Ecuador", nompuesto == "Estambul Consulado" ~ "Turquía",
    nompuesto == "Estocolmo - Consulado" ~ "Suecia", nompuesto == "Florencia" ~ "Italia", nompuesto == "Florianopolis" ~ "Brasil",
    nompuesto == "Fortaleza" ~ "Brasil", nompuesto == "Foz de Iguazu" ~ "Brasil", nompuesto == "Frankfurt Consulado" ~ "Alemania",
    nompuesto == "Georgetown" ~ "Guyana", nompuesto == "Ginebra" ~ "Suiza", nompuesto == "Graz" ~ "Austria", nompuesto == "Guadalajara Consulado" ~ "México",
    nompuesto == "Guangzhou Consulado" ~ "China", str_detect(nompuesto, "^Guasdualito") ~ "Venezuela", nompuesto == "Guatemala - Consulado" ~ "Guatemala",
    nompuesto == "Guayaquil - Consulado" ~ "Ecuador", nompuesto == "Génova" ~ "Italia", nompuesto == "Hamburgo" ~ "Alemania",
    nompuesto == "Hanoi Consulado" ~ "Vietnam", nompuesto == "Hawaii" ~ "Estados Unidos", nompuesto == "Helsinki - Consulado" ~ "Finlandia",
    nompuesto == "Hong Kong - Consulado" ~ "Hong Kong", str_detect(nompuesto, "^Houston") ~ "Estados Unidos", nompuesto == "Ibiza" ~ "España",
    nompuesto == "Iquique" ~ "Chile", nompuesto == "Iquitos - Consulado" ~ "Perú", nompuesto == "Islas Caimán" ~ "Islas Caimán",
    nompuesto == "Jakarta Consulado" ~ "Indonesia", nompuesto == "Jaqué Consulado" ~ "Panamá", nompuesto == "Kansas City" ~ "Estados Unidos",
    nompuesto == "Kingston - Consulado" ~ "Jamaica", nompuesto == "Kualalumpur - Consulado" ~ "Malasia", nompuesto == "La Habana - Consulado" ~ "Cuba",
    nompuesto == "La Haya" ~ "Países Bajos", nompuesto == "La Paz Consulado" ~ "Bolivia", nompuesto == "Lanzarote" ~ "España",
    nompuesto == "Lima - Consulado" ~ "Perú", nompuesto == "Limassol - Chipre" ~ "Chipre", nompuesto == "Lisboa - Consulado" ~ "Portugal",
    nompuesto == "Logroño" ~ "España", nompuesto == "London" ~ "Canadá", nompuesto == "Londres - Consulado" ~ "Reino Unido",
    nompuesto == "Londres - Edimburgo" ~ "Reino Unido", str_detect(nompuesto, "^Los Angeles") ~ "Estados Unidos", nompuesto == "Lugano" ~ "Suiza",
    nompuesto == "Lugo" ~ "España", nompuesto == "Luxemburgo" ~ "Luxemburgo", nompuesto == "Madrid - Consulado" ~ "España",
    nompuesto == "Malta - La Valeta" ~ "Malta", nompuesto == "Managua - Consulado" ~ "Nicaragua", nompuesto == "Manaos - Consulado" ~ "Brasil",
    nompuesto == "Manila Consulado" ~ "Filipinas", nompuesto == "Manta" ~ "Ecuador", str_detect(nompuesto, "^Maracaibo") ~ "Venezuela",
    nompuesto == "Melbourne" ~ "Australia", nompuesto == "Mendoza" ~ "Argentina", str_detect(nompuesto, "^Miami") ~ "Estados Unidos",
    nompuesto == "Michigan" ~ "Estados Unidos", nompuesto == "Milán Consulado" ~ "Italia", nompuesto == "Minnesota" ~ "Estados Unidos",
    nompuesto == "Missouri" ~ "Estados Unidos", nompuesto == "Monterrey Consulado" ~ "México", nompuesto == "Montevideo - Consulado" ~ "Uruguay",
    nompuesto == "Montreal Consulado" ~ "Canadá", nompuesto == "Murcia" ~ "España", nompuesto == "Málaga" ~ "España", nompuesto == "Mérida" ~ "México",
    nompuesto == "Nagoya" ~ "Japón", nompuesto == "Nairobi - Consulado" ~ "Kenia", nompuesto == "Nantes" ~ "Francia",
    str_detect(nompuesto, "^Newark") ~ "Estados Unidos", nompuesto == "Nueva Delhi - Consulado" ~ "India", nompuesto == "Nueva Loja - Consulado" ~ "Ecuador",
    str_detect(nompuesto, "^Nueva York") ~ "Estados Unidos", nompuesto == "Nápoles" ~ "Italia", nompuesto == "Oporto" ~ "Portugal",
    nompuesto == "Oranjestad Consulado" ~ "Aruba", str_detect(nompuesto, "^Orlando") ~ "Estados Unidos", nompuesto == "Osaka" ~ "Japón",
    nompuesto == "Oslo - Consulado" ~ "Noruega", nompuesto == "Ottawa Consulado" ~ "Canadá", nompuesto == "Oviedo - Bilbao" ~ "España",
    nompuesto == "Palma de Mallorca - Consulado" ~ "España", nompuesto == "Palmas de Gran Canaria - Consulado" ~ "España",
    nompuesto == "Pamplona" ~ "España", nompuesto == "Panama - Consulado" ~ "Panamá", nompuesto == "Panama - David" ~ "Panamá",
    nompuesto == "Paris - Consulado" ~ "Francia", nompuesto == "Paris - Estrasburgo" ~ "Francia", nompuesto == "Paris - Lyon" ~ "Francia",
    nompuesto == "Paris - Montpellier" ~ "Francia", nompuesto == "Paris - Toulouse" ~ "Francia", nompuesto == "Perth" ~ "Australia",
    nompuesto == "Porto Alegre" ~ "Brasil", nompuesto == "Praga" ~ "Chequia", nompuesto == "Pretoria - Consulado" ~ "Sudáfrica",
    nompuesto == "Puerto Ayacucho" ~ "Venezuela", nompuesto == "Puerto España - Consulado" ~ "Trinidad y Tobago", nompuesto == "Puerto La Cruz" ~ "Venezuela",
    nompuesto == "Puerto Obaldía - Consulado" ~ "Panamá", nompuesto == "Puerto Ordaz" ~ "Venezuela", nompuesto == "Puerto Ordaz - Bolívar" ~ "Venezuela",
    nompuesto == "Puerto Principe - Consulado" ~ "Haití", nompuesto == "Quito - Consulado" ~ "Ecuador", nompuesto == "Rabat Consulado" ~ "Marruecos",
    nompuesto == "Ramallah - Palestina" ~ "Palestina", nompuesto == "Recife" ~ "Brasil", nompuesto == "Riad" ~ "Arabia Saudita",
    nompuesto == "Roma - Consulado" ~ "Italia", nompuesto == "Río de Janeiro - Consulado" ~ "Brasil", nompuesto == "Salt Lake City" ~ "Estados Unidos",
    str_detect(nompuesto, "^San Antonio del Táchira") ~ "Venezuela", str_detect(nompuesto, "^San Cristóbal") ~ "Venezuela",
    nompuesto == "San Fernando de Atabapo" ~ "Venezuela", str_detect(nompuesto, "^San Francisco") ~ "Estados Unidos",
    nompuesto == "San José - Consulado" ~ "Costa Rica", nompuesto == "San Juan de Puerto Rico - Consulado" ~ "Puerto Rico",
    nompuesto == "San Salvador - Consulado" ~ "El Salvador", nompuesto == "Santa Cruz de Tenerife" ~ "España",
    nompuesto == "Santa Cruz de la Sierra" ~ "Bolivia", nompuesto == "Santiago Consulado" ~ "Chile", nompuesto == "Santo Domingo - Consulado" ~ "República Dominicana",
    nompuesto == "Santo Domingo Tsachilas - Consulado" ~ "Ecuador", nompuesto == "Sao Paulo Consulado" ~ "Brasil", nompuesto == "Seul - Consulado" ~ "Corea del Sur",
    nompuesto == "Sevilla - Consulado" ~ "España", nompuesto == "Shanghai Consulado" ~ "China", nompuesto == "Singapur - Consulado" ~ "Singapur",
    nompuesto == "Stuttgart" ~ "Alemania", nompuesto == "Sydney Consulado" ~ "Australia", nompuesto == "Tabatinga Consulado" ~ "Brasil",
    nompuesto == "Tarragona" ~ "España", nompuesto == "Tegucigalpa - Consulado" ~ "Honduras", nompuesto == "Tel Aviv - Consulado" ~ "Israel",
    nompuesto == "Tokio - Consulado" ~ "Japón", nompuesto == "Toronto Consulado" ~ "Canadá", nompuesto == "Tulcán - Consulado" ~ "Ecuador",
    nompuesto == "Turín" ~ "Italia", nompuesto == "Valencia" & zona == 5 ~ "Venezuela", nompuesto == "Valencia - Consulado" ~ "España",
    nompuesto == "Valladolid" ~ "España", nompuesto == "Vancouver Consulado" ~ "Canadá", nompuesto == "Varsovia - Consulado" ~ "Polonia",
    nompuesto == "Viena Consulado" ~ "Austria", nompuesto == "Villahermosa" ~ "México", nompuesto == "Washington - Consulado" ~ "Estados Unidos",
    nompuesto == "Wellington" ~ "Nueva Zelanda", nompuesto == "Willemstad Consulado" ~ "Curazao", nompuesto == "Zaragoza" ~ "España",
    nompuesto == "Zurich" ~ "Suiza", TRUE ~ NA_character_
  ),
  iso3 = case_when(
    pais == "Ghana" ~ "GHA", pais == "Senegal" ~ "SEN", pais == "Emiratos Árabes Unidos" ~ "ARE", pais == "España" ~ "ESP",
    pais == "Países Bajos" ~ "NLD", pais == "Turquía" ~ "TUR", pais == "Chile" ~ "CHL", pais == "Argelia" ~ "DZA",
    pais == "Paraguay" ~ "PRY", pais == "Grecia" ~ "GRC", pais == "Estados Unidos" ~ "USA", pais == "Nueva Zelanda" ~ "NZL",
    pais == "Azerbaiyán" ~ "AZE", pais == "Tailandia" ~ "THA", pais == "Venezuela" ~ "VEN", pais == "China" ~ "CHN",
    pais == "Líbano" ~ "LBN", pais == "Belice" ~ "BLZ", pais == "Brasil" ~ "BRA", pais == "Alemania" ~ "DEU",
    pais == "Suiza" ~ "CHE", pais == "Barbados" ~ "BRB", pais == "Australia" ~ "AUS", pais == "Bélgica" ~ "BEL",
    pais == "Rumania" ~ "ROU", pais == "Hungría" ~ "HUN", pais == "Argentina" ~ "ARG", pais == "Canadá" ~ "CAN",
    pais == "México" ~ "MEX", pais == "Sudáfrica" ~ "ZAF", pais == "Panamá" ~ "PAN", pais == "Dinamarca" ~ "DNK",
    pais == "Ecuador" ~ "ECU", pais == "Catar" ~ "QAT", pais == "Irlanda" ~ "IRL", pais == "Egipto" ~ "EGY",
    pais == "Suecia" ~ "SWE", pais == "Guyana" ~ "GUY", pais == "Austria" ~ "AUT", pais == "Guatemala" ~ "GTM",
    pais == "Italia" ~ "ITA", pais == "Vietnam" ~ "VNM", pais == "Finlandia" ~ "FIN", pais == "Hong Kong" ~ "HKG",
    pais == "Islas Caimán" ~ "CYM", pais == "Indonesia" ~ "IDN", pais == "Jamaica" ~ "JAM", pais == "Malasia" ~ "MYS",
    pais == "Cuba" ~ "CUB", pais == "Bolivia" ~ "BOL", pais == "Perú" ~ "PER", pais == "Chipre" ~ "CYP",
    pais == "Portugal" ~ "PRT", pais == "Reino Unido" ~ "GBR", pais == "Luxemburgo" ~ "LUX", pais == "Malta" ~ "MLT",
    pais == "Nicaragua" ~ "NIC", pais == "Filipinas" ~ "PHL", pais == "Uruguay" ~ "URY", pais == "Japón" ~ "JPN",
    pais == "Kenia" ~ "KEN", pais == "Francia" ~ "FRA", pais == "India" ~ "IND", pais == "Aruba" ~ "ABW",
    pais == "Noruega" ~ "NOR", pais == "Chequia" ~ "CZE", pais == "Trinidad y Tobago" ~ "TTO", pais == "Haití" ~ "HTI",
    pais == "Marruecos" ~ "MAR", pais == "Palestina" ~ "PSE", pais == "Arabia Saudita" ~ "SAU", pais == "Costa Rica" ~ "CRI",
    pais == "Puerto Rico" ~ "PRI", pais == "El Salvador" ~ "SLV", pais == "República Dominicana" ~ "DOM", pais == "Corea del Sur" ~ "KOR",
    pais == "Singapur" ~ "SGP", pais == "Honduras" ~ "HND", pais == "Polonia" ~ "POL", pais == "Israel" ~ "ISR", pais == "Curazao" ~ "CUW", TRUE ~ NA_character_
  )
  )

base_ext_pais_2026 <- base_congreso %>% 
  filter(COD_DPTO == 88, annoh == 2026, corp %in% c("SENADO", "CAMARA")) %>% 
  mutate(corp = case_when(corp == "SENADO" ~ "Senado", corp == "CAMARA" ~ "Cámara", TRUE ~ corp)) %>% 
  left_join(dic_pais_ext %>% select(nompuesto, zona, pais, iso3), by = c("nompuesto", "zona"))

votos_pais_2026 <- base_ext_pais_2026 %>% group_by(corp, iso3, pais) %>% summarise(votos = sum(votos, na.rm = TRUE), .groups = "drop") %>% group_by(corp) %>% mutate(prop_votos_pct = 100 * votos / sum(votos)) %>% ungroup()
censo_pais_2026 <- base_ext_pais_2026 %>% distinct(iso3, pais, code_RNEC, zona, puesto, censo) %>% group_by(iso3, pais) %>% summarise(censo = sum(censo, na.rm = TRUE), .groups = "drop")
part_pais_2026 <- votos_pais_2026 %>% left_join(censo_pais_2026, by = c("iso3", "pais")) %>% mutate(participacion_pct = 100 * votos / censo)

# Unir datos con el mapa de Natural Earth para OJS y Leaflet
mundo <- ne_countries(scale = 50, type = "map_units", returnclass = "sf")
col_iso_mundo <- intersect(c("adm0_a3", "gu_a3", "iso_a3", "sov_a3"), names(mundo))[1]

mapa_mundo_sen <- mundo %>% mutate(iso3 = .data[[col_iso_mundo]]) %>% select(iso3, name) %>% inner_join(part_pais_2026 %>% filter(corp == "Senado"), by = "iso3")
mapa_mundo_cam <- mundo %>% mutate(iso3 = .data[[col_iso_mundo]]) %>% select(iso3, name) %>% inner_join(part_pais_2026 %>% filter(corp == "Cámara"), by = "iso3")
mapa_mundo_final <- bind_rows(mapa_mundo_sen, mapa_mundo_cam)

# 4. Guardar archivo final
saveRDS(list(
  part_ext_congreso = part_ext_congreso,
  votos_ext_dia = votos_ext_dia_2026,
  mapa_mundo = mapa_mundo_final,
  censo_ext = censo_ext
), "datos_exterior_part.rds")

message("Listo: datos_exterior_part.rds generados con Senado y Cámara")

# =====================================================================3
#  SECCION 03-6 PARTICIPACIÓN ESPECIALES ----
# =====================================================================3

sf_use_s2(FALSE)

quitar_huecos_sf <- function(sf_obj, area_min_hueco = 5e7) {
  crs_obj <- st_crs(sf_obj)
  quitar_huecos_geom <- function(g) {
    if (inherits(g, "MULTIPOLYGON")) {
      polys <- lapply(g, function(p) {
        if (length(p) == 0) return(p)
        exterior <- p[1]
        holes <- p[-1]
        if (length(holes) == 0) return(p)
        holes_ok <- holes[sapply(holes, function(r) {
          hole_poly <- st_polygon(list(r))
          as.numeric(st_area(st_sfc(hole_poly, crs = crs_obj))) >= area_min_hueco
        })]
        c(exterior, holes_ok)
      })
      st_multipolygon(polys)
    } else if (inherits(g, "POLYGON")) {
      if (length(g) == 0) return(g)
      exterior <- g[1]
      holes <- g[-1]
      if (length(holes) == 0) return(g)
      holes_ok <- holes[sapply(holes, function(r) {
        hole_poly <- st_polygon(list(r))
        as.numeric(st_area(st_sfc(hole_poly, crs = crs_obj))) >= area_min_hueco
      })]
      st_polygon(c(exterior, holes_ok))
    } else { g }
  }
  sf_obj %>% mutate(geometry = st_sfc(lapply(st_geometry(.), quitar_huecos_geom), crs = crs_obj)) %>% st_as_sf()
}

# 1. Censo Total Municipal y Departamental (Usando base_congreso distinct para evitar NAs)
censo_mun_total <- base_congreso %>%
  filter(annoh %in% c(2018, 2022, 2026)) %>%
  mutate(coddepto = floor(codmpio / 1000)) %>%
  distinct(annoh, codmpio, coddepto, code_RNEC, zona, puesto, censo)

censo_dep_tot <- censo_mun_total %>% group_by(annoh, coddepto) %>% summarise(censo = sum(censo, na.rm=TRUE), .groups="drop")
censo_mun_tot <- censo_mun_total %>% group_by(annoh, codmpio) %>% summarise(censo = sum(censo, na.rm=TRUE), .groups="drop")

# 2. Votos y Censo CITREP (Usando su lógica exacta)
base_citrep <- base_congreso %>%
  filter(annoh %in% c(2022, 2026), codcirc == 9, zona == 99, !is.na(CTEP), CTEP != 0)

censo_citrep_reg <- base_citrep %>% distinct(annoh, CTEP, code_RNEC, zona, puesto, censo) %>% group_by(annoh, CTEP) %>% summarise(censo = sum(censo, na.rm=TRUE), .groups="drop")
votos_citrep_reg <- base_citrep %>% group_by(annoh, CTEP) %>% summarise(votos = sum(votos, na.rm=TRUE), .groups="drop")

censo_citrep_mun <- base_citrep %>% distinct(annoh, codmpio, code_RNEC, zona, puesto, censo) %>% group_by(annoh, codmpio) %>% summarise(censo = sum(censo, na.rm=TRUE), .groups="drop")
votos_citrep_mun <- base_citrep %>% group_by(annoh, codmpio) %>% summarise(votos = sum(votos, na.rm=TRUE), .groups="drop")

# 3. Votos Afro e Indígena (Municipal y Departamental)
votos_esp_base <- base_congreso %>%
  filter(annoh %in% c(2018, 2022, 2026)) %>%
  mutate(
    coddepto = floor(codmpio / 1000),
    eleccion = case_when(
      corp == "SENADO" & codcirc == 4 ~ "Senado Indígena",
      corp == "CAMARA" & codcirc == 4 ~ "Cámara Indígena",
      corp == "CAMARA" & codcirc == 5 ~ "Cámara Afro",
      TRUE ~ NA_character_
    )
  ) %>% filter(!is.na(eleccion))

votos_esp_mun <- votos_esp_base %>% group_by(eleccion, annoh, codmpio) %>% summarise(votos = sum(votos, na.rm=TRUE), .groups="drop")
votos_esp_dep <- votos_esp_base %>% group_by(eleccion, annoh, coddepto) %>% summarise(votos = sum(votos, na.rm=TRUE), .groups="drop")

# 4. Grillas y Consolidación MUNICIPAL
mpios_validos <- todos_municipios %>% select(codmpio, Municipio, Depto, CTEP) %>% distinct()

grilla_mun_esp <- expand_grid(
  eleccion = c("Senado Indígena", "Cámara Indígena", "Cámara Afro"),
  annoh = c(2018, 2022, 2026),
  codmpio = unique(mpios_validos$codmpio)
)
part_mun_esp <- grilla_mun_esp %>%
  left_join(votos_esp_mun, by=c("eleccion","annoh","codmpio")) %>%
  left_join(censo_mun_tot, by=c("annoh","codmpio")) %>%
  mutate(votos = replace_na(votos, 0), part = if_else(!is.na(censo) & censo > 0, 100 * votos / censo, NA_real_))

grilla_mun_citrep <- expand_grid(
  eleccion = "CITREP",
  annoh = c(2022, 2026),
  codmpio = unique(mpios_validos$codmpio[!is.na(mpios_validos$CTEP) & mpios_validos$CTEP != 0])
)
part_mun_citrep <- grilla_mun_citrep %>%
  left_join(votos_citrep_mun, by=c("annoh","codmpio")) %>%
  left_join(censo_citrep_mun, by=c("annoh","codmpio")) %>%
  mutate(votos = replace_na(votos, 0), part = if_else(!is.na(censo) & censo > 0, 100 * votos / censo, NA_real_))

part_mun_all <- bind_rows(part_mun_esp, part_mun_citrep) %>%
  select(eleccion, annoh, codmpio, part) %>%
  pivot_wider(names_from = annoh, values_from = part, names_prefix = "part_")

if(!"part_2018" %in% names(part_mun_all)) part_mun_all$part_2018 <- NA_real_

part_mun_all <- part_mun_all %>%
  mutate(cambio_22 = part_2026 - part_2022, cambio_18 = if_else(eleccion == "CITREP", NA_real_, part_2026 - part_2018)) %>%
  left_join(mpios_validos %>% select(codmpio, Municipio, Depto), by="codmpio")

# 5. Grillas y Consolidación REGIONAL
deptos_validos <- map_depto_nom %>% distinct()

grilla_dep_esp <- expand_grid(
  eleccion = c("Senado Indígena", "Cámara Indígena", "Cámara Afro"),
  annoh = c(2018, 2022, 2026),
  coddepto = unique(deptos_validos$coddepto)
)
part_dep_esp <- grilla_dep_esp %>%
  left_join(votos_esp_dep, by=c("eleccion","annoh","coddepto")) %>%
  left_join(censo_dep_tot, by=c("annoh","coddepto")) %>%
  mutate(votos = replace_na(votos, 0), part = if_else(!is.na(censo) & censo > 0, 100 * votos / censo, NA_real_), id_region = coddepto) %>%
  select(eleccion, annoh, id_region, part)

grilla_reg_citrep <- expand_grid(eleccion = "CITREP", annoh = c(2022, 2026), CTEP = 1:16)
part_reg_citrep <- grilla_reg_citrep %>%
  left_join(votos_citrep_reg, by=c("annoh","CTEP")) %>%
  left_join(censo_citrep_reg, by=c("annoh","CTEP")) %>%
  mutate(votos = replace_na(votos, 0), part = if_else(!is.na(censo) & censo > 0, 100 * votos / censo, NA_real_), id_region = CTEP) %>%
  select(eleccion, annoh, id_region, part)

part_reg_all <- bind_rows(part_dep_esp, part_reg_citrep) %>%
  pivot_wider(names_from = annoh, values_from = part, names_prefix = "part_")

if(!"part_2018" %in% names(part_reg_all)) part_reg_all$part_2018 <- NA_real_

map_citrep_lbl <- tibble::tribble(
  ~CTEP, ~lbl,
  1L, "1. Cauca-Nariño-Valle", 2L, "2. Arauca", 3L, "3. Bajo Cauca", 4L, "4. Catatumbo",
  5L, "5. Caquetá-Huila", 6L, "6. Chocó", 7L, "7. Sur de Meta-Guaviare", 8L, "8. Montes de María",
  9L, "9. Pacífico Cauca-Valle", 10L, "10. Pacífico Nariño", 11L, "11. Putumayo", 12L, "12. Cesar-Guajira-Magdalena",
  13L, "13. Sur de Bolívar", 14L, "14. Sur de Córdoba", 15L, "15. Sur de Tolima", 16L, "16. Urabá"
)

part_reg_all <- part_reg_all %>%
  mutate(cambio_22 = part_2026 - part_2022, cambio_18 = if_else(eleccion == "CITREP", NA_real_, part_2026 - part_2018)) %>%
  left_join(deptos_validos %>% rename(id_region = coddepto, nombre_region = Depto) %>% mutate(is_citrep=FALSE), by="id_region") %>%
  left_join(map_citrep_lbl %>% rename(id_region_citrep = CTEP, nombre_citrep = lbl), by=c("id_region"="id_region_citrep")) %>%
  mutate(nombre_region = if_else(eleccion == "CITREP", nombre_citrep, nombre_region)) %>%
  select(-nombre_citrep, -is_citrep) %>% filter(!is.na(nombre_region))

# 6. Geometría CITREP
sf_citrep <- readRDS(file.path(ruta_general, "shapes", "muni_simpl_sanandres_cache.rds"))$sf_obj %>%
  mutate(codmpio = as.integer(codmpio)) %>%
  left_join(mpios_validos %>% select(codmpio, CTEP), by = "codmpio") %>%
  filter(!is.na(CTEP), CTEP != 0) %>%
  group_by(CTEP) %>% summarise(do_union = TRUE, .groups = "drop") %>%
  st_make_valid() %>% st_collection_extract("POLYGON") %>% st_cast("MULTIPOLYGON") %>%
  quitar_huecos_sf(area_min_hueco = 5e7)

sf_use_s2(TRUE)

saveRDS(list(regional = part_reg_all, municipal = part_mun_all, sf_citrep = sf_citrep), "datos_especiales_part.rds")
message("Listo: datos_especiales_part.rds generados con éxito")

# Geometrías a geojson para el mapa interactivo en ojs (no consumen memoria al render)
st_write(readRDS(file.path(ruta_general,"shapes","muni_simpl_sanandres_cache.rds"))$sf_obj %>%
           mutate(codmpio = as.integer(codmpio)) %>% select(codmpio) %>% st_transform(4326),
         "geo_mpios.geojson", delete_dsn = TRUE)
st_write(readRDS(file.path(ruta_general,"shapes","departamento_simpl_sanandres_cache.rds"))$sf_obj %>%
           mutate(coddepto = as.integer(cod_depto)) %>% select(coddepto) %>% st_transform(4326),
         "geo_deptos.geojson", delete_dsn = TRUE)
st_write(sf_citrep %>% select(CTEP) %>% st_transform(4326), "geo_citrep.geojson", delete_dsn = TRUE)

# =====================================================================3
#  SECCION 03-7 BRECHA CITREP Y PARTICIPACIÓN RURAL/URBANA ----
# =====================================================================3

# --- 1. BRECHA CÁMARA RURAL VS CITREP (2026) ---

# Censo Rural (zona 99) 2026
censo_rb_mun_26 <- censo_divipol_hist  %>% 
  filter(annoh == 2026, zona == 99) %>%
  group_by(codmpio) %>%
  summarise(censo = sum(censo, na.rm = TRUE), .groups = "drop") %>%
  left_join(todos_municipios %>% select(codmpio, CTEP) %>% distinct(), by = "codmpio")

# Votos Cámara Rural y CITREP 2026
votos_cam_citrep <- base_congreso %>% filter(annoh == 2026, corp == "CAMARA", codcirc %in% c(1,4,5), zona == 99) %>% group_by(codmpio) %>% summarise(votos_cam = sum(votos, na.rm=TRUE), .groups="drop")
votos_citrep_solo <- base_congreso %>% filter(annoh == 2026, codcirc == 9, zona == 99) %>% group_by(codmpio) %>% summarise(votos_citrep = sum(votos, na.rm=TRUE), .groups="drop")

# Consolidado de Diferencias a nivel Municipal
diff_citrep_camara <- censo_rb_mun_26 %>% 
  filter(!is.na(CTEP), CTEP != 0) %>%
  left_join(votos_cam_citrep, by="codmpio") %>%
  left_join(votos_citrep_solo, by="codmpio") %>%
  mutate(
    votos_cam = replace_na(votos_cam, 0),
    votos_citrep = replace_na(votos_citrep, 0),
    part_cam = if_else(censo > 0, 100 * votos_cam / censo, NA_real_),
    part_citrep = if_else(censo > 0, 100 * votos_citrep / censo, NA_real_),
    diff_pp = part_cam - part_citrep
  ) %>%
  left_join(todos_municipios %>% select(codmpio, Municipio, Depto) %>% distinct(), by="codmpio") %>%
  filter(!is.na(diff_pp)) %>%
  arrange(desc(diff_pp))

# KPIs (Sumatorias absolutas para la franja superior)
kpi_citrep <- sum(diff_citrep_camara$votos_citrep, na.rm=T) / sum(diff_citrep_camara$censo, na.rm=T) * 100
kpi_cam_en_citrep <- sum(diff_citrep_camara$votos_cam, na.rm=T) / sum(diff_citrep_camara$censo, na.rm=T) * 100

censo_no_citrep <- censo_rb_mun_26 %>% filter(is.na(CTEP) | CTEP == 0)
votos_cam_no_citrep <- votos_cam_citrep %>% filter(codmpio %in% censo_no_citrep$codmpio)
kpi_cam_no_citrep <- sum(votos_cam_no_citrep$votos_cam, na.rm=T) / sum(censo_no_citrep$censo, na.rm=T) * 100

kpis_brecha <- tibble(
  metrica = c("CITREP", "Cámara rural en municipios CITREP", "Cámara rural en el resto del país"),
  valor = c(kpi_citrep, kpi_cam_en_citrep, kpi_cam_no_citrep)
)


# --- 2. PARTICIPACIÓN RURAL Y URBANA HISTÓRICA ---

# Censo por zona (Rural = 99, Urbano = otros)
censo_rb_hist <- censo_divipol_hist  %>%
  mutate(zona_rb = if_else(zona == 99, "Rural", "Urbano")) %>%
  group_by(annoh, zona_rb) %>%
  summarise(censo = sum(censo, na.rm=TRUE), .groups="drop")

censo_citrep_hist <- censo_citrep_mun %>%
  mutate(zona_rb = "Rural") %>%
  group_by(annoh, zona_rb) %>%
  summarise(censo = sum(censo, na.rm=TRUE), .groups="drop")

# Votos por zona
votos_rb_nac <- base_congreso %>%
  filter(annoh %in% c(2018, 2022, 2026)) %>%
  mutate(
    zona_rb = if_else(zona == 99, "Rural", "Urbano"),
    Eleccion = case_when(
      corp == "SENADO" & codcirc %in% c(0,4) ~ "Senado",
      corp == "CAMARA" & codcirc %in% c(1,4,5) ~ "Cámara",
      codcirc == 9 & zona == 99 & CTEP != 0 ~ "CITREP",
      TRUE ~ NA_character_
    )
  ) %>% filter(!is.na(Eleccion)) %>%
  group_by(annoh, Eleccion, zona_rb) %>%
  summarise(votos = sum(votos, na.rm=TRUE), .groups="drop")

# Consolidado
part_rb_nac <- votos_rb_nac %>%
  left_join(
    bind_rows(
      censo_rb_hist %>% mutate(Eleccion = "Senado"),
      censo_rb_hist %>% mutate(Eleccion = "Cámara"),
      censo_citrep_hist %>% mutate(Eleccion = "CITREP")
    ), by = c("annoh", "Eleccion", "zona_rb")
  ) %>%
  mutate(part = if_else(censo > 0, 100 * votos / censo, NA_real_)) %>%
  arrange(annoh, Eleccion, zona_rb)

part_rb_nac_wide <- part_rb_nac %>%
  select(annoh, Eleccion, zona_rb, part) %>%
  pivot_wider(names_from = zona_rb, values_from = part) %>%
  filter(!is.na(Rural) & !is.na(Urbano))

# Guardar todo
saveRDS(list(
  kpis_brecha = kpis_brecha,
  diff_citrep_camara = diff_citrep_camara,
  part_rb_nac = part_rb_nac,
  part_rb_nac_wide = part_rb_nac_wide
), "datos_rural_urbano.rds")

message("Listo: datos_rural_urbano.rds generados con éxito")

# =====================================================================3
#  SECCION 03-8 IMPACTO NUEVOS PUESTOS RURALES ----
# =====================================================================3

# 1. Leer puestos nuevos desde Excel
puestos_nuevos_26 <- read_excel("../Inscripción de cédulas/Match codigos puestos 2023 y 2026.xlsx") %>%
  mutate(id_puesto = str_replace(codpuesto26, "^(\\d{2})-(\\d{3})-(\\d{2})-(\\d{2})$", "\\1\\2-\\3-\\4")) %>%
  filter(flag_nuevo_26 == TRUE) %>%
  select(id_puesto) %>%
  distinct()

# 2. Filtrar base rural y mapear a códigos DANE para evitar NAs
base_rural <- base_congreso %>%
  filter(annoh %in% c(2022, 2026), corp %in% c("SENADO", "CAMARA"), zona == 99) %>%
  filter(!is.na(codmpio)) %>%
  mutate(
    id_puesto = paste0(
      str_pad(as.character(code_RNEC), 5, pad = "0"), "-",
      str_pad(as.character(zona), 2, pad = "0"), "-",
      str_pad(as.character(puesto), 2, pad = "0")
    ),
    grupo = case_when(
      corp == "SENADO" & codcirc %in% c(0, 4) ~ "Senado",
      corp == "CAMARA" & codcirc %in% c(1, 4, 5) ~ "Cámara",
      corp == "CAMARA" & codcirc == 9 & CTEP != 0 ~ "CITREP",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(grupo))

# 3. Calcular el Neto de Nuevos Puestos Rurales
puestos_rural_22 <- base_rural %>% filter(annoh == 2022) %>% distinct(id_puesto) %>% nrow()
puestos_rural_26 <- base_rural %>% filter(annoh == 2026) %>% distinct(id_puesto) %>% nrow()
neto_nuevos_rurales <- puestos_rural_26 - puestos_rural_22

# 4. Cálculo de Censo y Votos
censo_rural_sc <- base_rural %>% filter(grupo %in% c("Senado", "Cámara")) %>% distinct(annoh, codmpio, id_puesto, censo)
censo_rural_citrep <- base_rural %>% filter(grupo == "CITREP") %>% distinct(annoh, codmpio, id_puesto, censo)

part_sc <- base_rural %>%
  filter(grupo %in% c("Senado", "Cámara")) %>%
  group_by(annoh, grupo, codmpio) %>%
  summarise(votos = sum(votos, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    censo_rural_sc %>% group_by(annoh, codmpio) %>% summarise(censo = sum(censo, na.rm = TRUE), .groups = "drop"),
    by = c("annoh", "codmpio")
  ) %>% mutate(part_pct = 100 * votos / censo)

part_citrep <- base_rural %>%
  filter(grupo == "CITREP") %>%
  group_by(annoh, grupo, codmpio) %>%
  summarise(votos = sum(votos, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    censo_rural_citrep %>% group_by(annoh, codmpio) %>% summarise(censo = sum(censo, na.rm = TRUE), .groups = "drop"),
    by = c("annoh", "codmpio")
  ) %>% mutate(part_pct = 100 * votos / censo)

# 5. Deltas y Cruce con Puestos Nuevos
part_mun_rural <- bind_rows(part_sc, part_citrep)

delta_part_rural <- part_mun_rural %>%
  select(annoh, grupo, codmpio, part_pct) %>%
  pivot_wider(names_from = annoh, values_from = part_pct, names_prefix = "part_") %>%
  mutate(delta_part_pp = part_2026 - part_2022) %>%
  select(grupo, codmpio, delta_part_pp)

puestos_nuevos_mun <- base_rural %>%
  filter(annoh == 2026) %>%
  distinct(codmpio, id_puesto) %>%
  semi_join(puestos_nuevos_26, by = "id_puesto") %>%
  count(codmpio, name = "puestos_nuevos")

df_plot_puestos <- delta_part_rural %>%
  left_join(puestos_nuevos_mun, by = "codmpio") %>%
  mutate(
    puestos_nuevos = replace_na(puestos_nuevos, 0L),
    bin_puestos = case_when(
      puestos_nuevos == 0 ~ "0",
      puestos_nuevos == 1 ~ "1",
      puestos_nuevos == 2 ~ "2",
      puestos_nuevos >= 3 ~ "3 o más",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(bin_puestos), !is.na(delta_part_pp)) %>%
  group_by(grupo, bin_puestos) %>%
  summarise(
    delta_part_pp = mean(delta_part_pp, na.rm = TRUE),
    n_mun = n(),
    .groups = "drop"
  )

# 6. Guardar Resultados
saveRDS(list(
  df_plot = df_plot_puestos,
  neto_nuevos = neto_nuevos_rurales
), "datos_impacto_puestos.rds")

message("Listo: datos_impacto_puestos.rds generados con éxito")