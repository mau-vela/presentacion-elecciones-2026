# =====================================================================
# 00_preparar_datos.R   —   Preparacion de datos del informe
# Rutas RELATIVAS a: ...\Presentacion elecciones 2026
# =====================================================================
library(dplyr); library(tidyr); library(stringr); library(readr)
library(readxl); library(sf); library(rstudioapi)
library(rnaturalearth)   # mapa mundial (install.packages c("rnaturalearth","rnaturalearthdata"))
Sys.setlocale("LC_ALL", "en_US.UTF-8")

current_path <- getActiveDocumentContext()$path
setwd(dirname(dirname(current_path)))

# ---- RUTAS (AJUSTA) ------------------------------------------------
ruta_general       <- "../datos generales"
ruta_participacion <- "../Participación electoral"
ruta_basicos_cong  <- "../Congreso 2026/datos/PRECONTEO/DIA_ELECTORAL_08_MARZO/ArchivosBasicos_Dia electoral_congreso2026"
ruta_basicos_pres  <- "../Presidencia 2026/preconteo segunda vuelta/datos/archivos básicos"

load(file.path(ruta_general, "municipios.rda"))   # 'todos_municipios'
map_rnec_codmpio <- todos_municipios %>% distinct(code_RNEC, codmpio, Depto, Municipio)
map_depto_nom    <- todos_municipios %>% mutate(coddepto = floor(codmpio/1000)) %>% distinct(coddepto, Depto)

# =====================================================================
#  SECCION 01 — DIVIPOLE
# =====================================================================
fw   <- function(x, a, b) str_sub(x, a, b)
num0 <- function(x) suppressWarnings(as.numeric(str_trim(x)))
chr0 <- function(x) str_squish(x)

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
corregir_censo_exterior <- function(df) {
  df %>% mutate(
    art = COD_DPTO == 88 & as.character(puesto) %in% as.character(81:86),
    potencial_hombres = ifelse(art, NA_real_, potencial_hombres),
    potencial_mujeres = ifelse(art, NA_real_, potencial_mujeres),
    censo             = ifelse(art, NA_real_, censo)
  ) %>% select(-art)
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

# ---- 2026 ----------------------------------------------------------
divipol_cong_26 <- leer_divipol(ruta_basicos_cong) %>% corregir_censo_exterior()
divipol_pres_26 <- leer_divipol(ruta_basicos_pres) %>% corregir_censo_exterior()

# ---- HISTORICO 2018 / 2022 -----------------------------------------
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

# ---- (A) Resumen Total / Nacional / Exterior por eleccion y anio ----
metricas <- function(df) {
  df %>% summarise(
    n_puestos = n_distinct(paste(code_RNEC, zona, puesto)),
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

# ---- (B) Municipio y departamento 2026 (mapa nacional) -------------
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

# ---- (C) Exterior por pais 2026 (tu diccionario) -> mapa mundial ----
dic_pais_ext <- divipol_cong_26 %>% filter(COD_DPTO == 88) %>% distinct(nompuesto, zona) %>%
  mutate(pais = case_when(
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
    nompuesto=="Logroño"~"España", nompuesto=="London"~"Reino Unido", nompuesto=="Londres - Consulado"~"Reino Unido",
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

paises_iso <- c("Emiratos Árabes Unidos"="ARE","España"="ESP","Países Bajos"="NLD","Turquía"="TUR","Chile"="CHL",
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

# ---- (E) Geometrias DENTRO del rds (proyecto autosuficiente) --------
# Solo se guarda la geometria + la llave; los datos se unen en el .qmd.
sf_mpios_geom  <- readRDS(file.path(ruta_general, "shapes", "muni_simpl_sanandres_cache.rds"))$sf_obj %>%
  mutate(codmpio = as.integer(codmpio)) %>% select(codmpio)
sf_deptos_geom <- readRDS(file.path(ruta_general, "shapes", "departamento_simpl_sanandres_cache.rds"))$sf_obj %>%
  mutate(coddepto = as.integer(cod_depto)) %>% select(coddepto)

# ---- GUARDAR -------------------------------------------------------
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