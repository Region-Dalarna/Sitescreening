# libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf, sp, httr, mapview, leaflet, readxl, keyring)

source("G:/skript/func/func_GIS.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_postgis.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)

# För att komma förbi proxyn
set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, 
                     username = key_list(service = "auth")$username, password = key_get("auth", key_list(service = "auth")$username)))
set_config(config(ssl_verifypeer = 0L))

# avoid scientific notation
options(scipen=999)

#hämta data

#Här hämtas kommun-polygoner från RUF:s geodatabas
##ersätt med din region/kommun (polygoner)

con <- dbConnect(          # use in other settings
  RPostgres::Postgres(),   # funkar ej när jag tar bort denna raden, tänker att den inte behövs eftersom paketet laddas av pacman.
  # without the previous and next lines, some functions fail with bigint data 
  #   so change int64 to integer
  bigint = "integer",  
  user = key_list(service = "postgres")$username,     #användare sparat i lösenordshanterare Keyring
  password = key_get("postgres", key_list(service = "postgres")$username),   #lösenord sparat i lösenordshanterare
  host = "WFALMITVS526.ltdalarna.se",
  port = 5432,
  dbname = "geodata",
  options="-c search_path=public")

kommun <- st_read(con, query = "SELECT * FROM karta.kommun_scb") %>% 
  filter(lanskod_tx == 20) %>%    ##ändra länskod_tx till din regionkod
  select("kom_kod" = "knkod",
         "kommun" = "knnamn",
         "area" = "landareakm",
         "befolkning" = "knbef96")

#multikriterieanalys resultat mer än 500 p
vectoriserad_fil <- "G:/skript/henrik/GitHub/Region-Atlas/fran_philip/Vectorized_Dalarna_meran500.gpkg"
vectoriserad <- st_read(vectoriserad_fil, crs = 3006)

  #vägar
#### NVDB från lokal drive G: (hämta från geodatabas i framtid)
nvdb <- "G:/Samhällsanalys/GIS/Grundkartor/nvdb/"
fil_nvdb <- "fixat_nvdb_riket_dalarna.gpkg"  # oklart vad som är fixat, men filen väger mindre än nvdb_riket
sokvag_nvdb_sv <- paste0(nvdb, fil_nvdb)
nvdb_sv <- st_read(sokvag_nvdb_sv)  

nvdb_dalarna <- nvdb_sv %>%         
  select("id" = "id",                                  #och rename
         "antal korfalt" = "Antal_korfalt2_Korfaltsantal", 
         "barighet" = "Barighet_Barighetsklass",
         "vagklass" = "FunkVagklass_Klass", 
         "gatunamn" = "Gatunamn_Namn",
         "hastighet_f" = "Hastighetsgrans_HogstaTillatnaHastighet_F",
         "hastighet_b" = "Hastighetsgrans_HogstaTillatnaHastighet_B",
         "Rekomenderad vag for farligt gods" = "RekomVagFarligtGods_Rekommendation",
         "ars dygns trafik" = "Trafik_ADT_fordon",
         "vag bredd" = "Vagbredd_Bredd", 
         "vaghallare" = "Vaghallare_Vaghallartyp", 
         "vagnr_europavag" = "Vagnummer_Europavag",
         "vagnummer" = "Vagnummer_Huvudnummer_Vard") 

nvdb_dalarna <- nvdb_dalarna %>% 
  mutate(vagnr_europavag = replace(vagnr_europavag, vagnr_europavag == -1, "E"))

#järnväg
jarnvag_fil <- "G:/skript/henrik/GitHub/Region-Atlas/QGIS/ralstrafik.gpkg"
jarnvag <- st_read(jarnvag_fil, crs = 3006)

#utvalda fastigheter över 1500 p
utvalda_fastigheter_fil <- "G:/skript/henrik/GitHub/Region-Atlas/fran_philip/Extracted_location.gpkg"
utvalda_fastigheter <- st_read(utvalda_fastigheter_fil, crs = 3006)

#naturvård
naturvard_fil <- "G:/skript/henrik/GitHub/Region-Atlas/fran_philip/naturvardsomr.gpkg"
naturvard <- st_read(naturvard_fil, crs = 3006)

#industri
industriomr_fil <- "G:/skript/henrik/GitHub/Region-Atlas/fran_philip/industriomr.gpkg"
industriomr <- st_read(industriomr_fil, crs = 3006)

#Elinfrastruktur
transformatoromr_fil <- "G:/skript/henrik/GitHub/Region-Atlas/fran_philip/transformatoromr.gpkg"
transformatoromr <- st_read(transformatoromr_fil, crs = 3006) %>% 
  st_buffer(100)

elledningar_fil <- "G:/Samhällsanalys/GIS/projekt/Invest in Dalarna/utdata/elledningar/ledningar_ellevio.gpkg" #G:\Samhällsanalys\GIS\projekt\Invest in Dalarna\utdata\elledningar
elledningar <- st_read(elledningar_fil, crs = 3006)


#3 speciella transformatorstationer
repbacken_etc_fil <- "G:/Samhällsanalys/GIS/projekt/Invest in Dalarna/utdata/Repbäcken_etc.gpkg"
transformator_vip <- st_read(repbacken_etc_fil, crs = 3006) %>%
  st_buffer(1000)

#flygplatser

flygplats_fil <- "G:/Samhällsanalys/GIS/projekt/Invest in Dalarna/utdata/Flygplats.gpkg"

flygplats_1 <- st_read(flygplats_fil, crs = 3006) %>%
  mutate(rep(c("Dalaflyget", "Dalaflyget", "Scandinavian Mountains AB"))) %>%
  mutate(rep(c("Dala Airport", "Mora/Siljan Flygplats", "Sälen/Trysil Airport")))

#glimpse(flygplats_1)
flygplats <- flygplats_1 %>%
  rename(operator = `rep(c("Dalaflyget", "Dalaflyget", "Scandinavian Mountains AB"))`,
         namn = `rep(c("Dala Airport", "Mora/Siljan Flygplats", "Sälen/Trysil Airport"))`)

# Stoplager
# bostader
bostad_buffert_fil <- "G:/Samhällsanalys/GIS/projekt/Invest in Dalarna/utdata/till_rmd/bostad_buff_simp.gpkg"
bostader <- st_read(bostad_buffert_fil, crs = 3006)

#Vatten
vatten_dalarna_fil <- "G:/Samhällsanalys/GIS/projekt/Invest in Dalarna/utdata/till_rmd/vatten_buff_simp.gpkg"
vatten <- st_read(vatten_dalarna_fil, crs = 3006)

#Jordbruksmark
jordbruk_fil <- "G:/Samhällsanalys/GIS/projekt/Invest in Dalarna/utdata/till_rmd/jord_buff_simp.gpkg"
jordbruk <- st_read(jordbruk_fil, crs = 3006)

#kartan

#mapview(vectoriserad)

skane = colorRampPalette(c("lightyellow", 'yellow', "orange", "red", 'darkred'))

#mapview(vectoriserad, homebutton = FALSE, alpha.regions = 0.6, lwd = 0.0, col.regions = skane)

greys <- colorRampPalette(c("grey50", "grey87"))

greens <- colorRampPalette(c("green1", "green3", ("green4")))


mapview(kommun, alpha = TRUE, legend = FALSE, alpha.regions = 0.0, label = "kommun", hide = TRUE, homebutton = FALSE)+
  mapview(jarnvag, color = "grey29", lwd = 2, legend = FALSE, label = "straknamn", homebutton = FALSE, hide = TRUE)+
  mapview(vectoriserad, layer.name = "Sitescreening", homebutton = FALSE, alpha.regions = 0.5, lwd = 0.0, col.regions = skane)+ # ändra färg
  mapview(utvalda_fastigheter, label = "fastighet", legend = FALSE, homebutton = FALSE, alpha.regions = 0.5, col.regions = c("purple"), hide = TRUE)+
  mapview(transformatoromr, label = "objekttyp", homebutton = FALSE, col.regions = c("green"), legend = FALSE, hide = TRUE)+#transformatoromr
  mapview(industriomr, label = "objekttyp", homebutton = FALSE, alpha.regions = 0.5, col.regions = c("darkred"), legend = FALSE, hide = TRUE)+#industriomr
  mapview(naturvard, label = "objekttyp", homebutton = FALSE, alpha.regions = 0.5, col.regions = c("darkgreen"), legend = FALSE, hide = TRUE)+##naturvard
  mapview(elledningar, color = greens, zcol = "objekttyp", lwd = 2, legend = FALSE, label = "objekttyp", homebutton = FALSE, hide = TRUE)+
  mapview(nvdb_dalarna, zcol = "vagnummer", color = greys, label = "vagnummer", lwd = 2, alpha = 0.5, legend = FALSE, homebutton = FALSE, hide = TRUE, layer.name = "vagar")+
  mapview(flygplats, col.regions = "red", color = "white", cex = 6, legend = FALSE, label = "namn", homebutton = TRUE, hide = TRUE)+
  mapview(transformator_vip, col.regions = "green", color = "white", cex = 6, legend = FALSE, label = "objekttyp", homebutton = TRUE, hide = TRUE)+
  mapview(vatten, label = "NAMN1", col.regions = "dodgerblue4", alpha.regions = 0.9, homebutton = FALSE, hide = TRUE, legend = FALSE)+
  mapview(jordbruk, label = "objekttyp", col.regions = "chocolate4", alpha.regions = 0.9, homebutton = FALSE, hide = TRUE, legend = FALSE)+
  mapview(bostader, label = "objekttyp", col.regions = "black", alpha.regions = 0.9, homebutton = FALSE, hide = TRUE, legend = FALSE)



