# Prueba con OpenCage ----------------------------------------------------------
# install.packages("dplyr")
# install.packages("opencage")
# install.packages("leaflet")

library(dplyr)
library(opencage)
library(leaflet)

## Cargar datos y preparar la base para extraer coordenadas ----------------

enviados  <- rio::import("Input/BBDD_Ref_enviadas v1.xlsx")  %>% clean_names()

recibidos <- rio::import("Input/BBDD_Ref_recibidas v1.xlsx") %>% clean_names() %>%
  filter(., anio %in% c("2014", "2019", "2023"))


# Esta tabla de conteos es la manera en que se me ocurrio aislar los nombres de los hospitales en R
bdhosp1 <- enviados %>%
  select(hospitales = eess_emisor) %>%
  group_by(hospitales) %>%
  summarise(Frecuencia = n()) %>%
  na.omit()

bdhosp2 <- enviados %>%
  select(hospitales = transferido_a_recodif) %>%
  group_by(hospitales) %>%
  summarise(Frecuencia = n()) %>%
  na.omit()

bdhosp3 <- recibidos %>%
  select(hospitales = eess_receptor) %>%
  group_by(hospitales) %>%
  summarise(Frecuencia = n()) %>%
  na.omit()

bdhosp4 <- recibidos %>%
  select(hospitales = referencia_de) %>%
  group_by(hospitales) %>%
  summarise(Frecuencia = n()) %>%
  na.omit()


# Unir conteos en una BBDD
bd_unida <- bind_rows(bdhosp1, bdhosp2, bdhosp3, bdhosp4) %>%
  distinct(hospitales)

# Eliminar las bases redundantes
rm(list = setdiff(ls(), c("bd_unida")))

# Ajustar
bd_unida <- bd_unida %>% 
  mutate(Dir_completa = paste(hospitales, ", Bolivia"))


## Prueba OpenCage ---------------------------------------------------------

# API key propia, utilizar la que obtuvimos de OpenCage
Sys.setenv(OPENCAGE_KEY = "23032e9435c24a7595646aba8aa64361")

# Georeferenciar con la API
bd_unida <- bd_unida %>% oc_forward_df(placename = Dir_completa)

leaflet(na.omit(bd_unida)) %>% 
  addTiles() %>%
  addMarkers(lat = ~oc_lat, lng = ~oc_lng, label = ~hospitales)




