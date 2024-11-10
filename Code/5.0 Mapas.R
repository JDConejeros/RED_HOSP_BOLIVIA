# Georeferenciacion


## Cargar datos y preparar la base para extraer coordenadas ----------------

enviados  <- rio::import("Input/BBDD_Ref_enviadas v1.xlsx")  %>% clean_names()

recibidos <- rio::import("Input/BBDD_Ref_recibidas v1.xlsx") %>% clean_names()

leaflet(na.omit(coordenadas)) %>% 
  addTiles() %>%
  addMarkers(lat = ~oc_lat, lng = ~oc_lng, label = ~centro)



