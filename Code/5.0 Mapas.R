# Georeferenciacion


## Cargar datos y preparar la base para extraer coordenadas ----------------

coordenadas <- georeferenciacion_google

leaflet(na.omit(coordenadas)) %>% 
  addTiles() %>%
  addMarkers(lat = ~lat, lng = ~lon, label = ~centro)

# Esto es de la API de google maps tambien
mapa_paz <- get_googlemap(center = "Cochabamba", zoom = 7)

mapa_ggmap <- ggmap(mapa_paz) +
  geom_point(data = coordenadas,
             aes(x = lon, y = lat),
             size = 3,
             alpha = .8,
             color = "brown") +
  labs(title = "Centros de salud de la Paz")
