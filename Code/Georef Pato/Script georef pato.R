# Prueba georeferenciacion

library(dplyr) #manipulación de datos
library(ggplot2) #gráficos
library(sf) #elementos espaciales
library(rnaturalearth) #mapas del mundo
library(osmdata) #datos open street map


# Descargar mapa de Bolivia ----
mapa_1 <- rnaturalearth::ne_states(country = "Bolivia") |>
  select(name, name_es, iso_3166_2, code_hasc, geometry)

# Filtrar solo La Paz
mapa_2 <- mapa_1 |> 
  filter(name == "La Paz")

# obtener perímetro o área abarcada por el mapa
caja <- mapa_2 |> st_bbox()


# Obtener datos de OSM ----
calles_grandes <- caja |> 
  opq(timeout = 500) |> 
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link")) |> 
  osmdata_sf()

calles_medianas <- caja |> 
  opq(timeout = 500) |> 
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) |> 
  osmdata_sf()

centros_salud <- caja |> 
  opq(timeout = 500) |> 
  add_osm_feature(key = "amenity", 
                  value = c("hospital")) |> 
  osmdata_sf()

distritos <- caja |>
  opq(timeout = 500) |>
  add_osm_feature(key = "place",
                  value = c("district")) |> 
  osmdata_sf()

comunas <- caja |>
  opq(timeout = 500) |>
  add_osm_feature(key = "place",
                  value = c("county")) |> 
  osmdata_sf()

# Extraer lineas de calles ----
# extraer toda la geometría
calles_grandes_a <- calles_grandes$osm_lines |>
  st_intersection(mapa_2)

calles_medianas_a <- calles_medianas$osm_lines |>
  st_intersection(mapa_2)

centros_salud_a <- centros_salud$osm_points |>
  st_intersection(mapa_2)

distritos_a <- distritos$osm_points |>
  st_intersection(mapa_2)

comunas_a <- comunas$osm_points |>
  st_intersection(mapa_2)


# Graficar mapa con calles ----
LaPaz <- ggplot() +
  # capa del mapa de la región
  geom_sf(data = mapa_2,
          linewidth = .4, 
          color = "grey80", fill = "grey95") +
  # capas osm
  geom_sf(data = calles_medianas_a, 
          linewidth = .3, alpha = .3, colour = "#7aafff") +
  geom_sf(data = calles_grandes_a, 
          linewidth = .4, alpha = .4, colour = "#0f6af2") +
  geom_sf(data = centros_salud_a, 
          linewidth = .4, alpha = .5, colour = "red") +
  geom_sf(data = comunas_a, 
          linewidth = .4, alpha = .4, colour = "blue") +
  theme_classic()

LaPaz

stop()

# Aqui quise probar una forma de revisar que centros hay, para que se
# correspondan con los datos que tenemos, pero no alcance a ir mas alla
centros <- centros_salud_a$name |> data.frame() |> na.omit()

centros
