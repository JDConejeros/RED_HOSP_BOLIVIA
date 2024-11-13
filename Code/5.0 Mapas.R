# Georeferenciacion ----------

source("Code/1.0 Settings.R")
ggmap::register_google(key="AIzaSyBb9VrHdo8iOvw5KuwGuWbiFLkqR7pYD6w")
#ggmap::register_stadiamaps(key="b8908535-0fbc-44be-a3f0-5bb5ee5c2890")


## Descargamos los datos  ---------------------------------------------------------------------

coordenadas  <- rio::import("Output/bases/Georeferenciacion_Hospitales.rds")
glimpse(coordenadas)

coordenadas <- coordenadas %>% 
  mutate(
    lon = if_else(centro == "Hospital El Alto Norte", -68.2043425480467, lon),  # Nueva longitud
    lat = if_else(centro == "Hospital El Alto Norte", -16.48949915, lat)   # Nueva latitud
  )

enviados  <- rio::import("Output/bases/BBDD_ref_enviadas.rds")  
recibidos  <- rio::import("Output/bases/BBDD_ref_recibidas.rds")  

## Ajustamos las tablas ---------------------------------------------------------------------

tab1 <- enviados %>% group_by(anio, hosp=eess_emisor) %>% summarise(n=n()) %>% select(-n)
tab2 <- recibidos %>% group_by(anio, hosp=eess_receptor) %>% summarise(n=n()) %>% select(-n)

tab3 <- enviados %>% group_by(hosp=transferido_a, nivel=transferido_a_nivel) %>% summarise(n=n()) %>% select(-n)
tab4 <- recibidos %>% group_by(hosp=recibido_de, nivel=recibido_de_nivel) %>% summarise(n=n()) %>% select(-n)

niveles <- tab3 %>% bind_rows(tab4) %>% distinct(hosp, nivel, .keep_all = TRUE) %>% filter(nivel!="T. Nuclear")

hosp <- tab1 %>% bind_rows(tab2) %>% ungroup() %>% distinct(anio, hosp, .keep_all = TRUE) %>% arrange(anio) 

# hosp <- c("Hospital Municipal Boliviano Coreano",  
#           "Hospital Municipal Boliviano Holandes",
#           "Hospital Municipal Los Andes",
#           "Hospital El Alto Norte",
#           "Hospital El Alto Sur", 
#           "Hospital de la Mujer", 
#           "Hospital de Clinicas",
#           "Hospital del Niño")

coordenadas_2014 <- coordenadas %>%
  left_join(filter(hosp, anio==2014), by=c("centro"="hosp")) %>% 
  mutate(label=if_else(anio==2014, centro, "")) %>% 
  drop_na(lon, lat) %>% 
  left_join(niveles, by=c("centro"="hosp")) %>% 
  drop_na(nivel)
# %>% 
#   distinct(lon, lat, .keep_all=TRUE)


coordenadas_2019 <- coordenadas %>% 
  left_join(filter(hosp, anio==2019), by=c("centro"="hosp")) %>% 
  mutate(label=if_else(anio==2019, centro, "")) %>% 
  drop_na(lon, lat) %>% 
  left_join(niveles, by=c("centro"="hosp")) %>% 
  drop_na(nivel)

coordenadas_2023 <- coordenadas %>% 
  left_join(filter(hosp, anio==2023), by=c("centro"="hosp")) %>% 
  mutate(label=if_else(anio==2023, centro, "")) %>% 
  drop_na(lon, lat) %>% 
  left_join(niveles, by=c("centro"="hosp")) %>% 
  drop_na(nivel) 

  
## Cargamos el shape de Bolivia ----------------

# Esto es de la API de google maps tambien
mapa_paz <- get_googlemap(center = c(long= -68.15, lat = -16.55), # "la paz, bolivia", #cochabamba
                          maptype='roadmap', 
                          size = c(1000, 1000),
                          zoom = 11,
                          scale = 2,
                          #markers=FALSE,
                          language = "es-ES")


ggmap(mapa_paz) 

## Generamos las visualizaciones ----------------

### 2014 -----

coordenadas_2014 <- coordenadas_2014 %>%
  mutate(alpha = if_else(nivel=="1er NIVEL", 0.5, 
                         if_else(nivel=="2do NIVEL", 0.7, 1))) %>% 
  filter(!centro %in% c("Hospital El Alto Norte", "Hospital El Alto Sur", "Hospital De El Alto Sur", "Hospital Del Norte"))

coordenadas_2019 <- coordenadas_2019 %>%
  mutate(alpha = if_else(nivel=="1er NIVEL", 0.5, 
                         if_else(nivel=="2do NIVEL", 0.7, 1)))

coordenadas_2023 <- coordenadas_2023 %>%
  mutate(alpha = if_else(nivel=="1er NIVEL", 0.5, 
                         if_else(nivel=="2do NIVEL", 0.7, 1)))  # Menos transparente para los que tienen etiquetas


# Paleta de colores para los niveles
colores <- c("1er NIVEL" = "#EC8305", "2do NIVEL" = "#31511E", "3er NIVEL" = "#091057")

g_2014 <- ggmap(mapa_paz) +
  geom_point(data = coordenadas_2014,
             aes(x = lon, y = lat, color = nivel, alpha=alpha),  # Usar el nivel de transparencia
             size = 2) +
  scale_color_manual(values = colores) +  # Aplicar la paleta de colores
  scale_alpha_identity() +  # Hacer que `alpha` tome los valores directamente desde los datos
  labs(title = "2014", x = NULL, y = NULL, color = "") +
  theme(
    legend.position = "top",
    legend.key = element_blank(),
    plot.title = element_text(face="bold", hjust=0.5),
    plot.margin = unit(c(0, 0, 0, 0), "cm")  # Reducir márgenes
  )  

g_2014

ggsave(filename = paste0("Output/mapas/", "map_center_2014", ".png"),
       res = 300,
       width = 20,
       height = 15,
       units = 'cm',
       scaling = 1,
       device = ragg::agg_png)

g_2019 <- ggmap(mapa_paz) +
  geom_point(data = coordenadas_2019,
             aes(x = lon, y = lat, color = nivel, alpha=alpha),  # Usar el nivel de transparencia
             size = 2) +
  scale_color_manual(values = colores) +  # Aplicar la paleta de colores
  scale_alpha_identity() +  # Hacer que `alpha` tome los valores directamente desde los datos
  labs(title = "2019", x = NULL, y = NULL, color = "") +
  theme(
    legend.position = "top",
    legend.key = element_blank(),
    plot.title = element_text(face="bold", hjust=0.5),
    plot.margin = unit(c(0, 0, 0, 0), "cm")  # Reducir márgenes
  ) +
  geom_text_repel(data = filter(coordenadas_2019, centro=="Hospital El Alto Norte"),
                  aes(x =  lon, y = lat, label = label, fontface = "bold"),  # Nombres en negrita
                  size = 3,
                  box.padding = 0.3,
                  max.overlaps = 10)

g_2019

ggsave(filename = paste0("Output/mapas/", "map_center_2019", ".png"),
       res = 300,
       width = 20,
       height = 15,
       units = 'cm',
       scaling = 1,
       device = ragg::agg_png)


g_2023 <- ggmap(mapa_paz) +
  geom_point(data = coordenadas_2023,
             aes(x = lon, y = lat, color = nivel, alpha=alpha),  # Usar el nivel de transparencia
             size = 2) +
  scale_color_manual(values = colores) +  # Aplicar la paleta de colores
  scale_alpha_identity() +  # Hacer que `alpha` tome los valores directamente desde los datos
  labs(title = NULL, x = NULL, y = NULL, color = "") +
  theme(
    legend.position = "top",
    legend.key = element_blank(),
    plot.title = element_text(face="bold", hjust=0.5),
    plot.margin = unit(c(0, 0, 0, 0), "cm") # Reducir márgenes
  ) +
  geom_text_repel(data = filter(coordenadas_2023, centro %in% c("Hospital El Alto Norte", "Hospital El Alto Sur")),
                  aes(x =  lon, y = lat, label = label, fontface = "bold"),  # Nombres en negrita
                  size = 3) +
                  #box.padding = 0.3,
                  #max.overlaps = 10) +  # Etiquetas de la variable `label`
  annotation_north_arrow(
    location = "topright", 
    which_north = "true", 
    style = north_arrow_fancy_orienteering()
  )  # Agregar una flecha indicando el norte

g_2023

ggsave(filename = paste0("Output/mapas/", "map_center_2023", ".png"),
       res = 300,
       width = 20,
       height = 15,
       units = 'cm',
       scaling = 1,
       device = ragg::agg_png)



