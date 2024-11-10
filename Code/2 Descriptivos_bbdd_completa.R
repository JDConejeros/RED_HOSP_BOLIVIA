# Segundo avance: Estadisticos descriptivos con bases completas

 # Limpiamos el enviroment y desactivamos la notación científica 
rm(list = ls())
options(scipen = 999)


# Cargamos las librerías que utilizaremos en el análisis
settings_packages <- function(packages){
  # Cargamos las tablas de datos
  for (i in packages) {
    if (i %in% rownames(installed.packages())) {
      library(i, character.only=TRUE)
    } else {
      install.packages(i)
      library(i, character.only = TRUE)
    }
  }
}

settings_packages(
  packages=c("readxl", "dplyr", "broom", "ggplot2", "jtools", 
             "ggraph", "tidygraph", "GGally", "texreg", "ggrepel",
             "igraph", "network", "sna", "ergm", # FUNDAMENTALES
             "ggmcmc", "patchwork", "DT", "ggnetwork", "visNetwork",
             "kableExtra", "knitr", "openxlsx", "tidyr", "janitor")
)

 # Vemos los files
list.files("Input/")


## Cargar bases de datos -------------------------------------------------------

## Bases completas
enviados  <- rio::import("Input/BBDD_Ref_enviadas v1.xlsx")  %>% clean_names()

recibidos <- rio::import("Input/BBDD_Ref_recibidas v1.xlsx") %>% clean_names() %>%
  filter(., anio %in% c("2014", "2019", "2023"))

## Bases conteos
enviados_hosp  <- rio::import("Input/Hosp_ref_enviadas_categorias.xlsx", sheet = "Enviados")  %>% clean_names()

recibidos_hosp <- rio::import("Input/Hosp_ref_enviadas_categorias.xlsx", sheet = "Recibidos") %>% clean_names()


## Binaria de estaticos/referidos ----------------------------------------------

### Enviados -----

 # Revisar variables
glimpse(enviados)
glimpse(enviados_hosp)

 # Quitar variables vacias 
enviados <- enviados %>% 
  select(1:16)

 # Corregir nombres de la base original
enviados <- enviados %>%  
  left_join(select(enviados_hosp, eess_emisor, nombre_ajustado_emisor), by = "eess_emisor", multiple = "first") %>% 
  left_join(select(enviados_hosp, transferido_a_recodif, nombre_ajustado_transferido), by = "transferido_a_recodif", multiple = "first") 

# Quitar variables reduntantes (usadas para los join)
enviados <- enviados %>% 
  select(-c(eess_emisor, transferido_a_recodif)) %>% 
  rename(eess_emisor = nombre_ajustado_emisor,
         transferido_a = nombre_ajustado_transferido,
         fecha = fecha_envio) %>% 
  select(-c(municipio,transferido_a_original, referido_de))
  

### Recibidos -----

 # Revisar variables
glimpse(recibidos)
glimpse(recibidos_hosp)

 # Corregir nombres de la base original
recibidos <- recibidos %>%  
  left_join(select(recibidos_hosp, eess_receptor, nombre_ajustado_receptor), by = "eess_receptor", multiple = "first") %>% 
  left_join(select(recibidos_hosp, referencia_de, nombre_ajustado_referencia), by = "referencia_de", multiple = "first")

 # Quitar variables reduntantes (usadas para los join)
recibidos <- recibidos %>% 
  select(-c(eess_receptor, referencia_de)) %>% 
  rename(eess_receptor = nombre_ajustado_receptor,
         referencia_de = nombre_ajustado_referencia)


### Variable binaria -----

## Variable binaria que indica:
   # 1: Personas transferidas de un centro a otro
   # 0: Personas transferidas a una especialidad del mismo centro

 # Binaria para enviados
enviados <- enviados %>% 
  mutate(estatico = case_when(eess_emisor == transferido_a ~ 1,
                            transferido_a == "SIN DATO" ~ NA,
                              eess_emisor != transferido_a ~ 0),
         estatico = factor(estatico, labels = c("Trasladado", "Estatico")))

 # Binaria para recibidos
recibidos <- recibidos %>% 
  mutate(estatico = case_when(eess_receptor == referencia_de ~ 1,
                              referencia_de == "SIN DATO" ~ NA,
                              eess_receptor != referencia_de ~ 0),
         estatico = factor(estatico, labels = c("Trasladado", "Estatico")))

 # Forzar variable numerica (originalmente es character en esta bbdd)
enviados <- enviados %>% 
  mutate(edad=as.numeric(edad))

 # Ultima revision
glimpse(enviados)
glimpse(recibidos)
colnames(enviados)
colnames(recibidos)

 # Exportar todas las tablas
haven::write_dta(enviados, "Output/BBDD_ref_enviadas_v1.dta")
haven::write_dta(recibidos, "Output/BBDD_ref_recibidas_v1.dta")
writexl::write_xlsx(enviados, "Output/BBDD_ref_enviadas_v1.xlsx")
writexl::write_xlsx(recibidos, "Output/BBDD_ref_recibidas_v1.xlsx")


## Conteos ---------------------------------------------------------------------

## Ingresados por año segun hospital y año

tab1 <- enviados %>% 
  group_by(eess_emisor, anio) %>%
  summarise(frecuencia = n()) %>%
  na.omit() 

tab2 <- recibidos %>% 
  group_by(eess_receptor, anio) %>%
  summarise(frecuencia = n()) %>%
  na.omit() 

 # Unir las sumas de las frecuencias de ambas tablas
tab1 <- tab1 %>% left_join(tab2, by = c("eess_emisor" = "eess_receptor", "anio"))

 # Sumar ambas frecuencias en una unica variable de pacientes totales en el hospital
tab1$frecuencia <- rowSums(tab1[, c("frecuencia.x", "frecuencia.y")], na.rm = TRUE) 

 # Eliminar las frecuencias individuales
tab1 <- tab1 %>% select(-c("frecuencia.x", "frecuencia.y"))

 # Pivotear (girar) los datos para separar frecuencias por año
tab1 <- tab1 %>% 
  pivot_wider(names_from = anio,
              values_from = frecuencia)

 # Unir en una lista y exportarlo como excel de 2 hojas
lista1 <- list("Enviados" = tab1, "Recibidos" = tab2)

openxlsx::write.xlsx(lista1, file = "Output/ingresos.xlsx")


## Tablas de conteo pareadas, agrupando por año 
 # (filtro para solo transferencias entre hospitales)

conteo1 <- filter(enviados, estatico == c("Trasladado")) %>%
  group_by(anio, eess_emisor, transferido_a) %>%
  summarise(Frecuencia = n()) %>%
  na.omit() 

conteo2 <- filter(recibidos, estatico == c("Trasladado")) %>%
  group_by(anio, eess_receptor, referencia_de) %>%
  summarise(Frecuencia = n()) %>%
  na.omit()

 # Unir en una lista y exportarlo como excel de 2 hojas
lista1 <- list("Enviados" = conteo1, "Recibidos" = conteo2)

openxlsx::write.xlsx(lista1, file = "Output/conteos.xlsx")


## Tasas -----------------------------------------------------------------------

tasa1 <- filter(enviados, estatico == c("Trasladado")) %>%
  group_by(anio, eess_emisor) %>%
  summarise(transferidos = n()
            ) %>% 
  mutate(tipo = "Transferido") %>% 
  rename(hospital = eess_emisor) %>% 
  rename(n = transferidos)
  
tasa2 <- filter(recibidos, estatico == c("Trasladado")) %>%
  group_by(anio, eess_receptor) %>%
  summarise(referidos = n()
            ) %>% 
  mutate(tipo = "Referido") %>% 
  rename(hospital = eess_receptor) %>% 
  rename(n = referidos)

tasa1 <- tasa1 %>% bind_rows(tasa2)
                                                                                           ## DUDAS ----
## No se que objetivo tiene esta union, se me hace reduntante unir datos de la
## tasa2 con datos de la tasa2 pero usando año como identificador
tasas <- left_join(tasa1, tasa2, by = c("anio"))

## La union anterior deja sin funcionar este codigo
tasas <- tasas %>% mutate(total = transferidos + referidos,
                          tasa_ref = referidos / total,
                          tasa_traf = transferidos / total,
                          porc_ref = tasa_ref * 100,
                          porc_traf = tasa_traf * 100)



openxlsx::write.xlsx(tasas, file = "Output/tasas.xlsx")

openxlsx::write.xlsx(tasa1, file = "Output/tasas_ajustadas.xlsx")

a <- enviados %>% 
  group_by(anio) %>% 
  summarise(n1=n())

b <- recibidos %>% 
  group_by(anio) %>% 
  summarise(n2=n())

a <- a %>% left_join(b, by="anio") %>% 
  summarise(total=n1+n2)

## Conteos variables secundarias -----------------------------------------------
anio_e <- enviados %>%
  group_by(anio) %>%
  summarise(frecuencia = n()
  )  %>% 
  ungroup() %>% 
  mutate(prop=frecuencia/sum(frecuencia))

sexo_e <- enviados %>%
  group_by(sexo) %>%
  summarise(frecuencia = n()
  ) %>% 
  ungroup() %>% 
  mutate(prop=frecuencia/sum(frecuencia))

edad_e <- enviados %>%
  summarise(media=mean(edad, na.rm=TRUE)
  ) 

anio_r <- recibidos %>%
  group_by(anio) %>%
  summarise(frecuencia = n()
  ) %>% 
  ungroup() %>% 
  mutate(prop=frecuencia/sum(frecuencia))

sexo_r <- recibidos %>%
  group_by(sexo) %>%
  summarise(frecuencia = n()
  ) %>% 
  ungroup() %>% 
  mutate(prop=frecuencia/sum(frecuencia))

edad_r <- recibidos %>%
  summarise(media=mean(edad, na.rm=TRUE)
  )

lista2 <- list("Año_enviados"   = anio_e,
               "Sexo_enviados"  = sexo_e,
               "Edad_enviados"  = edad_e,
               "Año_recibidos"  = anio_r,
               "Sexo_recibidos" = sexo_r,
               "Edad_recibidos" = edad_r)

openxlsx::write.xlsx(lista2, file = "Output/variables.xlsx")



# Matriz de flujo para transferencias
matriz_transferencias <- conteo1 %>%
  select(anio, eess_emisor, transferido_a, Frecuencia) %>%
  pivot_wider(names_from = transferido_a, values_from = Frecuencia, values_fill = 0)

# Matriz de flujo para referencias
matriz_referencias <- conteo2 %>%
  select(anio, eess_receptor, referencia_de, Frecuencia) %>%
  pivot_wider(names_from = referencia_de, values_from = Frecuencia, values_fill = 0)


### RED -----

# Crear una lista para almacenar los grafos de cada año
grafos_por_anio <- list()

# Listado de años únicos en los datos
anios <- unique(c(conteo1$anio, conteo2$anio))

# Iterar sobre cada año para construir los grafos
for (anio in anios) {
  # Filtrar datos por año
  datos_transferencias <- conteo1 %>%
    filter(anio == !!anio) %>%
    rename(emisor = eess_emisor, receptor = transferido_a) %>%
    select(emisor, receptor, Frecuencia)
  
  datos_referencias <- conteo2 %>%
    filter(anio == !!anio) %>%
    rename(emisor = referencia_de, receptor = eess_receptor) %>%
    select(emisor, receptor, Frecuencia)
  
  # Unir los datos de transferencias y referencias
  datos_enlaces <- bind_rows(datos_transferencias, datos_referencias) %>%
    group_by(emisor, receptor) %>%
    summarise(peso = sum(Frecuencia, na.rm = TRUE)) %>%
    ungroup()
  
  # Crear el grafo de igraph
  grafo <- graph_from_data_frame(datos_enlaces, directed = TRUE)
  
  # Agregar el grafo a la lista
  grafos_por_anio[[as.character(anio)]] <- grafo
}

net_2014 <- grafos_por_anio[["2014"]]
net_2019 <- grafos_por_anio[["2019"]]
net_2023 <- grafos_por_anio[["2023"]]

nodos_resaltados <- c("Hospital Municipal Boliviano Coreano",  
                      "Hospital Municipal Boliviano Holandes",
                      "Hospital Municipal Los Andes",
                      "Hospital El Alto Norte",
                      "Hospital El Alto Sur", 
                      "Hospital de la Mujer", 
                      "Hospital de Clinicas")


plot_base <- ggnet2(net_2014,
                    size = 2, 
                    shape = 15,
                    max_size = 3,  
                    edge.size = 0.5, 
                    edge.color = "grey",
                    node.color = "color_nodo",
                    node.alpha = 1,
                    label = FALSE)

# Extraer las posiciones de los nodos y almacenarlas en un data frame
nodos_pos <- plot_base$data

# Agregar una columna de estilo para negrita y tamaño de fuente en función de nodos_resaltados
nodos_pos <- nodos_pos %>%
  mutate(
    fontface = ifelse(label %in% nodos_resaltados, "bold", "plain")
  )

# Crear el gráfico final con geom_text_repel para las etiquetas personalizadas
plot_2014 <- plot_base +
  geom_text_repel(data = nodos_pos, aes(x = x, y = y, label = label, fontface = fontface), 
                  box.padding = 0.3, max.overlaps = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)  # Centrar el título
  )

# Mostrar el gráfico
plot_2014


ggsave(filename = paste0("Output/", "Net_2014", ".png"),
       res = 300,
       width = 25,
       height = 20,
       units = 'cm',
       scaling = 0.9,
       device = ragg::agg_png)


plot_base <- ggnet2(net_2019,
                    size = 2, 
                    shape = 15,
                    max_size = 3,  
                    edge.size = 0.5, 
                    edge.color = "grey",
                    node.color = "color_nodo",
                    node.alpha = 1,
                    label = FALSE)

# Extraer las posiciones de los nodos y almacenarlas en un data frame
nodos_pos <- plot_base$data

# Agregar una columna de estilo para negrita y tamaño de fuente en función de nodos_resaltados
nodos_pos <- nodos_pos %>%
  mutate(
    fontface = ifelse(label %in% nodos_resaltados, "bold", "plain")
  )

# Crear el gráfico final con geom_text_repel para las etiquetas personalizadas
plot_2019 <- plot_base +
  geom_text_repel(data = nodos_pos, aes(x = x, y = y, label = label, fontface = fontface), 
                  box.padding = 0.3, max.overlaps = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)  # Centrar el título
  )

# Mostrar el gráfico
plot_2019


ggsave(filename = paste0("Output/", "Net_2019", ".png"),
       res = 300,
       width = 25,
       height = 20,
       units = 'cm',
       scaling = 0.9,
       device = ragg::agg_png)

plot_base <- ggnet2(net_2023,
                    size = 2, 
                    shape = 15,
                    max_size = 3,  
                    edge.size = 0.5, 
                    edge.color = "grey",
                    node.color = "color_nodo",
                    node.alpha = 1,
                    label = FALSE)

# Extraer las posiciones de los nodos y almacenarlas en un data frame
nodos_pos <- plot_base$data

# Agregar una columna de estilo para negrita y tamaño de fuente en función de nodos_resaltados
nodos_pos <- nodos_pos %>%
  mutate(
    fontface = ifelse(label %in% nodos_resaltados, "bold", "plain")
  )

# Crear el gráfico final con geom_text_repel para las etiquetas personalizadas
plot_2023 <- plot_base +
  geom_text_repel(data = nodos_pos, aes(x = x, y = y, label = label, fontface = fontface), 
                  box.padding = 0.3, max.overlaps = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)  # Centrar el título
  )

# Mostrar el gráfico
plot_2023

ggsave(filename = paste0("Output/", "Net_2023", ".png"),
       res = 300,
       width = 25,
       height = 20,
       units = 'cm',
       scaling = 0.9,
       device = ragg::agg_png)

# Medidas de la red ----


# Lista de grafos para cada año
redes <- list(
  "2014" = net_2014,
  "2019" = net_2019,
  "2023" = net_2023
)

# Crear un data frame para almacenar los resultados
resultados <- data.frame(
  anio = character(),
  densidad = numeric(),
  distancia_promedio = numeric(),
  clustering_coef = numeric(),
  stringsAsFactors = FALSE
)

# Iterar sobre cada red para calcular las métricas
for (anio in names(redes)) {
  grafo <- redes[[anio]]
  
  # Calcular densidad de la red
  densidad <- edge_density(grafo, loops = FALSE)
  
  # Calcular distancia promedio (si la red es desconectada, el resultado será NA)
  distancia_promedio <- mean_distance(grafo, directed = TRUE, unconnected = TRUE)
  
  # Calcular coeficiente de clustering global
  clustering_coef <- transitivity(grafo, type = "global")
  
  # Agregar los resultados al data frame
  resultados <- rbind(resultados, data.frame(
    anio = anio,
    densidad = densidad,
    distancia_promedio = distancia_promedio,
    clustering_coef = clustering_coef
  ))
}

writexl::write_xlsx(resultados, "Output/Medidas_red.xlsx")

# Centralidad de la red ----

# Lista de redes por año 2014

# Calcular grado de centralidad (degree centrality) 
centralidad_grado <- igraph::degree(net_2014) 

# Calcular centralidad de cercanía 
centralidad_cercania <- igraph::closeness(net_2014, normalized = T) 

# Calcular la centralidad de intermediación
centralidad_intermediacion <- igraph::betweenness(net_2014, normalized = T)

# Calcular la centralidad de vector propio
centralidad_eigen <-igraph::eigen_centrality(net_2014)

centralidades_2014 <- cbind(centralidad_grado,
                       round(centralidad_cercania,3),
                       round(centralidad_intermediacion,3),
                       round(centralidad_eigen$vector,3)) 
colnames(centralidades_2014) <- c("grado", "cercania", "intermediacion", "eigen")
centralidades_2014 <- centralidades_2014 %>% as.data.frame() 
centralidades_2014 <- centralidades_2014 %>% rownames_to_column(var = "nodo") %>%  
  as_tibble() %>% 
  arrange(desc(grado)) %>% 
  slice(1:5) %>% 
  mutate(ano=2014)

centralidades_2014

# Lista de redes por año 2019

# Calcular grado de centralidad (degree centrality) 
centralidad_grado <- igraph::degree(net_2019) 

# Calcular centralidad de cercanía 
centralidad_cercania <- igraph::closeness(net_2019, normalized = T) 

# Calcular la centralidad de intermediación
centralidad_intermediacion <- igraph::betweenness(net_2019, normalized = T)

# Calcular la centralidad de vector propio
centralidad_eigen <-igraph::eigen_centrality(net_2019)

centralidades_2019 <- cbind(centralidad_grado,
                            round(centralidad_cercania,3),
                            round(centralidad_intermediacion,3),
                            round(centralidad_eigen$vector,3)) 
colnames(centralidades_2019) <- c("grado", "cercania", "intermediacion", "eigen")
centralidades_2019 <- centralidades_2019 %>% as.data.frame() 
centralidades_2019 <- centralidades_2019 %>% rownames_to_column(var = "nodo") %>%  
  as_tibble() %>% 
  arrange(desc(grado)) %>% 
  slice(1:5) %>% 
  mutate(ano=2019)

centralidades_2019

# Lista de redes por año 2023

# Calcular grado de centralidad (degree centrality) 
centralidad_grado <- igraph::degree(net_2023) 

# Calcular centralidad de cercanía 
centralidad_cercania <- igraph::closeness(net_2023, normalized = T) 

# Calcular la centralidad de intermediación
centralidad_intermediacion <- igraph::betweenness(net_2023, normalized = T)

# Calcular la centralidad de vector propio
centralidad_eigen <-igraph::eigen_centrality(net_2023)

centralidades_2023 <- cbind(centralidad_grado,
                            round(centralidad_cercania,3),
                            round(centralidad_intermediacion,3),
                            round(centralidad_eigen$vector,3)) 
colnames(centralidades_2023) <- c("grado", "cercania", "intermediacion", "eigen")
centralidades_2023 <- centralidades_2023 %>% as.data.frame() 
centralidades_2023 <- centralidades_2023 %>% rownames_to_column(var = "nodo") %>%  
  as_tibble() %>% 
  arrange(desc(grado)) %>% 
  slice(1:5) %>% 
  mutate(ano=2023)

centralidades_2023

tabla_centralidad <- centralidades_2014 %>% bind_rows(centralidades_2019) %>% bind_rows(centralidades_2023)

writexl::write_xlsx(tabla_centralidad, "Output/Tabla_centralidades.xlsx")
