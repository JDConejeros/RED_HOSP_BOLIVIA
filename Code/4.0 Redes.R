# Analisis de Redes


## RED -------------------------------------------------------------------------

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


## Medidas de la red ------------------------------------------------------------

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


## Centralidad de la red -------------------------------------------------------

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
