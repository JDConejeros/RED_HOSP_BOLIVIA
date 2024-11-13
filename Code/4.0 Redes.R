# Analisis de Redes

## Cargamos los settings -------------------------------------------------------
source("Code/1.0 Settings.R")

## Cargar bases de datos -------------------------------------------------------
enviados  <- rio::import("Output/bases/BBDD_ref_enviadas.rds")  
recibidos  <- rio::import("Output/bases/BBDD_ref_recibidas.rds")  

nodos_resaltados <- c("Hospital Municipal Boliviano Coreano",  
          "Hospital Municipal Boliviano Holandes",
          "Hospital Municipal Los Andes",
          "Hospital El Alto Norte",
          "Hospital El Alto Sur", 
          "Hospital de la Mujer", 
          "Hospital de Clinicas",
          "Hospital del Niño")


conteo1 <- filter(enviados, estatico == c("Trasladado a otro centro")) %>%
  group_by(anio, eess_emisor, transferido_a) %>%
  summarise(Frecuencia = n()) %>%
  #na.omit() %>% 
  ungroup()

conteo2 <- filter(recibidos, estatico == c("Trasladado a otro centro")) %>%
  group_by(anio, eess_receptor, recibido_de) %>%
  summarise(Frecuencia = n()) %>%
  na.omit() %>% 
  ungroup()

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
    rename(emisor = recibido_de, receptor = eess_receptor) %>%
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
                      "Hospital de Clinicas",
                      "Hospital del Niño")

## Gráficos -------------------------------------------------------------------------

### 2014 ----

plot_base <- ggnet2(net_2014,
                    size = 3, 
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
    fontface = ifelse(label %in% nodos_resaltados, "bold", "plain"),
    size = ifelse(label %in% nodos_resaltados, 4, 3)
  )

# Crear el gráfico final con geom_text_repel para las etiquetas personalizadas
plot_2014 <- plot_base +
  geom_text_repel(data = nodos_pos, aes(x = x, y = y, label = label, fontface = fontface), 
                  size=nodos_pos$size,
                  box.padding = 0.3, max.overlaps = 10) +
  theme(
    legend.position = "none",
    plot.title = element_blank()  # Centrar el título
  )


# Mostrar el gráfico
plot_2014

ggsave(filename = paste0("Output/redes/", "Net_2014", ".png"),
       res = 300,
       width = 20,
       height = 20,
       units = 'cm',
       scaling = 0.9,
       device = ragg::agg_png)

### 2019 ----

plot_base <- ggnet2(net_2019,
                    size = 3, 
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
    fontface = ifelse(label %in% nodos_resaltados, "bold", "plain"),
    size = ifelse(label %in% nodos_resaltados, 4, 3)
  )

# Crear el gráfico final con geom_text_repel para las etiquetas personalizadas
plot_2019 <- plot_base +
  geom_text_repel(data = nodos_pos, aes(x = x, y = y, label = label, fontface = fontface), 
                  size=nodos_pos$size,
                  box.padding = 0.3, max.overlaps = 10) +
  theme(
    legend.position = "none",
    plot.title = element_blank()  # Centrar el título
  )

# Mostrar el gráfico
plot_2019

### 2023 ----

ggsave(filename = paste0("Output/redes/", "Net_2019", ".png"),
       res = 300,
       width = 20,
       height = 20,
       units = 'cm',
       scaling = 0.9,
       device = ragg::agg_png)

plot_base <- ggnet2(net_2023,
                    size = 3, 
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
    fontface = ifelse(label %in% nodos_resaltados, "bold", "plain"),
    size = ifelse(label %in% nodos_resaltados, 4, 3)
  )

# Crear el gráfico final con geom_text_repel para las etiquetas personalizadas
plot_2023<- plot_base +
  geom_text_repel(data = nodos_pos, aes(x = x, y = y, label = label, fontface = fontface), 
                  size=nodos_pos$size,
                  box.padding = 0.3, max.overlaps = 10) +
  theme(
    legend.position = "none",
    plot.title = element_blank()  # Centrar el título
  )


# Mostrar el gráfico
plot_2023

ggsave(filename = paste0("Output/redes/", "Net_2023", ".png"),
       res = 300,
       width = 20,
       height = 20,
       units = 'cm',
       scaling = 0.9,
       device = ragg::agg_png)


## Medidas de la red ------------------------------------------------------------

# Crear una lista de redes para cada año
redes <- list(
  "2014" = net_2014,
  "2019" = net_2019,
  "2023" = net_2023
)

# Crear un data frame para almacenar los resultados
resultados <- data.frame(
  anio = character(),
  num_nodos = numeric(),
  num_enlaces = numeric(),
  densidad = numeric(),
  distancia_promedio = numeric(),
  clustering_coef = numeric(),
  stringsAsFactors = FALSE
)

# Iterar sobre cada red para calcular las métricas
for (anio in names(redes)) {
  grafo <- redes[[anio]]
  
  # Calcular el número de nodos y enlaces
  num_nodos <- vcount(grafo)
  num_enlaces <- ecount(grafo)
  
  # Calcular densidad de la red
  densidad <- edge_density(grafo, loops = FALSE)
  
  # Calcular distancia promedio (si la red es desconectada, el resultado será NA)
  distancia_promedio <- mean_distance(grafo, directed = TRUE, unconnected = TRUE)
  
  # Calcular coeficiente de clustering global
  clustering_coef <- transitivity(grafo, type = "global")
  
  # Agregar los resultados al data frame
  resultados <- rbind(resultados, data.frame(
    anio = anio,
    num_nodos = num_nodos,
    num_enlaces = num_enlaces,
    densidad = densidad,
    distancia_promedio = distancia_promedio,
    clustering_coef = clustering_coef
  ))
}

# Exportar los resultados a un archivo Excel
writexl::write_xlsx(resultados, "Output/redes/Medidas_red.xlsx")


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
  mutate(ano=2023)

centralidades_2023

tabla_centralidad <- centralidades_2014 %>% bind_rows(centralidades_2019) %>% bind_rows(centralidades_2023)


writexl::write_xlsx(filter(tabla_centralidad, nodo %in% nodos_resaltados) , "Output/redes/Tabla_centralidades.xlsx")

## Bootstrapin para calcular diferencias significativas-------------------------------------------------------

set.seed(2024)  # Para reproducibilidad
nodos_interes <- nodos_resaltados  # Nodos de interés
num_bootstraps <- 1000  # Número de réplicas

# Bootstrapping para cada año
bootstrap_2014 <- bootstrap_centrality(net_2014, nodos_interes, num_bootstraps)
bootstrap_2019 <- bootstrap_centrality(net_2019, nodos_interes, num_bootstraps)
bootstrap_2023 <- bootstrap_centrality(net_2023, nodos_interes, num_bootstraps)

# Calcular intervalos para cada año
intervalos_2014 <- calcular_intervalos(bootstrap_2014)
intervalos_2019 <- calcular_intervalos(bootstrap_2019)
intervalos_2023 <- calcular_intervalos(bootstrap_2023)

# Añadir una columna para identificar el año
intervalos_2014 <- intervalos_2014 %>% mutate(anio = 2014)
intervalos_2019 <- intervalos_2019 %>% mutate(anio = 2019)
intervalos_2023 <- intervalos_2023 %>% mutate(anio = 2023)

intervalos_centralidades <- bind_rows(intervalos_2014, intervalos_2019, intervalos_2023)

writexl::write_xlsx(intervalos_centralidades, "Output/redes/Intervalos_Centralidades.xlsx")

tabla_centralidad_final <- filter(tabla_centralidad, nodo %in% nodos_resaltados) %>% 
  left_join(intervalos_centralidades, by=c("ano"="anio", "nodo"="nodo"))

writexl::write_xlsx(tabla_centralidad_final, "Output/redes/Centralidades.xlsx")


ggplot(tabla_centralidad_final, aes(x = grado_media, y = factor(nodo, ), 
                                    color = factor(ano))) +
  geom_point(size = 3) +  # Punto central
  geom_errorbarh(aes(xmin = grado_inf, xmax = grado_sup), height = 0.2) +  # Intervalo de confianza
  scale_color_manual(values = c("2014" = "#1f77b4", "2019" = "#ff7f0e", "2023" = "#2ca02c")) +  # Colores por año
  labs(
    x = "Grado de Centralidad",
    y = NULL,
    color = "Año",
    title = "Centralidad de Grado con Intervalos de Confianza por Año"
  ) +
  geom_text(aes(label = round(grado_media, 1)), vjust = -0.8, size = 3.5, color = "black") +  # Etiqueta del grado promedio
  facet_wrap(~ano, ncol = 1) +
  theme_light() +
  theme(
    legend.text = element_text(size=11),
    axis.text.x = element_text(size=11),
    axis.text.y = element_text(size=11),
    strip.background = element_rect(color="gray", fill="white"),
    strip.text = element_text(color="black", size=11),
    legend.position = "none",
    plot.title = element_blank()
  )

ggsave(filename = paste0("Output/redes/", "Centralidad_grado", ".png"),
       res = 300,
       width = 20,
       height = 22,
       units = 'cm',
       scaling = 0.9,
       device = ragg::agg_png)

## ANOVA -------------------------------------------------------

anova_grado <- aov(grado ~ factor(ano), data = filter(tabla_centralidad))
anova_cercania <- aov(cercania ~ factor(ano), data = filter(tabla_centralidad))
anova_intermediacion <- aov(intermediacion ~ factor(ano), data = filter(tabla_centralidad))

summary(anova_grado)
summary(anova_cercania)
summary(anova_intermediacion)


anova_grado <- aov(grado ~ factor(nodo), data = filter(tabla_centralidad))
anova_cercania <- aov(cercania ~ factor(nodo), data = filter(tabla_centralidad))
anova_intermediacion <- aov(intermediacion ~ factor(nodo), data = filter(tabla_centralidad))

summary(anova_grado)
summary(anova_cercania)
summary(anova_intermediacion)

anova_grado <- aov(grado ~ factor(nodo)*factor(ano), data = filter(tabla_centralidad))
TukeyHSD(anova_grado)




