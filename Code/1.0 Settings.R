# Ajuste de paquetes

## Enviroment ------
# Limpiamos el enviroment y desactivamos la notación científica 
rm(list = ls())
options(scipen = 999)

## Packages ------
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
  packages=c("readxl", "dplyr", "broom", "ggplot2", "jtools", "stringr", "lubridate",
             "ggraph", "tidygraph", "GGally", "texreg", "ggrepel",
             "igraph", "network", "sna", "ergm", # FUNDAMENTALES
             "ggmcmc", "patchwork", "DT", "ggnetwork", "visNetwork",
             "kableExtra", "knitr", "openxlsx", "tidyr", "janitor",
             "opencage", "leaflet", "tibble", "ggmap", "flowmapblue",
             "mapgl", "sf", "maps", "sp", "tmaptools", "ggrepel", "ggspatial",
             "ggpubr"
             )
)

## Funciones----

contar_na <- function(df) {
  sapply(df, function(x) sum(is.na(x))) %>% as.data.frame() %>% 
    rownames_to_column(var = "Variable") %>% 
    rename(NA_Count = ".")
}


# Definir función de bootstrapping para centralidades
bootstrap_centrality <- function(grafo, nodos_resaltados, num_bootstraps = 1000) {
  results <- list()
  
  for (i in 1:num_bootstraps) {
    # Crear una muestra bootstrap del grafo (resampling de los nodos)
    sampled_graph <- induced_subgraph(grafo, sample(V(grafo), replace = TRUE))
    
    # Calcular centralidades
    grado <- igraph::degree(sampled_graph)
    cercania <- igraph::closeness(sampled_graph, normalized = TRUE)
    intermediacion <- igraph::betweenness(sampled_graph, normalized = TRUE)
    
    # Guardar centralidades solo para los nodos resaltados
    centralidades_resaltadas <- data.frame(
      nodo = names(grado)[names(grado) %in% nodos_resaltados],
      grado = grado[names(grado) %in% nodos_resaltados],
      cercania = cercania[names(cercania) %in% nodos_resaltados],
      intermediacion = intermediacion[names(intermediacion) %in% nodos_resaltados]
    )
    
    # Añadir al resultado general
    results[[i]] <- centralidades_resaltadas
  }
  
  # Unir todas las réplicas bootstrap en un solo data frame
  bootstrap_data <- bind_rows(results, .id = "replica")
  return(bootstrap_data)
}

calcular_intervalos <- function(bootstrap_data) {
  bootstrap_data %>%
    group_by(nodo) %>%
    summarise(
      grado_media = mean(grado, na.rm = TRUE),
      grado_inf = quantile(grado, 0.025, na.rm = TRUE),
      grado_sup = quantile(grado, 0.975, na.rm = TRUE),
      cercania_media = mean(cercania, na.rm = TRUE),
      cercania_inf = quantile(cercania, 0.025, na.rm = TRUE),
      cercania_sup = quantile(cercania, 0.975, na.rm = TRUE),
      intermediacion_media = mean(intermediacion, na.rm = TRUE),
      intermediacion_inf = quantile(intermediacion, 0.025, na.rm = TRUE),
      intermediacion_sup = quantile(intermediacion, 0.975, na.rm = TRUE)
    )
}



