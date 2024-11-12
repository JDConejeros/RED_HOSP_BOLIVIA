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
             "opencage", "leaflet", "tibble")
)

## Funciones----

contar_na <- function(df) {
  sapply(df, function(x) sum(is.na(x))) %>% as.data.frame() %>% 
    rownames_to_column(var = "Variable") %>% 
    rename(NA_Count = ".")
}

