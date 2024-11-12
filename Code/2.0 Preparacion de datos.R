# Preparacion de datos

## Cargamos los settings -------------------------------------------------------
source("Code/1.0 Settings.R")

## Cargar bases de datos -------------------------------------------------------

## Bases completas
enviados  <- rio::import("Input/01 bbdd_ref_enviadas_v2.xlsx")  %>% clean_names()

recibidos <- rio::import("Input/02 bbdd_ref_recibidas_v2.xlsx") %>% clean_names() %>%
  filter(., anio %in% c("2014", "2019", "2023"))

## Bases para ajuste de variables
enviados_hosp  <- rio::import("Input/Hosp_ref_enviadas_categorias.xlsx", sheet = "Enviados")  %>% clean_names()
recibidos_hosp <- rio::import("Input/Hosp_ref_enviadas_categorias.xlsx", sheet = "Recibidos") %>% clean_names()


## Limpieza, homologación y ajuste ----------------------------------------------

### Enviados -----

 # Revisar variables
glimpse(enviados); contar_na(enviados)
contar_na(enviados)
glimpse(enviados_hosp)

enviados <- enviados %>% 
  select(2:9, 11:15, 18:19, 21:32) %>% 
  rename(id=conti) %>% 
  mutate(fecha = as.Date(paste(anio, mes, dia, sep = "-"))) %>% 
  mutate(edad=as.numeric(edad)) %>% 
  rename(transferido_a_recodif=establecimiento_est_2,
         transferido_a_n=n_est_2,
         transferido_a_dpto=dpto_est_2,
         transferido_a_cod_dpto=coddepto_est_2,
         transferido_a_red_salud=red_salud_est_2,
         transferido_a_cod_mun=cod_mun_est_2,
         transferido_a_mun=mun_est_2,
         transferido_a_nivel=nivel_est_2,
         transferido_a_subsector=subsector_est_2,
         transferido_a_dependencia=dependencia_est_2,
         transferido_a_ambito=ambito_est_2
         ) %>% 
  relocate(transferido_a_recodif, .after=transferido_a_original) %>% 
  arrange(id) %>% 
  select(-depto_est_2) %>% 
  mutate(transferido_a_recodif=stringr::str_to_title(transferido_a_recodif)) %>% 
  rename(transferido_a=transferido_a_recodif)

# Corregir nombres de la base original
#enviados <- enviados %>%  
  #left_join(select(enviados_hosp, eess_emisor, nombre_ajustado_emisor), by = "eess_emisor", multiple = "first") %>% 
  #left_join(select(enviados_hosp, transferido_a_recodif, nombre_ajustado_transferido), by = "transferido_a_recodif", multiple = "first") 

# Quitar variables reduntantes (usadas para los join)
# enviados <- enviados %>% 
#   select(-c(eess_emisor, transferido_a_recodif)) %>% 
#   rename(eess_emisor = nombre_ajustado_emisor,
#          transferido_a = nombre_ajustado_transferido,
#          fecha = fecha_envio) %>% 
#   select(-c(municipio,transferido_a_original, referido_de))
  

### Recibidos -----

# Revisar variables
glimpse(recibidos); contar_na(recibidos)
glimpse(recibidos_hosp)

recibidos<- recibidos %>% 
  select(2:17, 22:24, 26:28, 30:33) %>% 
  rename(id=conti) %>% 
  mutate(fecha = as.Date(paste(anio, mes, dia, sep = "-"))) %>% 
  mutate(edad=as.numeric(edad)) %>% 
  rename(recibido_de_cod=cod_est_1,
         recibido_de_n=n,
         recibido_de_dpto=dpto,
         recibido_de_cod_dpto=coddepto,
         recibido_de_red_salud=red_salud,
         recibido_de_cod_mun=cod_mun,
         recibido_de_mun=mun,
         recibido_de_nivel=nivel,
         recibido_de_subsector=subsector,
         recibido_de_dependencia=dependencia,
         recibido_de_ambito=ambito
  ) %>% 
  arrange(id)

# Corregir nombres de la base original
recibidos <- recibidos %>%  
  #left_join(select(recibidos_hosp, eess_receptor, nombre_ajustado_receptor), by = "eess_receptor", multiple = "first") %>% 
  left_join(select(recibidos_hosp, referencia_de, nombre_ajustado_referencia), by = "referencia_de", multiple = "first")

# Quitar variables reduntantes (usadas para los join)
recibidos <- recibidos %>% 
  select(-referencia_de) %>% 
  rename(recibido_de = nombre_ajustado_referencia) %>% 
  relocate(recibido_de, .after=edad)


## Construcción de variables -----

## Variable binaria que indica:
   # 1: Personas transferidas de un centro a otro
   # 0: Personas transferidas a una especialidad del mismo centro

# Binaria para enviados
enviados <- enviados %>% 
  mutate(estatico = case_when(eess_emisor == transferido_a ~ 1,
                              transferido_a == "SIN DATO" ~ NA,
                              eess_emisor != transferido_a ~ 0),
         estatico = factor(estatico, labels = c("Trasladado a otro centro", "Se mantiene en el mismo centro")))

# Binaria para recibidos
recibidos <- recibidos %>% 
  mutate(estatico = case_when(eess_receptor == recibido_de ~ 1,
                              recibido_de == "SIN DATO" ~ NA,
                              eess_receptor != recibido_de ~ 0),
         estatico = factor(estatico, labels = c("Trasladado a otro centro", "Se mantiene en el mismo centro")))

 # Ultima revision
glimpse(enviados)
glimpse(recibidos)
colnames(enviados)
colnames(recibidos)

## Georeferenciación hospitales --------------------------------------------

bd1 <- enviados %>% select(centro = eess_emisor, mun=municipio) %>% distinct()
bd2 <- enviados %>% select(centro = transferido_a, mun=transferido_a_mun) %>% distinct()
bd3 <- recibidos %>% select(centro = eess_receptor, mun=municipio) %>% distinct()
bd4 <- recibidos %>% select(centro = recibido_de, mun=recibido_de_mun) %>% distinct()

# Unir bases de datos y eliminar duplicados para extraer nombres sin repetir
coordenadas <- bind_rows(bd1, bd2, bd3, bd4) %>%
  distinct(centro, .keep_all = TRUE) %>% 
  mutate(mun=stringr::str_to_title(mun))

# Recodificar "otros"
coordenadas <- coordenadas %>%
  mutate(centro = case_when(centro %in% c("Clinica San Juan de Dios SERVIMED",
                                          "Institutos de Salud",
                                          "Instituto Nacional de Oftalmologia",
                                          "Programa de Salud Renal",
                                          "Policlinicos",
                                          "Otro Centro de Salud",
                                          "Otra Caja de Salud",
                                          "Institutos de Salud",
                                          "Centro de Salud Mercedes",
                                          "Clinicas Privadas",
                                          "Centros de Salud") ~ "Otros",
                            centro == "San Juan de Dios Camargo" ~ "Hospital San Juan de Dios Camargo",
                            TRUE ~ centro))

# Pegar pais a los hospitales para buscar en OpenCage
coordenadas <- coordenadas %>%
  filter(!Dir_completa %in% c("Otros", "Sin Dato")) %>%
  mutate(Dir_completa = paste0(centro,", ", mun, ", Bolivia"))


## Consulta OpenCage y unir coordenadas 

# API key propia, utilizar la que obtuvimos de OpenCage
Sys.setenv(OPENCAGE_KEY = "") # encriptar este key cuando hagamos el envio de estos códigos 

# Georeferenciar con la API
coordenadas <- coordenadas %>%
  filter(!Dir_completa %in% c("Otros", "Sin Dato")) %>% oc_forward_df(placename = Dir_completa, countrycode="BO")

# Filtramos los que no encontramos data
coordenadas_na <- coordenadas %>% filter(is.na(oc_formatted))

# Georeferenciación con la API de google maps
api_key <- ""
register_google(api_key, write = TRUE)

# Consulta solo para NA
coordenadas_na <- coordenadas_na[,1:3] %>% mutate_geocode(location = Dir_completa, countrycodes = "BO")

#Consulta completa
coordenadas_gg <- coordenadas[,1:3] %>% mutate_geocode(location = Dir_completa, countrycodes = "BO")

# Exportar nombres
writexl::write_xlsx(coordenadas, "Output/bases/georeferenciacion_hospitales.xlsx")
writexl::write_xlsx(coordenadas_na, "Output/bases/georeferenciacion_hospitales_perdidos.xlsx")
writexl::write_xlsx(coordenadas_gg, "Output/bases/georeferenciacion_google.xlsx")

## Data Productos --------------------------------------------

haven::write_dta(enviados, "Output/bases/BBDD_ref_enviadas.dta")
haven::write_dta(recibidos, "Output/bases/BBDD_ref_recibidas.dta")

saveRDS(enviados, file="Output/bases/BBDD_ref_enviadas.rds")
saveRDS(recibidos, "Output/bases/BBDD_ref_recibidas.rds")

writexl::write_xlsx(enviados, "Output/bases/BBDD_ref_enviadas.xlsx")
writexl::write_xlsx(recibidos, "Output/bases/BBDD_ref_recibidas.xlsx")

