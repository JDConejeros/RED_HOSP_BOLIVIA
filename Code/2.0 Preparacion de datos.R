# Preparacion de datos


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

stop("Lo siguiente son las coords y exportar tablas")

haven::write_dta(enviados, "Output/bases/BBDD_ref_enviadas_v1.dta")
haven::write_dta(recibidos, "Output/bases/BBDD_ref_recibidas_v1.dta")
writexl::write_xlsx(enviados, "Output/bases/BBDD_ref_enviadas_v1.xlsx")
writexl::write_xlsx(recibidos, "Output/bases/BBDD_ref_recibidas_v1.xlsx")



## Extraer nombres de cada hospital --------------------------------------------

bd1 <- enviados_hosp %>% select(centro = nombre_ajustado_emisor) %>% distinct()
bd2 <- recibidos_hosp %>% select(centro = nombre_ajustado_receptor) %>% distinct()
bd3 <- enviados_hosp %>% select(centro = nombre_ajustado_transferido) %>% distinct()
bd4 <- recibidos_hosp %>% select(centro = nombre_ajustado_referencia) %>% distinct()

 # Unir bases de datos y eliminar duplicados para extraer nombres sin repetir
coordenadas <- bind_rows(bd1, bd2, bd3, bd4) %>%
  distinct(centro)

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
  mutate(Dir_completa = paste(centro, ", Bolivia"))


## Consulta OpenCage y unir coordenadas ----------------------------------------

 # API key propia, utilizar la que obtuvimos de OpenCage
Sys.setenv(OPENCAGE_KEY = "23032e9435c24a7595646aba8aa64361")

 # Georeferenciar con la API
coordenadas <- coordenadas %>%
  filter(Dir_completa != "Otros") %>% oc_forward_df(placename = Dir_completa)

 # Exportar nombres
writexl::write_xlsx(coordenadas, "Output/bases/coordenadas.xlsx")













