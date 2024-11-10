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
haven::write_dta(enviados, "Output/BBDD_ref_enviadas_v1.dta")
haven::write_dta(recibidos, "Output/BBDD_ref_recibidas_v1.dta")
writexl::write_xlsx(enviados, "Output/BBDD_ref_enviadas_v1.xlsx")
writexl::write_xlsx(recibidos, "Output/BBDD_ref_recibidas_v1.xlsx")

