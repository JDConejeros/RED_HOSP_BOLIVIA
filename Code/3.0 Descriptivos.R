# Analisis descriptivos

## Cargamos los settings -------------------------------------------------------
source("Code/1.0 Settings.R")

## Cargar bases de datos -------------------------------------------------------

## Bases completas
enviados  <- rio::import("Output/bases/BBDD_ref_enviadas.rds")  
recibidos  <- rio::import("Output/bases/BBDD_ref_recibidas.rds")  

hosp <- c("Hospital Municipal Boliviano Coreano",  
                      "Hospital Municipal Boliviano Holandes",
                      "Hospital Municipal Los Andes",
                      "Hospital El Alto Norte",
                      "Hospital El Alto Sur", 
                      "Hospital de la Mujer", 
                      "Hospital de Clinicas",
                      "Hospital del Niño")

## Ingresos hospitalarios ---------------------------------------------------------------------

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
tabla1 <- tab1 %>% full_join(tab2, by = c("eess_emisor" = "eess_receptor", "anio"))

# Sumar ambas frecuencias en una unica variable de pacientes totales en el hospital
tabla1$frecuencia <- rowSums(tabla1[, c("frecuencia.x", "frecuencia.y")], na.rm = TRUE) 

# Eliminar las frecuencias individuales
tabla1 <- tabla1 %>% select(-c("frecuencia.x", "frecuencia.y")) 

# Pivotear (girar) los datos para separar frecuencias por año
tabla1 <- tabla1 %>% 
  pivot_wider(names_from = anio,
              values_from = frecuencia) %>% 
  select(1,4,2,3)

# Unir en una lista y exportarlo como excel de 2 hojas
lista1 <- list("Resumen"=tabla1, "Enviados" = tab1, "Recibidos" = tab2)

openxlsx::write.xlsx(lista1, file = "Output/conteos/ingresos.xlsx")

## Descriptivos variables relevantes ----------------------------------------------- 

anio_e <- enviados %>%
  group_by(var=anio) %>%
  summarise(frecuencia = n()
  )  %>% 
  ungroup() %>% 
  mutate(prop=frecuencia/sum(frecuencia)) %>% 
  mutate(tipo="enviados")

sexo_e <- enviados %>%
  group_by(var=sexo) %>%
  summarise(frecuencia = n()
  ) %>% 
  ungroup() %>% 
  mutate(prop=frecuencia/sum(frecuencia)) %>% 
  mutate(tipo="enviados")

edad_e <- enviados %>%
  summarise(prop=mean(edad, na.rm=TRUE)
  ) %>% 
  mutate(var="Edad") %>% 
  mutate(frecuencia=nrow(enviados)) %>% 
  mutate(tipo="enviados") %>% 
  select(2,3,1,4)

nivel_e <- enviados %>%
  group_by(var=transferido_a_nivel) %>%
  summarise(frecuencia = n()
  ) %>% 
  ungroup() %>% 
  mutate(prop=frecuencia/sum(frecuencia)) %>% 
  mutate(tipo="recibidos")


anio_r <- recibidos %>%
  group_by(var=anio) %>%
  summarise(frecuencia = n()
  ) %>% 
  ungroup() %>% 
  mutate(prop=frecuencia/sum(frecuencia)) %>% 
  mutate(tipo="recibidos")

sexo_r <- recibidos %>%
  group_by(var=sexo) %>%
  summarise(frecuencia = n()
  ) %>% 
  ungroup() %>% 
  mutate(prop=frecuencia/sum(frecuencia)) %>% 
  mutate(tipo="recibidos")

edad_r <- recibidos %>%
  summarise(prop=mean(edad, na.rm=TRUE)
  ) %>% 
  mutate(var="Edad") %>% 
  mutate(frecuencia=nrow(recibidos)) %>% 
  mutate(tipo="recibidos") %>% 
  select(2,3,1,4)

nivel_r <- recibidos %>%
  group_by(var=recibido_de_nivel) %>%
  summarise(frecuencia = n()
  ) %>% 
  ungroup() %>% 
  mutate(prop=frecuencia/sum(frecuencia)) %>% 
  mutate(tipo="recibidos")

var_e <- rbind(anio_e, sexo_e, edad_e, nivel_e)
var_r <- rbind(anio_r, sexo_r, edad_r, nivel_r)

tabla2 <- var_e %>% left_join(var_r, by="var")

# Exportar lista
openxlsx::write.xlsx(tabla2, file = "Output/conteos/freq_variables.xlsx")

## Tasas -----------------------------------------------------------------------

tasa0a <- filter(enviados, estatico == c("Trasladado a otro centro")) %>%
  group_by(anio) %>%
  summarise(transferidos = n()
  ) %>% 
  mutate(tipo = "Transferidos") %>% 
  rename(n = transferidos) %>% 
  ungroup()

tasa0b <- filter(recibidos, estatico == c("Trasladado a otro centro")) %>%
  group_by(anio) %>%
  summarise(referido= n()
  ) %>% 
  mutate(tipo = "Referidos") %>% 
  rename(n = referido) %>% 
  ungroup()


tasa1 <- filter(enviados, estatico == c("Trasladado a otro centro")) %>%
  group_by(anio, eess_emisor) %>%
  summarise(transferidos = n()
  ) %>% 
  mutate(tipo = "Transferidos") %>% 
  rename(hospital = eess_emisor) %>% 
  rename(n = transferidos) %>% 
  ungroup()

tasa2 <- filter(recibidos, estatico == c("Trasladado a otro centro")) %>%
  group_by(anio, eess_receptor) %>%
  summarise(referidos = n()
  ) %>% 
  mutate(tipo = "Referidos") %>% 
  rename(hospital = eess_receptor) %>% 
  rename(n = referidos) %>% 
  ungroup()

tabla_tasa0 <- tasa0a %>% 
  bind_rows(tasa0b) %>% 
  pivot_wider(names_from = tipo,
              values_from = n) %>% 
  select(1,3,2) %>% 
  mutate(hospital="Total")

tabla_tasa <- tasa1 %>% 
  bind_rows(tasa2) %>% 
  pivot_wider(names_from = tipo,
              values_from = n) %>% 
  #filter(hospital %in% hosp) %>% 
  arrange(hospital)

tabla3 <-  tabla_tasa %>% bind_rows(tabla_tasa0)

## La union anterior deja sin funcionar este codigo
tabla3 <- tabla3 %>%
  mutate(
    Transferidos = replace_na(Transferidos, 0),  # Reemplaza NA por 0 en Transferidos
    Referidos = replace_na(Referidos, 0),        # Reemplaza NA por 0 en Referidos
    Ingresos = Transferidos + Referidos,
    tasa_ref = Referidos / Ingresos,
    tasa_traf = Transferidos / Ingresos,
    porc_ref = tasa_ref * 100,
    porc_traf = tasa_traf * 100
  )

openxlsx::write.xlsx(tabla3, file = "Output/conteos/tasas.xlsx")

## Matriz de Flujo -----------------------------------------------------------------------

# Matriz de flujo para transferencias
matriz_transferencias <- conteo1 %>%
  select(anio, eess_emisor, transferido_a, Frecuencia) %>%
  pivot_wider(names_from = transferido_a, values_from = Frecuencia, values_fill = 0)

# Matriz de flujo para referencias
matriz_referencias <- conteo2 %>%
  select(anio, eess_receptor, referencia_de, Frecuencia) %>%
  pivot_wider(names_from = referencia_de, values_from = Frecuencia, values_fill = 0)

## Datos de red -----------------------------------------------------------------------

## Tablas de conteo pareadas, agrupando por año 
# (filtro para solo transferencias entre hospitales)

conteo1 <- filter(enviados, estatico == c("Trasladado a otro centro")) %>%
  group_by(anio, eess_emisor, transferido_a) %>%
  summarise(Frecuencia = n()) %>%
  na.omit() 

conteo2 <- filter(recibidos, estatico == c("Trasladado a otro centro")) %>%
  group_by(anio, eess_receptor, recibido_de) %>%
  summarise(Frecuencia = n()) %>%
  na.omit()

# Unir en una lista y exportarlo como excel de 2 hojas
lista1 <- list("Enviados" = conteo1, "Recibidos" = conteo2)
openxlsx::write.xlsx(lista1, file = "Output/conteos/Redes.xlsx")

