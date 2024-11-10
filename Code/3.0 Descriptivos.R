# Analisis descriptivos


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

openxlsx::write.xlsx(lista1, file = "Output/conteos/ingresos.xlsx")


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

openxlsx::write.xlsx(lista1, file = "Output/conteos/conteos.xlsx")


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



openxlsx::write.xlsx(tasas, file = "Output/conteos/tasas.xlsx")

openxlsx::write.xlsx(tasa1, file = "Output/conteos/tasas_ajustadas.xlsx")


## Calculo de totales por año

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

# Unir en lista
lista2 <- list("Año_enviados"   = anio_e,
               "Sexo_enviados"  = sexo_e,
               "Edad_enviados"  = edad_e,
               "Año_recibidos"  = anio_r,
               "Sexo_recibidos" = sexo_r,
               "Edad_recibidos" = edad_r)

# Exportar lista
openxlsx::write.xlsx(lista2, file = "Output/conteos/variables.xlsx")



# Matriz de flujo para transferencias
matriz_transferencias <- conteo1 %>%
  select(anio, eess_emisor, transferido_a, Frecuencia) %>%
  pivot_wider(names_from = transferido_a, values_from = Frecuencia, values_fill = 0)

# Matriz de flujo para referencias
matriz_referencias <- conteo2 %>%
  select(anio, eess_receptor, referencia_de, Frecuencia) %>%
  pivot_wider(names_from = referencia_de, values_from = Frecuencia, values_fill = 0)

