# Primer avance: Conteo de referidos y transferidos

library(dplyr)
library(kableExtra)
library(knitr)
library(readxl)
library(writexl)
library(janitor)

list.files("Input/") # Vemos los files

## Cargar bases de datos -------------------------------------------------------

## Pato: por favor corrige esté código con el nombre de las variables ajustadas

gine2014 <- rio::import("Input/E. GINECOLOGIA 2014.xls") |> clean_names()
gine2019 <- rio::import("Input/E. GINECOLOGIA 2019.xls") |> clean_names()
gine2023 <- rio::import("Input/E. GINECOLOGIA 2023.xls") |> clean_names()

obst2014 <- rio::import("Input/E. OBSTETRICIA 2014.xls") |> clean_names()
obst2019 <- rio::import("Input/E. OBSTETRICIA 2019.xls") |> clean_names()
obst2023 <- rio::import("Input/E. OBSTETRICIA 2023.xls") |> clean_names()

ref <- rio::import("Input/REF_19-23.xlsx", skip = 1) |> clean_names()
ref_heas <- rio::import("Input/referencias HEAS 2020.2023.xlsx") |> clean_names()


# Ginecologia ------------------------------------------------------------------

## Binaria de estaticos/referidos ------------------------------------------

gine2014 <- gine2014 %>% 
  mutate(dummy = ifelse(is.na(gine2014$`REFERIDO DE:`) & is.na(gine2014$`TRANSFERIDO A:`), 0, 1),
         dummy = factor(dummy, labels = c("Estatico", "Trasladado")))

gine2019 <- gine2019 %>% 
  mutate(dummy = ifelse(is.na(gine2019$`REFERIDO DE:`) & is.na(gine2019$`TRANSFERIDO A:`), 0, 1),
         dummy = factor(dummy, labels = c("Estatico", "Trasladado")))

gine2023 <- gine2023 %>% 
  mutate(dummy = ifelse(is.na(gine2023$`REFERIDO DE:`) & is.na(gine2023$`TRANSFERIDO A:`), 0, 1),
         dummy = factor(dummy, labels = c("Estatico", "Trasladado")))


## Conteos REFERIDOS -------------------------------------------------------

# Conteo 1

ref1_g <- gine2014 %>%
  group_by(`REFERIDO DE:`) %>%
  summarise(Frecuencia = n()) %>%
  na.omit()

ref1_g <- cbind(ref1_g, Origen = c("Hospital del Norte")) %>%
  relocate(Origen)

# Conteo 2

ref2_g <- gine2019 %>%
  group_by(`REFERIDO DE:`) %>%
  summarise(Frecuencia = n()) %>%
  na.omit()

ref2_g <- cbind(ref2_g, Origen = c("Hospital del Norte")) %>%
  relocate(Origen)

# Conteo 3

ref3_g <- gine2023 %>%
  group_by(`REFERIDO DE:`) %>%
  summarise(Frecuencia = n()) %>%
  na.omit()

ref3_g <- cbind(ref3_g, Origen = c("Hospital del Norte")) %>%
  relocate(Origen)


## Exportar REFERIDOS ------------------------------------------------------

write_xlsx(ref1_g, "Output/ref1_g.xlsx")
write_xlsx(ref2_g, "Output/ref2_g.xlsx")
write_xlsx(ref3_g, "Output/ref3_g.xlsx")


## Conteos TRANSFERIDOS ----------------------------------------------------

# Conteo 1

traf1_g <- gine2014 %>%
  group_by(`TRANSFERIDO A:`) %>%
  summarise(Frecuencia = n()) %>%
  na.omit()

traf1_g <- cbind(traf1_g, Origen = c("Hospital del Norte")) %>%
  relocate(Origen)

# Conteo 2

traf2_g <- gine2019 %>%
  group_by(`TRANSFERIDO A:`) %>%
  summarise(Frecuencia = n()) %>%
  na.omit()

traf2_g <- cbind(traf2_g, Origen = c("Hospital del Norte")) %>%
  relocate(Origen)

# Conteo 3

traf3_g <- gine2023 %>%
  group_by(`TRANSFERIDO A:`) %>%
  summarise(Frecuencia = n()) %>%
  na.omit()

traf3_g <- cbind(traf3_g, Origen = c("Hospital del Norte")) %>%
  relocate(Origen)


## Exportar TRANSFERIDOS ---------------------------------------------------

write_xlsx(traf1_g, "Output/traf1_g.xlsx")
write_xlsx(traf2_g, "Output/traf2_g.xlsx")
write_xlsx(traf3_g, "Output/traf3_g.xlsx")




################################################################################

# Obstetricia ------------------------------------------------------------------

## Binaria de estaticos/referidos ------------------------------------------

obst2014 <- obst2014 %>% 
  mutate(dummy = ifelse(is.na(obst2014$`REFERIDO DE:`) & is.na(obst2014$`TRANSFERIDO A:`), 0, 1),
         dummy = factor(dummy, labels = c("Estatico", "Trasladado")))

obst2019 <- obst2019 %>% 
  mutate(dummy = ifelse(is.na(obst2019$`REFERIDO DE:`) & is.na(obst2019$`TRANSFERIDO A:`), 0, 1),
         dummy = factor(dummy, labels = c("Estatico", "Trasladado")))

obst2023 <- obst2023 %>% 
  mutate(dummy = ifelse(is.na(obst2023$`REFERIDO DE:`) & is.na(obst2023$`TRANSFERIDO A:`), 0, 1),
         dummy = factor(dummy, labels = c("Estatico", "Trasladado")))



## Conteos REFERIDOS -------------------------------------------------------

# Conteo 1

ref1_o <- obst2014 %>%
  group_by(`REFERIDO DE:`) %>%
  summarise(Frecuencia = n()) %>%
  na.omit()

ref1_o <- cbind(ref1_o, Origen = c("Hospital del Norte")) %>%
  relocate(Origen)

# Conteo 2

ref2_o <- obst2019 %>%
  group_by(`REFERIDO DE:`) %>%
  summarise(Frecuencia = n()) %>%
  na.omit()

ref2_o <- cbind(ref2_o, Origen = c("Hospital del Norte")) %>%
  relocate(Origen)

# Conteo 3

ref3_o <- obst2023 %>%
  group_by(`REFERIDO DE:`) %>%
  summarise(Frecuencia = n()) %>%
  na.omit()

ref3_o <- cbind(ref3_o, Origen = c("Hospital del Norte")) %>%
  relocate(Origen)


## Exportar REFERIDOS ------------------------------------------------------

write_xlsx(ref1_o, "Output/ref1_o.xlsx")
write_xlsx(ref2_o, "Output/ref2_o.xlsx")
write_xlsx(ref3_o, "Output/ref3_o.xlsx")



## Conteos TRANSFERIDOS ----------------------------------------------------

# Conteo 1

traf1_o <- obst2014 %>%
  group_by(`TRANSFERIDO A:`) %>%
  summarise(Frecuencia = n()) %>%
  na.omit()

traf1_o <- cbind(traf1_o, Origen = c("Hospital del Norte")) %>%
  relocate(Origen)

# Conteo 2

traf2_o <- obst2019 %>%
  group_by(`TRANSFERIDO A:`) %>%
  summarise(Frecuencia = n()) %>%
  na.omit()

traf2_o <- cbind(traf2_o, Origen = c("Hospital del Norte")) %>%
  relocate(Origen)

# Conteo 3

traf3_o <- obst2023 %>%
  group_by(`TRANSFERIDO A:`) %>%
  summarise(Frecuencia = n()) %>%
  na.omit()

traf3_o <- cbind(traf3_o, Origen = c("Hospital del Norte")) %>%
  relocate(Origen)


## Exportar TRANSFERIDOS ---------------------------------------------------

write_xlsx(traf1_o, "Output/traf1_o.xlsx")
write_xlsx(traf2_o, "Output/traf2_o.xlsx")
write_xlsx(traf3_o, "Output/traf3_o.xlsx")


################################################################################

# Tabla para tasas -------------------------------------------------------------

# Estaria bacan hacerla altiro aqui pero no se me ocurrio como xd


