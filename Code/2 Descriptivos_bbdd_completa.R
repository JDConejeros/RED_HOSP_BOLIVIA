# Segundo avance: Estadisticos descriptivos con bases completas

library(dplyr)
library(kableExtra)
library(knitr)
library(readxl)
library(openxlsx)
library(janitor)

list.files("Input/") # Vemos los files

## Cargar bases de datos ---------------------------------------------------

enviados  <- rio::import("Input/BBDD_Ref_enviadas v1.xlsx")  %>% clean_names()

recibidos <- rio::import("Input/BBDD_Ref_recibidas v1.xlsx") %>% clean_names() %>%
  filter(., anio %in% c("2014", "2019", "2023"))


## Binaria de estaticos/referidos ------------------------------------------

## Homologar nombres
 # Aqui homologue algunos nombres solamente, para poder armar el filtro de transferidos o estaticos

enviados <- enviados %>% mutate(
  transferido_a_recodif = 
    case_when(transferido_a_recodif == "HOSPITAL DEL NORTE" ~ "H_ALTO_NORTE",
              transferido_a_recodif %in% c("HOSPITAL EL ALTO SUR", "HOSPITAL DE EL ALTO SUR") ~ "H_ALTO_SUR",
              transferido_a_recodif == "HOSPITAL MUNICIPAL BOLIVIANO HOLANDES" ~ "H_HOLANDES",
              transferido_a_recodif == "H_Korea" ~ "HOSPITAL MUNICIPAL BOLIVIANO COREANO",
              transferido_a_recodif == "H_LOS_ANDES" ~ "HOSP.LOS ANDES",
              TRUE ~ transferido_a_recodif))

recibidos <- recibidos %>% mutate(
  eess_receptor = 
    case_when(eess_receptor == "Hospital El Alto Norte" ~ "H_ALTO_NORTE",
              eess_receptor == "Hospital El Alto Sur" ~ "H_ALTO_SUR",
              TRUE ~ eess_receptor),
  referencia_de = 
    case_when(referencia_de == "HOSPITAL DE CLINICAS" ~ "Hospital de Clinicas",
              referencia_de == "HOSPITAL DEL NIÑO" ~ "Hospital del Niño",
              referencia_de == "HOSPITAL UNIVERSITARIO NUESTRA SEÑORA DE LA PAZ" ~ "HOSPITAL UNIVERSITARIO NUESTRA SEÑORA DE LA PAZ",
              referencia_de == "HOSPITAL DE LA MUJER" ~ "Hospital de la Mujer",
              referencia_de %in% c("HOSPITAL DEL NORTE","LA PAZ - EL ALTO - HOSPITAL GENERAL : HOSPITAL DEL NORTE") ~ "H_ALTO_NORTE",
              referencia_de %in% c("HOSPITAL DE EL ALTO SUR","LA PAZ - EL ALTO - HOSPITAL GENERAL : HOSPITAL DE EL ALTO SUR") ~ "H_ALTO_SUR",
              referencia_de %in% c("HOSPITAL VILLA DOLORES","LA PAZ - EL ALTO - HOSPITAL BASICO : HOSPITAL VILLA DOLORES ") ~ "HOSPITAL VILLA DOLORES",
              TRUE ~ referencia_de))


## Crear variable binaria

enviados <- enviados %>% 
  mutate(dummy = case_when(eess_emisor == transferido_a_recodif ~ 1,
                           transferido_a_recodif == "SIN DATO" ~ NA,
                           eess_emisor != transferido_a_recodif ~ 0),
         dummy = factor(dummy, labels = c("Trasladado", "Estatico")))

recibidos <- recibidos %>% 
  mutate(dummy = case_when(eess_receptor == referencia_de ~ 1,
                           referencia_de == "SIN DATO" ~ NA,
                           eess_receptor != referencia_de ~ 0),
         dummy = factor(dummy, labels = c("Trasladado", "Estatico")))


## Conteos -----------------------------------------------------------------

conteo1 <- filter(enviados, dummy == c("Trasladado")) %>%
  group_by(eess_emisor, transferido_a_recodif) %>%
  summarise(Frecuencia = n()) %>%
  na.omit()

conteo2 <- filter(recibidos, dummy == c("Trasladado")) %>%
  group_by(eess_receptor, referencia_de) %>%
  summarise(Frecuencia = n()) %>%
  na.omit()

# Exportar conteos en una hoja excel
lista1 <- list("Enviados" = conteo1, "Recibidos" = conteo2)

openxlsx::write.xlsx(lista1, file = "Output/conteos.xlsx")


## Tasas -------------------------------------------------------------------

tasa1 <- filter(enviados, dummy == c("Trasladado")) %>%
  group_by(anio) %>%
  summarise(transferidos = n()
            )

tasa2 <- filter(recibidos, dummy == c("Trasladado")) %>%
  group_by(anio) %>%
  summarise(referidos = n()
            )

tasas <- merge(tasa1, tasa2, by = c("anio"))

tasas <- tasas %>% mutate(total = transferidos + referidos,
                          tasa_ref = referidos / total,
                          tasa_traf = transferidos / total,
                          porc_ref = tasa_ref * 100,
                          porc_traf = tasa_traf * 100)


## Conteos variables secundarias -------------------------------------------
anio_e <- enviados %>%
  group_by(anio) %>%
  summarise(Frecuencia = n()
  )

sexo_e <- enviados %>%
  group_by(sexo) %>%
  summarise(Frecuencia = n()
  )

edad_e <- enviados %>%
  group_by(edad) %>%
  summarise(Frecuencia = n()
  )

anio_r <- recibidos %>%
  group_by(anio) %>%
  summarise(Frecuencia = n()
  )

sexo_r <- recibidos %>%
  group_by(sexo) %>%
  summarise(Frecuencia = n()
  )

edad_r <- recibidos %>%
  group_by(edad) %>%
  summarise(Frecuencia = n()
  )

lista2 <- list("Año_enviados"   = anio_e,
               "Sexo_enviados"  = sexo_e,
               "Edad_enviados"  = edad_e,
               "Año_recibidos"  = anio_r,
               "Sexo_recibidos" = sexo_r,
               "Edad_recibidos" = edad_r)

openxlsx::write.xlsx(lista2, file = "Output/variables.xlsx")











