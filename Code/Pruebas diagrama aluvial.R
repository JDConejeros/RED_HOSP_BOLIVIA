# Pruebas diagrama de Sankey

# install.packages("remotes")
# remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)

# install.packages("highcharter")
library(highcharter)



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


conteo1 <- filter(enviados, estatico == c("Trasladado a otro centro")) %>%
  group_by(anio, eess_emisor, transferido_a, transferido_a_nivel) %>%
  summarise(Frecuencia = n()) %>%
  na.omit() 

conteo2 <- filter(recibidos, estatico == c("Trasladado a otro centro")) %>%
  group_by(anio, eess_receptor, recibido_de, recibido_de_nivel) %>%
  summarise(Frecuencia = n()) %>%
  na.omit()


env_diag <- conteo1 %>%
  select(anio, eess_emisor, transferido_a, Frecuencia, transferido_a_nivel) %>%
  mutate(Emisor = case_when(eess_emisor %in% hosp ~ eess_emisor,
                            eess_emisor == "Sin dato" ~ NA,
                            TRUE ~ "Otros"),
         Receptor = case_when(transferido_a %in% hosp ~ transferido_a,
                              transferido_a == "Sin dato" ~ NA,
                              TRUE ~ "Otros"))

rec_diag <- conteo2 %>%
  select(anio, eess_receptor, recibido_de, Frecuencia, recibido_de_nivel) %>%
  mutate(Receptor = case_when(eess_receptor %in% hosp ~ eess_receptor,
                              eess_receptor == "Sin dato" ~ NA,
                              TRUE ~ "Otros"),
         Emisor = case_when(recibido_de %in% hosp ~ recibido_de,
                            recibido_de == "Sin dato" ~ NA,
                            TRUE ~ "Otros"))


na.omit(env_diag) %>%
  select(Emisor, Receptor) %>%
  data_to_sankey() %>%
  hchart(
    type = "sankey",
    name = "Enviados"
  )




env_long <- env_diag %>%
  make_long(Emisor, Receptor)


ggplot(env_long, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node))) +
  geom_sankey() +
  theme_sankey(base_size = 10)

# Diagrama aluvial

install.packages("ggalluvial")
library(ggalluvial)

ggplot(data = env_diag,
       aes(axis1 = Emisor,   # Primera variable del eje X
           axis2 = Receptor, # Segunda variable del eje X
           axis3 = transferido_a_nivel,
           y = Frecuencia,
           label = Emisor,
           fill = Emisor)) +  
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none")
  
  
  
  
  
  
  
  
  geom_alluvium(aes(fill = as.character(Emisor))) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  theme_void() +
  guides(fill = guide_legend(title = "Año")) + 
  theme(legend.position = )














