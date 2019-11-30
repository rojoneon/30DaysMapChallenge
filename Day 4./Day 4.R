########################################################
#Day 1: Mapa de puntos | CDMX Delitos Tarde y noche
#Elaborado por: Máximo Ernesto Jaramillo-Molina 
#               Twitter: @rojo_neon
#               Github: @rojoneon
########################################################

##############
#Configuración
rm(list = ls())
library(pacman)
# Abrimos las paqueterías con un sólo comando:
p_load(ineq, haven, readr, readxl, ggplot2, shiny, tidyverse, expss, 
       DescTools, lmtest, MASS, knitr,gmodels)

##############
#Paqueterías para mapear
library(sf)
#install.packages("ggspatial")
library("ggspatial")
theme_set(theme_bw())

##############
#Directorio de trabajo
setwd ("~/Documents/Data Science/Repos/2019B/Mapas/30DaysMapChallenge/Day 1. Mapa Point")


##############
#Abrir capas de shape para mapas
shape <- read_sf(dsn = ".", 
                  layer = "coloniascdmx")

#Verifico funcionalidad del shape de alcaldías
ggplot(data = shape) +
  geom_sf()
colonias <- shape
st_crs(colonias) = 4326

#Pruebo ggplot con geom_sf
ggplot() + geom_sf(data = shape) + aes(fill = cve_alc) +
  theme(panel.grid.major = element_line(color = "white")) +
  scale_fill_gradientn(colors = sf.colors(20))

#Datos Municipales
municipios <- read_sf(dsn = ".", layer = "df_municipio")
ggplot() +
  geom_sf(data = municipios)

#Convertir datos municipales a SF y setear proyección
municipios_sf <- st_as_sf(municipios, coords = c("lon", "lat"), 
                          crs = 4326, agr = "constant")
st_crs(municipios_sf) = 4326
municipios_sf


############
#Datos de delitos CDMX
############

#Datos de robo a transeuntes con violencia
#victimas <- read.csv(file = "denuncias-victimas-pgj.csv", sep=";") 
#Descargar de aquí: 
#               https://datos.cdmx.gob.mx/explore/dataset/denuncias-victimas-pgj/export/?disjunctive.ao&refine.delito=ROBO+A+TRANSEUNTE+EN+VIA+PUBLICA+CON+VIOLENCIA


#Datos de  víctimas de violación
#Descargar de aquí: 
              # https://datos.cdmx.gob.mx/explore/dataset/denuncias-victimas-pgj/export/?disjunctive.ao&refine.delito=VIOLACION
victimas <- read.csv(file = "denuncias-victimas-pgj-2.csv", sep=";") 
victimas <- victimas %>% 
  filter(TipoPersona=="FISICA") %>%   
  na.omit()  
# filter(Año_hecho==2019 & TipoPersona=="FISICA") %>%   

#victimas <-select(victimas, FechaHecho, Sexo, lon, lat, geopoint) 

#Obtener hora de dato de víctima
library(lubridate)
victimas$FechaHecho <- as_datetime(victimas$FechaHecho)
victimas$hora <- hour(victimas$FechaHecho)
victimas$hora <- as.numeric(victimas$hora)

CrossTable(victimas$hora, victimas$Sexo,
           prop.r=F, prop.c=F,
           prop.t=T, prop.chisq=F, )

#Crear variable
victimas <- victimas %>%
  mutate(manana = case_when (hora > 4 & hora  < 11  ~ 2, hora > 17 & hora < 24 ~ 1, TRUE ~ 3))
cro(victimas$hora,victimas$manana)
# victimas <- victimas %>%
#   mutate(manana=      factor(manana,
#                             levels = c(1,2),
#                             labels = c("Noche", "Mañana")),
#   )


#Tabulados
victimas%>%
  group_by(manana)%>%
  summarize(n=n())%>%
  kable()

cro(victimas$Sexo, victimas$Año)
CrossTable(victimas$manana, victimas$Sexo,
           prop.r=F, prop.c=T,
           prop.t=F, prop.chisq=F, )

#Filtro: 2018 y 2019
victimas <- victimas %>% 
  filter(Año_hecho>2017) 

CrossTable(victimas$manana, victimas$Sexo,
           prop.r=F, prop.c=T,
           prop.t=F, prop.chisq=F, )


#Otro filtro: mujeres y horas
victimas <- victimas %>% 
  filter(Sexo=="Femenino" & (manana<=2 )) 

CrossTable(victimas$manana, victimas$Sexo,
           prop.r=F, prop.c=T,
           prop.t=F, prop.chisq=F, )


#Convertir datos de víctimas a SF y setear proyección
victimas_sf <- st_as_sf(victimas, coords = c("lon", "lat"), 
                         crs = 4326, agr = "constant")

#Mostrar juntos            
ggplot () +
  geom_sf(fill = "antiquewhite1") +
  geom_sf(data = municipios_sf) +
  geom_sf(data = colonias, fill = NA, color = gray(.5)) +
  geom_sf(data=victimas_sf, color = victimas_sf$manana, alpha = 0.5) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-99.4, -98.9), ylim = c(19,19.62), expand = FALSE) +
  xlab("Longitud") + ylab("Latitudud") +
  ggtitle("Robo a transeuntes en CDMX ", subtitle = "Por sexo y hora") +
  theme(panel.grid.major = element_line(color = gray(0.25), linetype = "dashed", 
                                        size = 0.25), 
        panel.background = element_rect(fill = "aliceblue"))



#Mostrar juntos            
ggplot () +
  geom_sf(fill = "antiquewhite1") +
  geom_sf(data = municipios_sf) +
  geom_sf(data=victimas_sf, color = victimas_sf$manana, alpha = 0.5) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-99.4, -98.9), ylim = c(19,19.62), expand = FALSE) +
  xlab("Longitud") + ylab("Latitudud") +
  ggtitle("Robo a transeuntes en CDMX ", subtitle = "Por sexo y hora") +
  theme(panel.grid.major = element_line(color = gray(0.25), linetype = "dashed", 
                                        size = 0.25), 
        panel.background = element_rect(fill = "aliceblue"))


#Mostrar juntos            
ggplot () +
  geom_sf(fill = "antiquewhite1") +
  geom_sf(data = municipios_sf) +
  geom_sf(data=victimas_sf, color = victimas_sf$manana, size=2, alpha = 0.5) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.15, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-99.4, -98.9), ylim = c(19,19.62), expand = FALSE) +
  xlab("Longitud") + ylab("Latitudud") +
  ggtitle("Violación a mujeres en CDMX", subtitle = "Víctimas en carpetas de investigación según hora") +
  theme(text = element_text(family = "Verdana"),
        panel.grid = element_blank(),
        plot.margin = unit(c(10,30,10,20), units = "point"),
        panel.background = element_rect(fill = "whitesmoke", linetype = "blank"),
        plot.background = element_rect(fill = "whitesmoke", linetype = "blank"),
        plot.subtitle =  element_text(size = 7),
        plot.caption = element_text(hjust = 0.5 ,size = 6)) +
  labs ( caption = "Fuente: Elaborado por @rojo_neon, con datos de Gob. CDMX.
         Mañana (5am-10am) en color rojo y Tarde (6pm-11pm) en negro.",
         color = "Mañana o tarde") 
ggsave("Violación a mujeres en CDMX.png", width = 8)







#Mostrar juntos            
ggplot () +
  geom_sf(fill = "antiquewhite1") +
  geom_sf(data = municipios_sf) +
  geom_sf(data=victimas_sf, color = victimas_sf$manana, size=2, alpha = 0.5) +
  geom_hex(data=victimas ,aes(x = lon, y = lat),
           # fill = cut(..value.., c(0, 100, 250, 500, 1000,
           #                         1500, 2000, 2500, Inf))),
           colour = NA, bins = 20, alpha = 0.35) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.15, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-99.4, -98.9), ylim = c(19,19.62), expand = FALSE) +
  xlab("Longitud") + ylab("Latitudud") +
  ggtitle("Violación a mujeres en CDMX", subtitle = "Víctimas en carpetas de investigación según hora") +
  theme(text = element_text(family = "Verdana"),
        panel.grid = element_blank(),
        plot.margin = unit(c(10,30,10,20), units = "point"),
        panel.background = element_rect(fill = "whitesmoke", linetype = "blank"),
        plot.background = element_rect(fill = "whitesmoke", linetype = "blank"),
        plot.subtitle =  element_text(size = 7),
        plot.caption = element_text(hjust = 0.5 ,size = 6)) +
  labs ( caption = "Fuente: Elaborado por @rojo_neon, con datos de Gob. CDMX.
         Mañana (5am-10am) en color rojo y Tarde (6pm-11pm) en negro.",
         color = "Mañana o tarde") 

  
  
ggplot()+
  geom_hex(data=victimas ,aes(x = lon, y = lat),
             # fill = cut(..value.., c(0, 100, 250, 500, 1000,
             #                         1500, 2000, 2500, Inf))),
         colour = NA, bins = 20, alpha = 0.35) 
