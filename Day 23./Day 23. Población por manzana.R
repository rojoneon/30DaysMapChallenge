######################################################################
# Day 23. Poblacion y zona metropolitana
#Elaborado por: Máximo Ernesto Jaramillo-Molina 
#               Twitter: @rojo_neon
#               Github: @rojoneon
######################################################################


##############
#Configuración
rm(list = ls())


library(pacman)
# Abrimos las paqueterías con un sólo comando:
p_load(haven, readr, readxl, ggplot2, shiny, tidyverse, knitr,gmodels,foreign,expss,fishualize)

##############
#Paqueterías para mapear
library(sf)
#install.packages("ggspatial")
library("ggspatial")
theme_set(theme_bw())
library("colorspace")
library("magick") # this is call to animate/read pngs
#hcl_palettes(plot = TRUE)
p_load(rstudioapi, lintr, raster, viridis, cowplot, rmarkdown)
sessionInfo()

#remove.packages("ggmap")
require("devtools")
#devtools::install_github("dkahle/ggmap")
library(ggmap)
library(devtools)

##############
#Directorio de trabajo
setwd ("~/Documents/Data Science/Repos/2019B/Mapas/30DaysMapChallenge/Day 23.")
##############


#register_google(key="")
s <- "element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative%7Celement:geometry%7Cvisibility:off&style=feature:administrative.country%7Celement:geometry.stroke%7Ccolor:0x000000%7Cvisibility:on&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Cvisibility:off&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road%7Celement:labels.icon%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit%7Cvisibility:off&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x4BA9C5&size=480x360"
# Get the basemap
cdmx <- get_googlemap(
  c(lon=-99.1276627,lat=19.4284706),
  zoom = 10, crop=T, 
  scale = 4, 
  #color="bw", 
  maptype="terrain", # can change to terrain
  style = s)

ggmap(cdmx)  

gg <- ggmap(cdmx) 

 
 
 ########################
 ########################
 #EDOMEX
 ########################
 ########################
 
 #ageb_edomex <- st_read("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/mex/mex_ageb_urb.shp")
 manz_edomex <- st_read("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/mex/mex_manzanas.shp")
 #Crear variable para filtrar por municipios de interés
 manz_edomex$mun <- substr(manz_edomex$CVEGEO, 3, 5)
 #Filtrar a ZMG
 manz_edomex <- manz_edomex %>% 
   filter(mun %in% c("002", "009", "010", "011", "013", "015", "016", "017", "020", "022", "023", "024", "025",
                     "028", "029", "030", "031", "033", "034", "035", "036", "037", "038", "039", "044", "046",
                     "050", "053", "057", "058", "059", "060", "061", "065", "068", "069", "070", "075", "081",
                     "083", "084", "089", "091", "092", "093", "094", "095", "096", "099", "100", "103", "104",
                     "108", "109", "112", "120", "121", "122", "125"))
# 
#  ggplot()+
#    geom_sf(
#      data = ageb_edomex,
#      fill = "transparent",
#      color = "black",
#      size = .5,
#      inherit.aes = FALSE
#    )
 
 #Municipios
 mun_edomex <- st_read("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/mex/mex_municipal.shp")
 
 #Crear variable para filtrar por municipios de interés
 mun_edomex$mun <- substr(mun_edomex$CVEGEO, 3, 5)
 #Filtrar a ZMG
 mun_edomex <- mun_edomex %>% 
   filter(mun %in% c("002", "009", "010", "011", "013", "015", "016", "017", "020", "022", "023", "024", "025",
                     "028", "029", "030", "031", "033", "034", "035", "036", "037", "038", "039", "044", "046",
                     "050", "053", "057", "058", "059", "060", "061", "065", "068", "069", "070", "075", "081",
                     "083", "084", "089", "091", "092", "093", "094", "095", "096", "099", "100", "103", "104",
                     "108", "109", "112", "120", "121", "122", "125"))
  
 
 ########################
 ########################
 #CDMX
 ########################
 ########################
 manz_cdmx <- st_read("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/df/df_manzanas.shp")
 #Crear variable para filtrar por municipios de interés
 manz_cdmx$mun <- substr(manz_cdmx$CVEGEO, 3, 5)

 #  ggplot()+
 #    geom_sf(
 #      data = manz,
 #      fill = "transparent",
 #      color = "black",
 #      size = .1,
 #      inherit.aes = FALSE
 #    )
 
 
 #####################
 #####################
 ##Mapas con cuantiles
 #####################
 #####################
 manz <- rbind(manz_cdmx, manz_edomex) 
 
 # ¿Cuántas clases quiero?
 no_classes <- 50
 # Extraer cuantiles
 qPOB1 <- manz %>%
   pull(POB1) %>%
   quantile(probs = seq(0, 1, length.out = no_classes + 1)) %>%
   as.vector() # to remove names of quantiles, so idx below is numeric
 
 #Metemos un pequeño error random para que difieran
 random <- runif(n = 51, #número de obs
                 min = 1, max = 10)
 #Lo sumamos
 qPOB1 <- Map("+", qPOB1, random)
 qPOB1 <- as.numeric(qPOB1)
 
 # Así se crean las etiquetas
 labels <- imap_chr(qPOB1, function(., idx){
   return(paste0(round(qPOB1[idx] , 0),
                 "",
                 " – ",
                 round(qPOB1[idx + 1] , 0),
                 ""))
 })
 
 
 # Se elimina la última etiqueta
 # En caso contrario sería hasta NA
  labels <- labels[1:length(labels) - 1]
 labels
 # Crear la variable
 #manz_complemen %<>%
 manz <- manz %>%
   mutate(q = cut(POB1,
                  breaks = qPOB1,
                  labels = labels,
                  include.lowest = T))
 
 
 #Municipios
 mun_cdmx <- st_read("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/df/df_municipal.shp")
 
 #Crear variable para filtrar por municipios de interés
 mun_cdmx$mun <- substr(mun_cdmx$CVEGEO, 3, 5)
 
 mun <- rbind(mun_cdmx, mun_edomex) 
 
 ##########################
 ####Mapa
 ##########################
 #Función para determinar theme de las gráficas
 theme_map <- function(...) {
   theme_minimal() +
     theme(
       text = element_text(family = "Verdana",
                           color = "#939486"),
       # remove all axes
       axis.line = element_blank(),
       axis.text.x = element_blank(),
       axis.text.y = element_blank(),
       axis.ticks = element_blank(),
       # add a subtle grid
       panel.grid.major = element_line(color = "#F5F5F3", size = 0.2),
       panel.grid.minor = element_blank(),
       # background colors
       plot.background = element_rect(fill = "#F5F5F3",
                                      color = NA),
       panel.background = element_rect(fill = "#F5F5F3",
                                       color = NA),
       legend.background = element_rect(fill = "#F5F5F3",
                                        color = NA),
       # borders and margins
       plot.margin = unit(c(.5, .5, .2, .5), "cm"),
       panel.border = element_blank(),
       panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
       # titles
       legend.title = element_text(size = 11),
       legend.text = element_text(size = 9,hjust = 1,
                                  color = "#939486"),
       plot.title = element_text(size = 15, hjust = 0.5,
                                 color = "#4B4C47"),
       plot.subtitle = element_text(size = 10, hjust = 0.5,
                                    color = "#939486",
                                    margin = margin(b = -0.1,
                                                    t = -0.1,
                                                    l = 2,
                                                    unit = "cm"),
                                    debug = F),
       # captions
       plot.caption = element_text(size = 7,
                                   hjust = .5,
                                   margin = margin(t = 0.2,
                                                   b = 0,
                                                   unit = "cm"),
                                   color = "#939184"),
       ...
     )
 }

 
 
 ##Va de poblacion
 #####################
 gg +
   # Agrego la capa principal
   geom_sf(
     data = manz,
     aes(
       fill = q
     ),
     color = "white",
     size = 0,
     inherit.aes = FALSE
   ) +
   # Viridis color scale
   scale_fill_viridis(
     option = "viridis",
     name = " ",
     alpha = 0.8, 
     begin = 0.1, 
     end = 0.95,
     discrete = T, # discrete classes
     direction = -1, # oscuro es el más alto, claro/amarillo el más bajo
     guide = guide_legend(
       keyheight = unit(5, units = "mm"),
       title.position = "top",
       reverse = T # El valor más alto hasta arriba
     )) +
   # Utilizar un borde más grueso para los municipios
   geom_sf(
     data = mun,
     fill = "transparent",
     color = "white",
     size = 0.3,
     inherit.aes = FALSE
   ) +
   # Agregar títulos
   labs(x = NULL,
        y = NULL,
        title = "Población por manzanas",
        subtitle = "en la Zona Metropolitana del Valle de México
        ",
        caption = "Fuente: Elaborado por @rojo_neon, con datos del CPV 2010, INEGI.") +
   # Hacer un pequeño zoom
   coord_sf(xlim = c(-99.33, -98.83), ylim = c(19.17,19.8), expand = FALSE) +
   # Finalmente, agregar theme
   theme_map() +
   theme(legend.position = "none")
 
 #Guardar el mapa
 ggsave("Población por manzanas CDMX.png", width = 7)
 

 
#  
#  
# library(rgeos)
# library(rgdal)
# gArea(mun_cdmx) 
# library(raster)
# s <- shapefile("/Users/maximoernestojaramillomolina/Documents/Encuestas/Shape/Ageb y mas 2010/shps/df/df_municipal.shp")
# # shape <- readOGR(dsn = "/Users/maximoernestojaramillomolina/Documents/Encuestas/Shape/Ageb y mas 2010/shps/df",
# #                  layer = "df_municipal.shp")
# # shape <- readOGR(dsn = ".", layer = "~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/mex/mex_municipal.shp")
# # dsn<-"~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/mex/"
# # mun_cdmx_geo <- readOGR(dsn,"mun_mex.shp") 
# s$area <- gArea(s, byid=T) 
# table (s$area)
#  