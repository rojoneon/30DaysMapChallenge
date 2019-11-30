######################################################################
#Day 27: Recursos | Minas en Suroeste de México
#Elaborado por: Máximo Ernesto Jaramillo-Molina 
#               Twitter: @rojo_neon
#               Github: @rojoneon
######################################################################


##############
#Configuración
rm(list = ls())

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
setwd ("~/Documents/Data Science/Repos/2019B/Mapas/30DaysMapChallenge/Day 27.")
##############

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
      legend.text = element_text(size = 9, hjust = 0,
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



#Capa de municipios
mun <- st_read("~/Documents/Encuestas/Shape/CONTINUO_NACIONAL/ESTADOS.shp")
table(mun$NOM_ENT)
mun <- mun %>%
    filter(NOM_ENT %in% c("Oaxaca", "Guerrero", "Puebla", 
                           "México", "Morelos", "Michoacán de Ocampo", "Tlaxcala",
                          "Hidalgo", "Distrito Federal", "Veracruz de Ignacio de la Llave",
                          "Guanajuato", "Jalisco", "Querétaro", "San Luis Potosí",
                          "Aguascalientes", "Zacatecas", "Nayarit", "Colima"))


#####Capa de Minas
#Descargar de aquí: https://www.inegi.org.mx/app/descarga/
minas <- st_read("~/Documents/Encuestas/DENUE/denue_00_21_shp/conjunto_de_datos/denue_inegi_21_.shp")

table(minas$nombre_act)
table(minas$per_ocu)
'%!in%' <- function(x,y)!('%in%'(x,y))
# minas <- minas %>%
#    filter(per_ocu %!in% c("0 a 5 personas", "11 a 30 personas", "31 a 50 personas", 
#                           "6 a 10 personas", "51 a 100 personas"))
table(minas$per_ocu)

library(stringr)

# mun <- mun %>%
#   filter(str_detect(nombre_act, "^Miner"))
# 
#   filter(NOM_ENT %!in% starts_with("Miner"))
# 
#   filter(NOM_ENT %!in% c("Otros servicios relacionados con la miner\xeda", "Otros servicios relacionados con la minería"))
table(minas$nombre_act)                        

ggplot() +
  geom_point(
    data=minas,
    aes(x=longitud, y=latitud)
  )

ggplot()+
  geom_sf(
  data = mun,
  fill = "transparent",
  color = "black",
  size = .5,
  inherit.aes = FALSE
)

#Capa de elevación terrain

#register_google(key="CLAVE")
s <- "element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative%7Celement:geometry%7Cvisibility:off&style=feature:administrative.country%7Celement:geometry.stroke%7Ccolor:0x000000%7Cvisibility:on&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Cvisibility:off&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road%7Celement:labels.icon%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit%7Cvisibility:off&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x4BA9C5&size=480x360"
# Get the basemap
mexico <- get_googlemap(
  #"Mexico" ,
  c(lon=-100,lat=17.5),
  zoom = 6, crop=T, 
  scale = 2, 
  extent = "panel",
  #color="bw", 
  maptype="terrain", # can change to terrain
  style = s,
  force = T
)

ggmap(mexico)  

gg <- ggmap(mexico) 





#####################
##Mapa 1: minas y elevación
#####################

gg + 
  
  # Utilizar un borde más grueso para los municipios
  geom_sf(
    data = mun,
    fill = "transparent",
    color = "black",
    size = .5,
    #alpha=0.8,
    inherit.aes = FALSE
  ) +
  
  # Capa de minas
  geom_point(
    data=minas,
    aes(x=longitud, y=latitud),
    size=1, 
    alpha=0.6,
    inherit.aes = FALSE) +
  
  # Viridis color scale
  scale_color_manual(values=c("black")) +
  

  # Agregar títulos
  labs(x = NULL,
       y = NULL,
       title = "Minas en México",
       subtitle = "y elevación del terreno
       ",
       caption = "Fuente: Elaborado por @rojo_neon con datos de Denue de INEGI."
  ) +
  # Hacer un pequeño zoom
  coord_sf(xlim = c(-104, -94), ylim = c(16,21), expand = FALSE) +
  # Finalmente, agregar theme
  theme_map()


#Guardar el mapa
ggsave("minas_estados.png", width = 7)




