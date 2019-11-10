######################################################################
#Day 8: Mapa verder | Desigualdad en trabajos de cuidado en CDMX 
#Elaborado por: Máximo Ernesto Jaramillo-Molina 
#               Twitter: @rojo_neon
#               Github: @rojoneon
######################################################################

##############
#Configuración
rm(list = ls())
library(pacman)
# Abrimos las paqueterías con un sólo comando:
p_load(haven, readr, readxl, ggplot2, shiny, tidyverse, knitr,gmodels,foreign)


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

##############
#Directorio de trabajo
setwd ("~/Documents/Data Science/Repos/2019B/Mapas/30DaysMapChallenge/Day 8. Trabajo de cuidados CDMX")
##############




##############
#Abrir capas para mapas
##############


# # relief
# relief <- raster("input/02-relief-ascii.asc") %>%
#   # hide relief outside of Switzerland by masking with country borders
#   mask(country_geo) %>%
#   as("SpatialPixelsDataFrame") %>%
#   as.data.frame() %>%
#   rename(value = layer)




##############
#Municipios
mun_cdmx <- st_read("df_municipio.shp")
#Crear variable para filtrar por municipios de interés
mun_cdmx$mun <- substr(mun_cdmx$CVEGEO, 3, 5)
#Filtrar municipios
# mun_cdmx <- mun_cdmx %>% 
#   filter(mun=="002")


##############
#Manzanas
#La carpeta de manzanas está en otra carpeta diferente al proyecto
manz_cdmx <- st_read("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/df/df_manzanas.shp")

#Crear variable para filtrar por municipios de interés
manz_cdmx$mun <- substr(manz_cdmx$CVEGEO, 3, 5)
# #Filtrar a CDMX
# manz_cdmx <- manz_cdmx %>% 
#   filter(mun=="002")

#Seleccionar variables de interés
manz_cdmx <- manz_cdmx %>% 
  dplyr::select(CVEGEO, mun)


#############
#Leer las tablas complementarias
#############
#Vivienda
complemen_viv <- read.dbf("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/df/tablas/df_cpv2010_manzanas_caracteristicas_economicas.dbf")
complemen_viv <- complemen_viv %>% 
  dplyr::select(CVEGEO, ECO37_R) #A nivel manzana, solo está el dato de % total de pob dedicada a que haceres del hogar

#Pero... la mayoría de quienes se dedican a que haceres del hogar, son mujeres, ¿no?
#Como dice Mr. Peanutbutter: Let's find out!

complemen_viv_estatal <- read.dbf("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/df/tablas/df_cpv2010_estatal_caracteristicas_economicas.dbf")
glimpse(complemen_viv_estatal$ECO38_R)
glimpse(complemen_viv_estatal$ECO39_R)
#20 veces más es la probabilidad de hablar de una mujeres que de un hombre que se dedica a que haceres del hogar
#Seguiremos llamando el mapa como "de mujeres"

#Filtro de datos perdidos, registrados como negativos
complemen_viv <- complemen_viv %>%
  filter(ECO37_R!=-9 & ECO37_R!=-8 & ECO37_R!=-6 )

#Unir bases
manz_complemen<-merge(x=manz_cdmx,      y=complemen_viv,by="CVEGEO")



#####################
##Mapas con cuantiles
#####################

# ¿Cuántas clases quiero?
no_classes <- 5
# Extraer cuantiles
cuantil <- manz_complemen %>%
  pull(ECO37_R) %>%
  quantile(probs = seq(0, 1, length.out = no_classes + 1)) %>%
  as.vector() # to remove names of quantiles, so idx below is numeric
# Así se crean las etiquetas
labels <- imap_chr(cuantil, function(., idx){
  return(paste0(round(cuantil[idx] , 0),
                "%",
                " – ",
                round(cuantil[idx + 1] , 0),
                "%"))
})

# Se elimina la última etiqueta
# En caso contrario sería hasta NA
labels <- labels[1:length(labels) - 1]
labels
# Crear la variable
#manz_complemen %<>%
manz_complemen <- manz_complemen %>%
  mutate(q_cuidado = cut(ECO37_R,
                     breaks = cuantil,
                     labels = labels,
                     include.lowest = T))




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



##Va el mapa de educación
#####################
ggplot(data = manz_complemen) +
  # Agrego la capa principal
  geom_sf(
    mapping = aes(
      fill = q_cuidado
    ),
    color = "white",
    size = 0
  ) +
  # Viridis color scale
  scale_fill_viridis(
    option = "viridis",
    name = " ",
    alpha = 0.8, 
    begin = 0.6, 
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
    data = mun_cdmx,
    fill = "transparent",
    color = "white",
    size = 0.5
  ) +
  # Agregar títulos
  labs(x = NULL,
       y = NULL,
       title = "Desigualdad de trabajos de cuidado",
       subtitle = "% de personas dedicadas a 'que haceres del hogar', CDMX",
       caption = "Fuente: Elaborado por @rojo_neon, con datos del CPV 2010, INEGI.") +
  # Hacer un pequeño zoom
  coord_sf(xlim = c(-99.35, -98.9), ylim = c(19.17,19.6), expand = FALSE) +
  # Finalmente, agregar theme
  theme_map()

#Guardar el mapa
ggsave("Cuidados CDMX.png", width = 7)


# # relief
# ggplot()+
#   geom_raster(
#   data = relief,
#   inherit.aes = FALSE,
#   aes(
#     x = x,
#     y = y,
#     alpha = value
#   ))
# 
# 
# 
# 
# 
