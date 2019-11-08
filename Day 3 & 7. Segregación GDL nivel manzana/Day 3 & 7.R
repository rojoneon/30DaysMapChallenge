######################################################################
#Day 7 (tal vez 3): Mapa rojo | Segregación social en Guadalajara a nivel manzana
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

cat("
library(rstudioapi)
library(lintr) # code linting
library(raster) # raster handling (needed for relief)
library(viridis) # viridis color scale
library(cowplot) # stack ggplots
library(rmarkdown)",
    file = "manifest.R")

# if checkpoint is not yet installed, install it (for people using this
# system for the first time)
if (!require(checkpoint)) {
  if (!require(devtools)) {
    install.packages("devtools", repos = "http://cran.us.r-project.org")
    require(devtools)
  }
  devtools::install_github("RevolutionAnalytics/checkpoint",
                           ref = "v0.3.2", # could be adapted later,
                           # as of now (beginning of July 2017
                           # this is the current release on CRAN)
                           repos = "http://cran.us.r-project.org")
  require(checkpoint)
}
# nolint start
if (!dir.exists("~/.checkpoint")) {
  dir.create("~/.checkpoint")
}

source("manifest.R")
unlink("manifest.R")
sessionInfo()

##############
#Directorio de trabajo
setwd ("~/Documents/Data Science/Repos/2019B/Mapas/30DaysMapChallenge/Day 3 & 7. Segregación GDL nivel manzana")
##############



##############
#Abrir capas para mapas
##############

##############
#Municipios
mun_jal <- st_read("~/Documents/Data Science/Repos/2019B/Mapas/30DaysMapChallenge/Day 2 & 6. Segregación social en GDL/jal_municipal.shp")

#Crear variable para filtrar por municipios de interés
mun_jal$mun <- substr(mun_jal$CVEGEO, 3, 5)
#Filtrar a ZMG
mun_jal <- mun_jal %>% 
  filter(mun=="039" | mun=="045" | mun=="070" | mun=="097" | mun=="098" | mun=="101" | mun=="120" | mun=="124")

##############
#Manzanas
#La carpeta de manzanas está en otra carpeta diferente al proyecto
manz_jal <- st_read("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/jal/jal_manzanas.shp")

#Crear variable para filtrar por municipios de interés
manz_jal$mun <- substr(manz_jal$CVEGEO, 3, 5)
#Filtrar a ZMG
manz_jal <- manz_jal %>% 
  filter(mun=="039" | mun=="070" | mun=="097" | mun=="101" | mun=="098" | mun=="120")

#Seleccionar variables de interés
manz_jal <- manz_jal %>% 
  dplyr::select(CVEGEO, mun)

#############
#Leer las tablas complementarias
#############
#Vivienda
complemen_viv <- read.dbf("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/jal/tablas/jal_cpv2010_manzanas_viviendas.dbf")
complemen_viv <- complemen_viv %>% 
  dplyr::select(CVEGEO, VIV5_R, VIV36_R)

#Educación
complemen_edu <- read.dbf("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/jal/tablas/jal_cpv2010_manzanas_caracteristicas_educativas.dbf")
complemen_edu <- complemen_edu %>% 
  dplyr::select(CVEGEO, EDU49_R)

#Salud
complemen_salud <- read.dbf("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/jal/tablas/jal_cpv2010_manzanas_servicios_de_salud.dbf")
complemen_salud <- complemen_salud %>% 
  dplyr::select(CVEGEO, SALUD5_R)

#Unir bases
manz_complemen<-merge(x=manz_jal,      y=complemen_viv,by="CVEGEO")
manz_complemen<-merge(x=manz_complemen,y=complemen_edu,by="CVEGEO")
manz_complemen<-merge(x=manz_complemen,y=complemen_salud,by="CVEGEO")
rm(complemen_viv,complemen_edu,complemen_salud)

#Tidy Data
#Filtro de datos perdidos, registrados como negativos
#habitantes promeedio por cuarto
# manz_complemen <- manz_complemen %>%
#   filter(VIV5_R!=-9 & VIV5_R!=-8 & VIV5_R!=-6 )
# #años promedio de educación
# manz_complemen <- manz_complemen %>%
#   filter(EDU49_R!=-9 & EDU49_R!=-8 & EDU49_R!=-6 )
# #porcentaje de hogares con Seguro Popular
# manz_complemen <- manz_complemen %>%
#   filter(SALUD5_R!=-9 & SALUD5_R!=-8 & SALUD5_R!=-6 )
# #porceentaje de hogares con internet
# manz_complemen <- manz_complemen %>%
#   filter(VIV36_R!=-9 & VIV36_R!=-8 & VIV36_R!=-6 )


#Mapa simple
# ggplot () +
#   geom_sf(data = manz_complemen, aes(fill = EDU49_R), lwd = 0) +
#   scale_fill_viridis_c(option = "plasma")

# Datos para nombres de municipios
mun_jal <- cbind(mun_jal, st_coordinates(st_centroid(mun_jal)))

# 
# #I - Educación (tema normal)
# ggplot () +
#   theme_void()+
#   geom_sf(data = manz_complemen, mapping =  aes(fill = EDU49_R), lwd = 0) +
#   scale_fill_viridis_c(option = "plasma") +
#   geom_sf(data = mun_jal, fill = NA) +
#   geom_label(data = mun_jal, aes(X, Y, label = NOMBRE), size = 1.5, fontface = "bold") +
#   xlab("Longitud") + ylab("Latitud") +
#   ggtitle("Segregación espacial en la ZMG / I", subtitle = "Años promedio de educación") +
#   theme(text = element_text(family = "Verdana", color = "white"),
#         panel.grid = element_blank(),
#         plot.margin = unit(c(5,5,5,5), units = "point"),
#         panel.background = element_rect(fill = "black", linetype = "blank"),
#         plot.background = element_rect(fill = "black", linetype = "blank"),
#         plot.subtitle =  element_text(size = 7),
#         plot.caption = element_text(hjust = 0.5 ,size = 6)) +
#   labs ( caption = "Fuente: Elaborado por @rojo_neon, con datos del Censo 2010.",
#          fill = "Años") +
#   coord_sf(xlim = c(-103.55, -103.15), ylim = c(20.45,20.83), expand = FALSE) +
#   theme_map()



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

# 
# #I - Educación (theme final)
# ggplot () +
#   geom_sf(data = manz_complemen, mapping =  aes(fill = EDU49_R), lwd = 0) +
#   scale_fill_viridis_c(option = "plasma") +
#   geom_sf(data = mun_jal, fill = NA) +
#   geom_label(data = mun_jal, aes(X, Y, label = NOMBRE), size = 1.5, fontface = "bold") +
#   xlab(" ") + ylab(" ") +
#   ggtitle("Segregación espacial en la ZMG / I", subtitle = "Años promedio de educación") +
#   labs ( caption = "Fuente: Elaborado por @rojo_neon, con datos del Censo 2010.",
#          fill = "Años") +
#   coord_sf(xlim = c(-103.55, -103.15), ylim = c(20.45,20.83), expand = FALSE) +
#   theme_map() 
# 
# ggsave("Segre_ZMG_I.png", width = 7)


##Mapas con cuantiles
#####################

# ¿Cuántas clases quiero?
no_classes <- 6
# Extraer cuantiles
qEDU49_R <- manz_complemen %>%
  pull(EDU49_R) %>%
  quantile(probs = seq(0, 1, length.out = no_classes + 1)) %>%
  as.vector() # to remove names of quantiles, so idx below is numeric
# Así se crean las etiquetas
labels <- imap_chr(qEDU49_R, function(., idx){
  return(paste0(round(qEDU49_R[idx] , 0),
                "años",
                " – ",
                round(qEDU49_R[idx + 1] , 0),
                "años"))
})

# Se elimina la última etiqueta
# En caso contrario sería hasta NA
labels <- labels[1:length(labels) - 1]
labels
# Crear la variable
#manz_complemen %<>%
manz_complemen <- manz_complemen %>%
  mutate(q_edu = cut(EDU49_R,
                        breaks = qEDU49_R,
                        labels = labels,
                        include.lowest = T))



##Va el mapa de educación
#####################
ggplot(data = manz_complemen) +
  # Agrego la capa principal
  geom_sf(
    mapping = aes(
      fill = q_edu
    ),
    color = "white",
    size = 0.1
  ) +
  # Viridis color scale
  scale_fill_viridis(
    option = "magma",
    name = "Años",
    alpha = 0.8, 
    begin = 0.6, 
    end = 0.95,
    discrete = T, # discrete classes
    direction = 1, # oscuro es el más bajo, claro/amarillo el más alto
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = T # El valor más alto hasta arriba
    )) +
  # Utilizar un borde más grueso para los municipios
  geom_sf(
    data = mun_jal,
    fill = "transparent",
    color = "white",
    size = 0.5
  ) +
  # Agregar títulos
  labs(x = NULL,
       y = NULL,
       title = "Segregación espacial Guadalajara / II",
       subtitle = "Años promedio de educación - Zona Metropolitana",
       caption = "Fuente: Elaborado por @rojo_neon, con datos del Censo 2010.") +
  # Hacer un pequeño zoom
  coord_sf(xlim = c(-103.55, -103.15), ylim = c(20.45,20.83), expand = FALSE) +
  # Finalmente, agregar theme
  theme_map()

#Guardar el mapa
ggsave("Segre_ZMG_I.png", width = 7)
