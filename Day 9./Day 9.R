######################################################################
#Day 9: Mapa amarillo | ¿Queda algo del PRD?
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
#hcl_palettes(plot = TRUE)
p_load(rstudioapi, lintr, raster, viridis, cowplot, rmarkdown)
sessionInfo()

##############
#Directorio de trabajo
setwd ("~/Documents/Data Science/Repos/2019B/Mapas/30DaysMapChallenge/Day 9.")
##############

##############
#Pegado de datos de presidencias municipales
##############
#Shape Nacional
nac <- st_read("nacional.shp")

#Shape Municipal
mun_nac <- st_read("municipal.shp")

mun_nac <- mun_nac %>%
  dplyr::select(CVEGEO,NOM_ENT,NOM_MUN)
glimpse(mun_nac)
head(mun_nac)

#Datos de presidentes municipales
pres_nac <- read.csv(file = "~/Documents/Data Science/Repos/2019B/Mapas/30DaysMapChallenge/Day 9./Presi Mun/csv municipios/pres_nac.csv", sep=",") 

#Unir ambas bases
mun_nac<-merge(x=mun_nac,      y=pres_nac,by=c("NOM_MUN","NOM_ENT"))

#¿Cuántos partidos hay?
cro(mun_nac$partido)

#Generar variable con principales partidos:
mun_nac <- mun_nac %>%
  mutate(partido_princ = case_when (
                          partido == "PRI" ~ 1,
                          partido == "PAN" ~ 2,
                          partido == "MORENA" ~ 3,
                          partido == "PRD" ~ 4,
                          partido == "MC" ~ 5,
                          partido == "PVEM" ~ 6,
                          partido == "PANAL" ~ 7,
                          partido == "PES" ~ 8,
                          partido == "PT" ~ 9,TRUE ~ 99))
mun_nac <- mun_nac %>%
  mutate(partido_princ=     factor(partido_princ,
                            levels = c(1,2,3,4,5,6,7,8,9,99),
                            labels = c("PRI", "PAN", "MORENA", "PRD", "MC", "PVEM", "PANAL", "PES", "PT", "Otros")))
         
table(mun_nac$partido_princ)

#Dicotómica sobre el PRD
mun_nac <- mun_nac %>%
  mutate(PRD = case_when (partido == "PRD" ~ 1, TRUE ~ 0))
library(naniar)
mun_nac <- mun_nac %>% 
  replace_with_na(replace = list(PRD = 0))

glimpse(mun_nac)  
cro(mun_nac$partido,mun_nac$PRD)
class(mun_nac$PRD)
mun_nac$PRD <- as.factor(mun_nac$PRD)

##########################
####Mapas
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

fishualize()



##########Mapa PRD
##
#####################
ggplot(data = mun_nac) +
  # Agrego la capa principal
  geom_sf(
    mapping = aes(
      fill = PRD
    ),
    color = "white",
    size = 0
  ) +
  #Fishualize
  scale_fill_fish_d(option = "Chaetodon_pelewensis") +
  # Utilizar un borde más grueso para los municipios
  geom_sf(
      data = nac,
      fill = "transparent",
      color = "black",
      size = 0.5
    ) +
  # Agregar títulos
  labs(x = NULL,
       y = NULL,
       title = "Municipios donde gobierna PRD",
       subtitle = "2019",
       caption = "Fuente: Elaborado por @rojo_neon.") +
  # Finalmente, agregar theme
  theme_map()

#Guardar el mapa
ggsave("PRD.png", width = 7)


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


##########Mapa Partidos (todos)
ggplot(data = mun_nac) +
  # Agrego la capa principal
  geom_sf(
    mapping = aes(
      fill = partido_princ
    ),
    color = "white",
    size = 0
  ) +
  #Colores
  scale_fill_manual(values=c("#ff0000", "#3333cc", "#990000", "#ffff00", "#ff9900", "#339900", "#339999", "#663399", "#cc3300", "#cccccc")) +
  # Utilizar un borde más grueso para los municipios
  geom_sf(
    data = nac,
    fill = "transparent",
    color = "white",
    size = 1
  ) +
  # Agregar títulos
  labs(x = NULL,
       y = NULL,
       title = "Partidos gobernantes de municipios",
       subtitle = "2019",
       caption = 
       "Fuente: Elaborado por @rojo_neon.
       Nota: Sin datos en blanco",
       fill = "Partido") +
  # Finalmente, agregar theme
  theme_map()

#Guardar el mapa
ggsave("Partidos.png", width = 7)







