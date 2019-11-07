########################################################
#Day 2 y 6: Mapa de polígonos y azul | Segregación social en Guadalajara
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
#install.packages("tidyverse")

##############
#Paqueterías para mapear
library(sf)
#install.packages("ggspatial")
library("ggspatial")
theme_set(theme_bw())
library("colorspace")
#hcl_palettes(plot = TRUE)

##############
#Directorio de trabajo
setwd ("~/Documents/Data Science/Repos/2019B/Mapas/Mapas/Day 2 & 6. Segregación social en GDL/")
##############

##############
#Abrir capas para mapas
##############

##############
#Municipios
mun_jal <- st_read("jal_municipal.shp")

#Crear variable para filtrar por municipios de interés
mun_jal$mun <- substr(mun_jal$CVEGEO, 3, 5)
#Filtrar a ZMG
mun_jal <- mun_jal %>% 
  filter(mun=="039" | mun=="045" | mun=="070" | mun=="097" | mun=="098" | mun=="101" | mun=="120" | mun=="124")


##############
#AGEBs
ageb_jal <- st_read("jal_ageb_urb.shp")

#Crear variable para filtrar por municipios de interés
ageb_jal$mun <- substr(ageb_jal$CVEGEO, 3, 5)
#Filtrar a ZMG
ageb_jal <- ageb_jal %>% 
  filter(mun=="039" | mun=="097" | mun=="101" | mun=="098" | mun=="120")

#Seleccionar variables de interés
ageb_jal_2 <- ageb_jal %>% 
  dplyr::select(CVEGEO, mun)

#Leer los datos complementarios
datos_complemen <- read_dta("jal_ageb_urb.dta")

#Unir bases
ageb_complemen<-merge(x=ageb_jal_2,y=datos_complemen,by="CVEGEO")

#Seleccionar variables de interés
ageb_jal_2 <- ageb_complemen %>% 
  dplyr::select(CVEGEO, mun, viv5_r, edu49_r, salud5_r, viv36_r)

#Filtro de datos perdidos, registrados como negativos
#habitantes promeedio por cuarto
ageb_jal_2 <- ageb_jal_2 %>%
  filter(viv5_r!=-9 & viv5_r!=-8 & viv5_r!=-6 )
#años promedio de educación
ageb_jal_2 <- ageb_jal_2 %>%
  filter(edu49_r!=-9 & edu49_r!=-8 & edu49_r!=-6 )
#porcentaje de hogares con Seguro Popular
ageb_jal_2 <- ageb_jal_2 %>%
  filter(salud5_r!=-9 & salud5_r!=-8 & salud5_r!=-6 )
#porceentaje de hogares con internet
ageb_jal_2 <- ageb_jal_2 %>%
  filter(viv36_r!=-9 & viv36_r!=-8 & viv36_r!=-6 )


#Mapa simpl
ggplot () +
  geom_sf(data = ageb_jal_2, aes(fill = salud5_r), lwd = 0) +
  scale_fill_gradientn(colors = viridis::viridis(20))
table(ageb_jal_2$viv36_r)

# Datos para nombres de municipios
mun_jal <- cbind(mun_jal, st_coordinates(st_centroid(mun_jal)))


#I - Educación
ggplot () +
  theme_void()+
  geom_sf(data = ageb_jal_2, mapping =  aes(fill = edu49_r), lwd = 0) +
  geom_sf(data = mun_jal, fill = NA) +
  geom_label(data = mun_jal, aes(X, Y, label = NOMBRE), size = 1.5, fontface = "bold") +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Segregación espacial en la ZMG / I", subtitle = "Años promedio de educación") +
  theme(text = element_text(family = "Verdana", color = "white"),
        panel.grid = element_blank(),
        plot.margin = unit(c(5,5,5,5), units = "point"),
        panel.background = element_rect(fill = "black", linetype = "blank"),
        plot.background = element_rect(fill = "black", linetype = "blank"),
        plot.subtitle =  element_text(size = 7),
        plot.caption = element_text(hjust = 0.5 ,size = 6)) +
  labs ( caption = "Fuente: Elaborado por @rojo_neon, con datos del Censo 2010.",
         fill = "Años") +
  coord_sf(xlim = c(-103.55, -103.15), ylim = c(20.45,20.83), expand = FALSE) 

ggsave("Segre_ZMG_I.png", width = 7)

#II - Nivel de Hacinamiento
ggplot () +
  theme_void()+
  geom_sf(data = ageb_jal_2, mapping =  aes(fill = viv5_r), lwd = 0) +
  geom_sf(data = mun_jal, fill = NA) +
  geom_label(data = mun_jal, aes(X, Y, label = NOMBRE), size = 1.5, fontface = "bold")+ 
  scale_color_brewer(palette = "Blues") +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Segregación espacial en la ZMG / II", subtitle = "Promedio de ocupantes por cuarto") +
  theme(text = element_text(family = "Verdana", color = "white"),
        panel.grid = element_blank(),
        plot.margin = unit(c(5,5,5,5), units = "point"),
        panel.background = element_rect(fill = "black", linetype = "blank"),
        plot.background = element_rect(fill = "black", linetype = "blank"),
        plot.subtitle =  element_text(size = 7),
        plot.caption = element_text(hjust = 0.5 ,size = 6)) +
  labs ( caption = "Fuente: Elaborado por @rojo_neon, con datos del Censo 2010.",
         fill = "Promedio") +
  coord_sf(xlim = c(-103.55, -103.15), ylim = c(20.45,20.83), expand = FALSE) 

ggsave("Segre_ZMG_II.png", width = 7)


#III - % de hogares con Seg Pop
ggplot () +
  theme_void()+
  geom_sf(data = ageb_jal_2, mapping =  aes(fill = salud5_r), lwd = 0) +
  scale_color_hue(direction = -1, h.start=90) +
  geom_sf(data = mun_jal, fill = NA) +
  geom_label(data = mun_jal, aes(X, Y, label = NOMBRE), size = 1.5, fontface = "bold")+ 
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Segregación espacial en la ZMG / III", subtitle = "Porcentaje de hogares con Seguro Popular") +
  theme(text = element_text(family = "Verdana", color = "white"),
        panel.grid = element_blank(),
        plot.margin = unit(c(5,5,5,5), units = "point"),
        panel.background = element_rect(fill = "black", linetype = "blank"),
        plot.background = element_rect(fill = "black", linetype = "blank"),
        plot.subtitle =  element_text(size = 7),
        plot.caption = element_text(hjust = 0.5 ,size = 6)) +
  labs ( caption = "Fuente: Elaborado por @rojo_neon, con datos del Censo 2010.",
         fill = "%") +
  coord_sf(xlim = c(-103.55, -103.15), ylim = c(20.45,20.83), expand = FALSE) 

ggsave("Segre_ZMG_III.png", width = 7)


#IV - % de hogares con  internet
ggplot () +
  theme_void()+
  geom_sf(data = ageb_jal_2, mapping =  aes(fill = viv36_r), lwd = 0) +
  scale_color_hue(direction = -1, h.start=90) +
  geom_sf(data = mun_jal, fill = NA) +
  geom_label(data = mun_jal, aes(X, Y, label = NOMBRE), size = 1.5, fontface = "bold")+ 
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Segregación espacial en la ZMG / IV", subtitle = "Porcentaje de hogares con acceso a internet") +
  theme(text = element_text(family = "Verdana", color = "white"),
        panel.grid = element_blank(),
        plot.margin = unit(c(5,5,5,5), units = "point"),
        panel.background = element_rect(fill = "black", linetype = "blank"),
        plot.background = element_rect(fill = "black", linetype = "blank"),
        plot.subtitle =  element_text(size = 7),
        plot.caption = element_text(hjust = 0.5 ,size = 6)) +
  labs ( caption = "Fuente: Elaborado por @rojo_neon, con datos del Censo 2010.",
         fill = "%") +
  coord_sf(xlim = c(-103.55, -103.15), ylim = c(20.45,20.83), expand = FALSE) 

ggsave("Segre_ZMG_IV.png", width = 7)
