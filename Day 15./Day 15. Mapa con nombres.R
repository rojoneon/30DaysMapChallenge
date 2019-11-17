######################################################################
#Day 14: Mapa con límites | La calzada independencia como límite de dos ciudades en Guadalajara
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
setwd ("~/Documents/Data Science/Repos/2019B/Mapas/30DaysMapChallenge/Day 15.")
##############



#Llave Google
# register_google(key="")

#Stamen map GDL
# mapa_GDL <- get_stamenmap(c(left = -103.4, bottom = 20.6, right = -103.26, top = 20.76),
#                           #source = "osm",
#                           #scale = 9, 
#                           zoom = 13, 
#                           maptype="terrain",
#                           color="bw",
#                           force=TRUE)
# ggmap(mapa_GDL)



##Calles GDL
gdl_calles <- st_read("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/jal/jal_eje_vial.shp")

table(gdl_calles$TIPOVIAL)
#Crear variable para filtrar por municipios de interés
gdl_calles$mun <- substr(gdl_calles$CVEGEO, 3, 5)
#Filtrar a ZMG
gdl_calles <- gdl_calles %>% 
  filter(mun=="039")


table(gdl_calles$TIPOVIAL)

#Aislar calzada independencia
gdl_calles_calzada <- gdl_calles %>%
  filter(TIPOVIAL== "CALZADA")

calzadas_fed_y_norte <- gdl_calles_calzada %>%
  filter(TIPOVIAL== "CALZADA" & (NOMVIAL=="DEL FEDERALISMO SUR" | NOMVIAL== "INDEPENDENCIA NORTE"))

calzadas_sur <- gdl_calles %>%
  filter(OID=="19722" | OID=="04166" )

calzadas_sur_2 <- gdl_calles %>%
  filter(OID=="12513")

calzadas_sur_3 <- gdl_calles %>%
  filter(OID=="15235")



ggplot()+
  geom_sf(data=calzadas,
          inherit.aes =FALSE,
          colour="#238443",
          fill="#004529",
          alpha=.5,
          size=4,
          shape=21) 
  

#Aislar av colón
gdl_calles_avenida <- gdl_calles %>%
  filter(TIPOVIAL== "AVENIDA")

colon <- gdl_calles_avenida %>%
  filter(TIPOVIAL== "AVENIDA" & (NOMVIAL=="CRISTÓBAL COLÓN" | NOMVIAL== "JUÁREZ"))

ggplot()+
  geom_sf(data=colon,
          inherit.aes =FALSE,
          colour="#238443",
          fill="#004529",
          alpha=.5,
          size=4,
          shape=21) 


##############
#Municipios
mun_jal <- st_read("~/Documents/Data Science/Repos/2019B/Mapas/30DaysMapChallenge/Day 2 & 6. Segregación social en GDL/jal_municipal.shp")

#Crear variable para filtrar por municipios de interés
mun_jal$mun <- substr(mun_jal$CVEGEO, 3, 5)
#Filtrar a ZMG
mun_jal <- mun_jal %>% 
  filter(mun=="039" | mun=="045" | mun=="070" | mun=="097" | mun=="098" | mun=="101" | mun=="120" | mun=="124")


ggplot() +
  #ggmap(mapa_GDL)+
  # geom_sf(data=calzadas,
  #         inherit.aes =FALSE,
  #         colour="#238443",
  #         fill="#004529",
  #         alpha=.5,
  #         size=4,
  #         shape=21) +
# Utilizar un borde más grueso para los municipios
  geom_sf(
        data = mun_jal,
        fill = "transparent",
        color = "black",
        size = 1
                ) 





################################################
################################################
################################################

#AGEBs

################################################
################################################
################################################

ageb_jal <- st_read("~/Documents/Data Science/Repos/2019B/Mapas/30DaysMapChallenge/Day 2 & 6. Segregación social en GDL/jal_ageb_urb.shp")

#Crear variable para filtrar por municipios de interés
ageb_jal$mun <- substr(ageb_jal$CVEGEO, 3, 5)
#Filtrar a ZMG
ageb_jal <- ageb_jal %>% 
  filter(mun=="039" | mun=="045" | mun=="070" | mun=="097" | mun=="098" | mun=="101" | mun=="120" | mun=="124")

#Seleccionar variables de interés
ageb_jal <- ageb_jal %>% 
  dplyr::select(CVEGEO, mun)

##############
#Datos complementarios
#Leer los datos complementarios
datos_complemen <- read_dta("~/Documents/Data Science/Repos/2019B/Mapas/30DaysMapChallenge/Day 2 & 6. Segregación social en GDL/jal_ageb_urb.dta")

#Seleccionar variables para PCA
datos_complemen$mun <- substr(datos_complemen$CVEGEO, 3, 5)
solo_var <- datos_complemen %>% 
  dplyr:: filter(mun=="039" | mun=="045" | mun=="070" | mun=="097" | mun=="098" | mun=="101" | mun=="120" | mun=="124")%>%
  dplyr::select(viv5_r, edu49_r, salud5_r, viv36_r)

library(corrplot) #Librería para el gráfico de correlaciones
library(corrr)

matriz_correlaciones <- cor(solo_var, use = "pairwise.complete.obs")
matriz_correlaciones
corrplot(cor(solo_var), order = "hclust", tl.col='black', tl.cex=1) #Gráfico de las correlaciones
estudiantes_solo_var <- correlate(solo_var)  #Cálculo de un objeto de correlaciones
rplot(estudiantes_solo_var, legend = TRUE, colours = c("firebrick1", "black", 
                                                       "darkcyan"), print_cor = TRUE)  #Opción gráfica de las correlaciones
#Puntuaciones
solo_var_pca <- prcomp(solo_var,scale=T) # Scale = True -> utilizamos la matriz de CORRELACIÓN para obtener las componentes! 
summary(solo_var_pca)
plot(solo_var_pca)
solo_var_pcs <- solo_var_pca$x

# Unir bases
ageb_jal <- cbind(ageb_jal, solo_var_pcs)





##Mapas con cuantiles
#####################

# ¿Cuántas clases quiero?
no_classes <- 4
# Extraer cuantiles
qPC1 <- ageb_jal %>%
  pull(PC1) %>%
  quantile(probs = seq(0, 1, length.out = no_classes + 1)) %>%
  as.vector() # to remove names of quantiles, so idx below is numeric
# Así se crean las etiquetas
labels <- c("Bajo","Medio Bajo", "Medio", "Medio Alto")

# Se elimina la última etiqueta
# En caso contrario sería hasta NA
# labels <- labels[1:length(labels) - 1]
labels
# Crear la variable
#manz_complemen %<>%
ageb_jal <- ageb_jal %>%
  mutate(q = cut(PC1,
                 breaks = qPC1,
                 labels = labels,
                 include.lowest = T))



################################################
################################################
################################################

#Mapa

################################################
################################################
################################################

         

# Datos para nombres de municipios
mun_jal <- cbind(mun_jal, st_coordinates(st_centroid(mun_jal)))



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



##Va blanco y negro
#####################

ggplot(data = ageb_jal) +
#ggplot() +
  #ggmap(mapa_GDL)+
  # Utilizar un borde más grueso para los municipios
  # geom_sf(
  #   data = mun_jal,
  #   fill = "transparent",
  #   color = "black",
  #   size = 1
  # ) + 
# ggplot(data = ageb_jal) +
  # Agrego la capa principal
  geom_sf(
    mapping = aes(
      fill = q
    ),
    color = "black",
    size = 0.1,
    alpha=.9
  ) +
  # color scale
  scale_fill_manual(values=c("#D72638","#F49D37","#3F88C5","#140F2D")) +
  # Utilizar un borde más grueso para los municipios
  geom_sf(
    data = mun_jal,
    fill = "transparent",
    color = "black",
    size = 1
  ) +
  #Calzada
  geom_sf(data=calzadas_fed_y_norte,
          inherit.aes =FALSE,
          colour="white",
          fill="white",
          alpha=.5,
          size=1,
          shape=21) +
  # geom_sf(data=calzadas_sur,
  #         inherit.aes =FALSE,
  #         colour="white",
  #         fill="white",
  #         alpha=.5,
  #         size=1,
  #         shape=21) +
  # geom_sf(data=calzadas_sur_2,
  #         inherit.aes =FALSE,
  #         colour="white",
  #         fill="white",
  #         alpha=.5,
  #         size=1,
  #         shape=21) +
  # geom_sf(data=calzadas_sur_3,
  #         inherit.aes =FALSE,
  #         colour="white",
  #         fill="white",
  #         alpha=.5,
  #         size=1,
  #         shape=21) +
  geom_sf(data=colon,
          inherit.aes =FALSE,
          colour="white",
          fill="white",
          alpha=.5,
          size=1,
          shape=21) +

  #Etiquetas de municipios
  geom_label(data = mun_jal, aes(X, Y, label = NOMBRE), size = 1.5, fontface = "bold")+ 
  # Agregar títulos
  labs(x = NULL,
       y = NULL,
       title = "De la Calzada para allá...",
       subtitle = "Según índice de estratificación social",
       caption = "Fuente: Elaborado por @rojo_neon, con datos del Censo 2010.",
       fill = "Estratos") +
  # Hacer un pequeño zoom
  coord_sf(xlim = c(-103.52, -103.18), ylim = c(20.47,20.845), expand = FALSE) +
    # Finalmente, agregar theme
  theme_map()


#Guardar el mapa
ggsave("Calzada.png", width = 7)
