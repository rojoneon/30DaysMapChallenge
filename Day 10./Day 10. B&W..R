######################################################################
#Day 10: Blanco y negro | Segregación social en 4 ciuades mexicanas
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
setwd ("~/Documents/Data Science/Repos/2019B/Mapas/30DaysMapChallenge/Day 10.")
##############


#######################################################
#######################################################
########################GDL############################
#######################################################
#######################################################


##############
#Abrir capas para mapas
##############

##############
#Municipios
mun_jal <- st_read("~/Documents/Data Science/Repos/2019B/Mapas/30DaysMapChallenge/Day 2 & 6. Segregación social en GDL/jal_municipal.shp")

#Crear variable para filtrar por municipios de interés
mun_jal$mun <- substr(mun_jal$CVEGEO, 3, 5)
#Filtrar a ZMG
mun_jal <- mun_jal %>% 
  filter(mun=="039" | mun=="045" | mun=="070" | mun=="097" | mun=="098" | mun=="101" | mun=="120" | mun=="124")


##############
#AGEBs
ageb_jal <- st_read("~/Documents/Data Science/Repos/2019B/Mapas/30DaysMapChallenge/Day 2 & 6. Segregación social en GDL/jal_ageb_urb.shp")

#Crear variable para filtrar por municipios de interés
ageb_jal$mun <- substr(ageb_jal$CVEGEO, 3, 5)
#Filtrar a ZMG
ageb_jal <- ageb_jal %>% 
  filter(mun=="039" | mun=="070" | mun=="097" | mun=="101" | mun=="098" | mun=="120")

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
  dplyr::filter(mun=="039" | mun=="070" | mun=="097" | mun=="101" | mun=="098" | mun=="120")%>%
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


##Mapas con cuantiles
#####################

# ¿Cuántas clases quiero?
no_classes <- 2
# Extraer cuantiles
qPC1 <- ageb_jal %>%
  pull(PC1) %>%
  quantile(probs = seq(0, 1, length.out = no_classes + 1)) %>%
  as.vector() # to remove names of quantiles, so idx below is numeric
# Así se crean las etiquetas
labels <- c("50% más pobre","50% más rico")

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


# Datos para nombres de municipios
mun_jal <- cbind(mun_jal, st_coordinates(st_centroid(mun_jal)))



##Va blanco y negro
#####################
ggplot(data = ageb_jal) +
  # Agrego la capa principal
  geom_sf(
    mapping = aes(
      fill = q
    ),
    color = "black",
    size = 0.1
  ) +
  # color scale
  scale_fill_manual(values=c("#ffffff","#4B4C47")) +
  # Utilizar un borde más grueso para los municipios
  geom_sf(
    data = mun_jal,
    fill = "transparent",
    color = "black",
    size = 1
  ) +
  #Etiquetas de municipios
  #geom_label(data = mun_jal, aes(X, Y, label = NOMBRE), size = 1.5, fontface = "bold")+ 
  # Agregar títulos
  labs(x = NULL,
       y = NULL,
       title = "¿Dónde viven los ricos y pobres en GDL?",
       subtitle = "Según índice de estratificació social",
       caption = "Fuente: Elaborado por @rojo_neon, con datos del Censo 2010.",
       fill = "Estratos (2)") +
  # Hacer un pequeño zoom
  coord_sf(xlim = c(-103.55, -103.15), ylim = c(20.45,20.83), expand = FALSE) +
  # Finalmente, agregar theme
  theme_map()

#Guardar el mapa
ggsave("B&W_GDL.png", width = 7)




#######################################################
#######################################################
#######################CDMX############################
#######################################################
#######################################################


##############
#Abrir capas para mapas
##############

##############
#Municipios
mun_cdmx <- st_read("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/df/df_municipal.shp")

#Crear variable para filtrar por municipios de interés
mun_cdmx$mun <- substr(mun_cdmx$CVEGEO, 3, 5)

##############
#AGEBs
ageb_cdmx <- st_read("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/df/df_ageb_urb.shp")

#Crear variable para filtrar por municipios de interés
ageb_cdmx$mun <- substr(ageb_cdmx$cvegeo, 3, 5)

#Seleccionar variables de interés
ageb_cdmx <- ageb_cdmx %>% 
  dplyr::select(cvegeo, mun)

##############
#Datos complementarios
#Leer los datos complementarios
datos_salud <- read.dbf("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/df/tablas/df_cpv2010_ageb_urb_servicios_de_salud.dbf")
datos_viv <- read.dbf("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/df/tablas/df_cpv2010_ageb_urb_viviendas.dbf")
datos_edu <- read.dbf("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/df/tablas/df_cpv2010_ageb_urb_caracteristicas_educativas.dbf")
datos_complemen_cdmx <-merge(x=datos_salud,               y=datos_viv,by="CVEGEO")
datos_complemen_cdmx <-merge(x=datos_complemen_cdmx,      y=datos_edu,by="CVEGEO")

#Seleccionar variables para PCA
datos_complemen_cdmx$mun <- substr(datos_complemen_cdmx$CVEGEO, 3, 5)
datos_complemen_cdmx <- datos_complemen_cdmx %>% 
  dplyr::select(VIV5_R, EDU49_R, SALUD5_R, VIV36_R)

library(corrplot) #Librería para el gráfico de correlaciones
library(corrr)

solo_var<- datos_complemen_cdmx
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
ageb_cdmx <- cbind(ageb_cdmx, solo_var_pcs)



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


##Mapas con cuantiles
#####################

# ¿Cuántas clases quiero?
no_classes <- 2
# Extraer cuantiles
qPC1 <- ageb_cdmx %>%
  pull(PC1) %>%
  quantile(probs = seq(0, 1, length.out = no_classes + 1)) %>%
  as.vector() # to remove names of quantiles, so idx below is numeric
# Así se crean las etiquetas
# labels <- c("50% más pobre","50% más rico")
labels <- c("50% más rico","50% más pobre")
# Se elimina la última etiqueta
# En caso contrario sería hasta NA
# labels <- labels[1:length(labels) - 1]
labels
# Crear la variable
#manz_complemen %<>%
ageb_cdmx <- ageb_cdmx %>%
  mutate(q = cut(PC1,
                 breaks = qPC1,
                 labels = labels,
                 include.lowest = T))


# Datos para nombres de municipios
mun_cdmx <- cbind(mun_cdmx, st_coordinates(st_centroid(mun_cdmx)))



##Va blanco y negro
#####################
ggplot(data = ageb_cdmx) +
  # Agrego la capa principal
  geom_sf(
    mapping = aes(
      fill = q
    ),
    color = "black",
    size = 0.1
  ) +
  # color scale
  scale_fill_manual(values=c("#4B4C47" ,"#ffffff" )) +
  # Utilizar un borde más grueso para los municipios
  geom_sf(
    data = mun_cdmx,
    fill = "transparent",
    color = "black",
    size = 1
  ) +
  #Etiquetas de municipios
  #geom_label(data = mun_cdmx, aes(X, Y, label = NOMBRE), size = 1.5, fontface = "bold")+ 
  # Agregar títulos
  labs(x = NULL,
       y = NULL,
       title = "¿Dónde viven los ricos y pobres en CDMX?",
       subtitle = "Según índice de estratificació social",
       caption = "Fuente: Elaborado por @rojo_neon, con datos del Censo 2010.",
       fill = "Estratos (2)") +
  # Finalmente, agregar theme
  theme_map()

#Guardar el mapa
ggsave("B&W_CDMX.png", width = 7)

