######################################################################
#Day 21: Medio ambiente: Áreas verdes en la CDMX
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
setwd ("~/Documents/Data Science/Repos/2019B/Mapas/30DaysMapChallenge/Day 21.")
##############

##############
#Municipios
mun_cdmx <- st_read("~/Documents/Data Science/Repos/2019B/Mapas/30DaysMapChallenge/Day 8. Trabajo de cuidados CDMX/df_municipio.shp")
#Crear variable para filtrar por municipios de interés
mun_cdmx$mun <- substr(mun_cdmx$CVEGEO, 3, 5)
#Filtrar municipios
# mun_cdmx <- mun_cdmx %>% 
#   filter(mun=="002")



##############
##Áreas verdes CDMX
cdmx_a_verdes <- st_read("cdmx_areas_verdes_2017.shp")

table(cdmx_a_verdes$categoria)
table(cdmx_a_verdes$subcat_sed)

cdmx_a_verdes <- cdmx_a_verdes %>%
  filter(subcat_sed %in%  c("Arboledas", "Cerros", "Colinas", "Deportivos", "Depresiones orográficas",
                            "Jardines públicos", "Parques", "Plazas"))


# cdmx_a_verdes <- cdmx_a_verdes %>%
#   filter(!subcat_sed %in%  c("Camellones centrales y laterales", "Instituciones académicas privadas",
#                              "Terrenos baldíos", "Vialidades", "Instituciones de salud pub/priv",
#                              "Asistencia Social con vegetación CDMX", "Instituciones académicas públicas",
#                              "Jardineras públicas y privadas", "Promontorios", "Unidades habitacionales"))






################################################
################################################
################################################

#AGEBs

################################################
################################################
################################################

ageb_cdmx <- st_read("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/df/df_ageb_urb.shp")

#Crear variable para filtrar por municipios de interés
ageb_cdmx$mun <- substr(ageb_cdmx$cvegeo, 3, 5)
#Filtrar a CDMX
# ageb_jal <- ageb_jal %>% 
#   filter(mun=="039")

#Seleccionar variables de interés
ageb_cdmx <- ageb_cdmx %>% 
  dplyr::select(cvegeo, mun)

##############
#Datos complementarios
#Leer los datos complementarios
datos_complemen <- read_dta("/Users/maximoernestojaramillomolina/Documents/Encuestas/Shape/Ageb y mas 2010/shps/df/df_ageb_urb.dta")


# #Seleccionar variables para PCA
# datos_complemen$mun <- substr(datos_complemen$cvegeo, 3, 5)
# solo_var <- datos_complemen %>% 
#   dplyr::select(viv5_r, edu49_r, salud5_r, viv36_r)

# 
# 
# library(corrplot) #Librería para el gráfico de correlaciones
# library(corrr)
# 
# matriz_correlaciones <- cor(solo_var, use = "pairwise.complete.obs")
# matriz_correlaciones
# corrplot(cor(solo_var), order = "hclust", tl.col='black', tl.cex=1) #Gráfico de las correlaciones
# estudiantes_solo_var <- correlate(solo_var)  #Cálculo de un objeto de correlaciones
# rplot(estudiantes_solo_var, legend = TRUE, colours = c("firebrick1", "black", 
#                                                        "darkcyan"), print_cor = TRUE)  #Opción gráfica de las correlaciones
# #Puntuaciones
# solo_var_pca <- prcomp(solo_var,scale=T) # Scale = True -> utilizamos la matriz de CORRELACIÓN para obtener las componentes! 
# summary(solo_var_pca)
# plot(solo_var_pca)
# solo_var_pcs <- solo_var_pca$x
# 
# # Unir bases
# ageb_jal <- cbind(ageb_jal, solo_var_pcs)





##Mapas con cuantiles
#####################

# ¿Cuántas clases quiero?
no_classes <- 5
# Extraer cuantiles
qPC1 <- datos_complemen %>%
  pull(i_marg) %>%
  quantile(na.rm=T, probs = seq(0, 1, length.out = no_classes + 1)) %>%
  as.vector() # to remove names of quantiles, so idx below is numeric
# Así se crean las etiquetas
labels <- c("Muy bajo", "Bajo","Medio Bajo", "Medio", "Medio Alto")

# Se elimina la última etiqueta
# En caso contrario sería hasta NA
# labels <- labels[1:length(labels) - 1]
labels
# Crear la variable
#manz_complemen %<>%
datos_complemen <- datos_complemen %>%
  mutate(q = cut(i_marg,
                 breaks = qPC1,
                 labels = labels,
                 include.lowest = T))

# Unir bases
ageb_complemen<-merge(x=ageb_cdmx,      y=datos_complemen, by="cvegeo")

table(ageb_complemen$q)







################################################
################################################
################################################

#Mapa

################################################
################################################
################################################



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



##Mapa de áreas verdes
#####################
ggplot() +
  # Agrego la capa principal
geom_sf(data =ageb_complemen,
  mapping = aes(
    fill = q
  ),
  color = "white",
  size = 0
) +
  
  # Viridis color scale
  scale_fill_viridis(
    option = "magma",
    name = " ",
    alpha = 0.8, 
    begin = 0.6, 
    end = 0.95,
    discrete = T, # discrete classes
    direction = -1, # oscuro es el más alto, claro/amarillo el más bajo
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = F # El valor más alto hasta arriba
    )) +
  
  # Agrego la capa principal
  geom_sf(data = cdmx_a_verdes,
          mapping = aes(
            fill = "green"  
          ),
          color = "#50A38C",
          fill = "#50A38C",
          size = 0.1,
          alpha=.9
  ) +
  
  # Utilizar un borde más grueso para los municipios
  geom_sf(
    data = mun_cdmx,
    fill = "transparent",
    color = "black",
    size = 0.5
  ) +

#Etiquetas de municipios
#geom_label(data = mun_jal, aes(X, Y, label = NOMBRE), size = 1.5, fontface = "bold")+ 
# Agregar títulos
labs(x = NULL,
     y = NULL,
     title = "Áreas verdes en la CDMX",
     subtitle = "y marginación",
     caption = "Fuente: Elaborado por @rojo_neon, con datos del Censo 2010.",
     fill = "Estratos") +
  # Hacer un pequeño zoom
  #coord_sf(xlim = c(-103.42, -103.25), ylim = c(20.6,20.76), expand = FALSE) +
  # Finalmente, agregar theme
  theme_map()


ggsave("Áreas Verdes.png", width = 7)

