######################################################################
#Day 24: Mapa con eestadísticas |
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


##############
#Directorio de trabajo
setwd ("~/Documents/Data Science/Repos/2019B/Mapas/30DaysMapChallenge/Day 24.")
##############


##############
##############
#Municipios
##############
##############
mun_cdmx <- st_read("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/df/df_municipal.shp")

#Crear variable para filtrar por municipios de interés
mun_cdmx$mun <- substr(mun_cdmx$CVEGEO, 3, 5)


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
    data = mun_cdmx,
    fill = "transparent",
    color = "black",
    size = 1
  ) 


##############
##############
#Agebs
##############
##############

ageb_cdmx<- st_read("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/df/df_ageb_urb.shp")

#Crear variable para filtrar por municipios de interés
ageb_cdmx$mun <- substr(ageb_cdmx$cvegeo, 3, 5)

#Seleccionar variables de interés
ageb_cdmx <- ageb_cdmx %>% 
  dplyr::select(cvegeo, mun)

##############
#Datos complementarios
#Leer los datos complementarios
datos_complemen_edu <- read.dbf("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/df/tablas/df_cpv2010_ageb_urb_caracteristicas_educativas.dbf")
datos_complemen_viv <- read.dbf("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/df/tablas/df_cpv2010_ageb_urb_viviendas.dbf")

#Seleccionar variables para PCA
datos_complemen  <- merge(x=datos_complemen_edu, y= datos_complemen_viv,by="CVEGEO")
datos_complemen$mun <- substr(datos_complemen$CVEGEO, 3, 5)

vars <- datos_complemen %>% 
  dplyr::select(CVEGEO,VIV5_R, EDU49_R, VIV36_R)

vars <- vars %>% 
  mutate(cvegeo=CVEGEO)

ageb_cdmx<-merge(x=ageb_cdmx,y=vars,by="cvegeo")

ageb_cdmx %<>%
  mutate(viv5_r= VIV5_R,
         edu49_r=EDU49_R)

#Transformar hacinamiento en variable contraria
ageb_cdmx %<>%
  mutate(viv5_r= viv5_r*(-1))


##############
##############
#FOndo con terrero
##############
##############

#register_google(key="")

s <- "element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative%7Celement:geometry%7Cvisibility:off&style=feature:administrative.country%7Celement:geometry.stroke%7Ccolor:0x000000%7Cvisibility:on&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Cvisibility:off&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road%7Celement:labels.icon%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit%7Cvisibility:off&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x4BA9C5&size=480x360"

# Get the basemap
gdl <- get_googlemap(
  c(lon=-99.1276627,lat=19.4284706),
  zoom = 10, crop=T, 
  scale = 2, 
  #color="bw", 
  maptype="terrain", # can change to terrain
  style = s)

ggmap(gdl)  

gg <- ggmap(gdl) 


##############
##############
#Creando escala
##############
##############



# create 3 buckets for social hacinamiento
quantiles_hacin <- ageb_cdmx %>%
  pull(viv5_r) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create 3 buckets for edu
quantiles_edu <- ageb_cdmx %>%
  pull(edu49_r) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create color scale that encodes two variables
# red for hacinamiento and blue for mean educacion
# the special notation with gather is due to readibility reasons
bivariate_color_scale <- tibble(
  "3 - 3" = "#3F2949", # high inequality, high income
  "2 - 3" = "#435786",
  "1 - 3" = "#4885C1", # low inequality, high income
  "3 - 2" = "#77324C",
  "2 - 2" = "#806A8A", # medium inequality, medium income
  "1 - 2" = "#89A1C8",
  "3 - 1" = "#AE3A4E", # high inequality, low income
  "2 - 1" = "#BC7C8F",
  "1 - 1" = "#CABED0" # low inequality, low income
) %>%
  gather("group", "fill")

##############
##############
#Unir códigos de color con datos
##############
##############

# cut into groups defined above and join fill
ageb_cdmx %<>%
  mutate(
    hacin_quantiles = cut(
      viv5_r,
      breaks = quantiles_hacin,
      include.lowest = TRUE
    ),
    edu_quantiles = cut(
      edu49_r,
      breaks = quantiles_edu,
      include.lowest = TRUE
    ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    group = paste(
      as.numeric(hacin_quantiles), "-",
      as.numeric(edu_quantiles)
    )
  ) %>%
  # we now join the actual hex values per "group"
  # so each municipality knows its hex value based on the his gini and avg
  # income value
  left_join(bivariate_color_scale, by = "group")


##############
##############
#Bivariate annotations
##############
##############


annotations <- tibble(
  label = c(
    "Grey areas mean\nlow income and\nlow inequality",
    "Blue areas mean\nhigh income and\nlow inequality",
    "Violet areas mean\nhigh income and\nhigh inequality",
    "Red areas mean\nlow income and\nhigh inequality"
  ),
  arrow_from = c(
    "548921,232972", # grey
    "771356,238335", # blue
    "781136,125067", # violet
    "616348,81869" # red
  ),
  arrow_to = c(
    "622435,206784", # grey
    "712671,261998", # blue
    "786229,149597", # violet
    "602334,122674" # red
  ),
  curvature = c(
    0.2, # grey
    0.1, # blue
    -0.1, # violet
    -0.2 # red
  ),
  nudge = c(
    "-3000,0", # grey
    "3000,5000", # blue
    "0,-5000", # violet
    "3000,0" # red
  ),
  just = c(
    "1,0", # grey
    "0,1", # blue
    "0.5,1", # violet
    "0,1" # red
  )
) %>%
  separate(arrow_from, into = c("x", "y")) %>%
  separate(arrow_to, into = c("xend", "yend")) %>%
  separate(nudge, into = c("nudge_x", "nudge_y"), sep = "\\,") %>%
  separate(just, into = c("hjust", "vjust"), sep = "\\,")


##############
##############
#Mapa con anotaciones
##############
##############


############Primero poner el mapa

map <- 
  gg+
  # use the "alpha hack" (as the "fill" aesthetic is already taken)
  scale_alpha(name = "",
              range = c(0.6, 0),
              guide = F) + # suppress legend
  # color municipalities according to their gini / income combination
  geom_sf(
  # use the same dataset as before
  data = ageb_cdmx,
    aes(
      fill = fill
    ),
    # use thin white stroke for municipalities
    color = "white",
    size = 0.1,
    inherit.aes = FALSE
  ) +
  # as the sf object municipality_prod_geo has a column with name "fill" that
  # contains the literal color as hex code for each municipality, we can use
  # scale_fill_identity here
  scale_fill_identity() +
  # use thicker white stroke for cantons
  geom_sf(
    data = mun_cdmx,
    fill = "transparent",
    color = "white",
    size = 0.5,
    inherit.aes = FALSE
  ) +

  # add titles
  # Agregar títulos
  labs(x = NULL,
       y = NULL,
       title = "Segregación espacial bivariada, CDMX",
       subtitle = "hacinamiento y años de educación
       ",
       caption = "Fuente: Elaborado por @rojo_neon con datos de CPV-2010 de INEGI.",
       fill = "Estratos"
       ) +
  # Hacer un pequeño zoom
  coord_sf(
    xlim = c(-99.35, -98.94), 
    ylim = c(19.16,19.63), 
    expand = FALSE) +
  # add the theme
  theme_map()

  map
############Hacer anotaciones

# add annotations one by one by walking over the annotations data frame
# this is necessary because we cannot define nudge_x, nudge_y and curvature
# in the aes in a data-driven way like as with x and y, for example
# annotations %>%
#   pwalk(function(...) {
#     # collect all values in the row in a one-rowed data frame
#     current <- tibble(...)
#     
#     # convert all columns from x to vjust to numeric
#     # as pwalk apparently turns everything into a character (why???)
#     current %<>%
#       mutate_at(vars(x:vjust), as.numeric)
#     
#     # update the plot object with global assignment
#     map <<- map +
#       # for each annotation, add an arrow
#       geom_curve(
#         data = current,
#         aes(
#           x = x,
#           xend = xend,
#           y = y,
#           yend = yend
#         ),
#         # that's the whole point of doing this loop:
#         curvature = current %>% pull(curvature),
#         size = 0.2,
#         arrow = arrow(
#           length = unit(0.005, "npc")
#         )
#       ) +
#       # for each annotation, add a label
#       geom_text(
#         data = current,
#         aes(
#           x = x,
#           y = y,
#           label = label,
#           hjust = hjust,
#           vjust = vjust
#         ),
#         # that's the whole point of doing this loop:
#         nudge_x = current %>% pull(nudge_x),
#         nudge_y = current %>% pull(nudge_y),
#         # other styles
#         family = default_font_family,
#         color = default_font_color,
#         size = 3
#       )
#   })


##############
##############
#Leyenda
##############
##############


# separate the groups
bivariate_color_scale %<>%
  separate(group, into = c("viv5_r", "edu49_r"), sep = " - ") %>%
  mutate(viv5_r = as.integer(viv5_r),
         edu49_r = as.integer(edu49_r))

# separate the groups
legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = viv5_r,
      y = edu49_r,
      fill = fill)
  ) +
  scale_fill_identity() +
  # labs(x = "Higher inequality ⟶️",
  #      y = "Higher income ⟶️") +
  labs(x = "+ Hacinamiento -",
       y ="- Educación +")+
  theme_map() +
  # make font small enough
  theme(
    axis.title = element_text(size = 5)
  ) +
  # quadratic tiles
  coord_fixed()


ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, .7, .6, 0.25, 0.25)


#Guardar el mapa
ggsave("bivariate_cdmx.png", width = 6)






