##############
#Configuración
rm(list = ls())

#remove.packages("ggmap")
require("devtools")
#devtools::install_github("dkahle/ggmap")
library(ggmap)
library(devtools)


#Acá está la llave https://console.cloud.google.com/apis/credentials?project=intricate-mix-170518
register_google(key="")



###Primer intento para CDMX
cdmx_street <- st_read("/Users/maximoernestojaramillomolina/Documents/Encuestas/Shape/Ageb y mas 2010/shps/df/df_eje_vial.shp")
table(cdmx_street$TIPOVIAL)

cdmx_street_peri <- cdmx_street %>%
  filter(TIPOVIAL== "PERIFÉRICO")

ggplot()+
  geom_sf(data=cdmx_street_peri,
        inherit.aes =FALSE,
        colour="#238443",
        fill="#004529",
        alpha=.5,
        size=4,
        shape=21)

###Primer intento para CDMX
cdmx_street <- st_read("/Users/maximoernestojaramillomolina/Documents/Encuestas/Shape/Ageb y mas 2010/shps/df/df_eje_vial.shp")
table(cdmx_street$TIPOVIAL)

cdmx_street_calz_tl <- cdmx_street %>%
  filter(TIPOVIAL== "CALZADA" & NOMVIAL=="DE TLALPAN")

ggmap(cdmx_map) +
  geom_sf(data=cdmx_street_calz_tl,
          inherit.aes =FALSE,
          colour="#238443",
          fill="#004529",
          alpha=.5,
          size=4,
          shape=21)

tlalpan + coord_sf(xlim = c(-99.35, -98.9), ylim = c(19.17,19.6), expand = FALSE) 

mapa_cdmx <- get_map(c(left = -99.3500, bottom = 19.170000, right = -99.00000, top = 19.600000),
                scale = 13, 
                maptype = c( "toner-2010"), 
                source = c("stamen"),
                color = c("bw"))
ggmap(mapa_cdmx)


#our background map
#cdmx_map <- get_map(getbb("CDMX"),maptype = "toner-background")



  map <- get_map(
    c(left = -99.2, bottom = 19.25, right = -99.05, top = 19.5),
    source = c("osm"),
    maptype="terrain",
    GRAYSCALE=TRUE
    )
  ggmap(map)
  
maps <- get_map(
    c(left = -99.25, bottom = 19.25, right = -99.05, top = 19.5),
    source = "stamen",
    #scale = 15, 
    maptype="watercolor",
    #color="bw",
    force = TRUE
  )
ggmap(maps)  
  
mapa_stament <- get_stamenmap(c(left = -99.25, bottom = 19.25, right = -99.05, top = 19.5),
              #source = "osm",
              #scale = 9, 
              zoom = 11, 
              maptype="terrain-lines",
              #color="color",
              force=TRUE)
ggmap(mapa_stament)


mapa_google <- get_googlemap(c(-99.123, 19.375 ),
                             zoom = 12,
                             scale = 1,
                             force = TRUE,
                             maptype = "terrain",
                             style = c(feature = "all", element = "labels", visibility = "off"),
                             color = "bw"
                             )
ggmap(mapa_google)


  ggmap(mapa_google)+
  geom_sf(data=cdmx_street_calz_tl,
          inherit.aes =FALSE,
          colour="#238443",
          fill="#004529",
          alpha=.5,
          size=4,
          shape=21)







## basic usage
########################################

bbox <- c(left = -97.1268, bottom = 31.536245, right = -97.099334, top = 31.559652)

ggmap(get_stamenmap(bbox, zoom = 13))
ggmap(get_stamenmap(bbox, zoom = 14))
ggmap(get_stamenmap(bbox, zoom = 15))
ggmap(get_stamenmap(bbox, zoom = 16, messaging = TRUE))

place <- "mount everest"
(google <- get_googlemap(place, zoom = 9))
ggmap(google)
bbox_everest <- c(left = 86.05, bottom = 27.21, right = 87.81, top = 28.76)
bbox_everest <- c(left = -99.2, bottom = 19.25, right = -99.05, top = 19.5)
bbox_everest <- c(left = -97.1268, bottom = 31.536245, right = -97.099334, top = 31.559652)
ggmap(get_stamenmap(bbox_everest, zoom = 12))
