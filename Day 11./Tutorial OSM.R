######
###### Tutorial accesing OSM data
#https://dominicroye.github.io/en/2018/accessing-openstreetmap-data-with-r/
######

#install the osmdata, sf, tidyverse and ggmap package
if(!require("osmdata")) install.packages("osmdata")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("sf")) install.packages("sf")
if(!require("ggmap")) install.packages("ggmap")

#load packages
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)

#the first five features
head(available_features())
#amenities
head(available_tags("amenity"))
#shops
head(available_tags("shop"))

#building the query
q <- getbb("Madrid")%>%
  opq()%>%
  add_osm_feature("amenity", "cinema")

str(q) #query structure

cinema <- osmdata_sf(q)
cinema

#our background map
mad_map <- get_map(getbb("Madrid"),maptype = "toner-background")

#final map
ggmap(mad_map)+
  geom_sf(data=cinema$osm_points,
          inherit.aes =FALSE,
          colour="#238443",
          fill="#004529",
          alpha=.5,
          size=4,
          shape=21)+
  labs(x="",y="")


#bounding box for the Iberian Peninsula
m <- matrix(c(-10,5,30,46),ncol=2,byrow=TRUE)
row.names(m) <- c("x","y")
names(m) <- c("min","max")

#building the query
q <- m %>% 
  opq (timeout=25*100) %>%
  add_osm_feature("name","Mercadona")%>%
  add_osm_feature("shop","supermarket")

#query
mercadona <- osmdata_sf(q)

#final map
ggplot(mercadona$osm_points)+
  geom_sf(colour="#08519c",
          fill="#08306b",
          alpha=.5,
          size=1,
          shape=21)+
  coord_sf(datum=NA)+
  theme_void()
