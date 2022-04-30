library(sf)
library(tidyverse)
library(maps)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")
asia_cropped <- st_crop(world, xmin = 100, xmax = 110,
                        ymin = 7, ymax = 24) #cropping map

rivers50 <- ne_download(scale = 50, type = 'rivers_lake_centerlines', category = 'physical') 

rivers_cropped <- st_crop(st_as_sf(rivers50), xmin = 100, xmax = 110,
                          ymin = 7, ymax = 24)

rivs <- ne_download(scale = 10, type = 'rivers_lake_centerlines', category = 'physical', returnclass = "sf")
str(rivs)

ggplot() + 
  geom_sf(data = world) + 
  theme_bw() + #south east asia
  annotate(geom = "text", x = 107, y = 8, label = "South China Sea", #adding S' China sea  
           fontface = "italic", color = "grey22", size = 4)  + 
  geom_sf(data = rivers_cropped, col = 'blue')


# https://gisgeography.com/new-york-lakes-rivers-map/

