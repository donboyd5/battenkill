# https://geocompr.robinlovelace.net/adv-map.html
# https://geocompr.robinlovelace.net/
# https://r-tmap.github.io/tmap-book/

install_github("r-tmap/tmaptools")
install_github("r-tmap/tmap")

library(sf)
library(raster)
library(dplyr)
library(spData)
# install.packages('spDataLarge',
#                  repos='https://nowosad.github.io/drat/', type='source')
library(spDataLarge)
library(tmap)

library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package


# nz map ------------------------------------------------------------------
# The object passed to tm_shape() in this case is nz, an sf object representing the regions of New Zealand

tm_shape(nz) # needs at least one layer

# Add fill layer to nz shape
tm_shape(nz) +
  tm_fill() 

# Add border layer to nz shape
tm_shape(nz) +
  tm_borders() 

# Add fill and border layers to nz shape
tm_shape(nz) +
  tm_fill() +
  tm_borders() 

map_nz <- tm_shape(nz) + tm_polygons()
class(map_nz)
map_nz

map_nz1 <- map_nz +
  tm_shape(nz_elev) + tm_raster(alpha = 0.7) # these go together

nz_water <- st_union(nz) %>% st_buffer(22200) %>% 
  st_cast(to = "LINESTRING")

map_nz2 <- map_nz1 +
  tm_shape(nz_water) + tm_lines()

map_nz3 <- map_nz2 +
  tm_shape(nz_height) + tm_dots()
map_nz3

tmap_arrange(map_nz1, map_nz2, map_nz3)


ma1 = tm_shape(nz) + tm_fill(col = "red")
ma2 = tm_shape(nz) + tm_fill(col = "red", alpha = 0.3)
ma3 = tm_shape(nz) + tm_borders(col = "blue")
ma4 = tm_shape(nz) + tm_borders(lwd = 3)
ma5 = tm_shape(nz) + tm_borders(lty = 2)
ma6 = tm_shape(nz) + tm_fill(col = "red", alpha = 0.3) +
  tm_borders(col = "blue", lwd = 3, lty = 2)
tmap_arrange(ma1, ma2, ma3, ma4, ma5, ma6)


