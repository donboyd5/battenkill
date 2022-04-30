# https://dominicroye.github.io/en/2018/accessing-openstreetmap-data-with-r/7

source(here::here("r", "libraries.r"))
bkill <- readRDS(here::here("data", "bkill.rds"))

bkbb <- bkill %>%
  summarise(left=min(longitude),
            right=max(longitude),
            bottom=min(latitude),
            top=max(latitude))
bkbb %>% as.data.frame()
# left     right   bottom   top
# 1 -73.57 -72.98944 43.05393 43.24

bkill_bb <- unlist(bkbb[1,]) + c(-.01, +.01, -.01, +.01)
bkill_bb

library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)

head(available_features())
available_features()
available_tags("amenity")
available_tags("water")  # river, stream_pool (?)

#building the query
q <- getbb("Madrid") %>%
  opq() %>%
  add_osm_feature("amenity", "cinema")

str(q) #query structure

cinema <- osmdata_sf(q)
cinema
str(cinema)

mad_map <- get_map(getbb("Madrid"), maptype = "toner-background")

ggmap(mad_map) +
  geom_sf(data = cinema$osm_points,
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 4,
          shape = 21)+
  labs(x = "", y = "")

# my turn -----------------------------------------------------------------
remotes::install_github("michaeldorman/mapsapi")
library(mapsapi)
library(leaflet)
# ?register_google
# this sets your google map permanently
# register_google(key = "[your key]", write = TRUE)
gmaps_apikey <- "AIzaSyDFWxscpciI2HDziBJ3CIhqxKWEqbaD6VM"
register_google(key = gmaps_apikey, write = TRUE)
google_key()

# main info source:
# https://dominicroye.github.io/en/2018/accessing-openstreetmap-data-with-r/

# we get the background map from any of several sources using ggmap's get_map
#   for a bounding box, it defaults to stamen
# we get the special features from osm using osmdata_sf
# we plot with ggplot and geom_sf


# https://www.openstreetmap.org/export#map=11/43.1272/-73.1752
# bkbb %>% as.data.frame()
# left     right   bottom   top
# -73.57 -72.98944 43.05393 43.24
# what I see on the website -- a bit bigger in all dimensions
# 43.3281 -73.6084 -72.7419 42.9257

#.. define the bounding box ----
bkbb %>% as.data.frame()
# left     right   bottom   top
# 1 -73.57 -72.98944 43.05393 43.24

(lbrtbb <- c(bkbb$left, bkbb$bottom, bkbb$right, bkbb$top))

bkill_bb <- unlist(bkbb[1,]) + c(-.01, +.01, -.01, +.01)
bkill_bb

#.. get the background map from Stamen (or another source) ----
# maptype = c("terrain",
#             "terrain-background", "satellite", "roadmap", "hybrid", "toner",
#             "watercolor", "terrain-labels", "terrain-lines", "toner-2010",
#             "toner-2011", "toner-background", "toner-hybrid", "toner-labels",
#             "toner-lines", "toner-lite")
# http://maps.stamen.com/#terrain/12/37.7706/-122.3782
# Toner These high-contrast B+W (black and white) maps are featured in our Dotspotting project. 
# get_map returns a ggmap object (a classed raster object with a bounding box attribute)
batt_map <- get_map(location=lbrtbb, 
                    source="stamen",
                    force=TRUE,
                    crop=TRUE,
                    maptype="toner",
                    zoom=10)
# stamen allows terrain", "watercolor", and "toner"
ggmap(batt_map)

batt_map <- get_map(location=lbrtbb, 
                    source="google",
                    force=TRUE,
                    crop=TRUE,
                    maptype="roadmap",
                    scale=3,
                    zoom="auto")

batt_map <- get_map(location=lbrtbb, 
                    source="stamen",
                    force=TRUE,
                    zoom=10)

#.. get special features ----
available_tags("water")  # river, stream_pool (?)
q1 <- lbrtbb %>%
  opq() %>%
  add_osm_feature("water", "river")
str(q1)

q2 <- lbrtbb %>%
  opq() %>%
  add_osm_feature("water", "stream_pool")
str(q2)


battwater <- osmdata_sf(q1)
battpools <- osmdata_sf(q2)  # nothing

#.. display with special features ----
batt_map %>%
  ggmap() +
  geom_sf(data = battwater$osm_polygons,
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 4,
          shape = 21) 


# (map <- get_googlemap(c(-97.14667, 31.5493)))
# ggmap(map)
# 
# centers <- c(mean(bkbb$left, bkbb$right), mean(bkbb$top, bkbb$bottom))
# # -73.57  43.24
# centers <- c(-73.5, 43.2)
#   
# (map <- get_googlemap(center=centers,
#                       zoom=10,  # default 10, must be integer
#                       maptyp="terrain"))
# ggmap(map)

# (map <- get_googlemap(center=centers,
#                       zoom=10,  # default 10, must be integer
#                       maptyp="terrain"))
# ggmap(map)


# looks like I shouldn't use OSM ----
# https://github.com/dkahle/ggmap/issues/117
battmap_osm <- get_openstreetmap(bbox=bkill_bb)

# get_openstreetmap(bbox = c(left = -95.80204, bottom = 29.38048, right =
# -94.92313, top = 30.14344), scale = 606250, format = c("png", "jpeg", "svg",
# "pdf", "ps"), messaging = FALSE, urlonly = FALSE, filename = NULL, color =
# c("color", "bw"), ...)


q0 <- bkill_bb %>%
  opq()

q1 <- bkill_bb %>%
  opq() %>%
  add_osm_feature("water", "river")
str(q1)



battriver <- osmdata_sf(q1)
saveRDS(battriver_)
battriver
ggmap(battriver)

zoom	
# map zoom, an integer from 3 (continent) to 21 (building), default value 10
# (city). openstreetmaps limits a zoom of 18, and the limit on stamen maps
# depends on the maptype. "auto" automatically determines the zoom for bounding
# box specifications, and is defaulted to 10 with center/zoom specifications.
# maps of the whole world currently not supported.

# scale	scale argument of get_googlemap [2] or get_openstreetmap [606250]

# source	Google Maps ("google"), OpenStreetMap ("osm"), Stamen Maps ("stamen")

# crop	(stamen and cloudmade maps) crop tiles to bounding box

# filename	destination file for download (file extension added according to format). Default NULL means a random tempfile.

batt_map <- get_map(location=bkill_bb, 
                    maptype = "toner-background")
?get_map
ggmap(batt_map)

ggmap(batt_map)+
  geom_sf(data = battriver$osm_polygons,
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 4,
          shape = 21) +
  labs(x = "", y = "")



# mapsapi -----------------------------------------------------------------
library(stars)
# https://cran.rstudio.com/web/packages/mapsapi/vignettes/intro.html
rmap <- mp_map(center = "31.253205,34.791914", zoom = 14, key = gmaps_apikey, quiet = FALSE)
rmap <- mp_map(center = paste(rev(centers), collapse=","),
               zoom = 10, 
               maptype="terrain",
               key = gmaps_apikey, quiet = FALSE)
rmap
cols = attr(rmap[[1]], "colors")

ggplot() +
  geom_stars(data = rmap, aes(x = x, y = y, fill = color)) +
  scale_fill_manual(values = cols, guide = "none") +
  coord_sf()

# ggmap with google ----
centers <- c(mean(bkbb$left, bkbb$right), mean(bkbb$top, bkbb$bottom))
# # -73.57  43.24
centers <- c(-73.5, 43.2)
   
(map <- get_googlemap(center=centers,
                      size = c(640, 640), scale = 2,
                      zoom=10,  # default 10, must be integer
                      maptype="roadmap"))  # roadmap or terrain best
ggmap(map)

# (map <- get_googlemap(center=centers,
#                       zoom=10,  # default 10, must be integer
#                       maptyp="terrain"))
# ggmap(map)



# maptiles ----------------------------------------------------------------
# bbox = left,bottom,right,top
# bbox = min Longitude , min Latitude , max Longitude , max Latitude 
# {{bbox|-0.489|51.28|0.236|51.686}}
# (bbox=-0.489,51.28,0.236,51.686)
bb <- st_bbox(c(xmin = -81.74, ymin = 36.23,
                xmax  = -81.23, ymax = 36.58))

(bbox <- c(xmin=bkbb$left, ymin=bkbb$bottom, xmax=bkbb$right, ymax=bkbb$top)) # the osm ordering
stbbox <- st_bbox(bbox, crs = 4326)

library(maptiles)
bk_osm <- get_tiles(x=stbbox, crop = TRUE)

ggmap(bk_osm)
plot_tiles(bk_osm)
