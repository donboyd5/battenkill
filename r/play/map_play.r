# https://milospopovic.net/map-rivers-with-sf-and-ggplot2-in-r/

# https://waterdata.usgs.gov/blog/build-r-animations/#setup
# https://www.efrainmaps.es/english-version/free-downloads/united-states/  # I downloaded U.S. rivers
# https://www.weforum.org/agenda/2019/02/the-worlds-watersheds-mapped-in-gorgeous-detail

# ggmap is a package for R that retrieves raster map tiles from online mapping
# services like Google Maps and plots them using the ggplot2 framework. The map
# tiles are raster because they are static image files generated previously by
# the mapping service. You do not need any data files containing information on
# things like scale, projection, boundaries, etc. because that information is
# already created by the map tile. This severely limits your ability to redraw
# or change the appearance of the geographic map, however the tradeoff means you
# can immediately focus on incorporating additional data into the map.

source(here::here("r", "libraries.r"))
bkill <- readRDS(here::here("data", "bkill.rds"))


# library(tidyverse)
library(ggmap)
library(httr)
library(sf)
# library(RColorBrewer)
# library(patchwork)
# library(here)
library(remotes)
# remotes::install_github("ropensci/osmdata")

library(tidyverse)
library(osmdata) # package for working with streets
library(showtext) # for custom fonts
library(ggmap)
library(rvest)

# http://joshuamccrain.com/tutorials/maps/streets_tutorial.html
# big_streets <- getbb("Asheville United States") %>%
#   opq() %>%
#   add_osm_feature(key = "highway", 
#                   value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
#   osmdata_sf()

# big_streets

available_tags("waterway")

add_osm_features (features = c ("\"amenity\"=\"restaurant\"",
                                "\"amenity\"=\"pub\""))

river <- getbb("Asheville United States")%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>% # "river", "stream"
  osmdata_sf()


# bkill %>%
#   filter(latitude==max(latitude)) %>%
#   select(date, objectid, locid, locdesc, latitude, longitude, latitude_raw, longitude_raw)


chi_bb <- c(
  left = -87.936287,
  bottom = 41.679835,
  right = -87.447052,
  top = 42.000835
)

chicago_stamen <- get_stamenmap(
  bbox = chi_bb,
  zoom = 11
)
chicago_stamen
ggmap(chicago_stamen)

bkbb <- bkill %>%
  summarise(left=min(longitude),
            right=max(longitude),
            bottom=min(latitude),
            top=max(latitude))

# bkill_bb <- c(
#   left = -73.57,
#   bottom = 42.24,
#   right = -72.99,
#   top = 43.24
# )
bkill_bb <- unlist(bkbb[1,]) + c(-.01, +.01, -.01, +.01)
bkill_bb

bb_stamen <- get_stamenmap(
  bbox = bkill_bb,
  zoom = 11
)

ggmap(bb_stamen)

# https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html
bkillsf <- bkill_bb %>%
  opq() %>% # build an Overpass query
  # Add a feature to an Overpass query
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  # Return an OSM Overpass query 
  osmdata_sf()

bkillsf2 <- bkill_bb %>%
  opq() %>% # build an Overpass query
  # Add a features to an Overpass query
  add_osm_features(features = c ("\"waterway\"=\"river\"",
                                  "\"waterway\"=\"stream\"")) %>%
  # Return an OSM Overpass query 
  osmdata_sf()

bkillsf2 <- bkill_bb %>%
  opq() %>% # build an Overpass query
  # Add a features to an Overpass query
  add_osm_feature(key = "waterway",
                  value = c("river"))  %>%
  # Return an OSM Overpass query 
  osmdata_sf()

bkillsf3 <- bkill_bb %>%
  opq() %>% # build an Overpass query
  # Add a features to an Overpass query
  add_osm_feature(key = "waterway",
                  value = c("stream"))  %>%
  # Return an OSM Overpass query 
  osmdata_sf(timeout = 200) # default 25


tmp <- c ("\"waterway\"=\"river\"", "\"waterway\"=\"stream\"")
class(tmp)
str(tmp)

# river maps --------------------------------------------------------------

#===============
# LOAD PACKAGES
#===============
library(tidyverse)
library(maptools)


#===============
# GET RIVER DATA
#===============

#==========
# LOAD DATA
#==========

#DEFINE URL
# - this is the location of the file
url.river_data <- url("http://sharpsightlabs.com/wp-content/datasets/usa_rivers.RData")


# LOAD DATA
# - this will retrieve the data from the URL
load(url.river_data)
saveRDS(lines.rivers, here::here("data", "lines.rivers.rds"))


# INSPECT
summary(lines.rivers)
lines.rivers@data %>% glimpse()
# djb
df <- lines.rivers@data
tmp <- df %>% filter(STATE=="NY")
count(tmp, NAME)


levels(lines.rivers$FEATURE)
table(lines.rivers$FEATURE)

#==============================================
# REMOVE MISC FEATURES
# - there are some features in the data that we
#   want to remove
#==============================================
lines.rivers <- subset(lines.rivers, !(FEATURE %in% c("Shoreline"
                                                      ,"Shoreline Intermittent"
                                                      ,"Null"
                                                      ,"Closure Line"
                                                      ,"Apparent Limit"
)))

# RE-INSPECT
table(lines.rivers$FEATURE)

#==============
# REMOVE STATES
#==============

#-------------------------------
# IDENTIFY STATES
# - we need to find out
#   which states are in the data
#-------------------------------
table(lines.rivers$STATE)


#---------------------------------------------------------
# REMOVE STATES
# - remove Alaska, Hawaii, Puerto Rico, and Virgin Islands
# - these are hard to plot in a confined window, so 
#   we'll remove them for convenience
#---------------------------------------------------------

lines.rivers <- subset(lines.rivers, !(STATE %in% c('AK','HI','PR','VI')))

# RE-INSPECT
table(lines.rivers$STATE)


#============================================
# FORTIFY
# - fortify will convert the 
#   'SpatialLinesDataFrame' to a proper
#    data frame that we can use with ggplot2
#============================================

df.usa_rivers <- fortify(lines.rivers)


#============
# GET USA MAP
#============
map.usa_country <- map_data("usa")
map.usa_states <- map_data("state")


#=======
# PLOT
#=======

ggplot() +
  geom_polygon(data = map.usa_country, aes(x = long, y = lat, group = group), fill = "#484848") +
  geom_path(data = df.usa_rivers, aes(x = long, y = lat, group = group), color = "#8ca7c0", size = .08) +
  coord_map(projection = "albers", lat0 = 30, lat1 = 40, xlim = c(-121,-73), ylim = c(25,51)) +
  labs(title = "Rivers and waterways of the United States") +
  annotate("text", label = "sharpsightlabs.com", family = "Gill Sans", color = "#A1A1A1"
           , x = -89, y = 26.5, size = 5) +
  theme(panel.background = element_rect(fill = "#292929")
        ,plot.background = element_rect(fill = "#292929")
        ,panel.grid = element_blank()
        ,axis.title = element_blank()
        ,axis.text = element_blank()
        ,axis.ticks = element_blank()
        ,text = element_text(family = "Gill Sans", color = "#A1A1A1")
        ,plot.title = element_text(size = 34)
  ) 




# rnaturalearth --------------------------------------------------------------------

# https://stackoverflow.com/questions/66934312/adding-rivers-to-a-map-with-rnaturalearth

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

ggplot() + geom_sf(data = asia_cropped) + theme_bw() + #south east asia
  annotate(geom = "text", x = 107, y = 8, label = "South China Sea", #adding S' China sea  
           fontface = "italic", color = "grey22", size = 4)  + 
  geom_sf(data = rivers_cropped, col = 'blue')
