# https://github.com/rforjournalists/mapping
# https://rforjournalists.com/2020/12/15/how-to-access-open-street-map-in-r/
# https://rforjournalists.com/2020/12/15/how-to-access-open-street-map-in-r/
# https://wiki.openstreetmap.org/wiki/Tag:waterway%3Dstream

source(here::here("r", "libraries.r"))
library(osmdata)
library(tidyverse)
library(sf)

# define the bounding box ----
bkill <- readRDS(here::here("data", "bkill.rds"))
bkbb <- bkill %>%
  summarise(left=min(longitude),
            right=max(longitude),
            bottom=min(latitude),
            top=max(latitude))
# left     right   bottom   top
# 1 -73.57 -72.98944 43.05393 43.24
# bkill_bb <- unlist(bkbb[1,]) + shifts

# opq documentation says the bb should be in this order: c(xmin, ymin, xmax, ymax) -- lbrt
shift <- .01
bkbb2 <- c(bkbb$left - shift, bkbb$bottom - shift, bkbb$right + shift, bkbb$top + shift) 
bkbb2
location <- bkbb2 %>% opq()


# investigate features ----------------------------------------------------


#create different types of streets
main_st <- data.frame(type = c("motorway","trunk","primary","motorway_junction","trunk_link","primary_link","motorway_link"))
st <- data.frame(type = available_tags('highway'))
st <- subset(st, !type %in% main_st$type)

path <- data.frame(type = c("footway","path","steps","cycleway"))
st <- subset(st, !type %in% path$type)

st <- as.character(st$type)
main_st <- as.character(main_st$type)
path <- as.character(path$type)

available_features()
available_tags("boundary")
available_tags("administrative")
available_tags("border_type")

available_tags('natural')
available_tags('water')
available_tags('waterway')
available_tags('wetland')
available_tags("spring")

available_tags("boundary")  #  boundary=administrative admin_level=4 state borders
available_tags("political")

lagos_bb <- getbb("Lagos")
library(ggmap)
lagos_map <- get_map(lagos_bb, maptype = "roadmap")
ggmap(lagos_map)
washco_bb <- getbb("Washington County, NY")
washco_map <- get_map(washco_bb, maptype = "roadmap")
ggmap(washco_map)

# https://wiki.openstreetmap.org/wiki/Boundaries
# Subnational boundary=administrative + admin_level=3 to 11 marks subnational
# borders. The names of the subnational entities involved is specified as with
# national borders, with "parish", "district", "region", "province", "state"

# get and save features ----

stborders <- location %>%
  add_osm_feature(key = 'admin_level', value = '4') %>%
  osmdata_sf()
# > stborders
# Object of class 'osmdata' with:
#   $bbox : 43.0439298,-73.58,43.25,-72.9794409
# $overpass_call : The call submitted to the overpass API
# $meta : metadata including timestamp and version numbers
# $osm_points : 'sf' Simple Features Collection with 15893 points
# $osm_lines : 'sf' Simple Features Collection with 512 linestrings
# $osm_polygons : 'sf' Simple Features Collection with 1 polygons
# $osm_multilines : NULL
# $osm_multipolygons : 'sf' Simple Features Collection with 2 multipolygons


main_streets <- location %>%
  add_osm_feature(key = "highway", 
                  value = main_st) %>%
  osmdata_sf()

# > main_streets
# Object of class 'osmdata' with:
#   $bbox : 43.0439298,-73.58,43.25,-72.9794409
# $overpass_call : The call submitted to the overpass API
# $meta : metadata including timestamp and version numbers
# $osm_points : 'sf' Simple Features Collection with 3268 points
# $osm_lines : 'sf' Simple Features Collection with 150 linestrings
# $osm_polygons : 'sf' Simple Features Collection with 2 polygons
# $osm_multilines : NULL
# $osm_multipolygons : NULL

streets <- location %>%
  add_osm_feature(key = "highway", 
                  value = st) %>%
  osmdata_sf()
# > streets
# Object of class 'osmdata' with:
#   $bbox : 43.0439298,-73.58,43.25,-72.9794409
# $overpass_call : The call submitted to the overpass API
# $meta : metadata including timestamp and version numbers
# $osm_points : 'sf' Simple Features Collection with 59875 points
# $osm_lines : 'sf' Simple Features Collection with 4497 linestrings
# $osm_polygons : 'sf' Simple Features Collection with 120 polygons
# $osm_multilines : NULL
# $osm_multipolygons : 'sf' Simple Features Collection with 6 multipolygons

streets2 <- location %>%
  add_osm_feature(key = "highway", 
                  value = "secondary") %>%
  osmdata_sf()
# > streets2
# Object of class 'osmdata' with:
#   $bbox : 43.0439298,-73.58,43.25,-72.9794409
# $overpass_call : The call submitted to the overpass API
# $meta : metadata including timestamp and version numbers
# $osm_points : 'sf' Simple Features Collection with 1597 points
# $osm_lines : 'sf' Simple Features Collection with 69 linestrings
# $osm_polygons : 'sf' Simple Features Collection with 0 polygons
# $osm_multilines : NULL
# $osm_multipolygons : NULL

streets3 <- location %>%
  add_osm_feature(key = "highway", 
                  value = c("road", "secondary", "secondary_link",
                            "tertiary", "tertiary_link" )) %>%
  osmdata_sf()
# > streets3
# Object of class 'osmdata' with:
#   $bbox : 43.0439298,-73.58,43.25,-72.9794409
# $overpass_call : The call submitted to the overpass API
# $meta : metadata including timestamp and version numbers
# $osm_points : 'sf' Simple Features Collection with 6819 points
# $osm_lines : 'sf' Simple Features Collection with 233 linestrings
# $osm_polygons : 'sf' Simple Features Collection with 0 polygons
# $osm_multilines : NULL
# $osm_multipolygons : NULL


water <- location %>%
  add_osm_feature(key = "natural", 
                  value = c("water")) %>%
  osmdata_sf()
# > water
# Object of class 'osmdata' with:
#   $bbox : 43.0439298,-73.58,43.25,-72.9794409
# $overpass_call : The call submitted to the overpass API
# $meta : metadata including timestamp and version numbers
# $osm_points : 'sf' Simple Features Collection with 7974 points
# $osm_lines : NULL
# $osm_polygons : 'sf' Simple Features Collection with 219 polygons
# $osm_multilines : NULL
# $osm_multipolygons : 'sf' Simple Features Collection with 19 multipolygons

rivers <- location %>%
  add_osm_feature(key = "water", 
                  value = c("river")) %>%
  osmdata_sf()
# > rivers
# Object of class 'osmdata' with:
#   $bbox : 43.0439298,-73.58,43.25,-72.9794409
# $overpass_call : The call submitted to the overpass API
# $meta : metadata including timestamp and version numbers
# $osm_points : 'sf' Simple Features Collection with 3437 points
# $osm_lines : NULL
# $osm_polygons : 'sf' Simple Features Collection with 62 polygons
# $osm_multilines : NULL
# $osm_multipolygons : 'sf' Simple Features Collection with 14 multipolygons

wway <- location %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()
# > wway
# Object of class 'osmdata' with:
#   $bbox : 43.0439298,-73.58,43.25,-72.9794409
# $overpass_call : The call submitted to the overpass API
# $meta : metadata including timestamp and version numbers
# $osm_points : 'sf' Simple Features Collection with 6223 points
# $osm_lines : 'sf' Simple Features Collection with 120 linestrings
# $osm_polygons : 'sf' Simple Features Collection with 1 polygons
# $osm_multilines : 'sf' Simple Features Collection with 4 multilinestrings
# $osm_multipolygons : NULL

stream <- location %>%
  add_osm_feature(key = "waterway", value = "stream") %>%
  osmdata_sf()
# > stream
# Object of class 'osmdata' with:
#   $bbox : 43.0439298,-73.58,43.25,-72.9794409
# $overpass_call : The call submitted to the overpass API
# $meta : metadata including timestamp and version numbers
# $osm_points : 'sf' Simple Features Collection with 7783 points
# $osm_lines : 'sf' Simple Features Collection with 369 linestrings
# $osm_polygons : 'sf' Simple Features Collection with 0 polygons

brook <- location %>%
  add_osm_feature(key = "water", value = "brook") %>%
  osmdata_sf() # nothing

creek <- location %>%
  add_osm_feature(key = "water", value = "creek") %>%
  osmdata_sf() # nothing

brook <- location %>%
  add_osm_feature(key = "waterway", value = "brook") %>%
  osmdata_sf() # nothing

creek <- location %>%
  add_osm_feature(key = "waterway", value = "creek") %>%
  osmdata_sf() # nothing

wetland <- location %>%
  add_osm_feature(key = "natural", 
                  value = c("wetland")) %>%
  osmdata_sf()
# > wetland
# Object of class 'osmdata' with:
#   $bbox : 43.0439298,-73.58,43.25,-72.9794409
# $overpass_call : The call submitted to the overpass API
# $meta : metadata including timestamp and version numbers
# $osm_points : 'sf' Simple Features Collection with 1421 points
# $osm_lines : NULL
# $osm_polygons : 'sf' Simple Features Collection with 36 polygons
# $osm_multilines : NULL
# $osm_multipolygons : NULL

spring <- location %>%
  add_osm_feature(key = "natural", 
                  value = c("spring")) %>%
  osmdata_sf()  # no points
# > spring
# Object of class 'osmdata' with:
#   $bbox : 43.0439298,-73.58,43.25,-72.9794409
# $overpass_call : The call submitted to the overpass API
# $meta : metadata including timestamp and version numbers
# $osm_points : 'sf' Simple Features Collection with 0 points
# $osm_lines : NULL
# $osm_polygons : 'sf' Simple Features Collection with 0 polygons
# $osm_multilines : NULL
# $osm_multipolygons : NULL

rail <- location %>%
  add_osm_feature(key = "railway", 
                  value = c("rail")) %>%
  osmdata_sf()
# > rail
# Object of class 'osmdata' with:
#   $bbox : 43.0439298,-73.58,43.25,-72.9794409
# $overpass_call : The call submitted to the overpass API
# $meta : metadata including timestamp and version numbers
# $osm_points : 'sf' Simple Features Collection with 1193 points
# $osm_lines : 'sf' Simple Features Collection with 79 linestrings
# $osm_polygons : 'sf' Simple Features Collection with 0 polygons
# $osm_multilines : NULL
# $osm_multipolygons : NULL


parks <- location %>%
  add_osm_feature(key = "leisure", 
                  value = c("park","nature_reserve","recreation_ground","golf_course","pitch","garden")) %>%
  osmdata_sf()
# > parks
# Object of class 'osmdata' with:
#   $bbox : 43.0439298,-73.58,43.25,-72.9794409
# $overpass_call : The call submitted to the overpass API
# $meta : metadata including timestamp and version numbers
# $osm_points : 'sf' Simple Features Collection with 22908 points
# $osm_lines : 'sf' Simple Features Collection with 64 linestrings
# $osm_polygons : 'sf' Simple Features Collection with 848 polygons
# $osm_multilines : NULL
# $osm_multipolygons : 'sf' Simple Features Collection with 5 multipolygons

buildings <- location %>%
  add_osm_feature(key = "amenity", 
                  value = "pub") %>%
  osmdata_sf()
# > buildings
# Object of class 'osmdata' with:
#   $bbox : 43.0439298,-73.58,43.25,-72.9794409
# $overpass_call : The call submitted to the overpass API
# $meta : metadata including timestamp and version numbers
# $osm_points : 'sf' Simple Features Collection with 8 points
# $osm_lines : NULL
# $osm_polygons : 'sf' Simple Features Collection with 1 polygons
# $osm_multilines : NULL
# $osm_multipolygons : NULL


save(stborders, main_streets, streets, streets2, streets3, water, rivers, wetland, stream, wway, rail, parks, buildings, file=here::here("data", "batt_sf.rdata"))

#plot


check <- stborders$osm_lines %>%
  filter(is.na(admin_level) | admin_level=="4")

check <- stborders$osm_lines %>%
  filter(admin_level=="4")

ggplot() + 
  geom_sf(data = stborders$osm_lines, color = "blue", size = 1)

p1 <- ggplot() + 
  geom_sf(data = water$osm_polygons, fill = '#c6e1e3', colour = '#c6e1e3', size=1.5) +
  coord_sf(xlim=c(bkbb$left - shift - .015, bkbb$right + shift),
           ylim=c(bkbb$bottom - shift, bkbb$top + shift), 
           expand=FALSE) +
  theme_minimal()

p1 + geom_sf(data = stream$osm_lines, colour="green", size=1) +
  coord_sf(xlim=c(bkbb$left - shift - .015, bkbb$right + shift),
           ylim=c(bkbb$bottom - shift, bkbb$top + shift), 
           expand=FALSE)

p1 + geom_sf(data = wway$osm_lines, colour="green", size=1) +
  coord_sf(xlim=c(bkbb$left - shift - .015, bkbb$right + shift),
           ylim=c(bkbb$bottom - shift, bkbb$top + shift), 
           expand=FALSE)

mstrcolor <- "darkgrey"  #  '#ff9999'
strcolor <- "lightgrey"  # '#eedede'

mstrcolor <- "black"  #  '#ff9999'
strcolor <- "darkgrey"  # '#eedede'

p <- ggplot() + 
  geom_sf(data = main_streets$osm_lines, color = mstrcolor, size = 1) +
  geom_sf(data = streets3$osm_lines, size = 0.75, color = strcolor) +
  geom_sf(data = parks$osm_polygons, fill = "lightgreen") +  # #94ba8e
  # geom_sf(data = buildings$osm_polygons, color = '#40493f', fill = '#40493f', size = 0.5) +
  geom_sf(data = water$osm_polygons, fill = '#c6e1e3', colour = '#c6e1e3', size=1.5) +  # some borders are a little big
  geom_sf(data = water$osm_multipolygons, fill = '#c6e1e3', colour = '#c6e1e3', size=1.5) +
  geom_sf(data = stream$osm_lines, colour="#c6e1e3", , fill = '#c6e1e3', size=1.5) +
  geom_sf(data = wway$osm_polygons, colour="#c6e1e3", fill = '#c6e1e3') + 
  geom_sf(data = wway$osm_lines, colour="#c6e1e3", fill = '#c6e1e3', size=1.5) +
  geom_sf(data = wetland$osm_polygons, color = '#c6e1e3') +
  geom_sf(data = stborders$osm_lines, color = "grey55", size = 1.25, linetype="longdash") +
  coord_sf(xlim=c(bkbb$left - shift - .015, bkbb$right + shift),
           ylim=c(bkbb$bottom - shift, bkbb$top + shift), 
           expand=FALSE) +
  theme_minimal() +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()  #remove y axis ticks
  )
  # scale_y_continuous(name=NULL)
# p

# bkill
# pts <- bkill %>% select(locid, locdesc, latitude, longitude, hbi)
p + geom_point(aes(x=longitude, y=latitude, size=hbi), colour="blue", data=pts) +
  scale_size_area(max_size = 4) +
  labs(x=NULL, y=NULL, size="HBI")


sort(pts$hbi)






p + geom_point(aes(x=longitude, y=latitude, size=hbi, colour=hbi), data=pts)
p + geom_point(aes(x=long, y=lat), data=tibble(lat=43.15, long=-73.3), colour="red")

  
  # coord_sf(xlim = c(coords[1], coords[1,2]), 
  #          ylim = c(coords[2], coords[2,2]),
  #          expand = FALSE) + theme_minimal()
