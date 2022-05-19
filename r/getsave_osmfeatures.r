

# links -------------------------------------------------------------------

# https://github.com/rforjournalists/mapping
# https://rforjournalists.com/2020/12/15/how-to-access-open-street-map-in-r/
# https://rforjournalists.com/2020/12/15/how-to-access-open-street-map-in-r/
# https://wiki.openstreetmap.org/wiki/Tag:waterway%3Dstream

# https://lrouviere.github.io/TUTO_DATAVIZ/mapping.html

# https://2019.stateofthemap.us/program/sun/osm-water-how-well-are-minnesotas-water-features-mapped.html
# https://www.youtube.com/watch?v=Om0YDMJn5e0

# https://hydro.nationalmap.gov/arcgis/rest/services/nhd/MapServer
# https://www.usgs.gov/national-hydrography/access-national-hydrography-products


# setup -------------------------------------------------------------------

source(here::here("r", "libraries.r"))
library(osmdata)
library(sf)
library(ggmap)

# define the bounding box ----
bkill <- readRDS(here::here("data", "gis", "bkill.rds"))

bkbb <- bkill %>%
  # lbrt order
  summarise(left=min(longitude),
            bottom=min(latitude),
            right=max(longitude),
            top=max(latitude))

bkbb %>% as.data.frame() # to see more decimals
# left   bottom     right   top
# 1 -73.57 43.05393 -72.98944 43.24
# bkill_bb <- unlist(bkbb[1,]) + shifts

# opq documentation says the bb should be in this order: c(xmin, ymin, xmax, ymax)
# i.e., lbrt (left, bottom, right, top)

# we want the bound box to be bigger than the data by a bit, so add margins
margins <- c(-.01, -.01, .01, .01)
bkbb2 <- unlist(bkbb) + margins # convert to named vector
bkbb2

saveRDS(bkbb2, here::here("data", "gis", "bkbb2.rds"))

# create an opq overpass query object for the location
location <- bkbb2 %>% opq()
location


# # explore basemap options -------------------------------------------------
# washco_bb <- getbb("Washington County, NY")
# washco_map <- get_map(washco_bb, maptype = "roadmap")
# ggmap(washco_map)
# 
# bennco_bb <- getbb("Bennington County, VT")
# bennco_map <- get_map(bennco_bb, maptype = "roadmap")
# ggmap(bennco_map)
# 
# save(washco_map, bennco_map, file=here::here("data", "basemaps.rdata"))


# investigate features ----------------------------------------------------

avail_features <- available_features()
avail_features


# https://wiki.openstreetmap.org/wiki/Boundaries
# Subnational boundary=administrative + admin_level=3 to 11 marks subnational
# borders. The names of the subnational entities involved is specified as with
# national borders, with "parish", "district", "region", "province", "state"

# get and save features ----
available_tags("boundary")  #  boundary=administrative admin_level=4 state borders
available_tags("political")
available_tags("administrative")
available_tags("border_type")

stborders <- location %>%
  add_osm_feature(key = 'admin_level', value = '4') %>%
  osmdata_sf()
saveRDS(stborders, here::here("data", "gis", "sfeatures", "stborders.rds"))
# save(stborders, file=here::here("data", "gis", "sfeatures", "sfeatures.rdata"))
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

#.. places ----

available_tags("place")

# https://wiki.openstreetmap.org/wiki/Tag:place%3Dvillage


# > available_tags("place")
# [1] "allotments"        "archipelago"       "borough"           "city"              "city_block"        "continent"        
# [7] "country"           "county"            "district"          "farm"              "hamlet"            "island"           
# [13] "islet"             "isolated_dwelling" "locality"          "municipality"      "neighbourhood"     "ocean"            
# [19] "plot"              "province"          "quarter"           "region"            "sea"               "square"           
# [25] "state"             "suburb"            "town"              "village"  

villages <- location %>%
  add_osm_feature(key = 'place', value = 'village') %>%
  osmdata_sf()
saveRDS(villages, here::here("data", "gis", "sfeatures", "villages.rds"))
# save(stborders, villages, file=here::here("data", "gis", "sfeatures", "sfeatures.rdata"))
# > villages
# Object of class 'osmdata' with:
#   $bbox : 43.0439298,-73.58,43.25,-72.9794409
# $overpass_call : The call submitted to the overpass API
# $meta : metadata including timestamp and version numbers
# $osm_points : 'sf' Simple Features Collection with 258 points
# $osm_lines : 'sf' Simple Features Collection with 6 linestrings
# $osm_polygons : 'sf' Simple Features Collection with 3 polygons
# $osm_multilines : NULL
# $osm_multipolygons : 'sf' Simple Features Collection with 5 multipolygons

places <- location %>%
  add_osm_feature(key = 'place', value = c("city", "hamlet", "village")) %>%
  osmdata_sf()
saveRDS(places, here::here("data", "gis", "sfeatures", "places.rds"))
# save(stborders, villages, places, file=here::here("data", "gis", "sfeatures", "sfeatures.rdata"))
# > places
# Object of class 'osmdata' with:
#   $bbox : 43.0439298,-73.58,43.25,-72.9794409
# $overpass_call : The call submitted to the overpass API
# $meta : metadata including timestamp and version numbers
# $osm_points : 'sf' Simple Features Collection with 294 points
# $osm_lines : 'sf' Simple Features Collection with 6 linestrings
# $osm_polygons : 'sf' Simple Features Collection with 3 polygons
# $osm_multilines : NULL
# $osm_multipolygons : 'sf' Simple Features Collection with 5 multipolygons


# neighborhood <- location %>%
#   add_osm_feature(key = 'place', value = c("neighborhood")) %>%
#   osmdata_sf()
# # nothing

# suburb <- location %>%
#   add_osm_feature(key = 'place', value = c("suburb")) %>%
#   osmdata_sf()
# nothing

# municipality <- location %>%
#   add_osm_feature(key = 'place', value = c("municipality")) %>%
#   osmdata_sf()
# nothing

borough <- location %>%
  add_osm_feature(key = 'place', value = c("borough")) %>%
  osmdata_sf()
# nothing

locality <- location %>%
  add_osm_feature(key = 'place', value = c("locality")) %>%
  osmdata_sf()
saveRDS(locality, here::here("data", "gis", "sfeatures", "locality.rds"))
# save(stborders, villages, places, locality, file=here::here("data", "gis", "sfeatures", "sfeatures.rdata"))
# > locality
# Object of class 'osmdata' with:
#   $bbox : 43.0439298,-73.58,43.25,-72.9794409
# $overpass_call : The call submitted to the overpass API
# $meta : metadata including timestamp and version numbers
# $osm_points : 'sf' Simple Features Collection with 649 points
# $osm_lines : NULL
# $osm_polygons : 'sf' Simple Features Collection with 1 polygons
# $osm_multilines : NULL
# $osm_multipolygons : 'sf' Simple Features Collection with 1 multipolygons

#.. streets ----
hwytags <- available_tags('highway')
mainst <- c("motorway", "trunk", "primary", "motorway_junction", "trunk_link", 
            "primary_link", "motorway_link")
pathst <-  c("footway", "path", "steps", "cycleway")
stxmainxpath <- setdiff(setdiff(hwytags, mainst), pathst)

main_streets <- location %>%
  add_osm_feature(key = "highway", 
                  value = mainst) %>%
  osmdata_sf()
saveRDS(main_streets, here::here("data", "gis", "sfeatures", "main_streets.rds"))
# save(stborders, villages, places, locality, main_streets, 
#      file=here::here("data", "gis", "sfeatures", "sfeatures.rdata"))
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
                  value = stxmainxpath) %>%
  osmdata_sf()
saveRDS(streets, here::here("data", "gis", "sfeatures", "streets.rds"))
# save(stborders, villages, places, locality, main_streets, streets,
#      file=here::here("data", "gis", "sfeatures", "sfeatures.rdata"))
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

secondary <- location %>%
  add_osm_feature(key = "highway", 
                  value = "secondary") %>%
  osmdata_sf()
saveRDS(secondary, here::here("data", "gis", "sfeatures", "secondary.rds"))
# save(stborders, villages, places, locality, main_streets, streets, secondary,
#      file=here::here("data", "gis", "sfeatures", "sfeatures.rdata"))
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

roads <- location %>%
  add_osm_feature(key = "highway", 
                  value = c("road", "secondary", "secondary_link",
                            "tertiary", "tertiary_link" )) %>%
  osmdata_sf()
saveRDS(roads, here::here("data", "gis", "sfeatures", "roads.rds"))
# save(stborders, villages, places, locality, main_streets, streets, secondary, roads,
#      file=here::here("data", "gis", "sfeatures", "sfeatures.rdata"))
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


#.. water ----
available_tags('natural')
# [1] "arch"          "arete"         "bare_rock"     "bay"           "beach"         "blowhole"      "cape"          "cave_entrance"
# [9] "cliff"         "coastline"     "crevasse"      "dune"          "earth_bank"    "fell"          "fumarole"      "geyser"       
# [17] "glacier"       "grassland"     "heath"         "hill"          "hot_spring"    "isthmus"       "moor"          "mud"          
# [25] "peak"          "peninsula"     "reef"          "ridge"         "rock"          "saddle"        "sand"          "scree"        
# [33] "scrub"         "shingle"       "shoal"         "shrubbery"     "sinkhole"      "spring"        "stone"         "strait"       
# [41] "tree"          "tree_row"      "tundra"        "valley"        "volcano"       "water"         "wetland"       "wood"   
# spring strait water wetland waterway

available_tags('water')
# > available_tags('water')
# [1] "basin"           "canal"           "ditch"           "fish_pass"       "lagoon"          "lake"            "lock"           
# [8] "moat"            "oxbow"           "pond"            "reflecting_pool" "reservoir"       "river"           "stream_pool"    
# [15] "wastewater"  
waterbody <- c("lagoon", "lake", "oxbow", "pond", "reflecting_pool", "reservoir", "stream_pool")

available_tags('waterway')
# > available_tags('waterway')
# [1] "boatyard"      "canal"         "dam"           "ditch"         "dock"          "drain"         "fairway"       "fuel"         
# [9] "lock_gate"     "pressurised"   "river"         "riverbank"     "soakhole"      "stream"        "tidal_channel" "turning_point"
# [17] "water_point"   "waterfall"     "weir"  
waterway <- c("canal", "river", "stream", "waterfall")
owway <- setdiff(available_tags('waterway'), waterway)

available_tags('wetland')
available_tags("spring")
available_tags("spring")

#.... new water items ----
allnatwater <- location %>%
  add_osm_feature(key = "natural", 
                  value = c("water", "wetland", "shoal", "spring")) %>%
  osmdata_sf()

allwater <- location %>%
  add_osm_feature(key = "water", 
                  value = available_tags("water")) %>%
  osmdata_sf()

allwaterway <- location %>%
  add_osm_feature(key = "waterway", 
                  value = available_tags("waterway")) %>%
  osmdata_sf()

sfwaterbody <- location %>%
  add_osm_feature(key = "water", 
                  value = waterbody) %>%
  osmdata_sf()
saveRDS(sfwaterbody, here::here("data", "gis", "sfeatures", "sfwaterbody.rds"))

sfwaterway <- location %>%
  add_osm_feature(key = "waterway", 
                  value = waterway) %>%
  osmdata_sf()
saveRDS(sfwaterway, here::here("data", "gis", "sfeatures", "sfwaterway.rds"))

sfowway <- location %>%
  add_osm_feature(key = "waterway", 
                  value = owway) %>%
  osmdata_sf()

sfwetland <- location %>%
  add_osm_feature(key = "natural", 
                  value = 'wetland') %>%
  osmdata_sf()
saveRDS(sfwetland, here::here("data", "gis", "sfeatures", "sfwetland.rds"))

sfspring <- location %>%
  add_osm_feature(key = "natural", 
                  value = 'spring') %>%
  osmdata_sf()
# saveRDS(sfwaterway, here::here("data", "gis", "sfeatures", "sfwaterway.rds"))

save(sfwaterbody, sfwaterway,
     file=here::here("data", "gis", "sfeatures", "sfwater.rdata"))
load(here::here("data", "gis", "sfeatures", "sfwater.rdata"), verbose=TRUE)

#.... old water items
water <- location %>%
  add_osm_feature(key = "natural", 
                  value = c("water")) %>%
  osmdata_sf()
saveRDS(water, here::here("data", "gis", "sfeatures", "water.rds"))
# save(stborders, villages, places, locality, main_streets, streets, secondary, roads,
#      water,
#      file=here::here("data", "gis", "sfeatures", "sfeatures.rdata"))
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
saveRDS(rivers, here::here("data", "gis", "sfeatures", "rivers.rds"))
# save(stborders, villages, places, locality, main_streets, streets, secondary, roads,
#      water, rivers,
#      file=here::here("data", "gis", "sfeatures", "sfeatures.rdata"))
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
saveRDS(wway, here::here("data", "gis", "sfeatures", "wway.rds"))
save(stborders, villages, places, locality, main_streets, streets, secondary, roads,
     water, rivers, wway,
     file=here::here("data", "gis", "sfeatures", "sfeatures.rdata"))
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

# brook <- location %>%
#   add_osm_feature(key = "water", value = "brook") %>%
#   osmdata_sf() # nothing

# creek <- location %>%
#   add_osm_feature(key = "water", value = "creek") %>%
#   osmdata_sf() # nothing

# brook <- location %>%
#   add_osm_feature(key = "waterway", value = "brook") %>%
#   osmdata_sf() # nothing

# creek <- location %>%
#   add_osm_feature(key = "waterway", value = "creek") %>%
#   osmdata_sf() # nothing

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

# spring <- location %>%
#   add_osm_feature(key = "natural", 
#                   value = c("spring")) %>%
#   osmdata_sf()  # no points
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

#.. misc special features ----
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

save(avail_features,
     stborders, villages, places, locality,
     main_streets, streets, streets2, streets3, 
     water, rivers, wetland, stream, wway, 
     rail, parks, buildings, 
     file=here::here("data", "batt_sf.rdata"))


# prepare main map --------------------------------------------------------

#.. get data ----
# load(here::here("data", "basemaps.rdata"))
load(here::here("data", "gis", "batt_sf.rdata"))

summary(bkill)

pts <- bkill %>% 
  dplyr::select(year, locid, locdesc, latitude, longitude, hbi) %>%
  mutate(yearf=case_when(year < 2000 ~ "< 2000",
                         year %in% 2001:2009 ~ "2000-2009",
                         year >= 2010 ~ "2010+",
                         TRUE ~ "ERROR") %>% as.factor())
count(pts, yearf, year)

# ggplot() + 
#   geom_sf(data = stborders$osm_lines, color = "blue", size = 1)
#.. define map boundaries ----
hstretch <- c(-.02, 0)
vstretch <- c(-.01, 0)

xlims <- bkbb2[c("left", "right")] + hstretch
ylims <- bkbb2[c("bottom", "top")] + vstretch

#.. define colors ----
mstrcolor <- "darkgrey"  #  '#ff9999'
strcolor <- "lightgrey"  # '#eedede'

mstrcolor <- "black"  #  '#ff9999'
strcolor <- "darkgrey"  # '#eedede'

bodycolor <- "#c6e1e3"
waycolor <- "green"

#.. explore features on the map -----

#.. get the water features right ----
wmap <- ggplot() +
  geom_sf(data = water$osm_polygons, fill = '#c6e1e3', colour = '#c6e1e3', size=1.5) +
  geom_sf(data = water$osm_multipolygons, fill = '#c6e1e3', colour = '#c6e1e3', size=1.5) +
  geom_sf(data = wway$osm_polygons, fill = '#c6e1e3', colour = 'green', size=1) +
  geom_sf(data = wway$osm_multipolygons, fill = '#c6e1e3', colour = 'green', size=1) +
  geom_sf(data = wway$osm_lines, fill = '#c6e1e3', colour = 'green', size=1) +
  geom_sf(data = wway$osm_multilines, fill = '#c6e1e3', colour = 'green', size=1) +
  coord_sf(xlim=xlims,
           ylim=ylims, 
           expand=FALSE) +
  theme_minimal()


#.. get the municipalities right ----
ggplot() +
  geom_sf(data = water$osm_polygons, fill = '#c6e1e3', colour = '#c6e1e3', size=1.5) +
  geom_sf(data = water$osm_multipolygons, fill = '#c6e1e3', colour = '#c6e1e3', size=1.5) +
  geom_sf(data = places$osm_polygons, colour = 'lightgrey', fill="lightgrey") +
  geom_sf(data = places$osm_multipolygons, colour = 'lightgrey', fill="lightgrey") +
  geom_sf(data = places$osm_lines, colour = 'lightgrey') +
  geom_sf(data = locality$osm_polygons, colour = 'red', fill="red") +
  geom_sf(data = locality$osm_multipolygons, colour = 'red', fill="red") +
  coord_sf(xlim=xlims,
           ylim=ylims, 
           expand=FALSE)

wmap +   
  geom_sf(data = places$osm_polygons, colour = 'lightgrey', fill="lightgrey") +
  geom_sf(data = places$osm_multipolygons, colour = 'lightgrey', fill="lightgrey") +
  geom_sf(data = places$osm_lines, colour = 'lightgrey') +
  coord_sf(xlim=xlims,
           ylim=ylims, 
           expand=FALSE) +
  theme_minimal()


# make the base map ----
basemap <- ggplot() +
  geom_sf(data = water$osm_polygons, fill = bodycolor, colour = bodycolor, size=1) +
  geom_sf(data = water$osm_multipolygons, fill = bodycolor, colour = bodycolor, size=1) +
  geom_sf(data = wway$osm_polygons, fill = waycolor, colour = waycolor, size=1) +
  geom_sf(data = wway$osm_multipolygons, fill = waycolor, colour = waycolor, size=1) +
  geom_sf(data = wway$osm_lines, fill = waycolor, colour = waycolor, size=1) +
  geom_sf(data = wway$osm_multilines, fill = waycolor, colour = waycolor, size=1) +
  geom_sf(data = stream$osm_lines, colour=waycolor, fill = waycolor, size=1) +
  geom_sf(data = places$osm_polygons, colour = 'lightgrey', fill="lightgrey") +
  geom_sf(data = places$osm_multipolygons, colour = 'lightgrey', fill="lightgrey") +
  geom_sf(data = places$osm_lines, colour = 'black') +
  geom_sf(data = places$osm_polygons, colour = 'lightgrey', fill="lightgrey") +
  geom_sf(data = places$osm_multipolygons, colour = 'lightgrey', fill="lightgrey") +
  geom_sf(data = places$osm_lines, colour = 'lightgrey') +
  geom_sf(data = main_streets$osm_lines, color = mstrcolor, size = 1) +
  geom_sf(data = streets3$osm_lines, size = 0.75, color = strcolor) +
  geom_sf(data = stborders$osm_lines, color = "grey55", size = 1, linetype="longdash")
saveRDS(basemap, here::here("data", "basemap.rds"))

# add points ----
bkill <- readRDS(here::here("data", "bkill.rds"))
bkbb2 <- readRDS(here::here("data", "bkbb2.rds"))
basemap <- readRDS(here::here("data", "basemap.rds"))

pts <- bkill %>% 
  dplyr::select(year, locid, locdesc, latitude, longitude, hbi) %>%
  mutate(yearf=case_when(year < 2000 ~ "< 2000",
                         year %in% 2001:2009 ~ "2000-2009",
                         year >= 2010 ~ "2010+",
                         TRUE ~ "ERROR") %>% as.factor())

jit <- .02
# clrs <- c('#fee0d2','#fc9272','#de2d26') # reds
clrs <- c('#deebf7','#9ecae1','#3182bd') # blues
hbimap <- basemap +
  geom_point(aes(x=longitude, y=latitude, size=hbi, colour=yearf),
             position=position_dodge(width=jit),
             data=pts) +
  scale_size_area(max_size = 4) +
  scale_color_manual(values=clrs) +
  labs(x=NULL, y=NULL, size="HBI", colour=NULL) +
  coord_sf(xlim=xlims,
           ylim=ylims, 
           expand=FALSE) +
  scale_size_area(max_size = 3) +
  theme_minimal() +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()  #remove y axis ticks
  )

hbimap

