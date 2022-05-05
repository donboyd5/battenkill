

library(ggplot2)
library(sf)
fgdb = "OregonStateParks_20181010.gdb"

# List all feature classes in a file geodatabase
st_layers(fgdb)


gdir <- r"(E:\data\gis\nhd\NHD_H_Vermont_State_GDB\NHD_H_Vermont_State_GDB.gdb)"
st_layers(gdir)

# Read the feature class
# parks <- st_read(dsn=fgdb,layer="LO_PARKS")

gwaterbody <- st_read(dsn=gdir, layer="NHDWaterbody")  # a simple feature

p <- gwaterbody %>%
  ggplot() +
  geom_sf()
library(btools)
memory()

ggplot() +
  geom_sf(data = gwaterbody$osm_polygons, fill = wcolor, colour = wcolor)



wcolor <- '#c6e1e3'
wnew <- "red"
wnew2 <- "green"
theme_set(theme_minimal())

sfwaterbody
sfwaterway
sfwetland # NO does not add value
sfspring # NO does not add value
sfowway

ggplot() +
  geom_sf(data = sfwaterbody$osm_polygons, fill = wcolor, colour = wcolor) +
  geom_sf(data = sfwaterbody$osm_multipolygons, fill = wcolor, colour = wcolor) +
  
  geom_sf(data = sfwaterway$osm_lines, fill = wcolor, colour = wcolor) +
  geom_sf(data = sfwaterway$osm_polygons, fill = wcolor, colour = wcolor) +
  geom_sf(data = sfwaterway$osm_multilines, fill = wcolor, colour = wcolor) +
  geom_sf(data = sfwaterway$osm_multipolygons, fill = wcolor, colour = wcolor) +
  
  geom_sf(data = sfowway$osm_lines, fill = wnew, colour = wnew) +
  geom_sf(data = sfowway$osm_polygons, fill = wnew, colour = wnew) +
  geom_sf(data = sfowway$osm_multilines, fill = wnew, colour = wnew) +
  geom_sf(data = sfowway$osm_multipolygons, fill = wnew, colour = wnew) +
  
  coord_sf(xlim=xlims,
           ylim=ylims, 
           expand=FALSE)

ggplot() +
  geom_sf(data = allwater$osm_lines, fill = wcolor, colour = wcolor) +
  geom_sf(data = allwater$osm_polygons, fill = wcolor, colour = wcolor) +
  geom_sf(data = allwater$osm_multilines, fill = wcolor, colour = wcolor) +
  geom_sf(data = allwater$osm_multipolygons, fill = wcolor, colour = wcolor) +
  
  geom_sf(data = allwaterway$osm_lines, fill = wnew, colour = wnew) +
  geom_sf(data = allwaterway$osm_polygons, fill = wnew, colour = wnew) +
  geom_sf(data = allwaterway$osm_multilines, fill = wnew, colour = wnew) +
  geom_sf(data = allwaterway$osm_multipolygons, fill = wnew, colour = wnew) +
  
  geom_sf(data = sfwetland$osm_lines, fill = wnew2, colour = wnew2) +
  geom_sf(data = sfwetland$osm_polygons, fill = wnew2, colour = wnew2) +
  geom_sf(data = sfwetland$osm_multilines, fill = wnew2, colour = wnew2) +
  geom_sf(data = sfwetland$osm_multipolygons, fill = wnew2, colour = wnew2) +
  
  coord_sf(xlim=xlims,
           ylim=ylims, 
           expand=FALSE)




check1 <- readRDS(here::here("data", "gis", "sfeatures", "water.rds"))
check1

load(here::here("data", "gis", "sfeatures", "sfeatures.rdata"), verbose=TRUE)





# limit to bounding box ---------------------------------------------------
# https://stackoverflow.com/questions/71681038/subset-spatial-features-by-bounding-box-or-polygon
library(sf)

# Download river shapefile here: https://www.weather.gov/gis/Rivers
rivdir <- r"(E:\data\gis\rv16my07)"

# Import river data as SF
# my_rivers <- st_read(dsn = '/home/x/Downloads/rivers/', layer = 'rv16my07') 
my_rivers <- st_read(dsn = rivdir, layer = 'rv16my07') 
#> Reading layer `rv16my07' from data source `/home/x/Downloads/rivers' using driver `ESRI Shapefile'
#> Simple feature collection with 61122 features and 17 fields
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -124.7068 ymin: 25.83636 xmax: -67.11324 ymax: 52.80121
#> Geodetic CRS:  NAD83
str(my_rivers)


# The rivers data comes with a crs, so this step wasn't needed.
# Add a common CRS to the river dataset
#st_crs(my_rivers) <-  CRS('+proj=longlat')

# Set x and y limits for the plot, then make the points an sf object,
# set the crs as the same for my_rivers
ylims <- c(30.2, 31.4)
xlims <- c(-88.3, -87)
box_coords <- tibble(x = xlims, y = ylims) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(st_crs(my_rivers))

#get the bounding box of the two x & y coordintates, make sfc
bounding_box <- st_bbox(box_coords) %>% st_as_sfc()


river_subset <- st_intersection(my_rivers, bounding_box)

head(river_subset)
#> Simple feature collection with 6 features and 17 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -87.07318 ymin: 30.60249 xmax: -87 ymax: 30.83638
#> Geodetic CRS:  NAD83
#>      IHABBSRF_I          RR     HUC TYPE PMILE                  PNAME OWNAME
#> 8616       8616 03140104001 3140104    T   0.0           BLACKWATER R      0
#> 8617       8617 03140104002 3140104    R   0.5           BLACKWATER R      0
#> 8618       8618 03140104003 3140104    R   4.2           BLACKWATER R      0
#> 8630       8630 03140104015 3140104    R   7.8       BIG COLDWATER CR      0
#> 8631       8631 03140104016 3140104    R  17.3 BIG COLDWATER CR  E FK      0
#> 8634       8634 03140104019 3140104    R  17.3 BIG COLDWATER CR  W FK      0
#>           PNMCD OWNMCD       DSRR   DSHUC USDIR LEV J TERMID TRMBLV K
#> 8616 3140104001   <NA> 3140105007 3140105     R   1 0    205      1 0
#> 8617 3140104001   <NA> 3140104001 3140104     R   1 1    205      1 0
#> 8618 3140104001   <NA> 3140104002 3140104     R   1 1    205      1 0
#> 8630 3140104007   <NA> 3140104003 3140104     R   2 1    205      1 0
#> 8631 3140104008   <NA> 3140104015 3140104     R   2 2    205      1 0
#> 8634 3140104010   <NA> 3140104015 3140104     L   3 2    205      1 0
#>                            geometry
#> 8616 LINESTRING (-87.02298 30.60...
#> 8617 LINESTRING (-87.02928 30.60...
#> 8618 LINESTRING (-87.00626 30.64...
#> 8630 LINESTRING (-87 30.76254, -...
#> 8631 LINESTRING (-87.02458 30.78...
#> 8634 LINESTRING (-87.02458 30.78...

#Plot
ggplot() + 
  geom_sf(data = river_subset) + 
  geom_sf(data = bounding_box, fill = NA, color = 'red')
