
# , you can set default_crs = sf::st_crs(4326), the World Geodetic System 1984
# (WGS84). This means that x and y positions are interpreted as longitude and
# latitude, respectively. You can also specify any other valid CRS as the
# default CRS for non-sf geoms.


# https://ryanpeek.org/2018-11-13-a-few-mapping-tips/

source(here::here("r", "libraries.r"))
# library(osmdata)
library(sf)
# library(ggmap)
library(patchwork)

# get battenkill data and its bounding box ----------------------------------------------------

# define the bounding box ----
bkill <- readRDS(here::here("data", "gis", "bkill.rds"))

bkbbdf <- bkill %>%
  # lbrt order
  summarise(left=min(longitude),
            bottom=min(latitude),
            right=max(longitude),
            top=max(latitude))
bkbbdf %>% as.data.frame() # to see more decimals
# left   bottom     right   top
# 1 -73.57 43.05393 -72.98944 43.24
# bkill_bb <- unlist(bkbbdf[1,]) + shifts


# opq documentation says the bb should be in this order: c(xmin, ymin, xmax, ymax)
# i.e., lbrt (left, bottom, right, top)

# create bounding box vector
# we want the bound box to be bigger than the data by a bit, so add margins
margins <- c(-.02, -.02, .02, .02)
bkbb <- unlist(bkbbdf) + margins # convert to named vector
bkbb

# saveRDS(bkbb2, here::here("data", "bkbb2.rds"))


# get the waterbodies -----------------------------------------------------
nydir <- r"(E:\data\gis\nhd\NHD_H_New_York_State_GDB\NHD_H_New_York_State_GDB.gdb)"
st_layers(nydir)
# NHDWaterbody
# NHDFlowlineVAA
# NHDLine
# NHDFlowline 
# NHDArea


vtdir <- r"(E:\data\gis\nhd\NHD_H_Vermont_State_GDB\NHD_H_Vermont_State_GDB.gdb)"
st_layers(vtdir)

# Read the feature classes of interest ----
#.. New York ----
nywaterbody <- st_read(dsn=nydir, layer="NHDWaterbody")  # a simple feature
head(nywaterbody)
st_crs(nywaterbody)

nyline <- st_read(dsn=nydir, layer="NHDLine")  # a simple feature
head(nyline)
st_crs(nyline)
# 

nyfline <- st_read(dsn=nydir, layer="NHDFlowline")  # a simple feature
head(nyfline)
st_crs(nyfline)
# xmin: -83.53259 ymin: 39.37305 xmax: -71.76289 ymax: 46.04755


#.. Vermont ----
vtwaterbody <- st_read(dsn=vtdir, layer="NHDWaterbody")  # a simple feature
head(vtwaterbody)
st_crs(vtwaterbody)

#.. compare ----
st_crs(nywaterbody) == st_crs(vtwaterbody) # TRUE
crs_use <- st_crs(nywaterbody)


# define bounding box for area of interest --------------------------------
# Set x and y limits for the plot, then make the points an sf object,
# set the crs as the same for my_rivers

bkbb
(xlims <- c(bkbb["left"], bkbb["right"]))
(ylims <- c(bkbb["bottom"], bkbb["top"]))
box_coords <- tibble(x = xlims, y = ylims) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(crs_use)

#get the bounding box of the two x & y coordinates, make sfc
bounding_box <- st_bbox(box_coords) %>% st_as_sfc()


# get data subsets --------------------------------------------------------

ny_subset <- st_intersection(nywaterbody, bounding_box)
head(ny_subset)
# xmin: -73.08765 ymin: 43.04115 xmax: -73.01976 ymax: 43.25849  NY subset
# xmin: -78.10127 ymin: 41.25012 xmax: -73.79908 ymax: 42.20005  NY full

nyline_sub <- st_intersection(nyline, bounding_box)
nyfline_sub <- st_intersection(nyfline, bounding_box)

vt_subset <- st_intersection(vtwaterbody, bounding_box)
head(vt_subset)
# xmin: -73.17421 ymin: 43.03771 xmax: -73.03093 ymax: 43.17072  VT subset
# xmin: -74.04581 ymin: 42.97491 xmax: -73.39284 ymax: 43.53337  VT full 

#.. compare ----

# left    bottom     right       top 
# -73.59000  43.03393 -72.96944  43.26000 # full bounding box
# xmin: -73.08765 ymin: 43.04115 xmax: -73.01976 ymax: 43.25849  # NY bounds (subset)
# xmin: -73.17421 ymin: 43.03771 xmax: -73.03093 ymax: 43.17072  # VT bounds (subset)

# VT seems to capture almost all that we need (?)

# map ---------------------------------------------------------------------

wbcolor <- "lightblue"

ny <- ggplot() + 
  geom_sf(data = bounding_box, fill = NA, color = 'red') + 
  geom_sf(aes(geometry=SHAPE), data = ny_subset, fill=wbcolor) +
  ggtitle("New York")
ny

vt <- ggplot() + 
  geom_sf(data = bounding_box, fill = NA, color = 'red') + 
  geom_sf(data = vt_subset, fill=wbcolor) +
  ggtitle("Vermont")


ny / vt

# both together
nyvt <- ggplot() + 
  geom_sf(data = bounding_box, fill = NA, color = 'blue') + 
  geom_sf(data = ny_subset, fill="lightblue") +
  geom_sf(data = vt_subset, fill="red") +
  ggtitle("New York and Vermont")
nyvt

ggplot() + 
  geom_sf(data = bounding_box, fill = NA, color = 'red') + 
  geom_sf(aes(geometry=SHAPE), data = nyline_sub, fill=wbcolor, color=wbcolor, size=10) +
  ggtitle("New York")

ggplot() + 
  geom_sf(data = bounding_box, fill = NA, color = 'red') + 
  geom_sf(aes(geometry=SHAPE), data = nyfline_sub, fill=wbcolor, color=wbcolor) +
  ggtitle("New York")

ggplot() + 
  geom_sf(data = bounding_box, fill = NA, color = 'red') + 
  geom_sf(data = ny_subset, fill=wbcolor, color=wbcolor) +
  geom_sf(aes(geometry=SHAPE), data = nyfline_sub, fill=wbcolor, color=wbcolor) +
  ggtitle("New York data")


# https://ryanpeek.org/2018-11-13-a-few-mapping-tips/


