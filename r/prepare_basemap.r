

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


# get gis info ------------------------------------------------------------

#.. minimum bounding box ----
bkill <- readRDS(here::here("data", "gis", "bkill.rds"))

bkbb <- bkill %>%
  # lbrt order
  summarise(left=min(longitude),
            bottom=min(latitude),
            right=max(longitude),
            top=max(latitude))

bkbb %>% as.data.frame() # to see more decimals


# functions ---------------------------------------------------------------

getrds <- function(fnames){
  for(fname in fnames){
    sdf <- readRDS(here::here("data", "gis", "sfeatures", paste0(fname, ".rds")))
    assign(fname, sdf, envir = .GlobalEnv)
  }
}


# get special features ----------------------------------------------------

flist <- c("locality", "main_streets", "places", "roads", "secondary", 
           "stborders", "streets", "villages")

getrds(flist)
# getrds("stborders")

# NHD is a list of special features 
nhd <- readRDS(file=here::here("data", "gis", "sfeatures", "nhdplus_sf.rds"))

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


# define map boundaries ----
hstretch <- c(-.02, 0)
vstretch <- c(-.01, 0)

xlims <- bkbb2[c("left", "right")] + hstretch
ylims <- bkbb2[c("bottom", "top")] + vstretch

# define colors ----
mstrcolor <- "darkgrey"  #  '#ff9999'
strcolor <- "lightgrey"  # '#eedede'

mstrcolor <- "black"  #  '#ff9999'
strcolor <- "darkgrey"  # '#eedede'

bodycolor <- "#c6e1e3"
waycolor <- "green"

#.. explore features on the map -----

# explore osm special features
ggplot() +
  geom_sf(data = places$osm_polygons, colour = 'lightgrey', fill="lightgrey") +
  geom_sf(data = places$osm_multipolygons, colour = 'lightgrey', fill="lightgrey") +
  geom_sf(data = places$osm_lines, colour = 'black') +
  geom_sf(data = places$osm_polygons, colour = 'lightgrey', fill="lightgrey") +
  geom_sf(data = places$osm_multipolygons, colour = 'lightgrey', fill="lightgrey") +
  geom_sf(data = places$osm_lines, colour = 'lightgrey') +
  geom_sf(data = main_streets$osm_lines, color = mstrcolor, size = 1) +
  geom_sf(data = secondary$osm_lines, size = 0.75, color = strcolor) +
  geom_sf(data = stborders$osm_lines, color = "grey55", size = 1, linetype="longdash") +
  coord_sf(xlim=xlims,
           ylim=ylims, 
           expand=FALSE) +
  theme_minimal()

# explore nhd special features
flowline <- nhd$flowline
network_wtbd <- nhd$network_wtbd
off_network_wtbd <- nhd$off_network_wtbd

ggplot() +
  geom_sf(data=flowline %>% filter(gnis_id != "950128"), colour="blue", fill="blue", size=0.75) + # remove Fish Creek
  geom_sf(data=flowline %>% filter(gnis_id=="1460572"), colour="blue", fill="blue", size=1.5) + # Battenkill
  geom_sf(data=flowline %>% filter(gnis_id=="970226"), colour="blue", fill="blue", size=3) + # Hudson
  geom_sf(data=network_wtbd, colour="cyan", fill="cyan") +
  geom_sf(data=off_network_wtbd, colour="lightblue", fill="lightblue", alpha=0.1) +
  # geom_sf_text(aes(label = gnis_name), data=flowline, size=2, position=position_dodge(width=0.1)) +
  # geom_sf_text(aes(label = gnis_name), data=flowline, size=2, position=position_jitter()) +
  geom_sf_text(aes(label = gnis_name), data=flowline %>% filter(gnis_id != "950128"), size=2, position=position_dodge2()) +
  geom_sf_text(aes(label = gnis_name), data=network_wtbd, size=2, position=position_dodge2()) +
  labs(x=NULL, y=NULL) +
  coord_sf(xlim=xlims,
           ylim=ylims, 
           expand=FALSE) +
  theme_minimal()


# explore combined special features
posm <- ggplot() +
  geom_sf(data = places$osm_polygons, colour = 'lightgrey', fill="lightgrey") +
  geom_sf(data = places$osm_multipolygons, colour = 'lightgrey', fill="lightgrey") +
  geom_sf(data = places$osm_lines, colour = 'black') +
  geom_sf(data = places$osm_polygons, colour = 'lightgrey', fill="lightgrey") +
  geom_sf(data = places$osm_multipolygons, colour = 'lightgrey', fill="lightgrey") +
  geom_sf(data = places$osm_lines, colour = 'lightgrey') +
  geom_sf(data = main_streets$osm_lines, color = mstrcolor, size = 1) +
  geom_sf(data = secondary$osm_lines, size = 0.75, color = strcolor) +
  geom_sf(data = stborders$osm_lines, color = "grey55", size = 1, linetype="longdash") +
  coord_sf(xlim=xlims,
           ylim=ylims, 
           expand=FALSE)  

posm 

posm  +
  geom_sf(data=flowline %>% filter(gnis_id != "950128"), colour="blue", fill="blue", size=0.75) + # remove Fish Creek
  geom_sf(data=flowline %>% filter(gnis_id=="1460572"), colour="blue", fill="blue", size=1.5) + # Battenkill
  geom_sf(data=flowline %>% filter(gnis_id=="970226"), colour="blue", fill="blue", size=3) + # Hudson
  geom_sf(data=network_wtbd, colour="cyan", fill="cyan") +
  geom_sf(data=off_network_wtbd, colour="lightblue", fill="lightblue", alpha=0.1) +
  # geom_sf_text(aes(label = gnis_name), data=flowline, size=2, position=position_dodge(width=0.1)) +
  # geom_sf_text(aes(label = gnis_name), data=flowline, size=2, position=position_jitter()) +
  geom_sf_text(aes(label = gnis_name), data=flowline %>% filter(gnis_id != "950128"), size=2, position=position_dodge2()) +
  geom_sf_text(aes(label = gnis_name), data=network_wtbd, size=2, position=position_dodge2()) +
  labs(x=NULL, y=NULL) +
  coord_sf(xlim=xlims,
           ylim=ylims, 
           expand=FALSE) +
  theme_minimal()

# make the base map ----

basemap <- posm  +
  geom_sf(data=flowline %>% filter(gnis_id != "950128"), colour="blue", fill="blue", size=0.75) + # remove Fish Creek
  geom_sf(data=flowline %>% filter(gnis_id=="1460572"), colour="blue", fill="blue", size=1.5) + # Battenkill
  geom_sf(data=flowline %>% filter(gnis_id=="970226"), colour="blue", fill="blue", size=3) + # Hudson
  geom_sf(data=network_wtbd, colour="cyan", fill="cyan") +
  geom_sf(data=off_network_wtbd, colour="lightblue", fill="lightblue", alpha=0.1) +
  # geom_sf_text(aes(label = gnis_name), data=flowline, size=2, position=position_dodge(width=0.1)) +
  # geom_sf_text(aes(label = gnis_name), data=flowline, size=2, position=position_jitter()) +
  geom_sf_text(aes(label = gnis_name), data=flowline %>% filter(gnis_id != "950128"), size=2, position=position_dodge2()) +
  geom_sf_text(aes(label = gnis_name), data=network_wtbd, size=2, position=position_dodge2()) +
  labs(x=NULL, y=NULL) +
  coord_sf(xlim=xlims,
           ylim=ylims, 
           expand=FALSE) +
  theme_minimal()

saveRDS(basemap, here::here("data", "gis", "basemap.rds"))

# explore adding points ----
bkill <- readRDS(here::here("data", "bkill.rds"))
bkbb2 <- readRDS(here::here("data", "gis", "bkbb2.rds"))
# basemap <- readRDS(here::here("data", "basemap.rds"))

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
