# NOTE: I may want the high res data
# TODO:
#   figure out labeling
#   maybe remove Hudson, add separately
#   maybe remove Fish Creek west of Hudson
#   topo baselayer?

# Uncomment to install!
# install.packages("nhdplusTools")
# https://usgs-r.github.io/nhdplusTools/
# https://github.com/usgs-r/nhdplusTools/
# https://usgs-r.github.io/nhdplusTools/articles/nhdplusTools.html
# https://usgs-r.github.io/nhdplusTools/articles/US_data.html
# https://rdrr.io/cran/dataRetrieval/f/vignettes/dataRetrieval.Rmd

# https://www.epa.gov/waterdata/basic-information#:~:text=This%20hydrologically%2Dconditioned%20surface%20enables,for%20estimating%20NHDPlus%20stream%20flow.

# https://usgs-r.github.io/
# https://github.com/USGS-R/nhdplusTools/tree/main/docs/awra_2019

# https://ucd-cws.github.io/CABW2020_R_training/m3_2_nhdtoolsPlus.html
# https://ryanpeek.org/2017-11-05-mapping-with-sf-part-2/

# https://www.waterqualitydata.us/#countrycode=US&statecode=US%3A50&countycode=US%3A50%3A003&bBox=-74%2C43%2C-72%2C44&siteType=Stream&startDateLo=01-01-2010&startDateHi=04-01-2022&mimeType=csv&providers=NWIS&providers=STEWARDS&providers=STORET


# US water gages
# https://maps.waterdata.usgs.gov/mapper/index.html
# mouth of Battenkill is near this inactive site:
# Site Number: 01329644
# Site Name: BATTEN KILL AT CLARKS MILLS NR SCHUYLERVILLE NY
# Site Type: Stream
# Agency: USGS
# Access Data https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=01329644
# it has comid 22288441
# mouth appears to be at approximately 43.115, -73.574 lat, lon


library(tidyverse)
library(nhdplusTools)
library(dataRetrieval)
library(sf)
library(mapview)
library(tmap)

# > get_nldi_sources()
# source                                     sourceName                                                            features
# 1           comid                                  NHDPlus comid          https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid
# 2        ca_gages                 Streamgage catalog for CA SB19       https://labs.waterdata.usgs.gov/api/nldi/linked-data/ca_gages
# 3  geoconnex-demo              geoconnex contribution demo sites https://labs.waterdata.usgs.gov/api/nldi/linked-data/geoconnex-demo
# 4      gfv11_pois USGS Geospatial Fabric V1.1 Points of Interest     https://labs.waterdata.usgs.gov/api/nldi/linked-data/gfv11_pois
# 5         huc12pp                              HUC12 Pour Points        https://labs.waterdata.usgs.gov/api/nldi/linked-data/huc12pp
# 6        nmwdi-st          New Mexico Water Data Initative Sites       https://labs.waterdata.usgs.gov/api/nldi/linked-data/nmwdi-st
# 7          nwisgw                         NWIS Groundwater Sites         https://labs.waterdata.usgs.gov/api/nldi/linked-data/nwisgw
# 8        nwissite                       NWIS Surface Water Sites       https://labs.waterdata.usgs.gov/api/nldi/linked-data/nwissite
# 9        ref_gage                   geoconnex.us reference gages       https://labs.waterdata.usgs.gov/api/nldi/linked-data/ref_gage
# 10          vigil                             Vigil Network Data          https://labs.waterdata.usgs.gov/api/nldi/linked-data/vigil
# 11           wade                  Water Data Exchange 2.0 Sites           https://labs.waterdata.usgs.gov/api/nldi/linked-data/wade
# 12            WQP                           Water Quality Portal            https://labs.waterdata.usgs.gov/api/nldi/linked-data/wqp

# first identify the gage of interest: This on on the American  -- note that we can also use a latitude and longitude point
nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-11444500") # from example

nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-01329644") # right near Hudson
# nldi_nwis <- list(43.115, -73.574)  # does not work

# USGS 01329610 FLY CREEK TRIBUTARY NEAR FLY SUMMIT NY

# next download the basin:
basin <- get_nldi_basin(nldi_feature = nldi_nwis)
basin
# Simple feature collection with 1 feature and 0 fields
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: -73.57211 ymin: 42.97496 xmax: -72.93485 ymax: 43.34181
# Geodetic CRS:  WGS 84
plot(basin)
# look at Google maps -73.57, 43.2
# does not go far enough east!!!
# we need to go to -72.98944 or farther east, maybe -72.9

ggplot() +
  geom_sf(data = basin, fill = NA, color = "red") 

# pull mainstems from a USGS gage
us_main <- navigate_nldi(nldi_feature = nldi_nwis,
                         mode="UM",
                         distance_km =  100)
# $origin, $UM_flowlines
# $UM_flowlines
# Simple feature collection with 26 features and 1 field
# Geometry type: LINESTRING
# Dimension:     XY
# Bounding box:  xmin: -73.57143 ymin: 43.07037 xmax: -73.00656 ymax: 43.24904
# Geodetic CRS:  WGS 84

ggplot() +
  # geom_sf(data = basin, fill = NA, color = "red") +
  geom_sf(data = us_main$UM_flowlines, color = "blue", size = 1) # main battenkill


# get tributaries 
# find out comid of the gage or point:
(comid_pt <- discover_nhdplus_id(nldi_feature = nldi_nwis))

us_tribs <- navigate_nldi(
  nldi_feature = list(featureSource="comid", featureID=comid_pt),
  mode="UT",
  distance_km = 20)
us_tribs
# origin, UT_flowlines
# $UT_flowlines
# Simple feature collection with 10 features and 1 field
# Geometry type: LINESTRING
# Dimension:     XY
# Bounding box:  xmin: -73.57143 ymin: 43.02505 xmax: -73.41726 ymax: 43.15925
# Geodetic CRS:  WGS 84
# # A tibble: 10 × 2
# nhdplus_comid                                                                                  geometry
# <chr>                                                                                  <LINESTRING [°]>
#   1 22288441      (-73.53675 43.11928, -73.53682 43.1193, -73.53699 43.11933, -73.53717 43.11935, -73.53...


ggplot() +
  geom_sf(data = basin, fill = NA, color = "red") +
  geom_sf(data = us_main$UM_flowlines, color = "blue", size = 1) + # main battenkill
  geom_sf(data = us_tribs$UT_flowlines, color = "blue", size = 1)


# find out comid of the gage or point:
(comid_pt <- discover_nhdplus_id(nldi_feature = nldi_nwis))
# 14982644 for example
# 22288441 for my bk gage

# use the comid to download streamlines U/S or D/S of the gage
# here downstream mainstem segments 50 km from the starting COMID
ds_main <- navigate_nldi(list("comid", as.character(comid_pt)), # if leading zeros, pad??
                         mode="DM",
                         distance_km =  50)

# let's see what downstream diversions look like:
ds_div <- navigate_nldi(list("comid", "14983602"),
                        mode="DD",
                        distance_km =  50)

mapview(ds_div, legend=FALSE) + mapview(ds_main, legend=FALSE)


# now use his comid-pt ----
# let's see what downstream diversions look like:
ds_main <- navigate_nldi(list("comid", "14982644"),
                         mode="DM",
                         distance_km =  50)

ds_div <- navigate_nldi(list("comid", "14982644"),
                        mode="DD",
                        distance_km =  50)

mapview(ds_div, legend=FALSE) + mapview(ds_main, legend=FALSE)

# pull mainstems from a USGS gage
us_main <- navigate_nldi(nldi_feature = nldi_nwis,
                         mode="UM",
                         distance_km =  100)

# pull mainstems from an X/Y (lat/lon) point, needs to be st_sfc
xypoint <- st_sfc(st_point(c(-120.616, 38.81448)), crs = 4326)
xycomid <- discover_nhdplus_id(xypoint)

us_tribs <- navigate_nldi(
  nldi_feature = list(featureSource="comid", featureID=xycomid),
  mode="UT",
  distance_km = 20)

# find other upstream gages or NWIS sites
us_gages <- navigate_nldi(nldi_feature = nldi_nwis, 
                          mode="UM",
                          data_source = "nwissite",
                          distance=100)

# djb
# xmin: -120.8389 ymin: 38.61433 xmax: -119.9833 ymax: 38.96546
xlims <- c(-120.8389, -119.9833)
ylims <- c(38.61433, 38.96546)
# end djb

plot(basin$geometry, border = alpha("darkblue", alpha = 0.5), lwd=2)
plot(ds_main$geometry, col="darkblue", lwd=3, add=T, xlim = xlims, ylim = ylims)
plot(ds_div$geometry, col="skyblue", add=T)
plot(us_main$geometry, col="dodgerblue", add=T)
plot(us_tribs$geometry, col="purple", add=T)
plot(us_gages$geometry, bg="maroon", pch=21, add=T)

head(ds_main)
head(ds_div)

ggplot() +
  geom_sf(data=basin, color="red") +
  # geom_sf(data=ds_main$origin, colour="green") +
  # geom_sf(data=ds_main$DM_flowlines, colour="darkblue") +
  # geom_sf(data=ds_div$DD_flowlines, colour="green") +
  # geom_sf(data=us_main$UM_origin, colour="darkblue", fill="darkblue", size=20) +
  geom_sf(data=us_main$UM_flowlines, colour="darkblue") +
  geom_sf(data=us_tribs$UT_flowlines, colour="purple")


str(ds_main) # origin, sfc_LINESTRING; DM_flowlines, sfc_LINESTRING
str(ds_div) # origin, sfc_LINESTRING; DD_flowlines, sfc_LINESTRING
str(us_main) # origin, sfc_point; UM_flowlines, sfc_LINESTRING
str(us_tribs) # origin, sfc_LINESTRING; UT_flowlines, sfc_LINESTRING
str(us_gages) # origin sfc_point, UM_nwissite, sfc_point

tmp <- ds_main$origin
d <- tmp$geometry
str(d)


# my turn ----
# USGS 01329000 BATTEN KILL AT ARLINGTON, VT
# USGS 01329610 FLY CREEK TRIBUTARY NEAR FLY SUMMIT NY

# first identify the gage of interest: This on on the American
nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-01329000")

# next download the basin:
basin <- get_nldi_basin(nldi_feature = nldi_nwis)
basin
plot(basin)


# find out comid of the gage or point:
(comid_pt <- discover_nhdplus_id(nldi_feature = nldi_nwis))
# 22288053

# use the comid to download streamlines U/S or D/S of the gage
# here downstream mainstem segments 50 km from the starting COMID
ds_main <- navigate_nldi(list("comid", "22288053"),
                         mode="DM",
                         distance_km =  50)

# let's see what downstream diversions look like:
ds_div <- navigate_nldi(list("comid", "22288053"),
                        mode="DD",
                        distance_km =  50)

mapview(ds_div, legend=FALSE) + mapview(ds_main, legend=FALSE)

# find other upstream gages or NWIS sites
us_gages <- navigate_nldi(nldi_feature = nldi_nwis, 
                          mode="UM",
                          data_source = "nwissite",
                          distance=100)


plot(basin$geometry, border = alpha("darkblue", alpha = 0.5), lwd=2)
plot(ds_main$geometry, col="darkblue", lwd=3, add=T)
plot(ds_div$geometry, col="skyblue", add=T)
plot(us_main$geometry, col="dodgerblue", add=T)
plot(us_tribs$geometry, col="purple", add=T)
plot(us_gages$geometry, bg="maroon", pch=21, add=T)


# new section ----

bk <- c(mean(bkbb2[c("left", "right")]), mean(bkbb2[c("bottom", "top")]))
bk

start_point <- st_sfc(st_point(bk), crs = 4269)
start_point <- st_sfc(st_point(c(-89.362239, 43.090266)), crs = 4269)

start_comid <- discover_nhdplus_id(start_point)

dist <- 100
dist <- 1000
flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = dist)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

flowline <- subset$NHDFlowline_Network
catchment <- subset$CatchmentSP
waterbody <- subset$NHDWaterbody

## Or using a file:

flowline <- sf::read_sf(subset_file, "NHDFlowline_Network")
catchment <- sf::read_sf(subset_file, "CatchmentSP")
waterbody <- sf::read_sf(subset_file, "NHDWaterbody")

plot(sf::st_geometry(flowline), col = "blue")
plot(start_point, cex = 1.5, lwd = 2, col = "red", add = TRUE)
plot(sf::st_geometry(catchment), add = TRUE)
plot(sf::st_geometry(waterbody), col = rgb(0, 0, 1, alpha = 0.5), add = TRUE)


# version 2 ----
# https://usgs-r.github.io/nhdplusTools/
# https://usgs-r.github.io/nhdplusTools/articles/nhdplushr.html
library(nhdplusTools)
library(sf)

work_dir <- file.path(nhdplusTools_data_dir(), "hr_v_cache") # where we'll put the nhd data

# source the code we'll need to get at the data and get sample data
source(system.file("extdata/sample_data.R", package = "nhdplusTools"))
# source(system.file("extdata", "utils.R", package = "nhdplusTools"))
# data_dir <- file.path(tempdir(check = TRUE), "nhdplusTools") # maybe a place for interim data
# the download_pkg_data function is defined in utils.r
# download_pkg_data("sample_natseamless.gpkg",  
#                   "https://usgs-r.github.io/nhdplusTools/data/sample_natseamless.gpkg",
#                   data_dir)
# sample_data <- file.path(data_dir, "sample_natseamless.gpkg")

hr_gpkg <- file.path(work_dir, "hr_data.gpkg")

# Make a plot and get some background NHDPlusV2 data.
plot_data <- plot_nhdplus(list("nwissite", "USGS-05428500"), streamorder = 3,
                          nhdplus_data = sample_data,
                          stoponlargerequest = FALSE)


# Find the HU04 we are interested in.----
hu04 <- unique(substr(plot_data$flowline$reachcode, 1, 4))

# Download some NHDPlusHR Data
hr_data_dir <- download_nhdplushr(work_dir, hu04)
# C:\Users\donbo\AppData\Roaming\R\data\R\nhdplusTools\hr_v_cache\07
# NHDPLUS_H_0709_HU4_GDB.gdb
# NHDPLUS_H_0709_HU4_GDB.jpg
# NHDPLUS_H_0709_HU4_GDB.xml


# Projection and simplification for demo purposes.
hr <- get_nhdplushr(work_dir, out_gpkg = hr_gpkg,
                    proj = 3857)
str(hr)

(start_index <- get_flowline_index(st_transform(hr$NHDFlowline, 5070),
                                   st_transform(plot_data$outlets, 5070),
                                   search_radius = 200)) # meters albers eq area

ids <- get_UT(hr$NHDFlowline, start_index$COMID)

hr_subset <- subset_nhdplus(ids, nhdplus_data = hr_gpkg)

plot_nhdplus(list("nwissite", "USGS-05428500"), streamorder = 2, 
             nhdplus_data = sample_data, overwrite = TRUE,
             plot_config = list(flowline = list(lwd = 2.5),
                                basin = list(lwd = 3)),
             stoponlargerequest = FALSE)

plot(st_geometry(hr$NHDPlusCatchment), lwd = 0.25, add = TRUE)
plot(st_geometry(hr$NHDFlowline), col = "blue", lwd = 0.5, add = TRUE)

plot(st_geometry(st_transform(hr_subset$NHDFlowline, 3857)),
     col = "cyan", lwd = 1, add = TRUE)



# https://usgs-r.github.io/nhdplusTools/articles/plot_nhdplus.html

plot_nhdplus("05428500")

# us data vignette ----
# Uncomment to install!
# install.packages("nhdplusTools")

library(nhdplusTools)
library(sf)

start_point <- st_sfc(st_point(c(-89.362239, 43.090266)), crs = 4269)
start_comid <- discover_nhdplus_id(start_point)
class(start_comid) # integer

# this appears to 
flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 1000)
class(flowline) # list
names(flowline) # origin -- an sf, UT_flowlines -- an sf
str(flowline)
flowline
# note that flowline$UT_flowlines (abbreviated to UT below) has a column,
#  nhdplus_comid - with all of the comits on this path, I think
ggplot() +
  geom_sf(data=flowline$UT_flowlines)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)
class(subset)
names(subset)
str(subset)
# "NHDFlowline_Network" "CatchmentSP" "NHDArea" "NHDWaterbody" "NHDFlowline_NonNetwork"
# each is a special feature


flowline <- subset$NHDFlowline_Network
catchment <- subset$CatchmentSP
waterbody <- subset$NHDWaterbody

## Or using a file:

flowline <- sf::read_sf(subset_file, "NHDFlowline_Network")
catchment <- sf::read_sf(subset_file, "CatchmentSP")
waterbody <- sf::read_sf(subset_file, "NHDWaterbody")

plot(sf::st_geometry(flowline), col = "blue")
plot(start_point, cex = 1.5, lwd = 2, col = "red", add = TRUE)
plot(sf::st_geometry(catchment), add = TRUE)
plot(sf::st_geometry(waterbody), col = rgb(0, 0, 1, alpha = 0.5), add = TRUE)


# my turn ----
# nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-01329644") # right near Hudson
# it has comid 22288441
# mouth appears to be at approximately 43.115, -73.574 lat, lon

bkstart <- c(-73.574, 43.115) # note that they use long, lat
start_point <- st_sfc(st_point(bkstart), crs = 4269)
# start_point <- st_sfc(st_point(c(-89.362239, 43.090266)), crs = 4269)

start_comid <- discover_nhdplus_id(start_point)
start_comid # 22288445 -- a bit different than I expect

start_comid <- 22288441  # looks better

# this appears to 
flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 200)
# origin
# xmin: -73.5761 ymin: 43.11161 xmax: -73.57484 ymax: 43.11596
# 43.11161 -73.5761 is near Schuylerville, west side of hudson
# 43.11596 -73.57484 also Schuy, slightly farther north
# true mouth appears to be at 43.109753, -73.574194
# my comid (inactive) 22288441 might be best

# note that flowline$UT_flowlines (abbreviated to UT below) has a column,
#  nhdplus_comid - with all of the comits on this path, I think
ggplot() +
  geom_sf(data=flowline$UT_flowlines)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT_flowlines$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)
class(subset)
names(subset)
str(subset)
# "NHDFlowline_Network" "CatchmentSP" "NHDArea" "NHDWaterbody" "NHDFlowline_NonNetwork"
# each is a special feature


subflowline <- subset$NHDFlowline_Network
catchment <- subset$CatchmentSP
waterbody <- subset$NHDWaterbody

## Or using a file:
# subflowline <- sf::read_sf(subset_file, "NHDFlowline_Network")
# catchment <- sf::read_sf(subset_file, "CatchmentSP")
# waterbody <- sf::read_sf(subset_file, "NHDWaterbody")

ggplot() +
  geom_sf(data=subflowline, colour="blue") +
  geom_sf(data=catchment, colour="lightgrey", alpha=.1) + # adds fill for area, plus lines for other flows
  geom_sf(data=waterbody, colour="cyan", fill="cyan") +
  geom_point(aes(x, y), data=tibble(x=-73.574, y=43.115), colour="red") +
  theme_minimal()


plot_data <- plot_nhdplus(
  outlets = list(featureSource = "nwissite", featureID = "USGS-05428500"), 
  gpkg = "temp.gpkg", overwrite = TRUE)

plot_data <- plot_nhdplus(
  outlets = list(featureSource = "nwissite", featureID = "USGS-01329644"), 
  gpkg = "temp.gpkg", overwrite = TRUE)

vaa <- get_vaa()
class(vaa) # df
names(vaa)
glimpse(vaa)
saveRDS(vaa, file=here::here("data", "vaa.rds")) # 13.7 mb


# https://maps.waterdata.usgs.gov/mapper/index.html
# mouth of Battenkill is near this inactive site:
# Site Number: 01329644
# Site Name: BATTEN KILL AT CLARKS MILLS NR SCHUYLERVILLE NY
# Site Type: Stream
# Agency: USGS
# Access Data https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=01329644
# it has comid 22288441
# mouth appears to be at approximately 43.115, -73.574 lat, lon

lon <- -89.36
lat <- 43.09

start_point <- sf::st_sfc(sf::st_point(c(lon, lat)),
                          crs = 4269)
start_comid <- discover_nhdplus_id(start_point, raindrop = TRUE)
start_comid
#> Simple feature collection with 2 features and 7 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -89.37037 ymin: 43.08522 xmax: -89.35393 ymax: 43.09491
#> Geodetic CRS:  WGS 84
#> # A tibble: 2 x 8
#>   id       gnis_name   comid reachcode raindrop_pathDi~ measure intersection_po~
#>   <chr>    <chr>       <int> <chr>                <dbl>   <dbl> <list>          
#> 1 nhdFlow~ Yahara R~  1.33e7 07090002~             90.5    42.0 <dbl [2]>       
#> 2 raindro~ NA        NA      NA                    NA      NA   <dbl [0]>       
#> # ... with 1 more variable: geometry <LINESTRING [°]>m

plot(sf::st_geometry(start_comid))
plot(sf::st_geometry(flowline), add = TRUE, col = "blue", lwd = 2)
plot(start_point, cex = 1.5, lwd = 2, col = "red", add = TRUE)


# using a bounding box ----
# https://usgs-r.github.io/nhdplusTools/reference/plot_nhdplus.html
# bbox object of class bbox with a defined crs. See examples.
# if outlets is omitted, the bbox input is required and all nhdplus data in the bounding box is plotted.
# streamorder integer only streams of order greater than or equal will be returned
plot_data <- plot_nhdplus(
  outlets = list(featureSource = "nwissite", featureID = "USGS-01329644"), 
  gpkg = "temp.gpkg", overwrite = TRUE)

source(system.file("extdata/sample_data.R", package = "nhdplusTools"))
pdata <- plot_nhdplus(sf::st_as_sf(data.frame(x = -89.36083,
                                     y = 43.08944),
                          coords = c("x", "y"), crs = 4326),
             streamorder = 2, 
             nhdplus_data = sample_data)
str(pdata)
names(pdata)
# "plot_bbox"  "outlets"   "flowline"  "basin"    "catchment"   "network_wtbd" "off_network_wtbd"
pdata$plot_bbox
glimpse(pdata$flowline)
# gnis_name has the names of rivers, etc.
count(pdata$flowline, gnis_name)

ggplot() +
  # geom_sf(data=pdata$outlets) +
  geom_sf(data=pdata$basin, colour="red", fill="red", alpha=0.1) +
  geom_sf(data=pdata$flowline, colour="blue") +
  geom_sf(data=pdata$catchment, colour="green", fill=NA) +
  geom_sf(data=pdata$network_wtbd, colour="cyan", fill="cyan") +
  geom_sf(data=pdata$off_network_wtbd, colour="darkgreen", fill="darkgreen") +
  theme_minimal()


# try getting battenkill data
plot_nhdplus(sf::st_as_sf(data.frame(x = -89.36083,
                                     y = 43.08944),
                          coords = c("x", "y"), crs = 4326),
             streamorder = 2,
             nhdplus_data = sample_data)

# bbox
bbox <- sf::st_bbox(c(xmin = -89.43, ymin = 43, xmax = -89.28, ymax = 43.1),
                    crs = "+proj=longlat +datum=WGS84 +no_defs")
plot_nhdplus(bbox = bbox, nhdplus_data = sample_data)
plot_nhdplus(bbox = bbox)
p <- plot_nhdplus(bbox = bbox, nhdplus_data = sample_data)
class(p)
str(p)

bbox <- sf::st_bbox(c(xmin = -89.43, ymin = 43, xmax = -89.28, ymax = 43.1),
                    crs = "+proj=longlat +datum=WGS84 +no_defs")

(bbvals <- readRDS(here::here("data", "gis", "bkbb2.rds")))
# left    bottom     right       top 
# -73.58000  43.04393 -72.97944  43.25000 
bbvalsun <- unname(bbvals)
# bbox <- sf::st_bbox(c(xmin = bbvals["left"], ymin = bbvals["bottom"], xmax = bbvals["right"], ymax = bbvals["top"]),
#                     crs =  "+proj=longlat +datum=WGS84 +no_defs")

# bbvalsun <- c(-73.5, 43.0, -72.8, 43.3)
bbox <- sf::st_bbox(c(xmin = bbvalsun[1], ymin = bbvalsun[2], xmax = bbvalsun[3], ymax = bbvalsun[4]),
                    crs =  4326)
bbox
pdata <- plot_nhdplus(bbox = bbox, streamorder = 0) # streamorder higher numbers mean fewer streams, NULL is default
names(pdata)
# "plot_bbox" "outlets" "flowline" "basin" "catchment" "network_wtbd" "off_network_wtbd"
pdata$plot_bbox
glimpse(pdata$flowline)
# gnis_name has the names of rivers, etc.
count(pdata$flowline, gnis_name) %>% ht

tmp <- count(pdata$flowline %>% as.tibble(), gnis_id, gnis_name) %>% arrange(gnis_name)

ggplot() +
  # geom_sf(data=pdata$outlets) +
  # geom_sf(data=pdata$basin, colour="red", fill="red", alpha=0.1) +
  geom_sf(data=pdata$flowline %>% filter(gnis_id != "950128"), colour="blue", fill="blue", size=0.75) + # remove Fish Creek
  geom_sf(data=pdata$flowline %>% filter(gnis_id=="1460572"), colour="blue", fill="blue", size=1.5) + # Battenkill
  geom_sf(data=pdata$flowline %>% filter(gnis_id=="970226"), colour="blue", fill="blue", size=3) + # Hudson
  # geom_sf(data=pdata$catchment, colour="green", fill=NA) +
  geom_sf(data=pdata$network_wtbd, colour="cyan", fill="cyan") +
  geom_sf(data=pdata$off_network_wtbd, colour="lightblue", fill="lightblue", alpha=0.1) +
  # geom_sf_text(aes(label = gnis_name), data=pdata$flowline, size=2, position=position_dodge(width=0.1)) +
  # geom_sf_text(aes(label = gnis_name), data=pdata$flowline, size=2, position=position_jitter()) +
  geom_sf_text(aes(label = gnis_name), data=pdata$flowline %>% filter(gnis_id != "950128"), size=2, position=position_dodge2()) +
  geom_sf_text(aes(label = gnis_name), data=pdata$network_wtbd, size=2, position=position_dodge2()) +
  labs(x=NULL, y=NULL) +
  theme_minimal()
