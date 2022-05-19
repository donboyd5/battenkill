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




# ONETIME save vaa --------------------------------------------------------
vaa <- get_vaa()
class(vaa) # df
names(vaa)
glimpse(vaa)
saveRDS(vaa, file=here::here("data", "vaa.rds")) # 13.7 mb


# get NHDplus special features --------------------------------------------

#.. define bounding box ----
(bbvals <- readRDS(here::here("data", "gis", "bkbb2.rds")))
# left    bottom     right       top 
# -73.58000  43.04393 -72.97944  43.25000 
names(bbvals) <- c("xmin", "ymin", "xmax", "ymax") # sf requires these names
bbox <- sf::st_bbox(bbvals, crs =  4326)
bbox

#.. get and save the data ----
pdata <- plot_nhdplus(bbox = bbox, streamorder = NULL) # streamorder higher numbers mean fewer streams, NULL is default
names(pdata)
# "plot_bbox" "outlets" "flowline" "basin" "catchment" "network_wtbd" "off_network_wtbd"
# pdata is a list of special features -- save it
saveRDS(pdata, file=here::here("data", "gis", "sfeatures", "nhdplus_sf.rds"))



# explore the data --------------------------------------------------------


pdata$plot_bbox
glimpse(pdata$flowline)
# gnis_name has the names of rivers, etc.
count(pdata$flowline, gnis_name) %>% ht

# tmp <- count(pdata$flowline %>% as.tibble(), gnis_id, gnis_name) %>% arrange(gnis_name)

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

