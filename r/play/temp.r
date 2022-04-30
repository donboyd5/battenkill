library(RgoogleMaps)

# GetMap.bbox {RgoogleMaps}

GetMap.bbox(lonR, latR, center, size = c(640, 640), 
            destfile = "MyTile.png", MINIMUMSIZE = FALSE, RETURNIMAGE = TRUE, 
            GRAYSCALE = FALSE, NEWMAP = TRUE, zoom, verbose = 0, 
            SCALE = 1, type = c("google", "google-m", "google-s", 
                                "osm", "osm-hot", "stamen-toner", "stamen-terrain", 
                                "stamen-watercolor")[1], urlBase = "http://mt1.google.com/vt/lyrs=m", 
            tileDir = "/tmp/", ...)


mymarkers <- cbind.data.frame(lat = c(38.898648,38.889112, 38.880940), 
                              lon = c(-77.037692, -77.050273, -77.03660), size =  c('tiny','tiny','tiny'), 
                              col = c('blue', 'green', 'red'), char = c('','',''))

##get the bounding box:
bb <- qbbox(lat = mymarkers[,"lat"], lon = mymarkers[,"lon"])

##download the map:
MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "DC.png", GRAYSCALE =TRUE,
                     markers = mymarkers)

bb2 <- list()
bb2$latR <- c(bkbb$bottom, bkbb$top)
bb2$lonR <- c(bkbb$left, bkbb$right)
bb2
mean(bb2$latR)
mean(bb2$lonR)

bmap <- GetMap.bbox(bb2$lonR, bb2$latR, destfile = "batt.png", 
                    GRAYSCALE =FALSE, markers=c())
# $lat.center
# [1] 43.14696
# $lon.center
# [1] -73.27972
centers

ggmap(MyMap)


##The function qbbox() basically computes a bounding box for the given lat,lon 
#points with a few additional options such as quantile boxes, additional buffers, etc.  
bb <- qbbox(c(40.702147,40.711614,40.718217),c(-74.015794,-74.012318,-73.998284), 
            TYPE = "all", margin = list(m=rep(5,4), TYPE = c("perc", "abs")[1]));
##download the map:           
MyMap <- GetMap.bbox(bb$lonR, bb$latR,destfile = "MyTile3.png", maptype = "satellite") 


library(OpenStreetMap)
