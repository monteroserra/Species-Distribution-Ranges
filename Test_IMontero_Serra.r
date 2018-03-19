# Author: Ignasi Montero Serra, PhD Candidate, University of Barcelona, Spain
# E-mail: monteroserra@gmail.com 
# Date: 18 March 2018
# Title: R Test for Google Summer Course

# Required packages

install.packages(c("rgbif","rgeos", "sp","rworldmap", "mapdata"))

library(rgbif) 
library(rgeos)
library(sp)
library(rworldmap)
library(mapdata)

# Test Levels: Easy, Medium, Hard

# 1.Easy: Download occurrence data from GBIF and display them on a map

species = "Lynx lynx"
GBIF_Info <- occ_search(scientificName = species, limit = 1000)
GBIF_Occurrences = GBIF_Info$data

map("worldHires", col="grey20", fill=T, interior=T, bg="white",myborder=c(0.01,0.01),
    boundary=T,  lty=0.6)
box()

map.axes()

points(GBIF_Occurrences[,c("decimalLongitude","decimalLatitude")], pch=21, cex=1, bg="firebrick")

# 1.Medium: Draw a convex hull
xy <- na.omit(GBIF_Occurrences[,c('decimalLongitude','decimalLatitude')])
coordinates(xy) <- ~decimalLongitude + decimalLatitude
Convex_hull <- gConvexHull(xy)
proj4string(Convex_hull) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

map("worldHires",
    col="grey20", fill=T, interior=T, bg="white",myborder=c(0.01,0.01),
    boundary=T, resolution=1,  lty=0.6)

map.axes()
text(19.5,-40, "Lynx lynx", cex=0.6)

points(GBIF_Occurrences[,c("decimalLongitude","decimalLatitude")], pch=21, cex=1, bg="steelblue")
plot(Convex_hull, add=T, lwd=2, col="#B2222280")


# 3. Hard: intersesction of convex hull and world map (keep only part of the polygon which is on land) 

world_map <- getMap(resolution="low")

#  Here we select intersection areas between the convex hull and the world map
gIntersects_output<- gIntersects(world_map, Convex_hull, byid = TRUE)
gIntersects_output2 <- lapply(which(gIntersects_output), function(i){gIntersection(worldmap[i,], Convex_hull)})

clip <- SpatialPolygons(lapply(1:length(gIntersects_output2),
                                    function(a) {poligons <- slot(intersection2[[a]], "polygons")[[1]]
                                    slot(poligons, "ID") <- as.character(a); poligons}))

# Final result of clipped species range fotr "Lynx lynx"
plot(clip, col="steelblue", main = "Lynx lynx")
box()



