# Author: Ignasi Montero Serra, PhD Candidate, University of Barcelona, Spain
# E-mail: monteroserra@gmail.com 
# Date: 21 March 2018
# Title: Two functions to download, filter, and map species occurrences and species ranges


# Required packages

require(rgbif) 
require(rgeos)
require(sp)
require(rworldmap)
require(RColorBrewer)
require(geosphere)


get_and_map_distributions = function(species_names, occurrences_limit = 500, background_col="grey10"){ 
  
  #species_names is a list with the specific scientific species names in latin

occurrences = data.frame()


for(i in 1:length(species_Names)){
  
GBIF_Info <- occ_search(scientificName = species_Names[i], limit = occurrences_limit)

occurrences = rbind(occurrences, GBIF_Info$data[,c("species","decimalLongitude","decimalLatitude")])
}

map("worldHires", col=background_col, fill=T, interior=T, bg="white",myborder=c(0.01,0.01),boundary=T,  lty=0.6)
palette(brewer.pal(n = length(species_Names), name = "Set2"))
points(occurrences[,c("decimalLongitude","decimalLatitude")], pch=21, cex=1, bg=factor(occurrences$species))
map.axes()

return(occurrences)

}




# A Function to filter data on species occurrences from GBIF and computing species ranges 

Species_Ranges <- function(x){
  
  # x = a data.frame with occurrences and species names ("Ocurrence data.frame from map_distributions())

  #Setting projection WGS84
  wgs84 <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

  #We want to avoid inclduing species with very few records (at least 10), so we filter the dataframe
  
  filtered <- table(x$species)
  sortout <- names(filtered[filtered <= 9])
  filtered <- filtered[filtered > 9]
  
  data.filtered <- droplevels(subset(x, x$species %in% as.character(names(filtered))))
  
  #check for species where all coordinates are similar to avoid misleading ranges

  test <- split(data.filtered, f = data.filtered$species)
  test2 <- sapply(test, function(k){length(unique(k$decimalLongitude))})
  sortout2 <- names(test2[test2 == 1])
  sortout <- c(sortout, sortout2)
  data.filtered <- droplevels(subset(data.filtered, !data.filtered$species %in% sortout))

  test2 <- sapply(test, function(k){length(unique(k$decimalLatitude))})
  sortout2 <- names(test2[test2 == 1])
  sortout <- c(sortout, sortout2)
  data.filtered <- droplevels(subset(data.filtered, !data.filtered$species %in% sortout))
  
  #testing and deleting almost perfect fits
  test2 <- sapply(test, function(k){
  round(abs(cor(k[, "decimalLongitude"], k[, "decimalLatitude"])), 6)})
  sortout2 <- names(test2[test2 == 1])
  sortout <- c(sortout, sortout2)
  data.filtered <- droplevels(subset(data.filtered, !data.filtered$species %in% sortout))
  
  sortout <- sortout[!is.na(sortout)]
  
  if (length(sortout) > 0) {
    warning("some species with < 10 occurrences were found and deleted", paste("\n", sortout))
  }
  
data.filtered =  na.omit(data.filtered)
  
  if (length(data.filtered) == 0) {
    warning("no suitable distribution data was found for the selected species")
  }
  
  
  if (nrow(data.filtered) > 0) {

#calculate convex hulls
    
    
    
map("worldHires", col="grey20", fill=T, interior=T, bg="white",myborder=c(0.01,0.01),boundary=T,  lty=0.6)  

for(j in 1:length(levels(as.factor(data.filtered$species)))){


  xy = data.filtered[data.filtered$species==species_Names[j],-1]

coordinates(xy) <- ~decimalLongitude + decimalLatitude

Convex_hull <- gConvexHull(xy)

proj4string(Convex_hull) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

plot(Convex_hull, add=T, lwd=2, col=brewer.pal(n = length(levels(as.factor(data.filtered$species))), name = "BrBG")[j])

points(xy, pch=21, cex=1, bg=brewer.pal(n = length(levels(as.factor(data.filtered$species))), name = "BrBG")[j])

}
    
    
}
}
   


# Example for 4 species    

species_Names = c("Lynx lynx", "Lepus europaeus", "Corallium rubrum", "Panthera leo")

occurrences = get_and_map_distributions(species_Names, background_col = "grey30")

Species_Ranges(occurrences)


  
    
