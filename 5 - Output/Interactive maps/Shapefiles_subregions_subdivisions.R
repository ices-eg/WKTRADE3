# get shapefiles with subregions and subdivisions

setwd("C:/Users/danie/Documents/Online for git/WKTRADE3/1 - Input env")

load("Bay of Biscay and the Iberian Coast_region_grid_sensitivity.RData")  
BoB <- Region[,c("Ecoregion")]

load("Celtic Seas_region_grid_sensitivity.RData")  
CS <- Region[,c("Ecoregion")]

load("Greater North Sea_region_grid_sensitivity.RData")  
NS <- Region[,c("Ecoregion")]

load("Baltic Sea_region_grid_sensitivity.RData")  
BS <- Region[,c("Ecoregion","division")]
BS <- subset(BS,!(is.na(BS$division)))
BS <- BS[,"Ecoregion"]

new <- rbind(CS,BS,NS,BoB)

EcReg <- maptools::unionSpatialPolygons(new,new$Ecoregion)

load("Bay of Biscay and the Iberian Coast_region_grid_sensitivity.RData")  
BoB <- Region[,c("division")]

load("Celtic Seas_region_grid_sensitivity.RData")  
CS <- Region[,c("division")]

load("Greater North Sea_region_grid_sensitivity.RData")  
NS <- Region[,c("division")]

load("Baltic Sea_region_grid_sensitivity.RData")  
BS <- Region[,c("division")]

new <- rbind(CS,BS,NS,BoB)

subdiv <-  maptools::unionSpatialPolygons(new,new$division)

#subdiv <- sf::st_as_sf(subdiv)
#sf::write_sf(subdiv, "Subdivisions_ICES_TRADE3.shp")

