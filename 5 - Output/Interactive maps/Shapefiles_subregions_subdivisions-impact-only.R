# get shapefiles with subregions and subdivisions

setwd("C:/Users/pdvd/Online for git/WKTRADE3/1 - Input env")

load("Greater North Sea_region_grid_sensitivity.RData")  
NS <- Region[,c("Ecoregion")]

load("Baltic Sea_region_grid_sensitivity.RData")  
BS <- Region[,c("Ecoregion","division")]
BS <- subset(BS,!(is.na(BS$division)))
BS <- BS[,"Ecoregion"]

new <- rbind(BS,NS)

EcReg <- maptools::unionSpatialPolygons(new,new$Ecoregion)

load("Greater North Sea_region_grid_sensitivity.RData")  
NS <- Region[,c("division")]

load("Baltic Sea_region_grid_sensitivity.RData")  
BS <- Region[,c("division")]

new <- rbind(BS,NS)

subdiv <-  maptools::unionSpatialPolygons(new,new$division)