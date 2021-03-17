

load("Bay of Biscay and the Iberian Coast_region_grid_sensitivity.RData")  
BoB <- coordinates(Region)
BoB <- as.data.frame(BoB)
BoB <- cbind(BoB, as.factor(Region$division))
colnames(BoB) <- c("long","lat","colare")

load("Celtic Seas_region_grid_sensitivity.RData")  
CS <- coordinates(Region)
CS <- as.data.frame(CS)
CS <- cbind(CS, as.factor(Region$division))
colnames(CS) <- c("long","lat","colare")

load("Greater North Sea_region_grid_sensitivity.RData")  
NS <- coordinates(Region)
NS <- as.data.frame(NS)
NS <- cbind(NS, as.factor(Region$division))
colnames(NS) <- c("long","lat","colare")

load("Baltic Sea_region_grid_sensitivity.RData")  
BS <- coordinates(Region)
BS <- as.data.frame(BS)
BS <- cbind(BS, as.factor(Region$division))
colnames(BS) <- c("long","lat","colare")

colnew <- c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a")
colnew <- rep(colnew,3)

new <- rbind(CS,BS,NS,BoB)
map <- ggplot() + geom_point(data=new,aes(x=long,y=lat,color=colare)) +
                  scale_colour_manual(values=colnew)

# get shapefile ecoregion
pathnew <- "C:/Users/pdvd/Dropbox/Werk/Benthic assessments BENTHIS and ICES/ICES data one folder"
shapeEcReg <- readOGR(dsn = paste(pathnew,"/ICES_ecoregions",sep="/") ,layer="ICES_ecoregions_20171207_erase_ESRI")
shapeReg  <- subset(shapeEcReg, Ecoregion== "Celtic Seas" | Ecoregion== "Bay of Biscay and the Iberian Coast" |
                      Ecoregion== "Greater North Sea" | Ecoregion== "Baltic Sea")

 map +  geom_polygon(data = shapeReg, aes(x = long, y = lat, group = group),color="black",fill=NA)
