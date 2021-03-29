
# run library file in utilies

setwd("C:/Users/pdvd/Online for git/WKTRADE3/1 - Input env")

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

# Get the world map
worldMap <- map_data("world")
coordslim <- c(-15,30,30,70)
coordxmap <- round(seq(coordslim[1],coordslim[2],length.out = 4))
coordymap <- round(seq(coordslim[3],coordslim[4],length.out = 4))

# get shapefile ecoregion
pathnew <- "C:/Users/pdvd/Dropbox/Werk/Benthic assessments BENTHIS and ICES/ICES data one folder"
shapeEcReg <- readOGR(dsn = paste(pathnew,"/ICES_ecoregions",sep="/") ,layer="ICES_ecoregions_20171207_erase_ESRI")
shapeReg  <- subset(shapeEcReg, Ecoregion== "Celtic Seas" | Ecoregion== "Bay of Biscay and the Iberian Coast" |
                      Ecoregion== "Greater North Sea" | Ecoregion== "Baltic Sea")

new <- rbind(CS,BS,NS,BoB)
map <- ggplot() + geom_point(data=new,aes(x=long,y=lat,color=colare)) +
                  scale_colour_manual(values=colnew)
map <- map +  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")
map <- map +  geom_polygon(data = shapeReg, aes(x = long, y = lat, group = group),color="black",fill=NA)
map <- map +  theme(plot.background=element_blank(),
                          panel.background=element_blank(),
                          axis.text.y   = element_text(size=16),
                          axis.text.x   = element_text(size=16),
                          axis.title.y  = element_text(size=16),
                          axis.title.x  = element_text(size=16),
                          panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                          legend.text   = element_text(size=11),
                          legend.title  = element_text(size=11))+
  scale_x_continuous(breaks=coordxmap)+
  scale_y_continuous(breaks=coordymap)+
  coord_cartesian(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))
figmap<- figmap +   guides(colour = guide_legend(override.aes = list(size=5))) 