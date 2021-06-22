rm(list = ls())

### github folder
pathdir <- "C:/Users/pdvd/Online for git/WKTRADE3"

### folder for restricted VMS data
pathdir_nogit <- "C:/Users/pdvd/Online for git/WKTRADE3 - Fisheries restricted"

### get all libraries
source(paste(pathdir,"Utilities/Libraries_WKTRADE3.R",sep="/"))

### select time period
Period    <- 2009:2018    # period with fishing data to calculate impact
AssPeriod <- 2013:2018    # assessment period

### create list of marine reporting areas
Sregions <- c("Greater North Sea", "Baltic Sea","Celtic Seas","Bay of Biscay and the Iberian Coast")
NS_div <- c("Northern_NS", "Kattegat_NS", "Channel_NS", "Southern_NS" ,"NTrench_NS")
BS_div <- c("GulfF_BS", "GulfR_BS" ,"ArkBor_BS","Western_BS" ,"Proper_BS" ,"Bothnian_BS")
CS_div <- c("deep_CS" ,"south_CS", "North_CS" ,"Irishsea_CS", "Middle_CS") 
BoBIC_div <- c("Shallow_BoB", "ShallowNorth_IC","ShallowSouth_IC", "Galicia_IC",  "Deep_IC", "Deep_BoB")
divis <- c(NS_div,BS_div,CS_div,BoBIC_div)

### run all areas in a loop 
Assregion_index <- c(Sregions, divis)  # get the reporting region
EcoReg_index    <- c(Sregions, rep(Sregions[1],5),rep(Sregions[2],6),
                     rep(Sregions[3],5),rep(Sregions[4],6))  # get the (sub-)region for the reporting region
Assunit_index   <- c(rep("(sub-)Region",4),rep("Division",22)) # is reporting region a "(sub-)Region" or "Division"?
regions_with_impact <- c(1,2,5,6,7,8,10:15) # get all areas with longevity data

p <-1 
Assregion <- Assregion_index[p]
EcoReg    <- EcoReg_index[p]
Assunit <- Assunit_index[p]     

### load processed file, with longevity and state/impact 
load(paste(pathdir_nogit,paste(EcoReg,"Fisheries.RData",sep="_"),sep="/")) 
load(paste(pathdir_nogit,paste(EcoReg,"fisheries_per_metier_comb.RData",sep="_"),sep="/")) 
setwd(paste(pathdir,"1 - Input env",sep="/"))
load(paste(EcoReg,"region_grid_sensitivity.RData",sep="_")) 

## get SAR map average 2013-2018
SSAR_year <- paste("surface_sar",AssPeriod,sep="_")

idx <- which(names(Region@data)== "long")
colnames(Region@data)[idx]  <- "longitude"
idx <- which(names(Region@data)== "lat")
colnames(Region@data)[idx]  <- "latitude"

# show SAR
Trade_RE1 <- Region@data
nam <- c(SSAR_year)
Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
Trade_RE1$avgsar <- rowMeans(Trade_RE1[,SSAR_year])
cut_effort <- c(0,0.1,0.5,1,5,10,99)
Trade_RE1$cat<- as.factor(cut(Trade_RE1$avgsar,cut_effort,right=T))
Region <- cbind(Region, Trade_RE1[match(Region@data$csquares,Trade_RE1$csquares), c("cat")])
colnames(Region@data)[ncol(Region@data)] <- "cat"
Regionall <- Region[,c("cat")]

for (p in 2:4) {
  Assregion <- Assregion_index[p]
  EcoReg    <- EcoReg_index[p]
  Assunit <- Assunit_index[p]     
  
  ### load processed file, with longevity and state/impact 
  load(paste(pathdir_nogit,paste(EcoReg,"Fisheries.RData",sep="_"),sep="/")) 
  load(paste(pathdir_nogit,paste(EcoReg,"fisheries_per_metier_comb.RData",sep="_"),sep="/")) 
  setwd(paste(pathdir,"1 - Input env",sep="/"))
  load(paste(EcoReg,"region_grid_sensitivity.RData",sep="_")) 
  
  ## get SAR map average 2013-2018
  SSAR_year <- paste("surface_sar",AssPeriod,sep="_")
  
  idx <- which(names(Region@data)== "long")
  colnames(Region@data)[idx]  <- "longitude"
  idx <- which(names(Region@data)== "lat")
  colnames(Region@data)[idx]  <- "latitude"
  
  # show SAR
  Trade_RE1 <- Region@data
  nam <- c(SSAR_year)
  Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
  Trade_RE1$avgsar <- rowMeans(Trade_RE1[,SSAR_year])
  cut_effort <- c(0,0.1,0.5,1,5,10,99)
  Trade_RE1$cat<- as.factor(cut(Trade_RE1$avgsar,cut_effort,right=T))
  Region <- cbind(Region, Trade_RE1[match(Region@data$csquares,Trade_RE1$csquares), c("cat")])
  colnames(Region@data)[ncol(Region@data)] <- "cat"
  Region <- Region[,c("cat")]
  
  Regionall <- rbind(Regionall,Region)
}

  # get impact
  source(paste(pathdir,"5 - Output/Interactive maps/Pressure and impact/IM - pressure and impact source-a.R",sep="/"))

  # get shapefiles subregions, subdivisions    
  source(paste(pathdir,"5 - Output/Interactive maps/Shapefiles_subregions_subdivisions.R",sep="/"))

  regall <- raster::aggregate(Regionall)
  reg001 <- subset(Regionall,Regionall@data$cat == "(0,0.1]")
  reg001 <- raster::aggregate(reg001)
  reg0105 <- subset(Regionall,Regionall@data$cat == "(0.1,0.5]")
  reg0105 <- raster::aggregate(reg0105)
  reg051 <- subset(Regionall,Regionall@data$cat == "(0.5,1]")
  reg051 <- raster::aggregate(reg051)
  reg15 <- subset(Regionall,Regionall@data$cat == "(1,5]")
  reg15 <- raster::aggregate(reg15)
  reg510 <- subset(Regionall,Regionall@data$cat == "(5,10]")
  reg510 <- raster::aggregate(reg510)
  reg1099 <- subset(Regionall,Regionall@data$cat == "(10,99]")
  reg1099 <- raster::aggregate(reg1099)
  
  IL001 <- subset(Region_IL,Region_IL@data$cat == "(0,0.1]")
  IL001 <- raster::aggregate(IL001)
  IL0102 <- subset(Region_IL,Region_IL@data$cat == "(0.1,0.2]")
  IL0102 <- raster::aggregate(IL0102)
  IL0203 <- subset(Region_IL,Region_IL@data$cat == "(0.2,0.3]")
  IL0203 <- raster::aggregate(IL0203)
  IL0305 <- subset(Region_IL,Region_IL@data$cat == "(0.3,0.5]")
  IL0305 <- raster::aggregate(IL0305)
  IL0508 <- subset(Region_IL,Region_IL@data$cat == "(0.5,0.8]")
  IL0508 <- raster::aggregate(IL0508)
  IL081 <- subset(Region_IL,Region_IL@data$cat == "(0.8,1]")
  IL081 <- raster::aggregate(IL081)
  
  PD001 <- subset(Region_PD,Region_PD@data$cat == "(0,0.1]")
  PD001 <- raster::aggregate(PD001)
  PD0102 <- subset(Region_PD,Region_PD@data$cat == "(0.1,0.2]")
  PD0102 <- raster::aggregate(PD0102)
  PD0203 <- subset(Region_PD,Region_PD@data$cat == "(0.2,0.3]")
  PD0203 <- raster::aggregate(PD0203)
  PD0305 <- subset(Region_PD,Region_PD@data$cat == "(0.3,0.5]")
  PD0305 <- raster::aggregate(PD0305)
  PD0508 <- subset(Region_PD,Region_PD@data$cat == "(0.5,0.8]")
  PD0508 <- raster::aggregate(PD0508)
  PD081 <- subset(Region_PD,Region_PD@data$cat == "(0.8,1]")
  PD081 <- raster::aggregate(PD081)
  
  library(leaflet)
  library(htmlwidgets)
  
  mfs <- leaflet() %>%
    #addTiles() %>%  # Add default OpenStreetMap map tiles
    addProviderTiles(providers$Esri.WorldImagery) %>%
    
    # areal extent
    addPolygons(data = regall, group="Areal extent",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor = "grey") %>%
    
    # region boundaries
    addPolygons(data = EcReg, group="(Sub-)regions",
                stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
    # subdivision boundaries
    addPolygons(data = subdiv, group="Subdivisions",
                stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "yellow") %>%
    
   # pressure
    addPolygons(data = reg001, group="SAR >0 - 0.1",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#f2f0f7") %>%
    addPolygons(data = reg0105, group="SAR 0.1-0.5",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#dadaeb") %>%
    addPolygons(data = reg051, group="SAR 0.5-1",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#bcbddc") %>%
    addPolygons(data = reg15, group="SAR 1-5",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#9e9ac8") %>%
    addPolygons(data = reg510, group="SAR 5-10",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#756bb1") %>%
    addPolygons(data = reg1099, group="SAR >10",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#54278f") %>%
    
    # IL impact
    addPolygons(data = IL001, group="L1 impact >0 - 0.1",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#ffffcc") %>%
    addPolygons(data = IL0102, group="L1 impact 0.1-0.2",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#c7e9b4") %>%
    addPolygons(data = IL0203, group="L1 impact 0.2-0.3",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#7fcdbb") %>%
    addPolygons(data = IL0305, group="L1 impact 0.3-0.5",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#41b6c4") %>%
    addPolygons(data = IL0508, group="L1 impact 0.5-0.8",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#2c7fb8") %>%
    addPolygons(data = IL081, group="L1 impact 0.8-1",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#253494") %>%
    
    # PD impact
    addPolygons(data = PD001, group="PD impact >0 - 0.1",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#ffffcc") %>%
    addPolygons(data = PD0102, group="PD impact 0.1-0.2",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#c7e9b4") %>%
    addPolygons(data = PD0203, group="PD impact 0.2-0.3",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#7fcdbb") %>%
    addPolygons(data = PD0305, group="PD impact 0.3-0.5",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#41b6c4") %>%
    addPolygons(data = PD0508, group="PD impact 0.5-0.8",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#2c7fb8") %>%
    addPolygons(data = PD081, group="PD impact 0.8-1",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#253494") %>%
    
    
    # Layers control
    addLayersControl(
      overlayGroups = c("Areal extent","(Sub-)regions","Subdivisions","SAR >0 - 0.1","SAR 0.1-0.5","SAR 0.5-1",
                        "SAR 1-5","SAR 5-10","SAR >10",
                        "L1 impact >0 - 0.1","L1 impact 0.1-0.2","L1 impact 0.2-0.3",
                        "L1 impact 0.3-0.5","L1 impact 0.5-0.8","L1 impact 0.8-1",
                        "PD impact >0 - 0.1","PD impact 0.1-0.2","PD impact 0.2-0.3",
                        "PD impact 0.3-0.5","PD impact 0.5-0.8","PD impact 0.8-1"),
      options = layersControlOptions(collapsed = FALSE)
    )%>%
    
    addLegend(title = "" ,colors = c(NA,NA,NA) ,position = "bottomright",
              labels= c("The map shows  fishing pressure SAR (swept-area-ratio)", 
                        "and, where available, the PD and L1 benthic impact indicators",
                        "(all averaged for the period 2013-2018)")) %>%
    
    htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\"> <b>Pressure and impact</label>');
        }
    ")
  
  setwd(paste(pathdir,"5 - Output/Interactive maps/Pressure and impact",sep="/"))
  
  # save output
  saveWidget(mfs, file="Pressure and impact.html")  
  