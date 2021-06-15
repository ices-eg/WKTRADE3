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
 
  # load different footprint analysis
  source(paste(pathdir,"5 - Output/Interactive maps/Core fishing area/IM - core area source-a.R",sep="/"))
  source(paste(pathdir,"5 - Output/Interactive maps/Core fishing area/IM - core area source-b.R",sep="/"))
  
  ## run IM - core area fragmentation (takes some time)
  load(paste(pathdir,"5 - Output/Interactive maps/Core fishing area/solution90pc.RData",sep="/"))
  
  source(paste(pathdir,"5 - Output/Interactive maps/Core fishing area/IM - core area source-c.R",sep="/"))
     
  # get shapefiles subregions, subdivisions    
  source(paste(pathdir,"5 - Output/Interactive maps/Shapefiles_subregions_subdivisions.R",sep="/"))
  
  regfished <- subset(Regionall, !(Regionall@data$cat == "<NA>"))
  regfished <- raster::aggregate(regfished)
  regall <- raster::aggregate(Regionall)
  Regionnew <- raster::aggregate(Regionnew)
  Regionnew2 <- raster::aggregate(Regionnew2)
  sf2test <- raster::aggregate(newdat) 
  RegionJcore <- raster::aggregate(RegionJcore) 
  
  library(leaflet)
  library(htmlwidgets)
   
  mfs <- leaflet() %>%
    #addTiles() %>%  # Add default OpenStreetMap map tiles
    addProviderTiles(providers$Esri.WorldImagery) %>%
    
    # areal extent
    addPolygons(data = regall, group="Areal extent",
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor = "grey") %>%
    
    # region boundaries
    addPolygons(data = EcReg, group="(sub-)regions",
                stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
    # subdivision boundaries
    addPolygons(data = subdiv, group="subdivisions",
                stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "yellow") %>%
    

    # core scenarios
    addPolygons(data = regfished, group="Fished c-squares",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#ffffcc") %>%
    addPolygons(data = Regionnew2, group="Highest 90% of SAR (2013-2018) per subdiv.",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "red") %>%
    addPolygons(data = Regionnew, group="Highest 90% of SAR (2013-2018) per metier and subdiv.",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "black") %>%
    addPolygons(data = sf2test, group="90% of SAR (2013-2018) per metier and subdiv. with fragmentation penalty",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "orange") %>%
    addPolygons(data = RegionJcore, group="Highest 90% of SAR per metier and subdiv. in multiple years during the period 2013-2018",
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "blue") %>%

    # Layers control
    addLayersControl(
      overlayGroups = c("Areal extent","(sub-)regions","subdivisions","Fished c-squares","Highest 90% of SAR (2013-2018) per subdiv.",
                        "Highest 90% of SAR (2013-2018) per metier and subdiv.",
                        "90% of SAR (2013-2018) per metier and subdiv. with fragmentation penalty",
                        "Highest 90% of SAR per metier and subdiv. in multiple years during the period 2013-2018"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  setwd(paste(pathdir,"5 - Output/Interactive maps/Core fishing area",sep="/"))
  
  # save output
  saveWidget(mfs, file="Illustration of core fishing grounds.html")  
  
