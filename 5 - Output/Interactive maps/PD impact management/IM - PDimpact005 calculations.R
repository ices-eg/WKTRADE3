
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

p <-5
Assregion <- Assregion_index[p]
EcoReg    <- EcoReg_index[p]
Assunit <- Assunit_index[p]     

### load processed file, with longevity and state/impact 
load(paste(pathdir_nogit,paste(EcoReg,"state.RData",sep="_"),sep="/")) 
load(paste(pathdir_nogit,paste(EcoReg,"Fisheries.RData",sep="_"),sep="/")) 
setwd(paste(pathdir,"1 - Input env",sep="/"))
load(paste(EcoReg,"region_grid_sensitivity.RData",sep="_")) 
load(paste(EcoReg,"MSFD_per_csquare.RData",sep="_")) 

# select division from the region
if (Assregion != EcoReg){
  Region <-  subset(Region,Region@data$division == Assregion)
}

# remove areas deeper than 200 meter for overview of Greater North Sea
if (EcoReg == "Greater North Sea"){
  Region <-  subset(Region,Region$Depth >= -200)
}

## get state map average 2013-2018
state_year <- paste("state",AssPeriod,sep="_")

# get state
Trade_RE1 <- Region@data
nam <- c(state_year)
Trade_RE1 <- cbind(Trade_RE1, State_reg[match(Trade_RE1$csquares,State_reg$Fisheries.csquares), c(nam)])
Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 1
Trade_RE1$avgstate <- rowMeans(Trade_RE1[,state_year])

## get SAR map average 2013-2018
SSAR_year <- paste("surface_sar",AssPeriod,sep="_")

# get SAR
nam <- c(SSAR_year)
Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
Trade_RE1$avgsar <- rowMeans(Trade_RE1[,SSAR_year])

# rename habitat midpoint
colnames(Trade_RE1)[which(colnames(Trade_RE1)=="MSFD")] <- "MSFD_midpoint" 

# get dominant MSFD habitat type
tnew <- aggregate(msfd_csq$area_km2, by=list(msfd_csq$csquares),FUN = max)
colnames(tnew) <- c("csquares","areanew")
tnew <- merge(tnew, msfd_csq, by.x=c("csquares", "areanew"), by.y=c("csquares", "area_km2"), all.x=TRUE, all.y=FALSE)

Trade_RE1 <- cbind(Trade_RE1, tnew[match(Trade_RE1$csquares,tnew$csquares), c("MSFD")])
colnames(Trade_RE1)[ncol(Trade_RE1)] <- "MSFD"
Trade_RE1 <- subset(Trade_RE1,!(is.na(Trade_RE1$MSFD)))
Trade_RE1$MSFD <- as.character(Trade_RE1$MSFD)
Trade_RE1$MSFD[Trade_RE1$MSFD=="Na"]= "Unknown"

uniMSFD <-unique(Trade_RE1$MSFD)
csq005 <- c();csq010 <- c();csq020 <- c();csq030 <- c();csq040 <- c();csq050 <- c()
csq060 <- c();csq070 <- c();csq080 <- c();csq090 <- c();csq095 <- c()
#swept005 <- c();swept010 <- c();swept020 <- c();swept030 <- c();swept040 <- c()
#swept050 <- c();swept060 <- c();swept070 <- c();swept080 <- c();swept090 <- c();swept095 <- c()

for (imsfd in 1:length(uniMSFD)){
  hab <- subset(Trade_RE1,Trade_RE1$MSFD == uniMSFD[imsfd])
  hab         <- hab[order(hab[,"avgstate"],decreasing = T),]
  hab$cumarea <- cumsum(hab[,"area_sqkm"])
  hab$cumarea <- hab$cumarea / sum(hab[,"area_sqkm"])

  idx_stateTresh   <- min(which(hab$avgstate < 0.95 ))
  idx    <- min(which(hab$cumarea > 0.05))
  csq005 <- c(csq005,if (idx > idx_stateTresh){hab$csquares[idx_stateTresh:idx]})
  idx    <- min(which(hab$cumarea > 0.1))
  csq010 <-  c(csq010,if (idx > idx_stateTresh){hab$csquares[idx_stateTresh:idx]})
  idx    <- min(which(hab$cumarea > 0.2))
  csq020 <-  c(csq020,if (idx > idx_stateTresh){hab$csquares[idx_stateTresh:idx]})
  idx    <- min(which(hab$cumarea > 0.3))
  csq030 <-  c(csq030,if (idx > idx_stateTresh){hab$csquares[idx_stateTresh:idx]})
  idx    <- min(which(hab$cumarea > 0.4))
  csq040 <-  c(csq040,if (idx > idx_stateTresh){hab$csquares[idx_stateTresh:idx]})
  idx    <- min(which(hab$cumarea > 0.5))
  csq050 <-  c(csq050,if (idx > idx_stateTresh){hab$csquares[idx_stateTresh:idx]})
  idx    <- min(which(hab$cumarea > 0.6))
  csq060 <-  c(csq060,if (idx > idx_stateTresh){hab$csquares[idx_stateTresh:idx]})
  idx <- min(which(hab$cumarea > 0.7))
  csq070 <-  c(csq070,if (idx > idx_stateTresh){hab$csquares[idx_stateTresh:idx]})
  idx <- min(which(hab$cumarea > .80))
  csq080 <-  c(csq080,if (idx > idx_stateTresh){hab$csquares[idx_stateTresh:idx]})
  idx <- min(which(hab$cumarea > .90))
  csq090 <-  c(csq090,if (idx > idx_stateTresh){hab$csquares[idx_stateTresh:idx]})
  idx <- min(which(hab$cumarea > .95))
  csq095 <-  c(csq095,if (idx > idx_stateTresh){hab$csquares[idx_stateTresh:idx]})
}

areaunfished <- subset(Trade_RE1,Trade_RE1$avgsar == 0)
unfishedcsq  <- areaunfished$csquares 

areabelow <- subset(Trade_RE1,Trade_RE1$avgstate >0.95 & Trade_RE1$avgsar >0)
belowcsq  <- areabelow$csquares 

areaabove <- subset(Trade_RE1,Trade_RE1$avgstate <0.95 & Trade_RE1$avgsar >0)
abovecsq  <- areaabove$csquares 

#### now for all other areas
for(p in 6:15){ 
  Assregion <- Assregion_index[p]
  EcoReg    <- EcoReg_index[p]
  Assunit <- Assunit_index[p]     
  ### load processed file, with longevity and state/impact 
  load(paste(pathdir_nogit,paste(EcoReg,"state.RData",sep="_"),sep="/")) 
  load(paste(pathdir_nogit,paste(EcoReg,"Fisheries.RData",sep="_"),sep="/")) 
  setwd(paste(pathdir,"1 - Input env",sep="/"))
  load(paste(EcoReg,"region_grid_sensitivity.RData",sep="_")) 
  load(paste(EcoReg,"MSFD_per_csquare.RData",sep="_")) 
  
  # select division from the region
  if (Assregion != EcoReg){
    Region <-  subset(Region,Region@data$division == Assregion)
  }
  
  
  # remove areas deeper than 200 meter for overview of Greater North Sea
  if (EcoReg == "Greater North Sea"){
    Region <-  subset(Region,Region$Depth >= -200)
  }
  
  ## get state map average 2013-2018
  state_year <- paste("state",AssPeriod,sep="_")
  
  # get state
  Trade_RE1 <- Region@data
  nam <- c(state_year)
  Trade_RE1 <- cbind(Trade_RE1, State_reg[match(Trade_RE1$csquares,State_reg$Fisheries.csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 1
  Trade_RE1$avgstate <- rowMeans(Trade_RE1[,state_year])
  
  ## get SAR map average 2013-2018
  SSAR_year <- paste("surface_sar",AssPeriod,sep="_")
  
  # get SAR
  nam <- c(SSAR_year)
  Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
  Trade_RE1$avgsar <- rowMeans(Trade_RE1[,SSAR_year])
  
  # rename habitat midpoint
  colnames(Trade_RE1)[which(colnames(Trade_RE1)=="MSFD")] <- "MSFD_midpoint" 
  
  # get dominant MSFD habitat type
  tnew <- aggregate(msfd_csq$area_km2, by=list(msfd_csq$csquares),FUN = max)
  colnames(tnew) <- c("csquares","areanew")
  tnew <- merge(tnew, msfd_csq, by.x=c("csquares", "areanew"), by.y=c("csquares", "area_km2"), all.x=TRUE, all.y=FALSE)
  
  Trade_RE1 <- cbind(Trade_RE1, tnew[match(Trade_RE1$csquares,tnew$csquares), c("MSFD")])
  colnames(Trade_RE1)[ncol(Trade_RE1)] <- "MSFD"
  Trade_RE1 <- subset(Trade_RE1,!(is.na(Trade_RE1$MSFD)))
  Trade_RE1$MSFD <- as.character(Trade_RE1$MSFD)
  Trade_RE1$MSFD[Trade_RE1$MSFD=="Na"]= "Unknown"
  
  uniMSFD <-unique(Trade_RE1$MSFD)
  for (imsfd in 1:length(uniMSFD)){
    hab <- subset(Trade_RE1,Trade_RE1$MSFD == uniMSFD[imsfd])
    hab         <- hab[order(hab[,"avgstate"],decreasing = T),]
    hab$cumarea <- cumsum(hab[,"area_sqkm"])
    hab$cumarea <- hab$cumarea / sum(hab[,"area_sqkm"])
    
    idx_stateTresh   <- min(which(hab$avgstate < 0.95 ))
    idx    <- min(which(hab$cumarea > 0.05))
    csq005 <- c(csq005,if (idx > idx_stateTresh){hab$csquares[idx_stateTresh:idx]})
    idx    <- min(which(hab$cumarea > 0.1))
    csq010 <-  c(csq010,if (idx > idx_stateTresh){hab$csquares[idx_stateTresh:idx]})
    idx    <- min(which(hab$cumarea > 0.2))
    csq020 <-  c(csq020,if (idx > idx_stateTresh){hab$csquares[idx_stateTresh:idx]})
    idx    <- min(which(hab$cumarea > 0.3))
    csq030 <-  c(csq030,if (idx > idx_stateTresh){hab$csquares[idx_stateTresh:idx]})
    idx    <- min(which(hab$cumarea > 0.4))
    csq040 <-  c(csq040,if (idx > idx_stateTresh){hab$csquares[idx_stateTresh:idx]})
    idx    <- min(which(hab$cumarea > 0.5))
    csq050 <-  c(csq050,if (idx > idx_stateTresh){hab$csquares[idx_stateTresh:idx]})
    idx    <- min(which(hab$cumarea > 0.6))
    csq060 <-  c(csq060,if (idx > idx_stateTresh){hab$csquares[idx_stateTresh:idx]})
    idx <- min(which(hab$cumarea > 0.7))
    csq070 <-  c(csq070,if (idx > idx_stateTresh){hab$csquares[idx_stateTresh:idx]})
    idx <- min(which(hab$cumarea > .80))
    csq080 <-  c(csq080,if (idx > idx_stateTresh){hab$csquares[idx_stateTresh:idx]})
    idx <- min(which(hab$cumarea > .90))
    csq090 <-  c(csq090,if (idx > idx_stateTresh){hab$csquares[idx_stateTresh:idx]})
    idx <- min(which(hab$cumarea > .95))
    csq095 <-  c(csq095,if (idx > idx_stateTresh){hab$csquares[idx_stateTresh:idx]})
  }
  areaunfished <- subset(Trade_RE1,Trade_RE1$avgsar == 0)
  unfishedcsq  <- c(unfishedcsq, areaunfished$csquares) 
  
  areabelow <- subset(Trade_RE1,Trade_RE1$avgstate >0.95 & Trade_RE1$avgsar >0)
  belowcsq  <- c(belowcsq,areabelow$csquares) 
  
  areaabove <- subset(Trade_RE1,Trade_RE1$avgstate <0.95 & Trade_RE1$avgsar >0)
  abovecsq  <- c(abovecsq,areaabove$csquares) 
 }

load((paste("Baltic Sea","region_grid_sensitivity.RData",sep="_")) )
BS <- Region
load((paste("Greater North Sea","region_grid_sensitivity.RData",sep="_")) )
NS <- Region
all <- rbind(BS[,1],NS[,1])

Region005 <- subset(all,all$csquares %in% csq005)
Region010 <- subset(all,all$csquares %in% csq010)
Region020 <- subset(all,all$csquares %in% csq020)
Region030 <- subset(all,all$csquares %in% csq030)
Region040 <- subset(all,all$csquares %in% csq040)
Region050 <- subset(all,all$csquares %in% csq050)
Region060 <- subset(all,all$csquares %in% csq060)
Region070 <- subset(all,all$csquares %in% csq070)
Region080 <- subset(all,all$csquares %in% csq080)
Region090 <- subset(all,all$csquares %in% csq090)
Region095 <- subset(all,all$csquares %in% csq095)
Region_below <- subset(all,all$csquares %in% belowcsq)
Region_above <- subset(all,all$csquares %in% abovecsq)
Region_unfished <- subset(all,all$csquares %in% unfishedcsq)
RegionNA <- subset(NS, NS$Depth < -200)

Region010_T1 <- st_union(st_make_valid(st_as_sf(Region010)))
Region030_T1 <- st_union(st_make_valid(st_as_sf(Region030)))
Region060_T1 <- st_union(st_make_valid(st_as_sf(Region060)))
Region090_T1 <- st_union(st_make_valid(st_as_sf(Region090)))
Region_unfished <- st_union(st_make_valid(st_as_sf(Region_unfished)))
Region_below_T1 <- st_union(st_make_valid(st_as_sf(Region_below)))
Region_above_T1 <- st_union(st_make_valid(st_as_sf(Region_above)))
RegionNA <- st_union(st_make_valid(st_as_sf(RegionNA,crs = "EPSG:4326")))

#Region005_T1 <- raster::aggregate(Region005) #; save(Region005,file=("Region005.RData"))
#Region020_T1 <- raster::aggregate(Region020) #; save(Region020,file=("Region020.RData"))
#Region040_T1 <- raster::aggregate(Region040) #; save(Region040,file=("Region040.RData"))
#Region050_T1 <- raster::aggregate(Region050) #; save(Region050,file=("Region050.RData"))
#Region070_T1 <- raster::aggregate(Region070) #; save(Region070,file=("Region070.RData"))
#Region080_T1 <- raster::aggregate(Region080) #; save(Region080,file=("Region080.RData"))
#Region095_T1 <- raster::aggregate(Region095) #; save(Region095,file=("Region095.RData"))


