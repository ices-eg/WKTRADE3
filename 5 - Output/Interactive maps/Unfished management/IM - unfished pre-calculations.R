
rm(list = ls())

### github folder
pathdir <- "C:/Users/danie/Documents/Online for git/WKTRADE3"
pathdir_nogit <- "C:/Users/danie/Documents/Online for git/WKTRADE4 - Fisheries restricted"

### select time period
Period    <- 2009:2021    # period with fishing data to calculate impact
AssPeriod <- 2016:2021    # assessment period

### get all libraries
source(paste(pathdir,"Utilities/Libraries_WKTRADE3.R",sep="/")) 

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
load(paste(pathdir_nogit,paste(EcoReg,"Fisheries.RData",sep="_"),sep="/")) 
setwd(paste(pathdir,"1 - Input env",sep="/"))
load(paste(EcoReg,"region_grid_sensitivity.RData",sep="_")) 
load(paste(EcoReg,"MSFD_per_csquare.RData",sep="_")) 

# select division from the region
if (Assregion != EcoReg){
  Region <-  subset(Region,Region@data$division == Assregion)
}

## get SAR map average 2013-2018
SSAR_year <- paste("surface_sar",AssPeriod,sep="_")

# get SAR
Trade_RE1 <- Region@data
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
swept005 <- c();swept010 <- c();swept020 <- c();swept030 <- c();swept040 <- c()
swept050 <- c();swept060 <- c();swept070 <- c();swept080 <- c();swept090 <- c();swept095 <- c()

for (imsfd in 1:length(uniMSFD)){
  hab <- subset(Trade_RE1,Trade_RE1$MSFD == uniMSFD[imsfd])
  hab$swept   <- hab$avgsar * hab$area_sqkm 
  hab         <- hab[order(hab[,"swept"],decreasing = F),]
  hab$cumarea <- cumsum(hab[,"area_sqkm"])
  hab$cumarea <- hab$cumarea / sum(hab[,"area_sqkm"])

  idx_unf   <- ifelse(sum(hab$avgsar)>0, min(which(hab$avgsar > 0 )), 1)
  idx       <- min(which(hab$cumarea > 0.05))
  csq005    <- c(csq005,if (idx > idx_unf){hab$csquares[idx_unf:idx]})
  swept005  <- c(swept005,sum(hab$swept[1:idx]))
  idx <- min(which(hab$cumarea > 0.1))
  csq010 <-  c(csq010,if (idx > idx_unf){hab$csquares[idx_unf:idx]})
  swept010  <- c(swept010,sum(hab$swept[1:idx]))
  idx <- min(which(hab$cumarea > 0.2))
  csq020 <-  c(csq020,if (idx > idx_unf){hab$csquares[idx_unf:idx]})
  swept020  <- c(swept020,sum(hab$swept[1:idx]))
  idx <- min(which(hab$cumarea > 0.3))
  csq030 <-  c(csq030,if (idx > idx_unf){hab$csquares[idx_unf:idx]})
  swept030  <- c(swept030,sum(hab$swept[1:idx]))
  idx <- min(which(hab$cumarea > 0.4))
  csq040 <-  c(csq040,if (idx > idx_unf){hab$csquares[idx_unf:idx]})
  swept040  <- c(swept040,sum(hab$swept[1:idx]))
  idx <- min(which(hab$cumarea > 0.5))
  csq050 <-  c(csq050,if (idx > idx_unf){hab$csquares[idx_unf:idx]})
  swept050  <- c(swept050,sum(hab$swept[1:idx]))
  idx <- min(which(hab$cumarea > 0.6))
  csq060 <-  c(csq060,if (idx > idx_unf){hab$csquares[idx_unf:idx]})
  swept060  <- c(swept060,sum(hab$swept[1:idx]))
  idx <- min(which(hab$cumarea > 0.7))
  csq070 <-  c(csq070,if (idx > idx_unf){hab$csquares[idx_unf:idx]})
  swept070  <- c(swept070,sum(hab$swept[1:idx]))
  idx <- min(which(hab$cumarea > .80))
  csq080 <-  c(csq080,if (idx > idx_unf){hab$csquares[idx_unf:idx]})
  swept080  <- c(swept080,sum(hab$swept[1:idx]))
  idx <- min(which(hab$cumarea > .90))
  csq090 <-  c(csq090,if (idx > idx_unf){hab$csquares[idx_unf:idx]})
  swept090  <- c(swept090,sum(hab$swept[1:idx]))
  idx <- min(which(hab$cumarea > .95))
  csq095 <-  c(csq095,if (idx > idx_unf){hab$csquares[idx_unf:idx]})
  swept095  <- c(swept095,sum(hab$swept[1:idx]))
}
sweptall  <- sum(Trade_RE1$area_sqkm*Trade_RE1$avgsar)

# now select all c-squares
Region@data$incl <- 1

Region005 <- subset(Region, Region@data$csquares %in% csq005); Region005 <- Region005[,"incl"]
Region010 <- subset(Region, Region@data$csquares %in% csq010); Region010 <- Region010[,"incl"]
Region020 <- subset(Region, Region@data$csquares %in% csq020); Region020 <- Region020[,"incl"]
Region030 <- subset(Region, Region@data$csquares %in% csq030); Region030 <- Region030[,"incl"]
Region040 <- subset(Region, Region@data$csquares %in% csq040); Region040 <- Region040[,"incl"]
Region050 <- subset(Region, Region@data$csquares %in% csq050); Region050 <- Region050[,"incl"]
Region060 <- subset(Region, Region@data$csquares %in% csq060); Region060 <- Region060[,"incl"]
Region070 <- subset(Region, Region@data$csquares %in% csq070); Region070 <- Region070[,"incl"]
Region080 <- subset(Region, Region@data$csquares %in% csq080); Region080 <- Region080[,"incl"]
Region090 <- subset(Region, Region@data$csquares %in% csq090); Region090 <- Region090[,"incl"]
Region095 <- subset(Region, Region@data$csquares %in% csq095); Region095 <- Region095[,"incl"]

#### now for all other areas
for(p in 6:26){ 
  Assregion <- Assregion_index[p]
  EcoReg    <- EcoReg_index[p]
  Assunit <- Assunit_index[p]     
  
  ### load processed file, with longevity and state/impact 
  load(paste(pathdir_nogit,paste(EcoReg,"Fisheries.RData",sep="_"),sep="/")) 
  setwd(paste(pathdir,"1 - Input env",sep="/"))
  load(paste(EcoReg,"region_grid_sensitivity.RData",sep="_")) 
  load(paste(EcoReg,"MSFD_per_csquare.RData",sep="_")) 
  
  # select division from the region
  if (Assregion != EcoReg){
    Region <-  subset(Region,Region@data$division == Assregion)
  }
  
  ## get SAR map average 2013-2018
  SSAR_year <- paste("surface_sar",AssPeriod,sep="_")

  # get SAR
  Trade_RE1 <- Region@data
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

  for (imsfd in 1:length(uniMSFD)){
    hab <- subset(Trade_RE1,Trade_RE1$MSFD == uniMSFD[imsfd])
    hab$swept   <- hab$avgsar * hab$area_sqkm 
    hab         <- hab[order(hab[,"swept"],decreasing = F),]
    hab$cumarea <- cumsum(hab[,"area_sqkm"])
    hab$cumarea <- hab$cumarea / sum(hab[,"area_sqkm"])
    
    if(sum(hab$avgsar)>0){
      idx_unf   <- ifelse(sum(hab$avgsar)>0, min(which(hab$avgsar > 0 )), 1)
      idx       <- min(which(hab$cumarea > 0.05))
      csq005    <- c(csq005,if (idx > idx_unf){hab$csquares[idx_unf:idx]})
      swept005  <- c(swept005,sum(hab$swept[1:idx]))
      idx <- min(which(hab$cumarea > 0.1))
      csq010 <-  c(csq010,if (idx > idx_unf){hab$csquares[idx_unf:idx]})
      swept010  <- c(swept010,sum(hab$swept[1:idx]))
      idx <- min(which(hab$cumarea > 0.2))
      csq020 <-  c(csq020,if (idx > idx_unf){hab$csquares[idx_unf:idx]})
      swept020  <- c(swept020,sum(hab$swept[1:idx]))
      idx <- min(which(hab$cumarea > 0.3))
      csq030 <-  c(csq030,if (idx > idx_unf){hab$csquares[idx_unf:idx]})
      swept030  <- c(swept030,sum(hab$swept[1:idx]))
      idx <- min(which(hab$cumarea > 0.4))
      csq040 <-  c(csq040,if (idx > idx_unf){hab$csquares[idx_unf:idx]})
      swept040  <- c(swept040,sum(hab$swept[1:idx]))
      idx <- min(which(hab$cumarea > 0.5))
      csq050 <-  c(csq050,if (idx > idx_unf){hab$csquares[idx_unf:idx]})
      swept050  <- c(swept050,sum(hab$swept[1:idx]))
      idx <- min(which(hab$cumarea > 0.6))
      csq060 <-  c(csq060,if (idx > idx_unf){hab$csquares[idx_unf:idx]})
      swept060  <- c(swept060,sum(hab$swept[1:idx]))
      idx <- min(which(hab$cumarea > 0.7))
      csq070 <-  c(csq070,if (idx > idx_unf){hab$csquares[idx_unf:idx]})
      swept070  <- c(swept070,sum(hab$swept[1:idx]))
      idx <- min(which(hab$cumarea > .80))
      csq080 <-  c(csq080,if (idx > idx_unf){hab$csquares[idx_unf:idx]})
      swept080  <- c(swept080,sum(hab$swept[1:idx]))
      idx <- min(which(hab$cumarea > .90))
      csq090 <-  c(csq090,if (idx > idx_unf){hab$csquares[idx_unf:idx]})
      swept090  <- c(swept090,sum(hab$swept[1:idx]))
      idx <- min(which(hab$cumarea > .95))
      csq095 <-  c(csq095,if (idx > idx_unf){hab$csquares[idx_unf:idx]})
      swept095  <- c(swept095,sum(hab$swept[1:idx]))
  }}
  sweptall  <- c(sweptall, sum(Trade_RE1$area_sqkm*Trade_RE1$avgsar))
  
  # now select all c-squares
  Region@data$incl <- 1
  
  if(length(csq005)>0){
    int_005 <- subset(Region, Region@data$csquares %in% csq005); int_005 <- int_005[,"incl"]
    Region005 <- rbind(Region005,int_005)}
  
  if(length(csq010)>0){
    int_010 <- subset(Region, Region@data$csquares %in% csq010); int_010 <- int_010[,"incl"]
    Region010 <- rbind(Region010,int_010)}
    
  if(length(csq020)>0){
    int_020 <- subset(Region, Region@data$csquares %in% csq020); int_020 <- int_020[,"incl"]
    Region020 <- rbind(Region020,int_020)}
  
  if(length(csq030)>0){
    int_030 <- subset(Region, Region@data$csquares %in% csq030); int_030 <- int_030[,"incl"]
    Region030 <- rbind(Region030,int_030)}
  
  if(length(csq040)>0){
    int_040 <- subset(Region, Region@data$csquares %in% csq040); int_040 <- int_040[,"incl"]
    Region040 <- rbind(Region040,int_040)}
  
  if(length(csq050)>0){
    int_050 <- subset(Region, Region@data$csquares %in% csq050); int_050 <- int_050[,"incl"]
    Region050 <- rbind(Region050,int_050)}
  
  if(length(csq060)>0){
    int_060 <- subset(Region, Region@data$csquares %in% csq060); int_060 <- int_060[,"incl"]
    Region060 <- rbind(Region060,int_060)}
  
  if(length(csq070)>0){
    int_070 <- subset(Region, Region@data$csquares %in% csq070); int_070 <- int_070[,"incl"]
    Region070 <- rbind(Region070,int_070)}
  
  if(length(csq080)>0){
    int_080 <- subset(Region, Region@data$csquares %in% csq080); int_080 <- int_080[,"incl"]
    Region080 <- rbind(Region080,int_080)}
  
  if(length(csq090)>0){
    int_090 <- subset(Region, Region@data$csquares %in% csq090); int_090 <- int_090[,"incl"]
    Region090 <- rbind(Region090,int_090)}
  
  if(length(csq095)>0){
    int_095 <- subset(Region, Region@data$csquares %in% csq095); int_095 <- int_095[,"incl"]
    Region095 <- rbind(Region095,int_095)}

  print(p)
  
  }

setwd(paste(pathdir,"5 - Output/Interactive maps/Unfished management",sep="/"))
Region005 <- st_union(st_make_valid(st_as_sf(Region005))); save(Region005,file=("Region005.RData"))
Region010 <- st_union(st_make_valid(st_as_sf(Region010))); save(Region010,file=("Region010.RData"))
Region020 <- st_union(st_make_valid(st_as_sf(Region020))); save(Region020,file=("Region020.RData"))
Region030 <- st_union(st_make_valid(st_as_sf(Region030))); save(Region030,file=("Region030.RData"))
Region040 <- st_union(st_make_valid(st_as_sf(Region040))); save(Region040,file=("Region040.RData"))
Region050 <- st_union(st_make_valid(st_as_sf(Region050))); save(Region050,file=("Region050.RData"))
Region060 <- st_union(st_make_valid(st_as_sf(Region060))); save(Region060,file=("Region060.RData"))
Region070 <- st_union(st_make_valid(st_as_sf(Region070))); save(Region070,file=("Region070.RData"))
Region080 <- st_union(st_make_valid(st_as_sf(Region080))); save(Region080,file=("Region080.RData"))
Region090 <- st_union(st_make_valid(st_as_sf(Region090))); save(Region090,file=("Region090.RData"))
Region095 <- st_union(st_make_valid(st_as_sf(Region095))); save(Region095,file=("Region095.RData"))

sweptloss <- c(sum(swept005),sum(swept010),sum(swept020),sum(swept030),sum(swept040),
  sum(swept050),sum(swept060),sum(swept070),sum(swept080),sum(swept090),sum(swept095))
sweptloss <- sweptloss/sum(sweptall)

protect <- data.frame(fraction = c(0.05,seq(0.1,.9,0.1),0.95),sarloss = sweptloss)
save(protect,file=("protect.RData"))

