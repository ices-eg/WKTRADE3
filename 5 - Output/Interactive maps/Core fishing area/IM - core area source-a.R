

p <-5
Assregion <- Assregion_index[p]
EcoReg    <- EcoReg_index[p]
Assunit <- Assunit_index[p]     

### load processed file, with longevity and state/impact 
load(paste(pathdir_nogit,paste(EcoReg,"Fisheries.RData",sep="_"),sep="/")) 
load(paste(pathdir_nogit,paste(EcoReg,"fisheries_per_metier_comb.RData",sep="_"),sep="/")) 
setwd(paste(pathdir,"1 - Input env",sep="/"))
load(paste(EcoReg,"region_grid_sensitivity.RData",sep="_")) 

# select division from the region
if (Assregion != EcoReg){
  Region <-  subset(Region,Region@data$division == Assregion)
}

## get SAR map average per metier
gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
csq <- c()

for (pp in 1:length(gears)){
  datgear <- Region@data
  nam <- paste(rep(paste(gears[pp],"surface_sar",sep="_"),length(AssPeriod)),AssPeriod,sep="_")
  datgear <- cbind(datgear, FisheriesMet[match(datgear$csquares,FisheriesMet$csquares), c(nam)])
  datgear[,c(nam)][is.na(datgear[,c(nam)])] <- 0
  datgear$avgsar <- rowMeans(datgear[,nam])
  if(sum(datgear[,"avgsar"]) > 0){
    datgear <- datgear[order(datgear[,"avgsar"],decreasing = T),]
    datgear$cumSSAR <- cumsum(datgear[,"avgsar"])
    datgear$cumSSAR <- datgear$cumSSAR / sum(datgear[,"avgsar"] )
    idx <- min(which(datgear$cumSSAR > .9))
    datgear <- datgear[1:idx,]
    csq <- c(csq,datgear$csquares)
  }}
csq <- unique(csq)

Regionnew <- subset(Region, Region@data$csquares %in% csq)    
Regionnew$incl <- 1
Regionnew <- Regionnew[,"incl"]

for (p in 6:26) {
  Assregion <- Assregion_index[p]
  EcoReg    <- EcoReg_index[p]
  Assunit <- Assunit_index[p]     
  
  ### load processed file, with longevity and state/impact 
  load(paste(pathdir_nogit,paste(EcoReg,"Fisheries.RData",sep="_"),sep="/")) 
  load(paste(pathdir_nogit,paste(EcoReg,"fisheries_per_metier_comb.RData",sep="_"),sep="/")) 
  setwd(paste(pathdir,"1 - Input env",sep="/"))
  load(paste(EcoReg,"region_grid_sensitivity.RData",sep="_")) 
  
  # select division from the region
  if (Assregion != EcoReg){
    Region <-  subset(Region,Region@data$division == Assregion)
  }
  
  ## get SAR map average 2013-2018 per metier
  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  csq <- c()
  
  for (pp in 1:length(gears)){
    datgear <- Region@data
    nam <- paste(rep(paste(gears[pp],"surface_sar",sep="_"),length(AssPeriod)),AssPeriod,sep="_")
    datgear <- cbind(datgear, FisheriesMet[match(datgear$csquares,FisheriesMet$csquares), c(nam)])
    datgear[,c(nam)][is.na(datgear[,c(nam)])] <- 0
    datgear$avgsar <- rowMeans(datgear[,nam])
    if(sum(datgear[,"avgsar"]) > 0){
      datgear <- datgear[order(datgear[,"avgsar"],decreasing = T),]
      datgear$cumSSAR <- cumsum(datgear[,"avgsar"])
      datgear$cumSSAR <- datgear$cumSSAR / sum(datgear[,"avgsar"] )
      idx <- min(which(datgear$cumSSAR > .9))
      datgear <- datgear[1:idx,]
      csq <- c(csq,datgear$csquares)
    }}
  csq <- unique(csq)
  
  if (length(csq >0)){
  Region <- subset(Region, Region@data$csquares %in% csq)    
  Region$incl <- 1
  Region <- Region[,"incl"]
  Regionnew <- rbind(Regionnew,Region)
}}
