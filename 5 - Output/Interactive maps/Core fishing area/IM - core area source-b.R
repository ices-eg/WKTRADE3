

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

## get SAR map average 2013-2018 per metier
gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
csq <- c()

Trade_RE1 <- Region@data
nam <- c(SSAR_year)
Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
Trade_RE1$avgsar <- rowMeans(Trade_RE1[,SSAR_year])
if(sum(Trade_RE1[,"avgsar"]) > 0){
  Trade_RE1 <- Trade_RE1[order(Trade_RE1[,"avgsar"],decreasing = T),]
  Trade_RE1$cumSSAR <- cumsum(Trade_RE1[,"avgsar"])
  Trade_RE1$cumSSAR <- Trade_RE1$cumSSAR / sum(Trade_RE1[,"avgsar"] )
  idx <- min(which(Trade_RE1$cumSSAR > .9))
  Trade_RE1 <- Trade_RE1[1:idx,]
  csq <- c(csq,Trade_RE1$csquares)
  }
  csq <- unique(csq)

Regionnew2 <- subset(Region, Region@data$csquares %in% csq)    
Regionnew2$incl <- 1
Regionnew2 <- Regionnew2[,"incl"]

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
  
  Trade_RE1 <- Region@data
  nam <- c(SSAR_year)
  Trade_RE1 <- cbind(Trade_RE1, Fisheries[match(Trade_RE1$csquares,Fisheries$csquares), c(nam)])
  Trade_RE1[,c(nam)][is.na(Trade_RE1[,c(nam)])] <- 0
  Trade_RE1$avgsar <- rowMeans(Trade_RE1[,SSAR_year])
  if(sum(Trade_RE1[,"avgsar"]) > 0){
    Trade_RE1 <- Trade_RE1[order(Trade_RE1[,"avgsar"],decreasing = T),]
    Trade_RE1$cumSSAR <- cumsum(Trade_RE1[,"avgsar"])
    Trade_RE1$cumSSAR <- Trade_RE1$cumSSAR / sum(Trade_RE1[,"avgsar"] )
    idx <- min(which(Trade_RE1$cumSSAR > .9))
    Trade_RE1 <- Trade_RE1[1:idx,]
    csq <- c(csq,Trade_RE1$csquares)
  }
  csq <- unique(csq)
  
  Region <- subset(Region, Region@data$csquares %in% csq)    
  Region$incl <- 1
  Region <- Region[,"incl"]
  Regionnew2 <- rbind(Regionnew2,Region)
}
