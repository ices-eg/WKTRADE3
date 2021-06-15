

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

for (pp in 1:length(gears)){
  datgear <- Region@data
  nam <- paste(rep(paste(gears[pp],"surface_sar",sep="_"),length(AssPeriod)),AssPeriod,sep="_")
  datgear <- cbind(datgear, FisheriesMet[match(datgear$csquares,FisheriesMet$csquares), c(nam)])
  datgear[,c(nam)][is.na(datgear[,c(nam)])] <- 0
  datgear$avgsar <- rowMeans(datgear[,nam])
  if(sum(datgear[,"avgsar"]) > 0){
  
    for(iYear in 1:length(nam)){
        datgear <- datgear[order(datgear[,nam[iYear]],decreasing = T),]
        datgear$cumSSAR <- cumsum(datgear[,nam[iYear]])
        datgear$cumSSAR <- datgear$cumSSAR / sum(datgear[,nam[iYear]] )
        idx <- min(which(datgear$cumSSAR > .9))
        idx <- ifelse(idx == Inf, 0, idx)
        idx2 <- ifelse(idx>0,1,0)
        datgear[,paste("include",iYear)] <- 0
        datgear[,paste("include",iYear)][idx2:idx] <- 1
    }
    datgear$core <- rowSums(datgear[,paste("include",1:6)]) 
    csq_idx <- subset(datgear,datgear$core >= 2)
    csq_idx <- csq_idx$csquares
      }
  csq <- c(csq,csq_idx)
  }
  
  csq <- unique(csq)

RegionJcore <- subset(Region, Region@data$csquares %in% csq)    
RegionJcore$incl <- 1
RegionJcore <- RegionJcore[,"incl"]

for (p in 6:10) {
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
      
      for(iYear in 1:length(nam)){
        datgear <- datgear[order(datgear[,nam[iYear]],decreasing = T),]
        datgear$cumSSAR <- cumsum(datgear[,nam[iYear]])
        datgear$cumSSAR <- datgear$cumSSAR / sum(datgear[,nam[iYear]] )
        idx <- min(which(datgear$cumSSAR > .9))
        idx <- ifelse(idx == Inf, 0, idx)
        idx2 <- ifelse(idx>0,1,0)
        datgear[,paste("include",iYear)] <- 0
        datgear[,paste("include",iYear)][idx2:idx] <- 1
      }
      datgear$core <- rowSums(datgear[,paste("include",1:6)]) 
      csq_idx <- subset(datgear,datgear$core >= 2)
      csq_idx <- csq_idx$csquares
    }
    csq <- c(csq,csq_idx)
  }
  
  csq <- unique(csq)
  
  Region <- subset(Region, Region@data$csquares %in% csq)    
  Region$incl <- 1
  Region <- Region[,"incl"]
  RegionJcore <- rbind(RegionJcore,Region)
}

for (p in 12:26) {
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
      
      for(iYear in 1:length(nam)){
        datgear <- datgear[order(datgear[,nam[iYear]],decreasing = T),]
        datgear$cumSSAR <- cumsum(datgear[,nam[iYear]])
        datgear$cumSSAR <- datgear$cumSSAR / sum(datgear[,nam[iYear]] )
        idx <- min(which(datgear$cumSSAR > .9))
        idx <- ifelse(idx == Inf, 0, idx)
        idx2 <- ifelse(idx>0,1,0)
        datgear[,paste("include",iYear)] <- 0
        datgear[,paste("include",iYear)][idx2:idx] <- 1
      }
      datgear$core <- rowSums(datgear[,paste("include",1:6)]) 
      csq_idx <- subset(datgear,datgear$core >= 2)
      csq_idx <- csq_idx$csquares
    }
    csq <- c(csq,csq_idx)
  }
  
  csq <- unique(csq)
  
  Region <- subset(Region, Region@data$csquares %in% csq)    
  Region$incl <- 1
  Region <- Region[,"incl"]
  RegionJcore <- rbind(RegionJcore,Region)
}

