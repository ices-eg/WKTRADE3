
###### PROCESSING tradeoff-freeze footprint

  setwd(paste(pathdir_nogit,"Producing figures and tables",Assunit,Assregion,sep="/"))  
  dir.create("Trade-offs")
  pathdir_tradeoff <- (paste(pathdir_nogit,"Producing figures and tables",Assunit,Assregion,"Trade-offs",sep="/"))  
  
###### figures and tables for ICES WKTRADE3
  Region <- Region[!(is.na(Region$medlong)),]

# Freeze footprint measure 1
  SSAR_freeze <- paste("surface_sar",c(2012,2013,2014),sep="_")
  Fisheries[,c(SSAR_freeze)][is.na(Fisheries[,c(SSAR_freeze)])] <- 0
  Fisheries$freeze <- rowMeans(Fisheries[,SSAR_freeze])

  freeze <- subset(Fisheries, Fisheries$freeze == 0)
  FisheriesMet <- cbind(FisheriesMet, freeze[match(FisheriesMet$csquares,freeze$csquares), c("freeze")])  
  colnames(FisheriesMet)[ncol(FisheriesMet)] <- "freeze"
  FisheriesMet$freeze[is.na(FisheriesMet$freeze)] <- 1
  
  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  vars  <- c("surface_sar","subsurface_sar","total_weight","total_value","mw_fishinghours")
  years <- c(2015:2018)
  nam <- paste(rep(gears,each=length(vars)),vars,sep="_")
  nam <- paste(rep(nam,each=length(years)),years,sep="_")
  
  FisheriesMet[,nam] <- FisheriesMet[,nam]*FisheriesMet$freeze # if zero, no part of footprint

  # calculate state and impact for all metiers and per metier group for the frozen footprint
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("Impact_continuous_longevity.R")
  source("Habitatstatefishing.R") ## takes a few minutes
  
  setwd(pathdir_tradeoff)
  State_reg_freeze <- State_reg
  save(State_reg_freeze,file="state_freezefootprint.RData")
  FisheriesMet_freeze <- FisheriesMet
  save(FisheriesMet_freeze,file="FisheriesMet_freezefootprint.RData")
  
# Freeze footprint to core region measure 2 
  Fisheries <- Fisheries[order(Fisheries[,"freeze"],decreasing = F),]
  Fisheries$cumSSAR <- cumsum(Fisheries[,"freeze"])
  Fisheries$cumSSAR <- Fisheries$cumSSAR / sum(Fisheries[,"freeze"])
  nb <- min(which(Fisheries$cumSSAR > 0.1))
  
  freeze <- Fisheries[1:nb,]
  freeze$freeze <- 0
  
  FisheriesMet <- cbind(FisheriesMet, freeze[match(FisheriesMet$csquares,freeze$csquares), c("freeze")])  
  colnames(FisheriesMet)[ncol(FisheriesMet)] <- "freeze_core"
  FisheriesMet$freeze_core[is.na(FisheriesMet$freeze_core)] <- 1
  
  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  vars  <- c("surface_sar","subsurface_sar","total_weight","total_value","mw_fishinghours")
  years <- c(2015:2018)
  nam <- paste(rep(gears,each=length(vars)),vars,sep="_")
  nam <- paste(rep(nam,each=length(years)),years,sep="_")
  
  FisheriesMet[,nam] <- FisheriesMet[,nam]*FisheriesMet$freeze_core # if zero, no part of footprint
  
  # calculate state and impact for all metiers and per metier group for the frozen footprint
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("Impact_continuous_longevity.R")
  source("Habitatstatefishing.R") ## takes a few minutes
  
  setwd(pathdir_tradeoff)
  State_reg_freezecore <- State_reg
  save(State_reg_freezecore,file="state_freezecorefootprint.RData")
  FisheriesMet_freezecore <- FisheriesMet
  save(FisheriesMet_freezecore,file="FisheriesMet_freezecorefootprint.RData" )
  
  rm(list= ls()[!(ls() %in% c('pathdir','pathdir_nogit','Assregion','Assunit','Period','AssPeriod'))])
    