
# load the grid for the region
  load(paste(pathdir,"1 - Input env",paste(EcoReg,"region_grid_sensitivity.RData",sep="_"),sep="/")) 

# ------------------------------------------------------------------------------
# get sum of all gears fishing data
# ------------------------------------------------------------------------------
  Fisheries <- data.frame(csquares = Region@data$csquares)
  for (i in 1:length(Period)){
    suby <- subset(Fisheries_Atlantic,Fisheries_Atlantic$year == Period[i])
    colnames(suby)[3:7] <- paste(colnames(suby)[3:7],Period[i],sep="_")
    Fisheries <- cbind(Fisheries, suby[match(Fisheries$csquares,suby$c_square), c(3:7)])
  }
  
  setwd(pathdir_nogit)
  save(Fisheries,file=paste(EcoReg,"fisheries.RData",sep="_"))
  
# ------------------------------------------------------------------------------
# get fishing data specified per metier
# ------------------------------------------------------------------------------
  FisheriesMet <- data.frame(csquares = Region@data$csquares)
  metier <- c("DRB_MOL","OT_CRU","OT_MIX_CRU","OT_MIX_CRU_DMF","OT_DMF","OT_MIX","OT_MIX_DMF_BEN",
              "OT_MIX_DMF_PEL","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  metier_name <- metier
  
  for (i in 1: length(Period)){
    for (p in 1:length(metier)){
      suby <- subset(FisheriesMet_Atlantic,
                     FisheriesMet_Atlantic$year == Period[i] &
                     FisheriesMet_Atlantic$gear_category == metier[p])
      colnames(suby)[4:8] <- paste(metier[p],colnames(suby)[4:8],Period[i],sep="_")
      FisheriesMet <- cbind(FisheriesMet, suby[match(FisheriesMet$csquares,suby$c_square), c(4:8)])
    }}
  
  setwd(pathdir_nogit)
  save(FisheriesMet,file=paste(EcoReg,"fisheries_per_metier.RData",sep="_"))
  
# ------------------------------------------------------------------------------
# now combine different metiers following WKTRADE3
# ------------------------------------------------------------------------------ 
  
  # combine OT_MIX
  groups <- c("surface_sar","subsurface_sar","total_weight","total_value","mw_fishinghours")
  year   <- Period
  combine <- c("OT_MIX","OT_MIX_DMF_BEN","OT_MIX_DMF_PEL")
  
  sar    <- paste(combine[1],"surface_sar",year,sep="_")
  ssar   <- paste(combine[1],"subsurface_sar",year,sep="_")
  weight <- paste(combine[1],"total_weight",year,sep="_")
  value  <- paste(combine[1],"total_value",year,sep="_")
  mwhour <- paste(combine[1],"mw_fishinghours",year,sep="_")
  
  for (i in 1:length(year)){
    nam <- paste(combine,groups[1],year[i], sep="_")
    FisheriesMet[,sar[i]]  <- rowSums(FisheriesMet[,nam],na.rm=T)
    nam <- paste(combine,groups[2],year[i], sep="_")
    FisheriesMet[,ssar[i]] <- rowSums(FisheriesMet[,nam],na.rm=T)
    nam <- paste(combine,groups[3],year[i], sep="_")
    FisheriesMet[,weight[i]] <- rowSums(FisheriesMet[,nam],na.rm=T)
    nam <- paste(combine,groups[4],year[i], sep="_")
    FisheriesMet[,value[i]] <- rowSums(FisheriesMet[,nam],na.rm=T)
    nam <- paste(combine,groups[5],year[i], sep="_")
    FisheriesMet[,mwhour[i]] <- rowSums(FisheriesMet[,nam],na.rm=T)
  }
  
  exclude <- c(paste(combine[2],"surface_sar",year,sep="_"),paste(combine[2],"subsurface_sar",year,sep="_"),
               paste(combine[2],"total_weight",year,sep="_"),  paste(combine[2],"total_value",year,sep="_"),
               paste(combine[2],"mw_fishinghours",year,sep="_"),
               paste(combine[3],"surface_sar",year,sep="_"),paste(combine[3],"subsurface_sar",year,sep="_"),
               paste(combine[3],"total_weight",year,sep="_"),  paste(combine[3],"total_value",year,sep="_"),
               paste(combine[3],"mw_fishinghours",year,sep="_"))
  indexcol <- which(names(FisheriesMet) %in% exclude)
  FisheriesMet <- FisheriesMet[,-indexcol]
  
  ### combine OT_CRU
  combine <- c("OT_CRU","OT_MIX_CRU","OT_MIX_CRU_DMF")
  
  sar    <- paste(combine[1],"surface_sar",year,sep="_")
  ssar   <- paste(combine[1],"subsurface_sar",year,sep="_")
  weight <- paste(combine[1],"total_weight",year,sep="_")
  value  <- paste(combine[1],"total_value",year,sep="_")
  mwhour <- paste(combine[1],"mw_fishinghours",year,sep="_")
  
  for (i in 1:length(year)){
    nam <- paste(combine,groups[1],year[i], sep="_")
    FisheriesMet[,sar[i]] <- tt <- rowSums(FisheriesMet[,nam],na.rm=T)
    nam <- paste(combine,groups[2],year[i], sep="_")
    FisheriesMet[,ssar[i]] <- rowSums(FisheriesMet[,nam],na.rm=T)
    nam <- paste(combine,groups[3],year[i], sep="_")
    FisheriesMet[,weight[i]] <- rowSums(FisheriesMet[,nam],na.rm=T)
    nam <- paste(combine,groups[4],year[i], sep="_")
    FisheriesMet[,value[i]] <- rowSums(FisheriesMet[,nam],na.rm=T)
    nam <- paste(combine,groups[5],year[i], sep="_")
    FisheriesMet[,mwhour[i]] <- rowSums(FisheriesMet[,nam],na.rm=T)
  }
  
  exclude <- c(paste(combine[2],"surface_sar",year,sep="_"),paste(combine[2],"subsurface_sar",year,sep="_"),
               paste(combine[2],"total_weight",year,sep="_"),  paste(combine[2],"total_value",year,sep="_"),
               paste(combine[2],"mw_fishinghours",year,sep="_"),
               paste(combine[3],"surface_sar",year,sep="_"),paste(combine[3],"subsurface_sar",year,sep="_"),
               paste(combine[3],"total_weight",year,sep="_"),  paste(combine[3],"total_value",year,sep="_"),
               paste(combine[3],"mw_fishinghours",year,sep="_"))
  indexcol <- which(names(FisheriesMet) %in% exclude)
  FisheriesMet <- FisheriesMet[,-indexcol]
  
  FisheriesMet[,2:ncol(FisheriesMet)][FisheriesMet[,2:ncol(FisheriesMet)] == 0] <- NA
  
  setwd(pathdir_nogit)
  save(FisheriesMet,file=paste(EcoReg,"fisheries_per_metier_comb.RData",sep="_"))
  
  rm(list=ls()[! ls() %in% c("Fisheries_Atlantic","FisheriesMet_Atlantic",
                             "Period","EcoReg","pathdir","pathdir_nogit")])
  