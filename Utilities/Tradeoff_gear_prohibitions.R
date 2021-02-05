
#### remove effort of one gear and check trade-off
  setwd(paste(pathdir_nogit,"Producing figures and tables",Assunit,Assregion,sep="/"))  
  dir.create("Trade-offs")
  pathdir_tradeoff <- (paste(pathdir_nogit,"Producing figures and tables",Assunit,Assregion,"Trade-offs",sep="/"))  

  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")

  for (iGear in 1:10){
    # load fisheries by metier data
    load(paste(pathdir_nogit,paste(EcoReg,"fisheries_per_metier_comb.RData",sep="_"),sep="/")) 
    nam_sar <- paste(paste(gears[iGear],"surface_sar",sep="_"),AssPeriod,sep="_")
    nam_value <- paste(paste(gears[iGear],"total_value",sep="_"),AssPeriod,sep="_")
    nam_weight <- paste(paste(gears[iGear],"total_weight",sep="_"),AssPeriod,sep="_")
  
    # remove all effort from one gear
    FisheriesMet[,c(nam_sar,nam_value,nam_weight)] <- FisheriesMet[,c(nam_sar,nam_value,nam_weight)] * 0
      
    # calculate state and impact PD method 
    setwd(paste(pathdir,"Utilities",sep="/"))
    source("Impact_continuous_longevity.R")
    source("Habitatstatefishing.R") ## takes a few minutes
    
    # save prediction
    setwd(pathdir_tradeoff)
    save(State_reg,file=paste(paste("state",gears[iGear],sep="_"),"RData",sep="."))
    
    # calculate state and impact IL method
    setwd(paste(pathdir,"Utilities",sep="/"))
    source("Impact_inverse_longevity.R")
    source("Habitatstatefishing_inverselongevity.R") ## takes a few minutes
    
    # save prediction
    setwd(pathdir_tradeoff)
    save(State_reg,file=paste(paste("state_IL",gears[iGear],sep="_"),"RData",sep="."))
    
    # save fisheries data
     save(FisheriesMet,file=paste(paste("FisheriesMet",gears[iGear],sep="_"),"RData",sep="."))
  }

  rm(list= ls()[!(ls() %in% c('pathdir','pathdir_nogit','Assregion_index','Assunit_index','Period','AssPeriod',"EcoReg",
                              'Fisheries','FisheriesMet','Region','State_reg','State_reg_IL',"EcoReg_index","Assunit","Assregion"))])
  
    