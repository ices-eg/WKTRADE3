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
regions_with_corefishing <- c(1:3,5:9,12:15,16:21)

namregion <-c("NS","BS","CS","BoBIC")


for (p in 1:4){
  Assregion <- Assregion_index[p]
  EcoReg    <- EcoReg_index[p]
  Assunit <- Assunit_index[p]     
  
  ### load processed file, with longevity and state/impact 
  load(paste(pathdir_nogit,paste(EcoReg,"Fisheries.RData",sep="_"),sep="/")) 
  load(paste(pathdir_nogit,paste(EcoReg,"fisheries_per_metier_comb.RData",sep="_"),sep="/")) 
  setwd(paste(pathdir,"1 - Input env",sep="/"))
  load(paste(EcoReg,"region_grid_sensitivity.RData",sep="_")) 
  
  # add correct column names
  idx <- which(names(Region@data)== "long")
  colnames(Region@data)[idx]  <- "longitude"
  idx <- which(names(Region@data)== "lat")
  colnames(Region@data)[idx]  <- "latitude"
  
  Region <- Region@data[,c("csquares","longitude", "latitude")]
  
# get total sar, value, weight  
  SSAR_year <- paste("surface_sar",AssPeriod,sep="_")
  weight_year <- paste("total_weight",AssPeriod,sep="_")
  value_year <- paste("total_value",AssPeriod,sep="_")
  
  Fisheries[,c(SSAR_year)][is.na(Fisheries[,c(SSAR_year)])] <- 0
  Fisheries$avg <- rowMeans(Fisheries[,SSAR_year])
  quat<-c(-1,0,0.1,0.5,1,5,10,100)
  Fisheries$cat <- as.factor(cut(Fisheries$avg,quat,right=T))
  Fisheries$cat <- as.character(Fisheries$cat)
  Fisheries$cat[Fisheries$cat == "(-1,0]"] <- "NA" 
  Fisheries$cat[Fisheries$cat == "(10,100]"] <- ">10" 
  Fisheries$cat <- as.factor(Fisheries$cat)
  
  Region <- cbind(Region, Fisheries[match(Region$csquares,Fisheries$csquares), c("cat")])
  colnames(Region)[ncol(Region)] <- "MBCG_surface_sar"
  Region$MBCG_surface_sar[is.na(Region$MBCG_surface_sar)] <- "NA"
  
  Fisheries[,c(value_year)][is.na(Fisheries[,c(value_year)])] <- 0
  Fisheries$avg <- rowMeans(Fisheries[,value_year])
  quat<-c(-1,0,100,1000,10000,100000,10000000)
  Fisheries$cat <- as.factor(cut(Fisheries$avg,quat,right=T))
  Fisheries$cat <- as.character(Fisheries$cat)
  Fisheries$cat[Fisheries$cat == "(-1,0]"] <- "NA" 
  Fisheries$cat[Fisheries$cat == "(1e+05,1e+07]"] <- ">1e+05" 
  Fisheries$cat <- as.factor(Fisheries$cat)
  
  Region <- cbind(Region, Fisheries[match(Region$csquares,Fisheries$csquares), c("cat")])
  colnames(Region)[ncol(Region)] <- "MBCG_total_value"
  Region$MBCG_total_value[is.na(Region$MBCG_total_value)] <- "NA"
  
  Fisheries[,c(weight_year)][is.na(Fisheries[,c(weight_year)])] <- 0
  Fisheries$avg <- rowMeans(Fisheries[,weight_year])
  quat<-c(-1,0,100,1000,10000,100000,10000000)
  Fisheries$cat <- as.factor(cut(Fisheries$avg,quat,right=T))
  Fisheries$cat <- as.character(Fisheries$cat)
  Fisheries$cat[Fisheries$cat == "(-1,0]"] <- "NA" 
  Fisheries$cat[Fisheries$cat == "(1e+05,1e+07]"] <- ">1e+05" 
  Fisheries$cat <- as.factor(Fisheries$cat)
  
  Region <- cbind(Region, Fisheries[match(Region$csquares,Fisheries$csquares), c("cat")])
  colnames(Region)[ncol(Region)] <- "MBCG_total_weight"
  Region$MBCG_total_weight[is.na(Region$MBCG_total_weight)] <- "NA"
  
  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")

  for (igear in 1:length(gears)){
    nam <- paste(rep(gears[igear],length(AssPeriod)),"surface_sar",AssPeriod,sep="_")
    FisheriesMet[,c(nam)][is.na(FisheriesMet[,c(nam)])] <- 0
    FisheriesMet$avg <- rowMeans(FisheriesMet[,nam])
    
    quat<-c(-1,0,0.1,0.5,1,5,10,100)
    FisheriesMet$cat <- as.factor(cut(FisheriesMet$avg,quat,right=T))
    FisheriesMet$cat <- as.character(FisheriesMet$cat)
    FisheriesMet$cat[FisheriesMet$cat == "(-1,0]"] <- "NA" 
    FisheriesMet$cat[FisheriesMet$cat == "(10,100]"] <- ">10" 
    FisheriesMet$cat <- as.factor(FisheriesMet$cat)
    
    Region  <- cbind(Region, FisheriesMet[match(Region$csquares,FisheriesMet$csquares), c("cat")])
    id_name <- paste(gears[igear],"surface_sar",sep = "_")
    colnames(Region)[ncol(Region)] <- id_name
    Region[,id_name][is.na(Region[,id_name])] <- "NA"
    
    # value
    nam <- paste(rep(gears[igear],length(AssPeriod)),"total_value",AssPeriod,sep="_")
    FisheriesMet[,c(nam)][is.na(FisheriesMet[,c(nam)])] <- 0
    FisheriesMet$avg <- rowMeans(FisheriesMet[,nam])
    
    quat<-c(-1,0,100,1000,10000,100000,10000000)
    FisheriesMet$cat <- as.factor(cut(FisheriesMet$avg,quat,right=T))
    FisheriesMet$cat <- as.character(FisheriesMet$cat)
    FisheriesMet$cat[FisheriesMet$cat == "(-1,0]"] <- "NA" 
    Fisheries$cat[Fisheries$cat == "(1e+05,1e+07]"] <- ">1e+05" 
    FisheriesMet$cat <- as.factor(FisheriesMet$cat)
    
    Region <- cbind(Region, FisheriesMet[match(Region$csquares,FisheriesMet$csquares), c("cat")])
    id_name <- paste(gears[igear],"total_value",sep = "_")
    colnames(Region)[ncol(Region)] <- id_name
    Region[,id_name][is.na(Region[,id_name])] <- "NA"
 
    # weight
    nam <- paste(rep(gears[igear],length(AssPeriod)),"total_weight",AssPeriod,sep="_")
    FisheriesMet[,c(nam)][is.na(FisheriesMet[,c(nam)])] <- 0
    FisheriesMet$avg <- rowMeans(FisheriesMet[,nam])
    
    quat<-c(-1,0,100,1000,10000,100000,10000000)
    FisheriesMet$cat <- as.factor(cut(FisheriesMet$avg,quat,right=T))
    FisheriesMet$cat <- as.character(FisheriesMet$cat)
    FisheriesMet$cat[FisheriesMet$cat == "(-1,0]"] <- "NA" 
    Fisheries$cat[Fisheries$cat == "(1e+05,1e+07]"] <- ">1e+05" 
    FisheriesMet$cat <- as.factor(FisheriesMet$cat)
    
    Region <- cbind(Region, FisheriesMet[match(Region$csquares,FisheriesMet$csquares), c("cat")])
    id_name <- paste(gears[igear],"total_weight",sep = "_")
    colnames(Region)[ncol(Region)] <- id_name
    Region[,id_name][is.na(Region[,id_name])] <- "NA"
  }
  
  assign(namregion[p], Region)
  
}
  
  data_out <- rbind(BS,NS,CS,BoBIC)
  data_out <- subset(data_out, !(data_out$MBCG_surface_sar == "NA" & 
                                   data_out$MBCG_total_value== "NA" &
                                   data_out$MBCG_total_weight == "NA"))
  
  setwd(pathdir_nogit)
  write.csv(data_out,file="TRADE3 data product.csv",row.names=F)
  